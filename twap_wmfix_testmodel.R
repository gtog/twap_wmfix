# This section of the project is where we'll do the out of sample testing of the model as well as look
# at some performance statistics based on a stylized experiment that is hopefully representative of what
# we'll do in live trading.

# We fit the model using minute interval and daily data from the year 2012. We also have only fit a model
# for the EURUSD fix - TWAP. We now need to see how well the model performs using new data.
# What this really involves is creating a new set of response or LHS variables. Given a model, we just want
# to see how well the model fits a new set of LHS data. Suppose we fit the model with 2010 data, then
# we want to run the fitted model over, say, 2011 and 2012 data to judge the out of sample performance
# of the model. To do this easily we need a LHS variable constructor function:

require(glmulti)
require(leaps)
require(rJava)
require(MASS)
suppressWarnings(library(zoo))
suppressWarnings(library(xts))
suppressWarnings(library(quantmod))
suppressWarnings(library(PerformanceAnalytics))
suppressWarnings(library(ggplot2))
suppressWarnings(library(plyr))
suppressWarnings(library(knitr))
suppressWarnings(library(chron))
suppressWarnings(library(glmulti))
suppressWarnings(library(Quandl))

# We also need the makeTimeStamps() function from twap_wmfix_buildmodel.R...
makeLHS<-function(data,binary) {
  # Takes a data object and year and constructs a quantmod OHLC object which is the FIX - TWAP for that
  # variable. The data should be one of the minute interval data sets in OHLC, xts format.
  # If binary==TRUE, then a binary response variable is assembled such that if FIX-TWAP > 0 -> 1, else 0.
  # If binary==TRUE, then the function makes the first column the binary indicator and the second
  # column the potential p/l = Fixing - TWAP.
  
  fx.time.stamps<-makeTimeStamps(data$time_stamp)
  prices<-Cl(data)
  fx.allt<-xts(prices,order.by=fx.time.stamps)
  names(fx.allt)<-"fxrate"
  
  fix.prices<-fx.allt[index(fx.allt,0)$hour==11 & index(fx.allt,0)$min==0] # vector of all 11am prices...
  fix.dates<-as.Date(index(fix.prices,0))
  fix.prices<-xts(fix.prices,fix.dates)
  names(fix.prices)<-"fixing.rate"
  
  hour10to11<-fx.allt[(index(fx.allt,0)$hour>=10 & index(fx.allt,0)$hour<11)] # A vector of all the prices from 10am - 11am NYT.
  twap10to11<-aggregate(hour10to11$fxrate,as.Date(index(hour10to11,0)),mean) # THe TWAP from 10 to 11am NYT.
  names(twap10to11)<-"twap"
  
  temp<-merge.xts(fix.prices,twap10to11,join="inner") # this is done to resolve vector length issues.
  potential.pl<-temp$fixing.rate-temp$twap
  
  if (binary==TRUE) {
    LHS<-sign(temp$fixing.rate-temp$twap)
    names(LHS)<-"binary"
    LHS[LHS$binary<0]<-0
    LHS[LHS$binary>0]<-1
  } else {
    LHS<-temp$fixing.rate-temp$twap
  }
  
  lhs.l<-xts(rep(0,length(LHS)),index(LHS,0))
  lhs.c<-xts(rep(0,length(LHS)),index(LHS,0))
  lhs.v<-xts(rep(0,length(LHS)),index(LHS,0))
  lhs.a<-xts(rep(0,length(LHS)),index(LHS,0))
  LHS<-merge.xts(LHS,potential.pl,lhs.l,lhs.c,lhs.v,lhs.a)
  LHS<-as.quantmod.OHLC(LHS,
                        col.names = c("Open", "High",
                                      "Low", "Close",
                                      "Volume", "Adjusted"),
                        name = NULL)
  return(LHS)
}
makeTimeStamps<-function(rawts) {
  # Takes raw time stamp data from histdata.com downloads and converts it into a POSIXct format
  # that we can pass to xts or other time based functions.
  dates<-apply(as.matrix(rawts),MARGIN=2,FUN=substr,1,8) # character vector of extracted dates
  times<-apply(as.matrix(rawts),MARGIN=2,FUN=substr,10,15) # character vector of extracted times
  
  # Add hypen to dates...
  year<-substr(dates,1,4)
  month<-substr(dates,5,6)
  day<-substr(dates,7,8)
  hour<-substr(times,1,2)
  min<-substr(times,3,4)
  sec<-substr(times,5,6)
  newstamps<-paste(year,"-",month,"-",day," ",hour,":",min,":",sec,sep="")
  ret<-strptime(newstamps,"%Y-%m-%d %H:%M:%S")
  return(ret)
}
getData<-function(tickers,datasrc){
  for (i in 1:length(tickers)){
    cat(tickers[i],i,"\n")
    getSymbols(tickers[i],src=datasrc,
               auto.assign=getOption("getSymbols.auto.assign",TRUE),
               env=parent.frame())
  }
}
makeIndex<-function(x,inv,ret){
  # Takes an xts object x and returns an index starting at 100 and evolving as the log returns of x.
  # The inv flag tells whether or not to invert the series before calculating returns.
  # The ret flag tells whether or not we have been passed a series of returns already.
  init.val<-100
  dts<-index(x,0)
  if (inv==TRUE) data<-1/x else data<-x
  if (ret==TRUE){ # we have a series of returns...
    ret.series<-x
  } else {
    ret.series<-periodReturn(data,period="daily",subset=NULL,type="log")
    dts<-index(ret.series,0)
  }
  n<-length(ret.series)
  new.series<-ret.series
  new.series[1]<-init.val
  
  for (i in 2:n){
    new.series[i]<-(1+ret.series[i-1])*new.series[i-1]
  }
  names(new.series)<-c("index")
  return(new.series)
} # My custom index funtion for converting indices to 100 based at inception.
getOHLC<-function(assets,OHLC){
  # Takes a list of assets and returns either the Open, High, Low, or Close depending
  # on the passed value of HLOC. Return value is of type xts/zoo.
  ret<-NULL
  for (i in 1:length(assets)){
    if (OHLC=="O" || OHLC=="Open"){
      ret<-cbind(ret,assets[[i]][,1])
    } else {
      if (OHLC=="H" || OHLC=="High"){
        ret<-cbind(ret,assets[[i]][,2])
      } else {
        if (OHLC=="L" || OHLC=="Low"){
          ret<-cbind(ret,assets[[i]][,3])
        } else {
          if (OHLC=="C" || OHLC=="Close"){
            ret<-cbind(ret,assets[[i]][,4])
          }
        }
      }
    }
  }
  return(ret)
}

# Now that we have a LHS constructor function that returns the LHS, the TWAP, and the fixing prices, 
# we can move on to setting some variables to pass to the function and making a new response variable.

data.names<-c("time_stamp","open_bid","high_bid","low_bid","close_bid","volume")

# If any other pair other than eurusd, use the read.table(). See twap_wmfix_getdata lines 116-127.
pair<-"eurusd"

training.year=c("2007")
train.data<-read.table(paste("~/R/R Projects/data/",training.year,"/DAT_ASCII_EURUSD_M1_",training.year,".csv",sep=""), sep=";", quote="\"")
names(train.data)<-data.names
first.train.date<-c("2007-01-03/")

new.year=c("2010")
new.data<-read.csv(paste("~/R/R Projects/data/",new.year,"/DAT_ASCII_EURUSD_M1_",new.year,".csv",sep=""), sep=";", quote="\"")
names(new.data)<-data.names
first.new.date<-c("2010-01-03/")

# Check to make sure we have 2 good looking data sets from different years...
head(train.data)
head(new.data)

# That all looks good. Don't proceed until the data looks proper. It should start at some time on January 1-5 and be 1 year's worth
# of minute interval data.

# Now, let's make our response (LHS) variable for our model. We have the function built and the data sets loaded.
train.LHS<-makeLHS(train.data,binary=FALSE)

# The next step is to make a data frame that contains our LHS variable in the first column and all the independent (RHS)
# variables in the subsequent columns. First we need to make sure we use getData() to load the variables we want. 
# Note here, that we are using information from running the model a few times and eliminating variables we found
# to be non-informative. If you were approaching this with no prior knowledge, you would want to use getData() to 
# bring in more candidate independent variables.

data.source<-c("yahoo")
model.tickers<-c("^IPC","DOW","^BSESN","^GDAXI","^SSMI","^TA100") #,"ELD","EMLC")
fx.fred<-c("DEXUSAL","DEXINUS","DEXBZUS","DEXCAUS","DEXUSEU","DEXJPUS","DEXMXUS","DEXKOUS")
suppressWarnings(getData(model.tickers,data.source))
suppressWarnings(getData(fx.fred,"FRED"))

# Now assemble the data frame...edit this area to make the model you want.
# Recall that OpCl(x) returns: [Cl(x)t - Op(x)t]/Op(x)t
# This is the arithmetic return from buying at the open and selling at the close on
# day t. This information is contemporaneous to our TWAP calculation and cannot be known
# in time to make predictions. Thus, we need OpCl(x)t-1 for each x. In general, the 
# formula we are trying to estimate is:
# Yt = Bo + Sum(Bi*OpCl(xi)t-1 + et)

train.df<-na.omit(merge.xts(Op(train.LHS),OpCl(lag(IPC[first.train.date],1)),join="left"))
train.df<-merge.xts(train.df,OpCl(lag(DOW,1)),join="inner")
train.df<-merge.xts(train.df,OpCl(lag(BSESN,1)),join="inner")
train.df<-merge.xts(train.df,OpCl(lag(GDAXI,1)),join="inner")
train.df<-merge.xts(train.df,OpCl(lag(SSMI,1)),join="inner")
train.df<-merge.xts(train.df,OpCl(lag(TA100,1)),join="inner")
names(train.df)<-c("LHS.Open","OpClL1.IPC","OpClL1.DOW","OpClL1.BSESN","OpClL1.GDAXI","OpClL1.SSMI","OpClL1.TA100")
# Same with Currency returns. We don't know today's currency return yet. We only know
# yesterday's return. This known return is, relative to today: ln(CCy(t-2)/Ccy(t-1))
train.df<-merge.xts(train.df,na.omit(lag(Return.calculate(DEXUSAL,method="log"),1)),join="inner") # AUDUSD 
#train.df<-merge.xts(train.df,na.omit(Return.calculate(DEXINUS)),join="inner") # USDINR
train.df<-merge.xts(train.df,na.omit(lag(Return.calculate(DEXBZUS,method="log"),1)),join="inner") # USDBRL
#train.df<-merge.xts(train.df,na.omit(Return.calculate(DEXCAUS)),join="inner") # USDCAD
train.df<-merge.xts(train.df,na.omit(lag(Return.calculate(DEXUSEU,method="log"),1)),join="inner") # EURUSD
train.df<-merge.xts(train.df,na.omit(lag(Return.calculate(DEXJPUS,method="log"),1)),join="inner") # USDJPY
train.df<-merge.xts(train.df,na.omit(lag(Return.calculate(DEXMXUS,method="log"),1)),join="inner") # USDMXN
#train.df<-merge.xts(train.df,na.omit(Return.calculate(DEXKOUS)),join="inner") # USDKRW

# NEW: Let's add some lagged variables of the currency pair were modeling. For example, let's
# add the first 3 daily lags to the model and see what happens out of sample...
ccy.lag1<-na.omit(lag(Return.calculate(DEXUSEU,method="log"),2))
names(ccy.lag1)=c("DEXUSEU.1")
ccy.lag2<-na.omit(lag(Return.calculate(DEXUSEU,method="log"),3))
names(ccy.lag2)=c("DEXUSEU.2")

train.df<-merge.xts(train.df,ccy.lag1,join="inner") # ccy pair returns lagged 1 day
train.df<-merge.xts(train.df,ccy.lag3,join="inner") # lagged 3 days

train.df<-as.data.frame(train.df)

# Let's have a look and make sure that looks good...
head(train.df)

# Now that we have a good data frame (albeit reduced in length due to so many incongruous dates), we can proceed to estimation.
# We'll use level==2 to allow for interaction terms and we'll use the genetic algorithm to help things along. Best to go have
# a coffee while this one runs...

train.fit<-glmulti(LHS.Open~., 
              data=train.df, 
              intercept=TRUE,
              level=2,
              marginality=FALSE,
              minsize=-1, # -1 = no constraint
              maxsize=-1,
              minK=-1,
              maxK=-1,
              crit=aic, 
              fitfunc=lm, 
              method="g", # "h"=exhaustive, "g"=genetic algorithm, "l"=very fast, exhaustive, branch and bound, "d"=simple summary
              plotty=FALSE, #plot progression of IC profile while running...
              report=TRUE,
              confsetsize=1000)

# We can store the formula for our "best" model from the genetic algorithm in train.form:
train.form<-as.formula(summary(train.fit)$bestmodel)
train.model<-lm(train.form,data=train.df)
# Let's have a look a summary of our derived model:
summary(train.model) 

# Let's look at how well our model fits the actual values in the training data set:
train.actuals<-as.vector(train.df$LHS.Open)
train.fitted<-as.vector(fitted(train.model))
plot(train.actuals,type="l",col="dark grey")
lines(train.fitted,col="blue")

# We can also look at a plot of the residuals to see if there are any obvious problems:
plot(residuals(train.model),type="l",col="red")
lines(train.fitted,col="blue")

# We really want to know, "What percentage of the time are we on the correct side of the trade?" The answer is...
train.signal.perf<-sign(train.fitted)*sign(train.actuals)
plot(density(train.signal.perf),type="l",col="red")
count(train.signal.perf<0)

#...about 77% of the time. That's all well and good, but in order to have faith in the model, we need to 
# see how the model performs when presented with NEW data. That is, we need to measure OUT OF SAMPLE performance.
# Well, we've prepared for that with the creation of the new.data set. For this part of the process, we'll need to
# create a new data frame with the new data:

new.LHS<-makeLHS(new.data) # The new response variable...

new.df<-na.omit(merge.xts(Op(new.LHS),OpCl(IPC[first.new.date]),join="left"))
names(new.df)<-c("LHS.Open","OpCl.IPC")
new.df<-merge.xts(new.df,OpCl(DOW),join="inner")
new.df<-merge.xts(new.df,OpCl(BSESN),join="inner")
new.df<-merge.xts(new.df,OpCl(GDAXI),join="inner")
new.df<-merge.xts(new.df,OpCl(SSMI),join="inner")
new.df<-merge.xts(new.df,OpCl(TA100),join="inner")
new.df<-merge.xts(new.df,na.omit(Return.calculate(DEXUSAL)),join="inner") # AUDUSD 
new.df<-merge.xts(new.df,na.omit(Return.calculate(DEXINUS)),join="inner") # USDINR
new.df<-merge.xts(new.df,na.omit(Return.calculate(DEXBZUS)),join="inner") # USDBRL
new.df<-merge.xts(new.df,na.omit(Return.calculate(DEXCAUS)),join="inner") # USDCAD
new.df<-merge.xts(new.df,na.omit(Return.calculate(DEXUSEU)),join="inner") # EURUSD
new.df<-merge.xts(new.df,na.omit(Return.calculate(DEXJPUS)),join="inner") # USDJPY
new.df<-merge.xts(new.df,na.omit(Return.calculate(DEXMXUS)),join="inner") # USDMXN
new.df<-merge.xts(new.df,na.omit(Return.calculate(DEXKOUS)),join="inner") # USDKRW

head(new.df)

# We don't want to estimate a new model, we want to use the one estimated on the training data: train.form
new.model<-lm(train.form,data=new.df)
summary(new.model)

new.actuals<-as.vector(new.df$LHS.Open)
new.fitted<-as.vector(fitted(new.model))
plot(new.actuals,type="l",col="dark grey")
lines(new.fitted,col="blue")

plot(residuals(new.model),type="l",col="red")

# So, once we're in production, how often are we on the right side of the trade?
new.signal.perf<-sign(new.fitted)*sign(new.actuals)
plot(density(new.signal.perf),col="red",ylim=c(0,1.3))
lines(density(train.signal.perf),col="dark grey")
count(new.signal.perf<0)

# The model performance has degraded on an out of sample basis from 23% error rate to 26% error rate.
# Whether that is acceptable or not is something we'll have to debate. We can easily now test the performance
# over many NEW data sets and see how we fare, assuming we never update the coefficients of the model, or
# perhaps under some other updating rule (daily, monthly, quarterly updating etc.). Ok, so let's run all the years
# we have. Here are the steps:
# 1. Set new.year
# 2. Get the new data
# 3. make new LHS
# 4. make new DF
# 5. make new model
# 6. plot new actuals vs. new fitted
# 7. return a list of fitted, actuals, and signal performance.

sample.years<-c("2008","2009","2010","2011","2012")
sample.model<-train.model # Currently 2007 trained...

runSamples<-function(years,trainedmodel,binary){
  # this function will run through a bunch of years and compare the results to whatever model is stored
  # in trainedmodel. trainedmodel should be of class "lm" or "glm".
  # years should be a vector of years, for example: years=c("2008","2009","2010","2012")
  retlist<-list()
  
  for (i in 1:length(years)) {
    newyear<-years[i]
    if (years[i]=="2012" || years[i]=="2006") separ=c(",") else separ=c(";")
    newdata<-read.csv(paste("~/R/R Projects/data/",newyear,"/DAT_ASCII_EURUSD_M1_",newyear,".csv",sep=""), sep=separ, quote="\"")
    names(newdata)<-c("time_stamp","open_bid","high_bid","low_bid","close_bid","volume")
    firstnewdate<-substr(newdata$time_stamp[1],0,8)
    firstnewdate<-paste(substr(firstnewdate,0,4),"-",substr(firstnewdate,5,6),"-",substr(firstnewdate,7,8),"/",sep="")
    
    cat(firstnewdate)
    
    newLHS<-makeLHS(newdata,binary)
    
    newdf<-na.omit(merge.xts(Op(newLHS),OpCl(IPC[firstnewdate]),join="left"))
    names(newdf)<-c("LHS.Open","OpCl.IPC")
    newdf<-merge.xts(newdf,OpCl(DOW),join="inner")
    newdf<-merge.xts(newdf,OpCl(BSESN),join="inner")
    newdf<-merge.xts(newdf,OpCl(GDAXI),join="inner")
    newdf<-merge.xts(newdf,OpCl(SSMI),join="inner")
    newdf<-merge.xts(newdf,OpCl(TA100),join="inner")
    newdf<-merge.xts(newdf,na.omit(Return.calculate(DEXUSAL)),join="inner") # AUDUSD 
    newdf<-merge.xts(newdf,na.omit(Return.calculate(DEXINUS)),join="inner") # USDINR
    newdf<-merge.xts(newdf,na.omit(Return.calculate(DEXBZUS)),join="inner") # USDBRL
    newdf<-merge.xts(newdf,na.omit(Return.calculate(DEXCAUS)),join="inner") # USDCAD
    newdf<-merge.xts(newdf,na.omit(Return.calculate(DEXUSEU)),join="inner") # EURUSD
    newdf<-merge.xts(newdf,na.omit(Return.calculate(DEXJPUS)),join="inner") # USDJPY
    newdf<-merge.xts(newdf,na.omit(Return.calculate(DEXMXUS)),join="inner") # USDMXN
    newdf<-merge.xts(newdf,na.omit(Return.calculate(DEXKOUS)),join="inner") # USDKRW

    dts<-as.Date(index(newdf,0))
    if (binary) {
      newmod<-glm(traindedmodel,data=newdf,family=binary(link="logit"))
    } else {
      newmod<-lm(traindedmodel,data=newdf)
    }
    
    newactuals<-as.vector(newdf$LHS.Open)
    newfitted<-as.vector(fitted(newmod))
    plot(newactuals,type="l",col="dark grey",main=paste("Fit vs. Actuals in Year= ",years[i]))
    lines(newfitted,col="blue")
    
    perf<-NULL
    if (binary) {
      for (i in 1:length(newfitted)){
        if ((newfitted[i]>.5 && newactuals[i]>.5) || (newfitted[i]<.5 && newactuals[i]<.5)){
          perf[i]<-1 # Trade was successful
        } else {
          perf[i]<-0 # Trade was not successful.
        }
      }
      err.rate<-count(perf==0)$freq[2]/length(perf)
    } else {
      perf<-sign(newfitted)*sign(newactuals)
      err.rate<-count(perf<0)$freq[2]/length(perf)
    }
    retlist[[i]]<-as.data.frame(cbind(dts,newfitted,newactuals,perf))
    names(retlist[[i]])<-c("date","fitted","actual","sig.perf")
    
    cat("Sample year complete. Error rate was: ",round(err.rate,4),"Starting next sample year.","\n")
  }
  cat("Finished all years. Returning data list and exiting.","\n")
  return(retlist)
}
out<-runSamples(sample.years,sample.model,binary=FALSE)

# Let's create some strategy performance metrics...
# For each out-of-sample year, we want to look at each day and measure how well we did. We'll store this 
# information in a new performance vector containing:
# 1. The trade date
# 2. What trade we did
# 3. The pips we captured or lost on the trade.
# The 'out' variable we created above is enough to construct our performance vector.

# For each year we've collected, look at each day...
# If the variable sig.perf==1, then we are at least doing the correct trade...
# In this case our P/L = +actual.
# If the variable sig.perf==-1, then we are on the wrong side of the trade...
# In this case, our P/L = -actual. 

profit<-NULL
for (i in 1:length(out)) {
  p<-xts(out[[i]]$sig.perf*abs(out[[i]]$actual),as.Date(out[[i]]$date))
  profit<-append(profit,p)
}
profit.index<-makeIndex(profit,inv=FALSE,ret=TRUE)
plot.xts(profit.index, main="Out of Sample Performance: Cumulative")

# or...we can look at each year's performance.
par(mfrow=c(2,2))

for (i in 1:length(out)){
  p<-xts(out[[i]]$sig.perf*abs(out[[i]]$actual),as.Date(out[[i]]$date))
  p<-makeIndex(p,inv=FALSE,ret=TRUE)
  y<-substr(index(p,0)[1],0,4)
  plot.xts(p,main=paste("Out of Sample Performance: ",y))
  rm(p)
}

# Looking at the error distributions...
err<-list()
for (i in 1:length(out)){
  err[[i]]<-subset(-1*abs(out[[i]]$actual),subset=c(out[[i]]$sig.perf<0))
}
plot(density(err[[1]]),col="dark grey",ylim=c(0,1100),xlim=c(-.0060,.0010),
     main="Error Densities for each sample year: 2008:2012")
for (i in 2:length(out)) {
  lines(density(err[[i]]),col="dark grey")
}
merr<-sapply(err,MARGIN=2,FUN=mean)
plot(density(merr),col="red",main="Density of Mean Error")
par(mfrow=c(1,1))

###### Experiment: Create a new response variable which is either "Above" == 1 or "Below"==0
# to represent cases where the FIX - TWAP > 0 and FIX - TWAP < 0. This is similar to our sign.perf
# variable which tries to capture whether we got the sign right. Meaning, did we have the right
# trade on, regardless of the magnitude of the prediction error. By converting to a binary response
# variable and estimating a logit regression, we should obtain similar results and perhaps our 
# response variable will have more intuitive meaning. Prediction values above .5 indicate that the
# fixing will come in above the TWAP and vice versa for values below .5.

# To do this, I have modified the makeLHS() function to take a "binary" parameter. If set to true,
# the makeLHS() function will return a binary response varible. 

train.bin.LHS<-makeLHS(train.data,binary=TRUE)

train.df<-na.omit(merge.xts(Op(train.bin.LHS),OpCl(lag(IPC[first.train.date],1)),join="left"))
train.df<-merge.xts(train.df,OpCl(lag(DOW,1)),join="inner")
train.df<-merge.xts(train.df,OpCl(lag(BSESN,1)),join="inner")
train.df<-merge.xts(train.df,OpCl(lag(GDAXI,1)),join="inner")
train.df<-merge.xts(train.df,OpCl(lag(SSMI,1)),join="inner")
train.df<-merge.xts(train.df,OpCl(lag(TA100,1)),join="inner")
names(train.df)<-c("LHS.Open","OpClL1.IPC","OpClL1.DOW","OpClL1.BSESN","OpClL1.GDAXI","OpClL1.SSMI","OpClL1.TA100")
train.df<-merge.xts(train.df,na.omit(lag(Return.calculate(DEXUSAL,method="log"),1)),join="inner") # AUDUSD 
train.df<-merge.xts(train.df,na.omit(lag(Return.calculate(DEXBZUS,method="log"),1)),join="inner") # USDBRL
train.df<-merge.xts(train.df,na.omit(lag(Return.calculate(DEXUSEU,method="log"),1)),join="inner") # EURUSD
train.df<-merge.xts(train.df,na.omit(lag(Return.calculate(DEXJPUS,method="log"),1)),join="inner") # USDJPY
train.df<-merge.xts(train.df,na.omit(lag(Return.calculate(DEXMXUS,method="log"),1)),join="inner") # USDMXN
train.df<-as.data.frame(train.df)
head(train.df)


# Now we have a data frame with a binary response variable and all the corresponding RHS variables.
# Now we can estimate the model using glm instead of lm with a family designation of "binary"...

my.glm<-function (formula,data,...) {
  glm(as.formula(paste(deparse(formula))),data=data,family=binomial(link="logit"),...)
}

train.fit<-glmulti(LHS.Open~., 
                   data=train.df, 
                   intercept=TRUE,
                   level=2,
                   marginality=FALSE,
                   minsize=-1, # -1 = no constraint
                   maxsize=-1,
                   minK=-1,
                   maxK=-1,
                   crit=aic, 
                   fitfunc=my.glm, 
                   method="g", # "h"=exhaustive, "g"=genetic algorithm, "l"=very fast, exhaustive, branch and bound, "d"=simple summary
                   plotty=FALSE, #plot progression of IC profile while running...
                   report=TRUE,
                   confsetsize=1000)

# We can store the formula for our "best" model from the genetic algorithm in train.form:
train.form<-as.formula(summary(train.fit)$bestmodel)
train.model<-glm(train.form,data=train.df,family=binomial(link="logit"))
# Let's have a look a summary of our derived model:
summary(train.model) 

# Let's look at how well our model fits the actual values in the training data set:
train.actuals<-as.vector(train.df$LHS.Open)
train.fitted<-as.vector(fitted(train.model))
train.fitted[train.fitted<.5]<-0
train.fitted[train.fitted>.5]<-1
plot(train.actuals,type="l",col="grey")
lines(train.fitted,col="light blue")
abline(h=c(.50),col="green")

train.err<-abs(train.actuals-train.fitted) # 0 means no error, 1 means trade error.
plot(density(train.err))
count(train.err) # Very similar error rate to previous estimation.

sample.years<-c("2008","2009","2010","2011","2012")
sample.model<-train.model # Currently 2007 trained...
out.binary<-runSamples(sample.years,sample.model,binary=TRUE)

profit<-NULL
for (i in 1:length(out)) {
  p<-xts(out.binary[[i]]$sig.perf*abs(out[[i]]$actual),as.Date(out[[i]]$date))
  profit<-append(profit,p)
}
profit.index<-makeIndex(profit,inv=FALSE,ret=TRUE)
plot.xts(profit.index, main="Out of Sample Performance: Cumulative")

# or...we can look at each year's performance.
par(mfrow=c(2,2))

for (i in 1:length(out)){
  p<-xts(out[[i]]$sig.perf*abs(out[[i]]$actual),as.Date(out[[i]]$date))
  p<-makeIndex(p,inv=FALSE,ret=TRUE)
  y<-substr(index(p,0)[1],0,4)
  plot.xts(p,main=paste("Out of Sample Performance: ",y))
  rm(p)
}

# Looking at the error distributions...
err<-list()
for (i in 1:length(out)){
  err[[i]]<-subset(-1*abs(out[[i]]$actual),subset=c(out[[i]]$sig.perf<0))
}
plot(density(err[[1]]),col="dark grey",ylim=c(0,1100),xlim=c(-.0060,.0010),
     main="Error Densities for each sample year: 2008:2012")
for (i in 2:length(out)) {
  lines(density(err[[i]]),col="dark grey")
}
merr<-sapply(err,MARGIN=2,FUN=mean)
plot(density(merr),col="red",main="Density of Mean Error")
par(mfrow=c(1,1))
