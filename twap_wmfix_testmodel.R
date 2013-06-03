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
require(fUnitRoots)
require(tseries)
require(rms)

suppressWarnings(library(zoo))
suppressWarnings(library(xts))
suppressWarnings(library(quantmod))
suppressWarnings(library(PerformanceAnalytics))
suppressWarnings(library(ggplot2))
suppressWarnings(library(plyr))
suppressWarnings(library(knitr))
suppressWarnings(library(chron))
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
    actuals.pips<-Hi(newLHS)
    
    cat("new LHS created successfully...","\n")
    
    newdf<-na.omit(merge.xts(Op(newLHS),OpCl(lag(IPC[firstnewdate],1)),join="left"))
    newdf<-merge.xts(newdf,OpCl(lag(DOW,1)),join="inner")
    newdf<-merge.xts(newdf,OpCl(lag(BSESN,1)),join="inner")
    newdf<-merge.xts(newdf,OpCl(lag(GDAXI,1)),join="inner")
    newdf<-merge.xts(newdf,OpCl(lag(SSMI,1)),join="inner")
    newdf<-merge.xts(newdf,OpCl(lag(TA100,1)),join="inner")
    newdf<-merge.xts(newdf,na.omit(lag(diff(DSWP2,1),1)),join="inner")
    names(newdf)<-c("LHS.Open","IPC","DOW","BSESN","GDAXI","SSMI","TA100","DSWP2")
    
    newdf<-merge.xts(newdf,na.omit(lag(Return.calculate(DEXUSAL),1)),join="inner") # AUDUSD 
    newdf<-merge.xts(newdf,na.omit(lag(Return.calculate(DEXINUS),1)),join="inner") # USDINR
    newdf<-merge.xts(newdf,na.omit(lag(Return.calculate(DEXBZUS),1)),join="inner") # USDBRL
    newdf<-merge.xts(newdf,na.omit(lag(Return.calculate(DEXCAUS),1)),join="inner") # USDCAD
    newdf<-merge.xts(newdf,na.omit(lag(Return.calculate(DEXUSEU),1)),join="inner") # EURUSD
    newdf<-merge.xts(newdf,na.omit(lag(Return.calculate(DEXJPUS),1)),join="inner") # USDJPY
    newdf<-merge.xts(newdf,na.omit(lag(Return.calculate(DEXMXUS),1)),join="inner") # USDMXN
    newdf<-merge.xts(newdf,na.omit(lag(Return.calculate(DEXKOUS),1)),join="inner") # USDKRW
    
    dts<-as.Date(index(newdf,0))
    if (binary) {
      newmod<-glm(trainedmodel,data=newdf,family=binomial(link="logit"))
    } else {
      newmod<-lm(trainedmodel,data=newdf)
    }
    
    newdf<-merge.xts(newdf,na.omit(actuals.pips),join="inner") # Add the actuals.pips column, but after estimation...
    
    newactuals<-as.vector(newdf$LHS.Open)
    newfitted<-as.vector(fitted(newmod))
    plot(newactuals,type="l",col="dark grey",main=paste("Fit vs. Actuals in Year= ",years[i]))
    lines(newfitted,col="blue")
    
    perf<-NULL
    if (binary) {
      for (j in 1:length(newfitted)){
        if ((newfitted[j]>.5 && newactuals[j]>.5) || (newfitted[j]<.5 && newactuals[j]<.5)){
          perf[j]<-1 # Trade was successful
        } else {
          perf[j]<-0 # Trade was not successful.
        }
      }
      err.rate<-count(perf==0)$freq[2]/length(perf)
    } else {
      perf<-sign(newfitted)*sign(newactuals)
      err.rate<-count(perf<0)$freq[2]/length(perf)
    }
    
    newdf<-merge.xts(newdf,perf,join="inner") # Add the performance stats...
    
    retlist[[i]]<-data.frame(dts,newfitted,newdf$LHS.Open,perf,newdf$LHS.High)
    names(retlist[[i]])<-c("dates","fitted","actual","sig.perf","actuals.pips")
    
    cat("Sample year complete. Error rate was: ",round(err.rate,4),"Starting next sample year.","\n")
  }
  cat("Finished all years. Returning data list and exiting.","\n")
  return(retlist)
} # Function for running out of sample years.

# Now that we have a LHS constructor function that returns the LHS, the TWAP, and the fixing prices, 
# we can move on to setting some variables to pass to the function and making a new response variable.
# If any other pair other than eurusd, use the read.table(). See twap_wmfix_getdata lines 116-127.

# Set some initial parameters...
pair<-"eurusd"
data.names<-c("time_stamp","open_bid","high_bid","low_bid","close_bid","volume")
training.year=c("2007")
train.data<-read.table(paste("~/R/R Projects/data/",training.year,"/DAT_ASCII_EURUSD_M1_",training.year,".csv",sep=""), sep=";", quote="\"")
names(train.data)<-data.names
first.train.date<-c("2007-01-03/")

# Check to make sure the data looks ok...
head(train.data)

# Call makeLHS() to construct the response variable for our regression...
train.LHS<-makeLHS(train.data,binary=FALSE)

# While were at it, let's run an Augmented Dickey-Fuller test on the response variable we just created.
# If the variable isn't stationary, we'll have problems. Given the nature of the variable, I would be surprised
# if it weren't stationary:
adf<-adf.test(train.LHS$LHS.Open,alternative="stationary")
adf

# The next step is to make a data frame that contains our LHS variable in the first column and all the independent (RHS)
# variables in the subsequent columns. First we need to make sure we use getData() to load the variables we want. 
# Note here, that we are using information from running the model a few times and eliminating variables we found
# to be non-informative. If you were approaching this with no prior knowledge, you would want to use getData() to 
# bring in more candidate independent variables:

data.source<-c("yahoo")
model.tickers<-c("^IPC","^TA100","DOW","^BSESN","^GDAXI","^SSMI") #,"^TA100","^FCHI","DJC",
                 #"^AORD","^SSEC","^HSI","^BSESN","^JKSE","^KLSE","^N225","^NZ50","^STI","^KS11","^TWII",
                 #"^ATX","^BFX","^AEX","^FTSE","FEU")
fx.fred<-c("DEXUSAL","DEXUSEU","DEXINUS","DEXBZUS","DEXCAUS","DEXMXUS","DEXKOUS","DEXJPUS")
rate.fred<-c("DSWP2")

suppressWarnings(getData(model.tickers,data.source))
suppressWarnings(getData(fx.fred,"FRED"))
suppressWarnings(getData(rate.fred,"FRED"))

# Now assemble the data frame...edit this area to make the model you want.
# Recall that OpCl(x) returns: [Cl(x)t - Op(x)t]/Op(x)t
# This is the arithmetic return from buying at the open and selling at the close on
# day t. This information is contemporaneous to our TWAP calculation and cannot be known
# in time to make predictions. Thus, we need OpCl(x)t-1 for each x. In general, the 
# formula we are trying to estimate is:
# Yt = Bo + Sum(Bi*OpCl(xi)t-1 + err)

train.df<-na.omit(merge.xts(Op(train.LHS),OpCl(lag(IPC[first.train.date],1)),join="left"))
#train.df<-na.omit(merge.xts(train.df,OpCl(lag(MXX,1)),join="inner"))
train.df<-na.omit(merge.xts(train.df,OpCl(lag(DOW,1)),join="inner"))
train.df<-na.omit(merge.xts(train.df,OpCl(lag(BSESN,1)),join="inner"))
train.df<-na.omit(merge.xts(train.df,OpCl(lag(GDAXI,1)),join="inner"))
train.df<-na.omit(merge.xts(train.df,OpCl(lag(SSMI,1)),join="inner"))
train.df<-na.omit(merge.xts(train.df,OpCl(lag(TA100,1)),join="inner"))
#train.df<-na.omit(merge.xts(train.df,OpCl(lag(FCHI,1)),join="inner"))
#train.df<-na.omit(merge.xts(train.df,OpCl(lag(DJC,1)),join="inner"))
#train.df<-na.omit(merge.xts(train.df,OpCl(lag(AORD,1)),join="inner"))
#train.df<-na.omit(merge.xts(train.df,OpCl(lag(SSEC,1)),join="inner"))
#train.df<-na.omit(merge.xts(train.df,OpCl(lag(HSI,1)),join="inner"))
#train.df<-na.omit(merge.xts(train.df,OpCl(lag(JKSE,1)),join="inner"))
#train.df<-na.omit(merge.xts(train.df,OpCl(lag(KLSE,1)),join="inner"))
#train.df<-na.omit(merge.xts(train.df,OpCl(lag(N225,1)),join="inner"))
#train.df<-na.omit(merge.xts(train.df,OpCl(lag(NZ50,1)),join="inner"))
#train.df<-na.omit(merge.xts(train.df,OpCl(lag(STI,1)),join="inner"))
#train.df<-na.omit(merge.xts(train.df,OpCl(lag(KS11,1)),join="inner"))
#train.df<-na.omit(merge.xts(train.df,OpCl(lag(TWII,1)),join="inner"))
#train.df<-na.omit(merge.xts(train.df,OpCl(lag(ATX,1)),join="inner"))
#train.df<-na.omit(merge.xts(train.df,OpCl(lag(BFX,1)),join="inner"))
#train.df<-na.omit(merge.xts(train.df,OpCl(lag(AEX,1)),join="inner"))
#train.df<-na.omit(merge.xts(train.df,OpCl(lag(FTSE,1)),join="inner"))
#train.df<-na.omit(merge.xts(train.df,OpCl(lag(FEU,1)),join="inner"))

train.df<-merge.xts(train.df,na.omit(lag(diff(DSWP2,1),1)),join="inner") # USD 2yr Swap rate change
names(train.df)<-c("LHS.Open","IPC","DOW","BSESN","GDAXI","SSMI","TA100","DSWP2")

# Same with Currency returns. We don't know today's currency return yet. We only know
# yesterday's return. This known return is, relative to today: ln(CCy(t-2)/Ccy(t-1))
train.df<-merge.xts(train.df,na.omit(lag(Return.calculate(DEXUSAL,method="log"),1)),join="inner") # AUDUSD 
train.df<-merge.xts(train.df,na.omit(lag(Return.calculate(DEXINUS,method="log"),1)),join="inner") # USDINR
train.df<-merge.xts(train.df,na.omit(lag(Return.calculate(DEXBZUS,method="log"),1)),join="inner") # USDBRL
train.df<-merge.xts(train.df,na.omit(lag(Return.calculate(DEXCAUS,method="log"),1)),join="inner") # USDCAD 
train.df<-merge.xts(train.df,na.omit(lag(Return.calculate(DEXUSEU,method="log"),1)),join="inner") # EURUSD
train.df<-merge.xts(train.df,na.omit(lag(Return.calculate(DEXJPUS,method="log"),1)),join="inner") # USDJPY
train.df<-merge.xts(train.df,na.omit(lag(Return.calculate(DEXMXUS,method="log"),1)),join="inner") # USDMXN
train.df<-merge.xts(train.df,na.omit(lag(Return.calculate(DEXKOUS,method="log"),1)),join="inner") # USDKRW

# Try new things here:
# Perhaps the 10am price - TWAP from the hours from, say, 7am-10am, is a good predictor of our
# LHS variable. Let's find out:

fx.time.stamps<-makeTimeStamps(train.data$time_stamp)
prices<-Cl(train.data)
fx.allt<-xts(prices,order.by=fx.time.stamps)
names(fx.allt)<-"fxrate"

fix.prices<-fx.allt[index(fx.allt,0)$hour==10 & index(fx.allt,0)$min==0] # vector of all 10am prices...
fix.dates<-as.Date(index(fix.prices,0))
fix.prices<-xts(fix.prices,fix.dates)
names(fix.prices)<-"fixing.rate"

hour7to10<-fx.allt[(index(fx.allt,0)$hour>=7 & index(fx.allt,0)$hour<10)] # A vector of all the prices from 7am - 10am NYT.
twap7to10<-aggregate(hour7to10$fxrate,as.Date(index(hour7to10,0)),mean) # THe TWAP from 7 to 10am NYT.
names(twap7to10)<-"twap"

twapdf<-merge.xts(fix.prices,twap7to10,join="inner")
twapfixdiff<-twapdf$fixing.rate-twapdf$twap
names(twapfixdiff)<-"twapfixdiff"

train.df<-merge.xts(train.df,twapfixdiff,join="inner")


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
abline(h=c(0),col="black")

# We can also look at a plot of the residuals to see if there are any obvious problems:
plot(residuals(train.model),type="l",col="red")
lines(train.fitted,col="blue")

# We really want to know, "What percentage of the time are we on the correct side of the trade?" The answer is...
train.signal.perf<-sign(train.fitted)*sign(train.actuals)
plot(density(train.signal.perf),type="l",col="red")
c<-count(train.signal.perf<0)
cat("Error rate is: ",round(c$freq[2]/sum(c$freq),4),"\n")

#...about 77% of the time. That's all well and good, but in order to have faith in the model, we need to 
# see how the model performs when presented with NEW data. That is, we need to measure OUT OF SAMPLE performance.
# The pseudo-code for running the out-of-sample tests is:

# 1. Set new year
# 2. Get the new data
# 3. make new LHS
# 4. make new DF
# 5. make new model
# 6. plot new actuals vs. new fitted
# 7. return a list of fitted, actuals, and signal performance.

sample.years<-c("2008","2009","2010","2011","2012")
sample.model<-train.model # Currently 2007 trained...

out<-runSamples(sample.years,sample.model,binary=FALSE)

# Let's create some strategy performance metrics...
# For each out-of-sample year, we want to look at each day and measure how well we did. We'll store this 
# information in a new performance vector containing:
# 1. The trade date
# 2. The pips we captured or lost on the trade.
# The 'out' variable we created above is enough to construct our performance vector.

# For each year we've collected, look at each day...
# If the variable sig.perf==1, then we at least did the correct trade on that day...
# In this case our P/L = +actual.
# If the variable sig.perf==-1, then we are on the wrong side of the trade...
# In this case, our P/L = -actual. 

profit<-NULL
bidoffer<-.0002
for (i in 1:length(out)) {
  p<-xts(out[[i]]$sig.perf*abs(out[[i]]$actual)-bidoffer,as.Date(out[[i]]$date))
  profit<-append(profit,p)
}
profit.index<-makeIndex(profit,inv=FALSE,ret=TRUE)
plot.xts(profit.index, main="Out of Sample Performance: Cumulative")

charts.PerformanceSummary(profit, Rf = 0, main = "Performance Summary", geometric=FALSE, 
                          methods = "none", width = 0, event.labels = FALSE, 
                          wealth.index = TRUE, gap = 12, begin ="first", 
                          legend.loc = "topleft", 
                          p=0.95)

table.AnnualizedReturns(profit, scale = NA, Rf = 0, geometric = FALSE, digits = 4)


# or...we can look at each year's performance individually...
par(mfrow=c(2,2))

for (i in 1:length(out)){
  p<-xts(out[[i]]$sig.perf*abs(out[[i]]$actual)-bidoffer,as.Date(out[[i]]$date))
  p<-makeIndex(p,inv=FALSE,ret=TRUE)
  y<-substr(index(p,0)[1],0,4)
  plot.xts(p,main=paste("Out of Sample Performance: ",y))
  rm(p)
}

# Here is a look at the distribution of errors. That is, all of the times we got the wrong
# signal, how were the losses distributed?

err<-list()
for (i in 1:length(out)){
  err[[i]]<-subset(-1*abs(out[[i]]$actual-bidoffer),subset=c(out[[i]]$sig.perf<0))
}
plot(density(err[[1]]),col="dark grey",ylim=c(0,1100),xlim=c(-.0060,.0010),
     main="Error Densities for each sample year: 2008:2012")
for (i in 2:length(out)) {
  lines(density(err[[i]]),col="dark grey")
}
merr<-sapply(err,MARGIN=2,FUN=mean)
plot(density(merr),col="red",main="Density of Mean Error")
par(mfrow=c(1,1))





# Experiment: Create a new response variable which is either "Above" == 1 or "Below"==0
# to represent cases where the FIX - TWAP > 0 and FIX - TWAP < 0. This is similar to our sign.perf
# variable which tries to capture whether we got the sign right. Meaning, did we have the right
# trade on, regardless of the magnitude of the prediction error. By converting to a binary response
# variable and estimating a logit regression, we should obtain similar results and perhaps our 
# response variable will have more intuitive meaning. Prediction values above .5 indicate that the
# fixing will come in above the TWAP and vice versa for values below .5.

# To do this, I have modified the makeLHS() function to take a "binary" parameter. If set to true,
# the makeLHS() function will return a binary response varible. 

train.bin.LHS<-makeLHS(train.data,binary=TRUE)

model.bin.tickers<-c("^IPC","DOW","^BSESN","^GDAXI","^SSMI","^TA100")
#"^AORD","^SSEC","^HSI","^BSESN","^JKSE","^KLSE","^N225","^NZ50","^STI","^KS11","^TWII",
#"^ATX","^BFX","^AEX","^FTSE","FEU")
suppressWarnings(getData(model.bin.tickers,data.source))


train.bin.df<-na.omit(merge.xts(Op(train.bin.LHS),OpCl(lag(IPC[first.train.date],1)),join="left"))
train.bin.df<-merge.xts(train.bin.df,OpCl(lag(DOW,1)),join="inner")
train.bin.df<-merge.xts(train.bin.df,OpCl(lag(BSESN,1)),join="inner")
train.bin.df<-merge.xts(train.bin.df,OpCl(lag(GDAXI,1)),join="inner")
train.bin.df<-merge.xts(train.bin.df,OpCl(lag(SSMI,1)),join="inner")
train.bin.df<-merge.xts(train.bin.df,OpCl(lag(TA100,1)),join="inner")
names(train.bin.df)<-c("LHS.Open","IPC","DOW","BSESN","GDAXI","SSMI","TA100")
train.bin.df<-merge.xts(train.bin.df,na.omit(lag(Return.calculate(DEXUSAL,method="log"),1)),join="inner") # AUDUSD 
train.bin.df<-merge.xts(train.bin.df,na.omit(lag(Return.calculate(DEXBZUS,method="log"),1)),join="inner") # USDBRL
train.bin.df<-merge.xts(train.bin.df,na.omit(lag(Return.calculate(DEXUSEU,method="log"),1)),join="inner") # EURUSD
train.bin.df<-merge.xts(train.bin.df,na.omit(lag(Return.calculate(DEXJPUS,method="log"),1)),join="inner") # USDJPY
train.bin.df<-merge.xts(train.bin.df,na.omit(lag(Return.calculate(DEXMXUS,method="log"),1)),join="inner") # USDMXN
train.bin.df<-as.data.frame(train.bin.df)
head(train.bin.df)


# Now we have a data frame with a binary response variable and all the corresponding RHS variables.
# Now we can estimate the model using glm instead of lm with a family designation of "binary"...

my.glm<-function (formula,data,...) {
  glm(as.formula(paste(deparse(formula))),data=data,family=binomial(link="logit"),...)
}

train.bin.fit<-glmulti(LHS.Open~., 
                   data=train.bin.df, 
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
train.bin.form<-as.formula(summary(train.bin.fit)$bestmodel)
train.bin.model<-glm(train.bin.form,data=train.bin.df,family=binomial(link="logit"))
# Let's have a look a summary of our derived model:
summary(train.bin.model) 

# Let's look at how well our model fits the actual values in the training data set:
train.bin.actuals<-as.vector(train.bin.df$LHS.Open)
train.bin.fitted<-as.vector(fitted(train.bin.model))
train.bin.fitted[train.bin.fitted<.5]<-0
train.bin.fitted[train.bin.fitted>.5]<-1
plot(train.bin.actuals,type="l",col="grey")
lines(train.bin.fitted,col="light blue")
abline(h=c(.50),col="black")

train.bin.err<-abs(train.bin.actuals-train.bin.fitted) # 0 means no error, 1 means trade error.
plot(density(train.bin.err))
count(train.bin.err) # Very similar error rate to previous estimation.

sample.bin.years<-c("2008","2009","2010","2011","2012")
sample.bin.model<-train.bin.model # Currently 2007 trained...
out.binary<-runSamples(sample.bin.years,sample.bin.model,binary=TRUE)

profit.bin<-NULL
for (i in 1:length(out.binary)) {
  out.binary[[i]]$sig.perf[out.binary[[i]]$sig.perf==0]<--1
  p.bin<-xts(out.binary[[i]]$sig.perf*abs(out.binary[[i]]$actuals.pips),as.Date(out.binary[[i]]$dates))
  profit.bin<-append(profit.bin,p.bin)
}
profit.bin.index<-makeIndex(profit.bin,inv=FALSE,ret=TRUE)
plot.xts(profit.bin.index, main="Out of Sample Performance: Cumulative")

# or...we can look at each year's performance.
par(mfrow=c(2,2))

for (i in 1:length(out.binary)){
  out.binary[[i]]$sig.perf[out.binary[[i]]$sig.perf==0]<--1
  p.bin<-xts(out.binary[[i]]$sig.perf*abs(out.binary[[i]]$actuals.pips),as.Date(out.binary[[i]]$dates))
  p.bin<-makeIndex(p.bin,inv=FALSE,ret=TRUE)
  y<-substr(index(p.bin,0)[1],0,4)
  plot.xts(p.bin,main=paste("Out of Sample Performance: ",y))
  rm(p.bin)
}

# Looking at the error distributions...
err.bin<-list()
for (i in 1:length(out.binary)){
  err.bin[[i]]<-subset(-1*abs(out.binary[[i]]$actuals.pips),subset=c(out.binary[[i]]$sig.perf==-1))
}
plot(density(err.bin[[1]]),col="dark grey",ylim=c(0,1100),xlim=c(-.0060,.0010),
     main="Error Densities for each sample year: 2008:2012")
for (i in 2:length(out.binary)) {
  lines(density(err.bin[[i]]),col="dark grey")
}
merr.bin<-sapply(err.bin,MARGIN=2,FUN=mean)
plot(density(merr.bin),col="red",main="Density of Mean Error")
par(mfrow=c(1,1))
