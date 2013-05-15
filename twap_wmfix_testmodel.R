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
makeLHS<-function(data) {
  # Takes a data object and year and constructs a quantmod OHLC object which is the FIX - TWAP for that
  # variable. The data should be one of the minute interval data sets in OHLC, xts format.
  # The first return column = FIX - TWAP
  
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
  
  LHS<-temp$fixing.rate-temp$twap
  lhs.h<-xts(rep(0,length(LHS)),index(LHS,0))
  lhs.l<-xts(rep(0,length(LHS)),index(LHS,0))
  lhs.c<-xts(rep(0,length(LHS)),index(LHS,0))
  lhs.v<-xts(rep(0,length(LHS)),index(LHS,0))
  lhs.a<-xts(rep(0,length(LHS)),index(LHS,0))
  LHS<-merge.xts(LHS,lhs.h,lhs.l,lhs.c,lhs.v,lhs.a)
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

training.year=c("2011")
train.data<-read.table(paste("~/R/R Projects/twap_wmfix/data/",training.year,"/DAT_ASCII_EURUSD_M1_",training.year,".csv",sep=""), sep=";", quote="\"")
names(train.data)<-data.names
first.train.date<-c("2011-01-03/")

new.year=c("2010")
new.data<-read.csv(paste("~/R/R Projects/twap_wmfix/data/",new.year,"/DAT_ASCII_EURUSD_M1_",new.year,".csv",sep=""), sep=";", quote="\"")
names(new.data)<-data.names
first.new.date<-c("2010-01-02/")

# Check to make sure we have 2 good looking data sets from different years...
head(train.data)
head(new.data)

# That all looks good. Don't proceed until the data looks proper. It should start at some time on January 1-5 and be 1 year's worth
# of minute interval data.

# Now, let's make our response (LHS) variable for our model. We have the function built and the data sets loaded.
train.LHS<-makeLHS(train.data)

# The next step is to make a data frame that contains our LHS variable in the first column and all the independent (RHS)
# variables in the subsequent columns. First we need to make sure we use getData() to load the variables we want. 
# Note here, that we are using information from running the model a few times and eliminating variables we found
# to be non-informative. If you were approaching this with no prior knowledge, you would want to use getData() to 
# bring in more candidate independent variables.

data.source<-c("yahoo")
model.tickers<-c("^IPC","DOW","^BSESN","^GDAXI","^SSMI","^TA100")
fx.fred<-c("DEXUSAL","DEXINUS","DEXBZUS","DEXCAUS","DEXUSEU","DEXJPUS","DEXMXUS","DEXKOUS")
suppressWarnings(getData(model.tickers,data.source))
suppressWarnings(getData(fx.fred,"FRED"))

# Now assemble the data frame...
train.df<-na.omit(merge.xts(Op(train.LHS),OpCl(IPC[first.train.date]),join="left"))
names(train.df)<-c("LHS.Open","OpCl.IPC")
train.df<-merge.xts(train.df,OpCl(DOW),join="inner")
train.df<-merge.xts(train.df,OpCl(BSESN),join="inner")
train.df<-merge.xts(train.df,OpCl(GDAXI),join="inner")
train.df<-merge.xts(train.df,OpCl(SSMI),join="inner")
train.df<-merge.xts(train.df,OpCl(TA100),join="inner")
train.df<-merge.xts(train.df,na.omit(Return.calculate(DEXUSAL)),join="inner") # AUDUSD 
train.df<-merge.xts(train.df,na.omit(Return.calculate(DEXINUS)),join="inner") # USDINR
train.df<-merge.xts(train.df,na.omit(Return.calculate(DEXBZUS)),join="inner") # USDBRL
train.df<-merge.xts(train.df,na.omit(Return.calculate(DEXCAUS)),join="inner") # USDCAD
train.df<-merge.xts(train.df,na.omit(Return.calculate(DEXUSEU)),join="inner") # EURUSD
train.df<-merge.xts(train.df,na.omit(Return.calculate(DEXJPUS)),join="inner") # USDJPY
train.df<-merge.xts(train.df,na.omit(Return.calculate(DEXMXUS)),join="inner") # USDMXN
train.df<-merge.xts(train.df,na.omit(Return.calculate(DEXKOUS)),join="inner") # USDKRW

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
plot(density(new.signal.perf),type="l",col="red",ylim=c(0,1.3))
lines(density(train.signal.perf),type="l",col="dark grey")
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
# 7. store and print error rate. 

runSamples<-function(years,trainedmodel){
  # this function will run through a bunch of years and compare the results to whatever model is stored
  # in trainedmodel.
  
}
