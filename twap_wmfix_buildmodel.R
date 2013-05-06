# # twap_wmfix_buildmodel
# Adam Duncan, Arnav Sheth Mar 2013

# This section of the project builds the actual model we are trying to estimate.
# There are a number of ways to do this. We can specify the model directly with calls
# to glm or we can use the quantmod package and build the model with calls to specifyModel
# and related functions. The latter has merits and well suited for the data we're using.
# We can implement it both ways, but I'm going to start with the quantmod implementation.

# begin:

# Let's start by getting the FX into xts/zoo format. We can use the chron package to make
# time based objects our to the strings contained in the time_stamps variable that comes from
# the histdata.com data.
require(glmulti)
require(leaps)
require(rJava)
require(MASS)

# Function defs should go here:
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

# We need to get our variables in the right format before we proceed. This basically means we need
# to convert the FX data we downloaded into xts/zoo format. The first step in doing so is creating 
# a series of time stamps that are in a legitimate time based class supported by xts().

fx.time.stamps<-makeTimeStamps(eurusd.data$time_stamp) # This can now be used to make xts/zoo series...
data<-Cl(eurusd.data) # Op()=open, Hi()=high, Lo()=low, Cl()=close...

fx.allt<-xts(data,order.by=fx.time.stamps) # All of the FX data for this pair...
names(fx.allt)<-"fxrate"

# Before we go further and assemble the dependent variables, we need to do a little more massaging 
# of the independent variable. In this study, we are interested in a very particular time slice of 
# data. Specifically, we are examining behavior in the FX market in the 1 hour interval between
# 10 am - 11am NYT (or 3pm-4pm London). It is during this period that the majority of "fixing" business
# happens. More detail will be provided later about how the 11am WMR fix works and why people use it.
# Specifically, we want our independent variable to be the difference between the FX rate at exactly 11am
# and the time-weighted average FX rate from 10am to 11am (the TWAP). That shouldn't be too tough, given
# that we have converted the LHS variable to a time based class.

fix.prices<-fx.allt[index(fx.allt,0)$hour==11 & index(fx.allt,0)$min==0] # A vector of all the 11am times. Our proxy for the WMR Fix.
names(fix.prices)<-"fixing.rate"
hour10to11<-fx.allt[(index(fx.allt,0)$hour>=10 & index(fx.allt,0)$hour<11)] # A vector of all the prices from 10am - 11am NYT.
twap10to11<-aggregate(hour10to11$fxrate,as.Date(index(hour10to11)),mean) #THe TWAP from 10 to 11am NYT.
names(twap10to11)<-"twap"
twap10to11<-xts(twap10to11,index(fix.prices,0))

# We have now successfully generated a once per day time series of 11am observations (fix.prices). This is
# our proxy for the WMR fixing rate published every day at 11 am. We could get more nuanced about it, but that
# will serve for now and any differences are unlikely to be meaningful. 
# We have now also generated a time series of once per day time weighted average prices (twap10to11). THis series
# is the average of the 60, 1 minute interval observations from 10am to 11am NYT. This is our TWAP measure.
# We can now create our independent variable which is going to simply bet the difference between these two variables:
# independet variable = (fix.prices - twap10to11).

LHS<-xts((fix.prices-twap10to11),as.Date(index(fix.prices,0)))
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



# Specify the formula for our model. We might want to make a function to build this on the fly and allow for
# easier edits/permutations.
form<-as.formula(Op(LHS)~OpCl(MERV)+OpCl(BVSP)+OpCl(GSPTSE)+OpCl(IPC)+OpCl(GSPC)+OpCl(DOW)+OpCl(AORD)+OpCl(SSEC)+OpCl(HSI)+OpCl(BSESN)+
                   OpCl(JKSE)+OpCl(KLSE)+OpCl(N225)+OpCl(NZ50)+OpCl(STI)+OpCl(KS11)+OpCl(TWII)+OpCl(ATX)+OpCl(BFX)+OpCl(FCHI)+OpCl(GDAXI)+
                   OpCl(AEX)+OpCl(SSMI)+OpCl(FTSE)+OpCl(TA100)+OpCl(DJC))#+OpCl(GD.AT)+OpCl(OMXSPI))#+OpCl(MCX)+OpCl(OSEAX)

# This is where I am going to depart from the traditional build using glm(...) and am going to 
# use the specifyModel function from quantmod. I think this could give added flexibility later on.
# We'll do it both ways in case the quantmod method doesn't pan out. See 'quantmod::specifyModel()' or
# type '?specifyModel' for details on the function. We will also use Next() and Lag() to test different
# model specifications. See also: getModelData,getSymbols, buildModel,tradeModel,formula setSymbolLookup.

the_model<-specifyModel(form,na.rm=TRUE) # Specify the form of the model for quantmod...
the_model.build<-buildModel(the_model,method='lm',training.per=c('2012-01-02','2012-03-23')) # build the model...
fittedModel(the_model.build) # fit the model...
summary(the_model.build) # Look at the reuslts.

plot(the_model.build) # Standard suite of lm plots
coef(the_model.build) # The coefficients of the model

plot(residuals(the_model.build),type="l")
fit<-as.xts(fitted.values(the_model.build))
plot.xts(fit,type="l",main="Fitted Values",ylim=c(-.0050,.0050))
plot.xts(Op(LHS),type="l",main="Fitted Values",ylim=c(-.0050,.0050))
lines(Op(LHS),col="red")

# Let's drop the insignificant predictors and re-fit the model. Here is the slimmed down model:
form2<-as.formula(Op(LHS)~OpCl(MERV)+OpCl(IPC)+OpCl(GSPC)+OpCl(BSESN)+OpCl(KLSE)+OpCl(N225)+OpCl(TWII)+OpCl(BFX)+OpCl(TA100)+OpCl(DJC))
model2<-specifyModel(form2,na.rm=TRUE)
model2.build<-buildModel(model2,method='lm',training.per=c('2012-01-02','2012-03-23'))
fittedModel(model2.build) # fit the model...
summary(model2.build) 

####### Begin AIC optimization section #######

# Ok. Now the model is built and fitted in quantmod fashion. That's great because it will help us later.
# But, we also need to set the model up in generic lm() format. This will allow us to do some other useful things
# that the quantmod format doesn't (like apply a step-wise AIC algorithm to the fit). The first thing we need to 
# do to get that set up is creat a big clean data frame with all our variables. We can use merge.xts to successively
# add variables using a standard "inner" join:

model.df<-na.omit(merge.xts(Op(LHS),OpCl(MERV[first.date]),join="left"))
names(model.df)<-c("LHS.Open","OpCl.MERV")
model.df<-merge.xts(model.df,OpCl(BVSP),join="inner")
model.df<-merge.xts(model.df,OpCl(GSPTSE),join="inner")
model.df<-merge.xts(model.df,OpCl(IPC),join="inner")
model.df<-merge.xts(model.df,OpCl(GSPC),join="inner")
model.df<-merge.xts(model.df,OpCl(DOW),join="inner")
model.df<-merge.xts(model.df,OpCl(AORD),join="inner")
model.df<-merge.xts(model.df,OpCl(SSEC),join="inner")
model.df<-merge.xts(model.df,OpCl(HSI),join="inner")
model.df<-merge.xts(model.df,OpCl(BSESN),join="inner")
model.df<-merge.xts(model.df,OpCl(JKSE),join="inner")
model.df<-merge.xts(model.df,OpCl(KLSE),join="inner")
model.df<-merge.xts(model.df,OpCl(N225),join="inner")
model.df<-merge.xts(model.df,OpCl(NZ50),join="inner")
model.df<-merge.xts(model.df,OpCl(STI),join="inner")
model.df<-merge.xts(model.df,OpCl(KS11),join="inner")
model.df<-merge.xts(model.df,OpCl(TWII),join="inner")
model.df<-merge.xts(model.df,OpCl(ATX),join="inner")
model.df<-merge.xts(model.df,OpCl(BFX),join="inner")
model.df<-merge.xts(model.df,OpCl(FCHI),join="inner")
model.df<-merge.xts(model.df,OpCl(GDAXI),join="inner")
model.df<-merge.xts(model.df,OpCl(AEX),join="inner")
model.df<-merge.xts(model.df,OpCl(SSMI),join="inner")
model.df<-merge.xts(model.df,OpCl(FTSE),join="inner")
model.df<-merge.xts(model.df,OpCl(TA100),join="inner")
model.df<-merge.xts(model.df,OpCl(DJC),join="inner")

# Now we can have a look at our data and coerce it to a data frame:
model.df<-as.data.frame(model.df)
head(model.df)
length(model.df$LHS.Open)

# fit1<-lm(LHS.Open~.,data=model.df) # This says "Construct a linear model with y = f(everything in the df except LHS)"
fit1<-glmulti(LHS.Open~., 
              data=model.df, 
              intercept=TRUE,
              level=1,
              marginality=FALSE,
              minsize=-1, # -1 = no constraint
              maxsize=-1,
              minK=-1,
              maxK=-1,
              crit=aic, 
              fitfunc=lm, 
              method="l", # "h"=exhaustive, "g"=genetic algorithm, "l"=very fast, exhaustive, branch and bound, "d"=simple summary
              plotty=TRUE, #plot progression of IC profile while running...
              report=TRUE,
              confsetsize=1000) # other params for controlling a genetic algorithm are not listed.

print(summary(fit1)$bestmodel) # just show me the best model, please...
summary(fit1) # show me all the details of all the model fitted...
plot(fit1,type="p") # plot the progression of the _ic...
aicvalues<-summary(fit1)$icvalues
complexity<-fit1@K
allmodels<-fit1@objects

# How complex are the models selected?
plot(aicvalues,complexity)

# Ok. Let's just look at the best model produced...
bestmod<-lm(LHS.Open~1+OpCl.IPC+OpCl.DOW+OpCl.BSESN+OpCl.GDAXI+OpCl.SSMI+OpCl.TA100, data=model.df)
summary(bestmod)
plot(residuals(bestmod),type="l")
actuals<-model.df$LHS.Open
fitvals<-as.vector(fitted(bestmod))
plot(actuals,type="l",col="red")
lines(fitvals,col="blue")

# It's a little hard to see how we're doing from the plot, but there are a few good things in the summary. We are able to 
# explain about 25% of the variation in our response variable. That's not great, but it's not all bad either. We seem unable
# to capture the full magnitude of the response fluctuations, but we really don't need to. Meaning, all we really care about 
# is getting the SIGN right. That is, we want to be able to predict when the difference between the TWAP and the FIX is going
# to be positive or negative. We don't really care by how much, we just want to be on the correct side of the trade. As long
# as the differences aren't below our bid/offer cost of execution, we should be profitable as long as we get the sign right.
# If we get the SIGN wrong, well then we have the exact wrong trade on and will lose the full magnitude of the difference.

# So, let's see how well we get the sign right with our "best" model. We'll create a new variable that is 1 if we get the sign
# correct and -1 otherwise.

signal.perf<-sign(fitvals)*sign(actuals)
plot(density(signal.perf),type="l",col="red")
count(signal.perf<0)

# So, we seem to get the sign right about 70% of the time. Conversely, we are wrong about 30% of the time with this model.
# Let's try adding some foreign exchange rates to our "best" model and re-run the exhaustive AIC minimization:


model2.df<-na.omit(merge.xts(Op(LHS),OpCl(IPC[first.date]),join="left"))
names(model2.df)<-c("LHS.Open","OpCl.IPC")
model2.df<-merge.xts(model2.df,OpCl(DOW),join="inner")
model2.df<-merge.xts(model2.df,OpCl(BSESN),join="inner")
model2.df<-merge.xts(model2.df,OpCl(GDAXI),join="inner")
model2.df<-merge.xts(model2.df,OpCl(SSMI),join="inner")
model2.df<-merge.xts(model2.df,OpCl(TA100),join="inner")
model2.df<-merge.xts(model2.df,na.omit(Return.calculate(DEXUSAL)),join="inner") # AUDUSD 
model2.df<-merge.xts(model2.df,na.omit(Return.calculate(DEXINUS)),join="inner") # USDINR
model2.df<-merge.xts(model2.df,na.omit(Return.calculate(DEXBZUS)),join="inner") # USDBRL
model2.df<-merge.xts(model2.df,na.omit(Return.calculate(DEXCAUS)),join="inner") # USDCAD
model2.df<-merge.xts(model2.df,na.omit(Return.calculate(DEXUSEU)),join="inner") # EURUSD
model2.df<-merge.xts(model2.df,na.omit(Return.calculate(DEXJPUS)),join="inner") # USDJPY
model2.df<-merge.xts(model2.df,na.omit(Return.calculate(DEXMXUS)),join="inner") # USDMXN
model2.df<-merge.xts(model2.df,na.omit(Return.calculate(DEXKOUS)),join="inner") # USDKRW

fit2<-glmulti(LHS.Open~., 
              data=model2.df, 
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
              plotty=TRUE, #plot progression of IC profile while running...
              report=TRUE,
              confsetsize=1000) # other params for controlling a genetic algorithm are not listed.

print(summary(fit2)$bestmodel) # just show me the best model, please...
summary(fit2) 

# We've now run a genetic algorithm with interaction terms and derived a new best model. Let's see how well this model can do
# in getting the sign right...

newform<-as.formula(summary(fit2)$bestmodel)
bestmod2<-lm(newform,data=model2.df)
actuals2<-as.vector(model2.df$LHS.Open)
fitvals2<-as.vector(fitted(bestmod2))
plot(actuals2,type="l",col="red")
lines(fitvals2,col="blue")

signal.perf2<-sign(fitvals2)*sign(actuals2)
plot(density(signal.perf2),type="l",col="red")
count(signal.perf2<0)

# Now we only get the sign wrong 23% of the time and we get it righ, 77% of the time. That's a significant improvement
# in our ability to predict the TWAP vs. FIX difference. Let's now do some out of sample testing on new data and plot the 
# performance of the strategy over time.

