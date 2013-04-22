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
makeRHS<-function()
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
hour10to11<-fx.allt[(index(fx.allt,0)$hour>=10 & index(fx.allt,0)$hour<11)] # A vector of all the prices from 10am - 11am NYT.
tempxts<-xts(as.vector(hour10to11),as.Date(index(hour10to11,0)))
twap10to11<-suppressWarnings(xts(as.vector(by(tempxts,index(tempxts,0),mean)),index(fix.prices,0))) #THe TWAP from 10 to 11am NYT.
names(twap10to11)<-"twap"
rm(tempxts)

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
                   OpCl(AEX)+OpCl(SSMI)+OpCl(FTSE)+OpCl(TA100)+OpCl(DJC)+OpCl(GD.AT))#+OpCl(OMXSPI))#+OpCl(MCX)+OpCl(OSEAX)

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

# Comment: Ok. This is a good setup so far. From here, we can refine the specification of the model. (the 'form" variable)
# TO DO:
# 1. exand 'form' variable to include all the indices.
# 2. convert RHS variables from Cl(x) to OpCl() or take the first differences of the indices. We want to model 
# CHANGES in the indices against the LHS variable.
# 3. Add some helper functions to make it easier to tinker with the data.






