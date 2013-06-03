# Cross validation of the fitted model. I want to use a couple of techniques to test for 
# overfitting. I'm concerned that the model we've estimated may be over-fit and I want to be
# more sure that the out-of-sample performance is what it appears to be.

# This process will make use of one main package and its dependents: rms

# First we'll use the rms::ols function to convert our estimated model into a form that
# the rms package likes. The main difference from the standard call to lm() in R is that
# ols() has the ability to return y and y-hat vectors by setting x=TRUE and y=TRUE. See
# the package documentation for more.

# Here is the call to ols(). If parameters are not listed, I am using the defualt for that
# parameter.
d<-datadist(train.df)
# The next statement is taken from train.form but the ':' have been changed to '*"...
modelform<-as.formula(LHS.Open ~ 1 + IPC + DEXMXUS + DEXKOUS + BSESN*IPC + GDAXI*DOW + 
                        SSMI*GDAXI + TA100*DOW + TA100*GDAXI + DSWP2*IPC + DEXINUS*IPC + 
                        DEXINUS*DOW + DEXINUS*SSMI + DEXINUS*DSWP2 + DEXBZUS*DOW + 
                        DEXBZUS*BSESN + DEXBZUS*DEXUSAL + DEXCAUS*DOW + DEXCAUS*SSMI + 
                        DEXCAUS*TA100 + DEXCAUS*DSWP2 + DEXCAUS*DEXBZUS + DEXUSEU*DOW + 
                        DEXUSEU*DSWP2 + DEXJPUS*DOW + DEXJPUS*BSESN + DEXJPUS*DEXUSAL + 
                        DEXJPUS*DEXINUS + DEXMXUS*DOW + DEXMXUS*GDAXI + DEXMXUS*TA100 + 
                        DEXMXUS*DSWP2 + DEXMXUS*DEXINUS + DEXMXUS*DEXCAUS + DEXMXUS*DEXUSEU + 
                        DEXKOUS*SSMI + DEXKOUS*TA100 + DEXKOUS*DEXUSAL + DEXKOUS*DEXINUS + 
                        DEXKOUS*DEXUSEU)

model1<-ols(formula=modelform,
            data=train.df,
            x=TRUE,
            y=TRUE,
            se.fit=FALSE,
            linear.predictors=TRUE)

model1
validate(model1,method="boot",B=50,bw=TRUE,rule="aic",type="residual",sls=.05,aics=0,
         force=NULL,estimates=TRUE,pr=TRUE)

