# twap_wmfix_getdata
# Adam Duncan, Arnav Sheth Mar 2013

# This script will establish all of the variables and get the data into the workspace.
# Adjust paths for local data as needed. (FX minute interval data)

# begin:
# Required Libraries: (I use the same base set regardless of use)

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

# Establish data source and tickers for each of the indices. These will be our RHS variables.
# Function getData() takes the tickers and the data source vectors and puts the variables 
# into the workspace. Human labels are given using the labels.xxx variables.
# makeIndex() is a function that returns an xts object that is index to 100 at the beginning
# of the time series and evolves as the log returns of the input index. Useful for comparing
# many indices of different base. 

# Global parameters should be defined here:
data.source = c("yahoo")
first.date<-c("2012-01-02/") # Base date for the analysis. 
year<-substr(first.date,1,4)
OHLC.indices<-c("Close") # Closing index value for the DAY...(ie. each DAY has an OHLC)
OHLC.FX<-c("Close") # Closing bid FX price for the MINUTE...(ie. each MINUTE has an OHLC)

# Function definitions should be defined here:
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

# Make symbol collection...
tickers.americas=c("^MERV","^BVSP","^GSPTSE","^IPC","^GSPC","DOW")
tickers.asiapac=c("^AORD","^SSEC","^HSI","^BSESN","^JKSE","^KLSE","^N225","^NZ50","^STI","^KS11","^TWII")
tickers.europe=c("^ATX","^BFX","^FCHI","^GDAXI","^AEX","^OSEAX","^OMXSPI","^SSMI","^FTSE",
                 "^MCX","GD.AT")
tickers.africame=c("^TA100")
tickers.commod=c("DJC")

labels.americas=c("Merval","Bovespa","SP.TSX.Comp","MXX","SP500","Dow")
labels.asiapac=c("All Ordinaries","Shanghai Comp","Hang Seng","BSE30","Jakarta Comp",
                 "KLSE Comp","Nikkei 225","NZSE50 Index","Straits Times Index","KOSPI Index",
                 "Taiwan Weighted")
labels.europe=c("ATX Index","BEL20 Index","CAC40 Index","DAX Index","AEX General","OSE All Share",
                "Stockholm General","Swiss Market","FTSE 100","MICEX Index","Athex Composite")
labels.africame=c("TA 100")
labesl.commod=c("DJ-UBS Comm. Index")

# Go get the data...
suppressWarnings(getData(tickers.americas,data.source))
suppressWarnings(getData(tickers.asiapac,data.source))
suppressWarnings(getData(tickers.europe,data.source))
suppressWarnings(getData(tickers.africame,data.source))
suppressWarnings(getData(tickers.commod,data.source))

# Import some high frequency data from local files...(Year 2012 in 1 minute intervals. Can be exanded.)
eurusd.data<-read.csv(paste("~/R/R Projects/twap_wmfix/data/",year,"/DAT_ASCII_EURUSD_M1_",year,".csv",sep="")) 
usdjpy.data<-read.table(paste("~/R/R Projects/twap_wmfix/data/",year,"/DAT_ASCII_USDJPY_M1_",year,".csv",sep=""), sep=";", quote="\"")
gbpusd.data<-read.table(paste("~/R/R Projects/twap_wmfix/data/",year,"/DAT_ASCII_GBPUSD_M1_",year,".csv",sep=""), sep=";", quote="\"")
usdchf.data<-read.table(paste("~/R/R Projects/twap_wmfix/data/",year,"/DAT_ASCII_USDCHF_M1_",year,".csv",sep=""), sep=";", quote="\"")
usdnok.data<-read.table(paste("~/R/R Projects/twap_wmfix/data/",year,"/DAT_ASCII_USDNOK_M1_",year,".csv",sep=""), sep=";", quote="\"")
usdsek.data<-read.table(paste("~/R/R Projects/twap_wmfix/data/",year,"/DAT_ASCII_USDSEK_M1_",year,".csv",sep=""), sep=";", quote="\"")
audusd.data<-read.table(paste("~/R/R Projects/twap_wmfix/data/",year,"/DAT_ASCII_AUDUSD_M1_",year,".csv",sep=""), sep=";", quote="\"")
nzdusd.data<-read.table(paste("~/R/R Projects/twap_wmfix/data/",year,"/DAT_ASCII_NZDUSD_M1_",year,".csv",sep=""), sep=";", quote="\"")
usdcad.data<-read.table(paste("~/R/R Projects/twap_wmfix/data/",year,"/DAT_ASCII_USDCAD_M1_",year,".csv",sep=""), sep=";", quote="\"")
usdmxn.data<-read.table(paste("~/R/R Projects/twap_wmfix/data/",year,"/DAT_ASCII_USDMXN_M1_",year,".csv",sep=""), sep=";", quote="\"")
usdtry.data<-read.table(paste("~/R/R Projects/twap_wmfix/data/",year,"/DAT_ASCII_USDTRY_M1_",year,".csv",sep=""), sep=";", quote="\"")
usdzar.data<-read.table(paste("~/R/R Projects/twap_wmfix/data/",year,"/DAT_ASCII_USDZAR_M1_",year,".csv",sep=""), sep=";", quote="\"")

# Establish common names for all the colums...
names(usdjpy.data)<-names(eurusd.data)
names(gbpusd.data)<-names(eurusd.dataf)
names(usdchf.data)<-names(eurusd.dataf)
names(usdnok.data)<-names(eurusd.dataf)
names(usdsek.data)<-names(eurusd.dataf)
names(audusd.data)<-names(eurusd.dataf)
names(nzdusd.data)<-names(eurusd.dataf)
names(usdcad.data)<-names(eurusd.dataf)
names(usdmxn.data)<-names(eurusd.dataf)
names(usdtry.data)<-names(eurusd.dataf)
names(usdzar.data)<-names(eurusd.dataf)

# Visually inspect a couple data elements to makes sure imports went well...
head(IPC)
head(KLSE)
head(eurusd.data)
head(gbpusd.data)

# Create vectors for all of the indices we have collected. Each of the indices imported
# from the data source will be of type 'zoo' / 'xts'. That's good because they are time based 
# objects. We'll want to make sure our minute interval FX data is also of the same class.

indices.americas<-c(MERV[first.date],BVSP[first.date],GSPTSE[first.date],IPC[first.date],GSPC[first.date],DOW[first.date])
inices.asiapac<-c(AORD[first.date],SSEC[first.date],HSI[first.date],BSESN[first.date],JKSE[first.date],KLSE[first.date],
                   N225[first.date],NZ50[first.date],STI[first.date],KS11[first.date],TWII[first.date])
indices.europe<-c(ATX[first.date],BFX[first.date],FCHI[first.date],GDAXI[first.date],AEX[first.date],
                  OSEAX[first.date],OMXSPI[first.date],SSMI[first.date],FTSE[first.date],
                    MCX[first.date],GD.AT[first.date])
indices.africame<-c(TA100[first.date])
indices.commod<-c(DJC[first.date])

# end twap_wmfix_getdata

# Some daily, exchange rate data from the FRED database. These will be used as predictor variables. The 
# other exchange rate data is high frequency data and is used for the creation of the response variable.
fx.fred<-c("DEXUSAL","DEXINUS","DEXBZUS","DEXCAUS","DEXUSEU","DEXJPUS","DEXMXUS","DEXKOUS")
suppressWarnings(getData(fx.fred,"FRED"))


# Begin Quandl setup, if needed in the future:
auth_token<-c("JBv8RdKEBzzQeqTox9Ad") # You can find this on your 'Account' page on Quandl under the API tab.
code="enter-code-here"

Quandl(code, type = "xts" #c("raw", "ts", "zoo", "xts"),
       start_date=substr(first.date,0,10), 
       end_date=Sys.Date(),
       transformation = "", #c("", "diff", "rdiff", "normalize", "cumul"),
       collapse = "", #c("", "weekly", "monthly", "quarterly", "annual"),
       rows,
       authcode = Quandl.auth(auth_token))


