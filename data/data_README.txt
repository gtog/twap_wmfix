# Data sources information for twap_wmfix analysis. 
# Adam Duncan Mar 2013

Website for collecting free, high-frequency, FX data:
http://www.histdata.com/download-free-forex-data/?/ascii/tick-data-quotes

Descriptions of all the data file download types:
http://www.histdata.com/f-a-q/data-files-detailed-specification/

EXAMPLE:
1. Generic ASCII in M1 Bars (1 Minute Bars):

As example, in DAT_ASCII_EURUSD_M1_201202.csv:
20120201 000000;1.306600;1.306600;1.306560;1.306560;0
20120201 000100;1.306570;1.306570;1.306470;1.306560;0
20120201 000200;1.306520;1.306560;1.306520;1.306560;0
20120201 000300;1.306610;1.306610;1.306450;1.306450;0
20120201 000400;1.306470;1.306540;1.306470;1.306520;0
20120201 000500;1.306510;1.306650;1.306510;1.306600;0
20120201 000600;1.306610;1.306760;1.306610;1.306650;0

Row Fields:
DateTime Stamp;Bar OPEN Bid Quote;Bar HIGH Bid Quote;Bar LOW Bid Quote;Bar CLOSE Bid Quote;Volume

DateTime Stamp Format:
YYYYMMDD HHMMSS

Legend:
YYYY – Year
MM – Month (01 to 12)
DD – Day of the Month
HH – Hour of the day (in 24h format)
MM – Minute
SS – Second, in this case it will be allways 00

TimeZone: Eastern Standard Time (EST) time-zone WITHOUT Day Light Savings adjustments