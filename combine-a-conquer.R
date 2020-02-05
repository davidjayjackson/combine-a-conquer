library(tidyverse)
library(lubridate)
library(data.table)
library(mice)
library(prophet)
library(scales)
# Add some solar data
rm(list=ls())
# Total days 27,639
kanzel <- fread("./../db/kh_spots.csv")
kanzel$Ymd <- as.Date(kanzel$Ymd)
kanzel$Year <- lubridate::year(kanzel$Ymd)
kanzel$Month <- month(kanzel$Ymd)
kanzel$s <- kanzel$s_n + kanzel$s_s
kanzel$g <- kanzel$g_n + kanzel$g_s
# kanzel$Cts <- ifelse(kanzel$R ==0,0,1)
# kanzel$Yes <- ifelse(kanzel$R >=1,"Yes","No")
kanzel<-kanzel[Ymd <="2020-01-31",.(Ymd,s,g,R)]
#
sidc <- fread("../db/SN_d_tot_V2.0.csv")
colnames(sidc) <- c("Year","Month","Day", "Fdate","R", "Sd","Obs" ,"Defin"  )
sidc$Ymd <- as.Date(paste(sidc$Year, sidc$Month, sidc$Day, sep = "-"))
sidc <- sidc %>% filter(Ymd <="2020-01-31") %>% select(Ymd,R)
#
s <- sidc %>% filter(!(Ymd %in% kanzel$Ymd))
k <- kanzel
kc <- rbind(k,s, fill=TRUE)
kc$g <- ifelse(is.na(kc$g),kc$R %/% 10,kc$g)
kc$s <-ifelse(is.na(kc$s),kc$R %% 10,kc$s)
kc$Cts <- ifelse(kc$R ==0,0,1)
kc$Yes <- ifelse(kc$R >=1,"Yes","No")
kc$MA <- forecast::ma(kc$R,order=30)
 kc %>% filter(Ymd >="2020-01-01" ) %>%
   ggplot(aes(x=Ymd,y=MA)) + geom_line() +labs(title="Jan. 2020 ISN 30 Day Moving Average",y="Moving Average",x="Date")
table(kc$Yes)
##
df.pred <- kc %>% select(Ymd,R)
colnames(df.pred) <- c("ds","y")
m <- prophet(seasonality.mode="multiplicative")
m <- add_seasonality(m, name="cycle_11year", period=365.25 * 11,fourier.order=5)
m <- fit.prophet(m, df.pred)
future <- make_future_dataframe(m,periods=8000,freq="day")
forecast <- predict(m, future)
plot(m,forecast) +xlab("Year of Prediction") + ylab("Total Sunspots")
##
fcast <- forecast %>% select(ds,yhat,yhat_lower,yhat_upper) %>% filter(ds >="2020-01-01" )
ggplot(data=fcast,aes(x=ds,y=yhat,col="Predicted")) +geom_line() +xlab("Year of Prediction") + ylab("Total Mean ISN") +geom_smooth(method="loess",aes(col="loess")) +
  ggtitle("Solar Cycle 25 and Beyond: 2020 - 2026")
##
fcast1 <- forecast %>% filter(ds >="2020-01-01")
isn1 <- kc %>% filter(Ymd >="2020-01-01")
ggplot(fcast1) + geom_line(aes(x=ds,y=yhat,col="yhat")) +
    geom_line(isn1,aes(x=Ymd,y=R,col="R"))
##
kc$Year <- lubridate::year(kc$Ymd)
df <- kc %>% filter(Ymd >="2009-01-01" & Ymd <="2019-12-31")
df %>% group_by(Year,Yes) %>%  summarise(Days= n()) %>% spread(Year,Days)
df %>% group_by(Year,Yes) %>%  summarise(Days= n()) %>%
  ggplot(aes(x=Year,y=Days,fill=Yes,scale="free_y")) + geom_bar(stat="identity") 
