
##### Guglielmo Berzano #####

##### University of Milan #####

##### Text Mining and Sentiment Analysis #####

#### Github repo: https://github.com/guber25/Forecasting_w_Sentiment ####

##### Importing the Libraries #####

Sys.setlocale("LC_TIME", "C")
library(dplyr)
library(ggplot2)
library(forecast)
library(Metrics)
library(extrafont)

#### Reading the file ####

sp500 = read.csv("https://raw.githubusercontent.com/guber25/Forecasting_w_Sentiment/refs/heads/main/Datasets/sp500_with_sentiment.csv", 
                 colClasses = c("double", "Date", "double", "double", "double"))


#### The ARIMAX(1,1,0) Script ####

predicted_values = list("FinBERT"=c(), "Vader"=c(), "Benchmark"=c())
direction_changes=list("FinBERT"=c(), "Vader"=c(), "Benchmark"=c())

for (i in seq(1,349)){
  series = sp500$Close[1:(299+i)] %>% log()
  
  h0 = exp(series[length(series)])
  
  predicted_values$FinBERT = series %>% Arima(order=c(1,1,0), xreg = sp500$Sentiment_fin_F2[1:(299+i)], include.constant = T, method="ML") %>% forecast(h=1, xreg = sp500$Sentiment_fin_F2[(300+i)]) %>% .$mean %>% exp() %>% append(predicted_values$FinBERT, .)
  
  h1=predicted_values$FinBERT[length(predicted_values$FinBERT)]
  
  if(((h1-h0 >0) & (sp500$Close[300+i]-h0 > 0)) | ((h1-h0 < 0) & (sp500$Close[300+i]-h0 < 0))){
    direction_changes$FinBERT = append(direction_changes$FinBERT, 1)
  }else{
    direction_changes$FinBERT = append(direction_changes$FinBERT, 0)
  }
  
  predicted_values$Vader = series %>% Arima(order=c(1,1,0), xreg = sp500$Sentiment_fin_V[1:(299+i)], include.constant = T, method="ML") %>% forecast(h=1, xreg = sp500$Sentiment_fin_V[(300+i)]) %>% .$mean %>% exp() %>% append(predicted_values$Vader, .)
  
  h1=predicted_values$Vader[length(predicted_values$Vader)]
  if(((h1-h0 >0) & (sp500$Close[300+i]-h0 > 0)) | ((h1-h0 < 0) & (sp500$Close[300+i]-h0 < 0))){
    direction_changes$Vader = append(direction_changes$Vader, 1)
  }else{
    direction_changes$Vader = append(direction_changes$Vader, 0)
  }
  
  predicted_values$Benchmark = series %>% Arima(order=c(1,1,0), include.constant = T, method="ML") %>% forecast(h=1) %>% .$mean %>% exp() %>% append(predicted_values$Benchmark, .)
  
  h1=predicted_values$Benchmark[length(predicted_values$Benchmark)]
  if(((h1-h0 >0) & (sp500$Close[300+i]-h0 > 0)) | ((h1-h0 < 0) & (sp500$Close[300+i]-h0 < 0))){
    direction_changes$Benchmark = append(direction_changes$Benchmark, 1)
  }else{
    direction_changes$Benchmark = append(direction_changes$Benchmark, 0)
  }
}

#### Error measures ####

cat("Directional change:\n")
for (i in direction_changes){
  i %>% mean() %>% cat()
  cat("\n")
}
cat("\n\n\nRMSPE:\n")
for (i in predicted_values){
  i %>% rmse(predicted=., actual = sp500$Close[301:(length(sp500$Close))]) %>% cat()
  cat("\n")
}

####### Charts #######


### First - Forecast over the entire period ###
sp500[301:649,] %>% ggplot()+
  geom_line(aes(x=Date, y=Close, color='Actual'), linewidth=.6)+
  geom_line(data=as.data.frame(predicted_values), aes(y=FinBERT, x=sp500[301:649,"Date"], color='FinBERT'), linewidth=.3)+
  #geom_line(data=as.data.frame(predicted_values), aes(y=Benchmark, x=sp500[301:649,"Date"], color='Vader'), linewidth=.25)+  
  scale_x_continuous(breaks=sp500[301:649,"Date"][seq(1, 649, length = 20)],
                     labels = sp500[301:649,"Date"][seq(1, 649, length = 20)] %>% substr(1,7))+
  scale_color_manual('',
                     breaks = c('Actual', 'FinBERT', 'Vader'),
                     values = c('Actual'='blue',
                                'FinBERT'='red'))+
  labs(title="Forecasts with FinBERT's sent. vs Actual Values",
       subtitle="Entire forecasting period",
       y="S&P500 Index",
       x="Date")+
  theme_bw()+
  theme(legend.position = 'bottom',
        text=element_text(family = "Palatino Linotype", size = 14, colour='black'),
        #       panel.grid = element_blank(),
        axis.text.x = element_text(angle = 30, vjust = 1, hjust=1, color = 'black'),
        axis.title.x = element_blank(),
        axis.text.y = element_text(color = 'black'),
        aspect.ratio = 10/20)



#### Second - Zooming in during COVID-19 Breakout ###

predicted_values$Date = sp500[sp500$Date>="2019-03-01", "Date"]

sp500[553:649,] %>% ggplot()+
  geom_line(aes(x=Date, y=Close, color='Actual'), linewidth=.6)+
  geom_line(data=as.data.frame(predicted_values %>% as.data.frame() %>% .[253:349,]), aes(y=FinBERT, x=Date, color='FinBERT'), linewidth=.3)+
  scale_x_continuous(breaks=sp500[553:649,"Date"][seq(1, 98, by = 14)],
                     labels = sp500[553:649,"Date"][seq(1, 98, by = 14)])+
  scale_color_manual('',
                     breaks = c('Actual', 'FinBERT', 'Benchmark'),
                     values = c('Actual'='blue',
                                'FinBERT'='red'#,
                                #"Benchmark"="forestgreen"
                     ))+
  labs(title="Forecasts with FinBERT's sent. vs Actual Values",
       subtitle='Zoom in COVID-19 outbreak',
       y="S&P500 Index",
       x="")+
  theme_bw()+
  theme(legend.position = 'bottom',
        text=element_text(family = "Palatino Linotype", size = 14, colour='black'),
        #       panel.grid = element_blank(),
        axis.text.x = element_text(angle = 30, vjust = 1, hjust=1, color = 'black'),
        axis.title.x = element_blank(),
        axis.text.y = element_text(color = 'black'),
        aspect.ratio = 10/20)

###################### THE MATERIAL FOR THE TMSA EXAM ENDS HERE ####################


#--------------------------------------#

############# IMPROVEMENTS #############

#--------------------------------------#


## NUMBER 1: Including the Opening in the model as external regressor ##


predicted_values_open = list("FinBERT"=c(), "Vader"=c(), "Benchmark"=c())
direction_changes_open=list("FinBERT"=c(), "Vader"=c(), "Benchmark"=c())

for (i in seq(1,349)){
  series = sp500$Close[1:(299+i)] %>% log()
  
  h0 = exp(series[length(series)])
  
  predicted_values_open$FinBERT = series %>% Arima(order=c(1,1,1), xreg = cbind("Sent"=sp500$Sentiment_fin_F2[1:(299+i)], "Open"=log(sp500$Open[1:(299+i)])), include.constant = T, method="ML") %>% forecast(h=1, xreg = cbind("Sent"=sp500$Sentiment_fin_F2[300+i], "Open"=log(sp500$Open[300+i]))) %>% .$mean %>% exp() %>% append(predicted_values_open$FinBERT, .)
  
  h1=predicted_values_open$FinBERT[length(predicted_values_open$FinBERT)]
  
  if(((h1-h0 >0) & (sp500$Close[300+i]-h0 > 0)) | ((h1-h0 < 0) & (sp500$Close[300+i]-h0 < 0))){
    direction_changes_open$FinBERT = append(direction_changes_open$FinBERT, 1)
  }else{
    direction_changes_open$FinBERT = append(direction_changes_open$FinBERT, 0)
  }
  
  predicted_values_open$Vader = series %>% Arima(order=c(1,1,1), xreg = cbind("Sent"=sp500$Sentiment_fin_V[1:(299+i)], "Open"=log(sp500$Open[1:(299+i)])), include.constant = T, method="ML") %>% forecast(h=1, xreg = cbind("Sent"=sp500$Sentiment_fin_V[300+i], "Open"=log(sp500$Open[300+i]))) %>% .$mean %>% exp() %>% append(predicted_values_open$Vader, .)
  
  h1=predicted_values_open$Vader[length(predicted_values_open$Vader)]
  if(((h1-h0 >0) & (sp500$Close[300+i]-h0 > 0)) | ((h1-h0 < 0) & (sp500$Close[300+i]-h0 < 0))){
    direction_changes_open$Vader = append(direction_changes_open$Vader, 1)
  }else{
    direction_changes_open$Vader = append(direction_changes_open$Vader, 0)
  }
  
  predicted_values_open$Benchmark = series %>% Arima(order=c(1,1,1), include.constant = T, xreg = cbind("Open"=log(sp500$Open[1:(299+i)])), method="ML") %>% forecast(h=1, xreg=cbind("Open"=log(sp500$Open[300+i]))) %>% .$mean %>% exp() %>% append(predicted_values_open$Benchmark, .)
  
  h1=predicted_values_open$Benchmark[length(predicted_values_open$Benchmark)]
  if(((h1-h0 >0) & (sp500$Close[300+i]-h0 > 0)) | ((h1-h0 < 0) & (sp500$Close[300+i]-h0 < 0))){
    direction_changes_open$Benchmark = append(direction_changes_open$Benchmark, 1)
  }else{
    direction_changes_open$Benchmark = append(direction_changes_open$Benchmark, 0)
  }
}

#### Error measures ####

cat("Directional change:\n")
for (i in direction_changes_open){
  i %>% mean() %>% cat()
  cat("\n")
}
cat("\n\n\nRMSPE:\n")
for (i in predicted_values_open){
  i %>% rmse(predicted=., actual = sp500$Close[301:(length(sp500$Close))]) %>% cat()
  cat("\n")
}

#### Zoomed-in Chart ####

predicted_values$Date = sp500[sp500$Date>="2019-03-01", "Date"]
predicted_values_open$Date = sp500[sp500$Date>="2019-03-01", "Date"]

sp500[553:649,] %>% ggplot()+
  geom_line(aes(x=Date, y=Close, color='Actual'), linewidth=.6)+
  geom_line(data=as.data.frame(predicted_values %>% as.data.frame() %>% .[253:349,]), aes(y=FinBERT, x=Date, color='FinBERT-Solo'), linewidth=.3)+
  geom_line(data=as.data.frame(predicted_values_open %>% as.data.frame() %>% .[253:349,]), aes(y=FinBERT, x=Date, color='FinBERT-Open'), linewidth=.3)+
  scale_x_continuous(breaks=sp500[553:649,"Date"][seq(1, 98, by = 14)],
                     labels = sp500[553:649,"Date"][seq(1, 98, by = 14)])+
  scale_color_manual('',
                     breaks = c('Actual', 'FinBERT-Solo', 'FinBERT-Open'),
                     values = c('Actual'='blue',
                                'FinBERT-Solo'='red',
                                'FinBERT-Open'="forestgreen"
                     ))+
  labs(title="Forecasts with FinBERT's sent. vs Actual Values",
       subtitle='Zoom in COVID-19 outbreak',
       y="S&P500 Index",
       x="")+
  theme_bw()+
  theme(legend.position = 'bottom',
        text=element_text(family = "Palatino Linotype", size = 14, colour='black'),
        #       panel.grid = element_blank(),
        axis.text.x = element_text(angle = 30, vjust = 1, hjust=1, color = 'black'),
        axis.title.x = element_blank(),
        axis.text.y = element_text(color = 'black'),
        aspect.ratio = 10/20)

##### RESULT ####
# By adding another regressor accounting for opening price, the precision rises consistently:
# -34% in RMSPE and +15% in terms of MDA


##### MAKING A MODEL COHERENT TO THE THEORY #####

#### ARMAX(p,q) ####

# I do not take the first difference because the parameter d=1 will also take the difference of the regressors.
# While it is fine if I take the difference of Open, it is not fine if the difference is taken also for the sentiments.
# Indeed those series are already stationary and do not need another differencing

# I will create a new dataframe with the series already stationary -> no need to differenciate again

sp500_stat = data.frame("Close" = sp500$Close %>% log() %>% diff(),
                    "Open" = sp500$Open %>% log() %>% diff(),
                    "Sentiment_fin_F2" = sp500$Sentiment_fin_F2[2:649],
                    "Date" = sp500$Date[2:649])


predicted_values_stat=list("FinBERT"=c(), "Vader"=c(), "Benchmark"=c())

direction_changes_stat=c("FinBERT"=c(), "Vader"=c(), "Benchmark"=c())

for (i in 299:647){
  series = sp500_stat$Close[1:i]
  h0 = sp500$Close[i+1] %>% log()
  
  #
  
  h1 = series %>% Arima(order=c(0,0,3), include.mean = T, xreg = cbind("Sent"=sp500_stat$Sentiment_fin_F2[1:i], "Open"=sp500_stat$Open[1:i])) %>% forecast(h=1, xreg=cbind("Sent"=sp500_stat$Sentiment_fin_F2[i+1], "Open"=sp500_stat$Open[i+1])) %>% .$mean #
  
  real_prediction = exp(h1+h0)
  predicted_values_stat$FinBERT = predicted_values_stat$FinBERT %>% append(real_prediction)
  
  if (((real_prediction-exp(h0))>0 & (sp500$Close[i+2]-exp(h0))>0) | ((real_prediction-exp(h0))<0 & (sp500$Close[i+2]-exp(h0))<0)){
    direction_changes_stat$FinBERT = direction_changes_stat$FinBERT %>% append(1)
  }else{
    direction_changes_stat$FinBERT = direction_changes_stat$FinBERT %>% append(0)
  }
  
  h1 = series %>% Arima(order=c(0,0,3), include.mean = T, xreg = cbind("Sent"=sp500_stat$Sentiment_fin_V[1:i], "Open"=sp500_stat$Open[1:i])) %>% forecast(h=1, xreg=cbind("Sent"=sp500_stat$Sentiment_fin_V[i+1], "Open"=sp500_stat$Open[i+1])) %>% .$mean #
  
  real_prediction = exp(h1+h0)
  predicted_values_stat$Vader = predicted_values_stat$Vader %>% append(real_prediction)
  
  if (((real_prediction-exp(h0))>0 & (sp500$Close[i+2]-exp(h0))>0) | ((real_prediction-exp(h0))<0 & (sp500$Close[i+2]-exp(h0))<0)){
    direction_changes_stat$Vader = direction_changes_stat$Vader %>% append(1)
  }else{
    direction_changes_stat$Vader = direction_changes_stat$Vader %>% append(0)
  }
  
  h1 = series %>% Arima(c(0,0,3), include.constant = T, xreg = cbind("Open"=sp500_stat$Open[1:i])) %>% forecast(h=1, xreg=cbind("Open"=sp500_stat$Open[i+1])) %>% .$mean
  
  real_prediction = exp(h1+h0)
  predicted_values_stat$Benchmark = predicted_values_stat$Benchmark %>% append(real_prediction)
  
  if (((real_prediction-exp(h0))>0 & (sp500$Close[i+2]-exp(h0))>0) | ((real_prediction-exp(h0))<0 & (sp500$Close[i+2]-exp(h0))<0)){
    direction_changes_stat$Benchmark = direction_changes_stat$Benchmark %>% append(1)
  }else{
    direction_changes_stat$Benchmark = direction_changes_stat$Benchmark %>% append(0)
  }
}

cat("Directional change:\n")
for (i in direction_changes_stat){
  i %>% mean() %>% cat()
  cat("\n")
}
cat("\n\n\nRMSPE:\n")
for (i in predicted_values_stat){
  i %>% rmse(predicted=., actual = sp500$Close[301:(length(sp500$Close))]) %>% cat()
  cat("\n")
}

#### Chart ####

predicted_values$Date = sp500[sp500$Date>="2019-03-01", "Date"]
predicted_values_open$Date = sp500[sp500$Date>="2019-03-01", "Date"]
predicted_values_stat$Date = sp500[sp500$Date>="2019-03-01", "Date"]

sp500[553:649,] %>% ggplot()+
  geom_line(aes(x=Date, y=Close, color='Actual'), linewidth=.6)+
  geom_line(data=as.data.frame(predicted_values %>% as.data.frame() %>% .[253:349,]), aes(y=FinBERT, x=Date, color='FinBERT-Solo'), linewidth=.3)+
  geom_line(data=as.data.frame(predicted_values_open %>% as.data.frame() %>% .[253:349,]), aes(y=FinBERT, x=Date, color='FinBERT-Open'), linewidth=.3)+
  geom_line(data=as.data.frame(predicted_values_stat %>% as.data.frame() %>% .[253:349,]), aes(y=FinBERT, x=Date, color='FinBERT-Stat'), linewidth=.3)+
  scale_x_continuous(breaks=sp500[553:649,"Date"][seq(1, 98, by = 14)],
                     labels = sp500[553:649,"Date"][seq(1, 98, by = 14)])+
  scale_color_manual('',
                     breaks = c('Actual', 'FinBERT-Solo', 'FinBERT-Open','FinBERT-Stat'),
                     values = c('Actual'='blue',
                                'FinBERT-Solo'='red',
                                'FinBERT-Open'="forestgreen",
                                'FinBERT-Stat'="orange"
                     ))+
  labs(title="FinBERT's sentiment vs Actual Values",
       subtitle='Zoom in COVID-19 outbreak',
       y="S&P500 Index",
       x="")+
  theme_bw()+
  theme(legend.position = 'bottom',
        text=element_text(family = "Palatino Linotype", size = 14, colour='black'),
        #       panel.grid = element_blank(),
        axis.text.x = element_text(angle = 30, vjust = 1, hjust=1, color = 'black'),
        axis.title.x = element_blank(),
        axis.text.y = element_text(color = 'black'),
        aspect.ratio = 10/20)


###### Further suggestions are appreciated !!! ######

#### Guglielmo Berzano :D ####