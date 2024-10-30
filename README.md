# Improving the Forecasts of the S&P 500 using Sentiment Analysis 💻+🎭= 📈

This repository is dedicated to the project for the **Text Mining and Sentiment Analysis** exam at UniMi.

This project builds on literature findings that affirm the inclusion of a regressor for economic and financial sentiment in a time series model significantly enhances the results. 

Economic sentiment is retrieved by [this](https://www.kaggle.com/datasets/notlucasp/financial-news-headlines) Kaggle dataset while historic S&P 500 data by the [yfinance](https://pypi.org/project/yfinance/) Python library. In particular, two sentiment analysis models will be implemented and compared: [**FinBERT**](https://huggingface.co/yiyanghkust/finbert-tone) and [**VADER**](https://github.com/cjhutto/vaderSentiment).

This project is organized in the following steps:
1. News headlines preprocessing;
2. Polarity and Intensity calculation with FinBERT and VADER;
3. Analysis and comparison of results;
4. Group by day and averaging to get the sentiment day-by-day;
5. Estimation of ARIMAX$(1,1,0)$ to forecast the S&P500 1-step-ahead including, as external regressor, the average daily sentiment computed by the models (one at the time);
6. Error measure analysis: see how the ARIMAX$(1,1,0)$ with FinBERT and the ARIMAX$(1,1,0)$ with VADER  performed and if they are better than a simple ARIMA$(1,1,0)$ according to the Root Mean Squared Prediction Error (RMSPE) and to the Mean Directional Accuracy (MDA);
7. The R file contains a small appendix aimed to improve the results.

Any feedback is highly appreciated! Hope to hear from you on [LinkedIn](https://linkedin.com/in/guglielmo-berzano)!

Guglielmo
