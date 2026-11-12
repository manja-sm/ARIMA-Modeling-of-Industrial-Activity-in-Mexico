# ARIMA Time Series Modeling and Forecasting

Implementation of ARIMA time series models to analyze and forecast Mexico's Industrial Activity Index using monthly historical data. The project focuses on time series characterization, stationarity assessment, model identification, statistical validation, and short-term forecasting.

**Key Features:**
- Monthly time series analysis and seasonal pattern assessment.
- Stationarity testing using the Augmented Dickey-Fuller (ADF) test.
- Box-Cox transformation and differencing to achieve stationarity.
- ACF and PACF analysis for ARIMA model identification.
- Manual and automatic model selection using `auto.arima()`.
- Comparative evaluation of ARIMA(1,1,2), ARIMA(2,1,1), ARIMA(1,1,1), and ARIMA(2,1,0).
- Residual diagnostics, including zero mean, constant variance, normality, and independence tests.
- Model validation through parameter significance, stationarity, invertibility, stability, and outlier analysis.
- Ten-period forecasting using the selected ARIMA(2,1,0) model.
- Construction of 95% prediction intervals and forecast visualization.

**Final Model:** ARIMA(2,1,0)

**Technologies:** R, Forecast, TSA, Tidyverse, Tseries, TSstudio, ggplot2.
