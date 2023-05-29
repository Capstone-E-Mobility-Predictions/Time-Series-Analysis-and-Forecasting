library(forecast)
library(Metrics)
library(prophet)

training <- grid_pred_dt[grid_pred_dt$Time < as.Date("2022-02-01"),]
setDT(training)
training_prophet <- subset(training, select= c("Time", "grid"))
names(training_prophet) <- c("ds", "y")

training_plot <- plot_ly(training_prophet, x=~ds, y=~y, type='scatter', mode="line")

test <- grid_pred_dt[grid_pred_dt$Time < as.Date("2022-02-08"),]
test <- test[test$Time > as.Date("2022-02-01"),]
setDT(test)
test_prophet <- subset(test, select= c("Time", "grid"))
names(test_prophet) <- c("ds", "y")

m <- prophet(training_prophet, weekly.seasonality=TRUE, daily.seasonality = TRUE)
future <- make_future_dataframe(m, periods=1)

forecast <- predict(m, future)
forecast

plot(m, forecast)

data <- subset(forecast, select = c("ds", "yhat_lower", "yhat_upper", "yhat"),)
data$actuals <- grid_pred_dt[grid_pred_dt$Time < as.Date("2022-02-08"),]$grid

y_true <- data$actuals
y_pred <- data$yhat
mae <- mae(y_true, y_pred)
mae

result_plot <- plot_ly(data, x = ~ds, y = ~yhat, name="Forecast", type="scatter", mode="line")
result_plot <- result_plot %>% add_trace(y=~actuals, name="Actuals", type="scatter")
result_plot

prophet_plot_components(m, forecast)
