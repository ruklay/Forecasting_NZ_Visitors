# Time Series NZ Visitors

Time Series analysis of the number of visitors to New Zealand by using R, and Shiny as a web server. In this case, we are using 2 models, which are simple linear regression, and quadratic regression.

- Select number of months to calculate moving average
- Select the range of years to be the training dataset
- Select a model to fit the training dataset

From the model, we can decompose the time series data into Trend, Seasonality, and Random Noise. Also, we can compare between the data from our model, and the actual data. We can go back to change above parameters at any time.

Finally, after finish adjusting all parameters, we can then forecast the future number of visitors by selecting a year. The output is the predicted number of visitors for each month of the specific year.

The final program can be tested at https://ruklay.shinyapps.io/ABI01/

(The dataset is from statistics New Zealand)
