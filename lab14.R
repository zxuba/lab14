###AGR 333
###Lab14
###James Czub

###Install & Load
install.packages('ggplot2')
install.packages('gridExtra')
install.packages('gtsummary')
install.packages('rlang')
library('ggplot2')
library('gridExtra')
library('gtsummary')
library('tidyverse')
library('rlang')

#setwd
setwd("C:/Users/Zubbi/Documents/AGR333/Lab14")
getwd()

WASDE <- read.csv("WASDE.csv")
head(WASDE)
str(WASDE)

#intial exploration
g_price <- ggplot(data = WASDE, aes(x = year, y = corn_price)) +
  geom_line(color = "red") +
  ggtitle("Corn Price over Time (1973 - 2019)") +
  labs(y = "Price ($)", x = "Year")

g_demand <- ggplot(data = WASDE, aes(x = year, y = total_use)) +
  geom_line(color = "blue") +
  ggtitle("Corn Demand over Time (1973 - 2019)") +
  labs(y = "Demand", x = "Year")

g_supply <- ggplot(data = WASDE, aes(x = year, y = total_supply)) +
  geom_line(color = "green") +
  ggtitle("Corn Supply over Time (1973 - 2019)") +
  labs(y = "Supply", x = "Year")

#arranging results in grid
grid.arrange(g_price, g_demand, g_supply)
ggsave("Corn_time.png")

#Step 4
WASDE$SUR <- (WASDE$end_stocks / WASDE$total_use) -1

#plottinh
ggplot(data = WASDE, aes(x = SUR, y = corn_price)) + 
  geom_point(shape = 1) +
  geom_smooth(method = lm, color = "red") +
  ggtitle("Stock to Use Ratio (1973-2019)") +
  labs(y = "Corn Price", x = "Stock to Use Ratio")

#linear regression
reg1 <- lm(corn_price ~ SUR, data = WASDE)
summary(reg1)
tbl_regression(reg1, intercept = TRUE) %>%
  add_glance_source_note(include = c(r.squared, nobs))

#price elasticity
mean_sur <- mean(WASDE$SUR)
mean_price <- mean(WASDE$corn_price)

#residual analysis
summary(resid(reg1))

hist(resid(reg1),
     main = "Histogram of Linear Regression Errors",
     xlab = "Linear Model Residuals")

ggplot(data=WASDE, aes(x=SUR, y=resid(reg1))) +
  geom_point(shape = 1)

#Inverse SUR model
WASDE$SUR_Inv <- 1/WASDE$SUR
reg2 <- lm(corn_price ~ SUR_Inv, data=WASDE)
summary(reg2)
tbl_regression(reg2, intercept = TRUE) %>%
  add_glance_source_note(include = c(r.squared, nobs))

# Residual analysis
summary(resid(reg2))
hist(resid(reg2), main="Histogram of Non-linear Regression Errors", xlab="Non-linear Model Residuals")

# Residuals vs SUR plot
ggplot(data=WASDE, aes(x=SUR, y=resid(reg2))) +
  geom_point(shape=1) +
  ggtitle("Non-linear Regression Errors vs. Stock-to-Use Ratio") +
  labs(y="Errors", x="Stock-to-Use Ratio")

##Time analysis
# Create a character variable denoting the two time periods, create a dummy variable for the post-2006 period, graph a scatterplot of price on SUR with unique colors and regression lines for each period
WASDE$period <- ifelse(WASDE$year >= 2006, "2006-2019", "1973-2005")
WASDE$P2006 <- as.numeric(WASDE$year >= 2006)

ggplot(data=WASDE, aes(x=SUR, y=corn_price, color=period)) +
  geom_point(shape=1) +
  geom_smooth(method=lm, se=FALSE) +
  ggtitle("Corn Prices vs. Stock-to-Use Ratio (1973–2019)") +
  labs(y="Corn Price ($)", x="Stock-to-Use Ratio")

# Run a linear regression with time period specific
reg3 <- lm(corn_price ~ SUR + P2006 + SUR:P2006, data=WASDE)
summary(reg3)
tbl_regression(reg3, intercept = TRUE) %>%
  add_glance_source_note(include = c(r.squared, nobs))

# Collect the residuals from the last regression, create a time series of the errors with a one-year lag of the error, then regress the error terms on the lagged error terms
error <- ts(resid(reg3), start=1973, end=2019, frequency=1)   # the ts() function tells R to set the errors as a time-series 
lag_error <- stats::lag(error, -1)                                   # the lag() function creates a one-period lag of the error term
error <- cbind(error, lag_error)                              # cbind() binds the specified vectors together as columns to create a new data frame

reg4 <- lm(error ~ lag_error, data=error)

summary(reg4)
