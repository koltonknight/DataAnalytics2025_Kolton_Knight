library("ggplot2")
library("readr")

## read dataset
NY_House_Dataset <- read_csv("C:/Users/kolto/Documents/Data Analytics/NY-House-Dataset.csv")

dataset <- NY_House_Dataset

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point()

## filter data
dataset <- dataset[dataset$PRICE<195000000,]

dataset <- dataset[dataset$PROPERTYSQFT!=2184.207862,]

dataset$PROPERTYSQFT[dataset$BROKERTITLE=="Brokered by Douglas Elliman - 575 Madison Ave"][85]


##Kolton's Work Below, Prof Eleish's Work above

## fit linear model
lmod1 <- lm(PRICE~BEDS, data = dataset)

lmod2 <- lm(PRICE~BATH, data = dataset)

lmod3 <- lm(log10(PRICE)~log10(BEDS + BATH), data = dataset)



## linear model summary
summary(lmod1)
summary(lmod2)
summary(lmod3)

## scatter plot with line of best fit
ggplot(dataset, aes(x = BEDS, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

ggplot(dataset, aes(x = BATH, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

ggplot(dataset, aes(x = log10(BEDS + BATH), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

## plot residuals

ggplot(lmod1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Lmod1 Residual vs. Fitted', x = 'Fitted', y = 'Residuals')

ggplot(lmod2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Lmod2 Residual vs. Fitted', x = 'Fitted', y = 'Residuals')

ggplot(lmod3, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Lmod3 Residual vs. Fitted', x = 'Fitted', y = 'Residuals')

