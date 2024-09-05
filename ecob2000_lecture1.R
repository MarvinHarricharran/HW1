#Marvin Harricharran 
#Hw 1

x <- 1:50
w <- 1 + sqrt(x)/2
example1 <- data.frame(x=x, y= x + rnorm(x)*w)
attach(example1)

fm <- lm(y ~ x)
summary(fm)

## 
## Call:
## lm(formula = y ~ x)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.5372 -1.5831  0.2436  1.4965  7.8226 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -0.45583    0.90898  -0.501    0.618    
## x            0.99171    0.03102  31.967   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.166 on 48 degrees of freedom
## Multiple R-squared:  0.9551, Adjusted R-squared:  0.9542 
## F-statistic:  1022 on 1 and 48 DF,  p-value: < 2.2e-16

lrf <- lowess(x, y)
plot(x, y)
lines(x, lrf$y)
abline(0, 1, lty=3)
abline(coef(fm))

detach()


getwd()
setwd("C:/Users/marvi/Downloads/Household_Pulse_data_ph4c2")

load("Household_Pulse_data_ph4c2.RData")
load("Household_Pulse_data_ph4c2.RData")
#glimpse(acs2017_ny) try this later
Household_Pulse_data[1:10,1:6]
attach(Household_Pulse_data)
summary(Household_Pulse_data)

summary(TBIRTH_YEAR[GENID_DESCRIBE == "female"])
summary(TBIRTH_YEAR[GENID_DESCRIBE == "male"])
summary(TBIRTH_YEAR[GENID_DESCRIBE == "transgender"])
summary(TBIRTH_YEAR[GENID_DESCRIBE == "other"])
summary(TBIRTH_YEAR[GENID_DESCRIBE == "NA"])
# here i want to find average ages of men and women
mean(TBIRTH_YEAR[GENID_DESCRIBE == "female"])
sd(TBIRTH_YEAR[GENID_DESCRIBE == "female"])
mean(TBIRTH_YEAR[GENID_DESCRIBE == "male"])
sd(TBIRTH_YEAR[GENID_DESCRIBE == "male"])


#1. The name of the members in my group are Alicia Persad, Flamur Kukaj, and Abdullah Al Iman.

#2. Somethings that surprised me about the data were the average education rates and the relation to the household income. Im not as surpirsed to see the median age and the relative household income to be so high. 

#3. Some questions I have are I would like to know more about the correlation between the average age and the response menatal health and the current working status.
