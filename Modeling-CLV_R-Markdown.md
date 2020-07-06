---
title: "Modeling Customer Lifetime and Churn PreventionC"
output: 
  html_document:
    keep_md: true
---




## Load required packages

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
library(corrplot)
```

```
## corrplot 0.84 loaded
```

```r
library(viridisLite)
library(viridis)
library(lattice)
library(car)
```

```
## Loading required package: carData
```

```
## 
## Attaching package: 'car'
```

```
## The following object is masked from 'package:dplyr':
## 
##     recode
```

```r
library(MASS)
```

```
## 
## Attaching package: 'MASS'
```

```
## The following object is masked from 'package:dplyr':
## 
##     select
```

```r
library(caret)
library(pROC)
```

```
## Type 'citation("pROC")' for a citation.
```

```
## 
## Attaching package: 'pROC'
```

```
## The following objects are masked from 'package:stats':
## 
##     cov, smooth, var
```

```r
library(boot)
```

```
## 
## Attaching package: 'boot'
```

```
## The following object is masked from 'package:car':
## 
##     logit
```

```
## The following object is masked from 'package:lattice':
## 
##     melanoma
```

```r
library(ModelMetrics)
```

```
## 
## Attaching package: 'ModelMetrics'
```

```
## The following object is masked from 'package:pROC':
## 
##     auc
```

```
## The following objects are masked from 'package:caret':
## 
##     confusionMatrix, precision, recall, sensitivity, specificity
```

```
## The following object is masked from 'package:base':
## 
##     kappa
```


## Load required files

```r
salesData = read.csv("/Users/bhavinghoghari/Desktop/Work/Self Learn/R/Marketing Analytics in R/Data Camp_Data sets/salesData.csv")
```


## Explore salesData using str()

```r
str(salesData, give.attr = F)
```

```
## 'data.frame':	5122 obs. of  14 variables:
##  $ id                   : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ nItems               : int  1469 1463 262 293 108 216 174 122 204 308 ...
##  $ mostFreqStore        : Factor w/ 10 levels "Boston","Colorado Springs",..: 10 10 2 2 2 1 3 9 6 9 ...
##  $ mostFreqCat          : Factor w/ 10 levels "Alcohol","Baby",..: 1 1 10 3 4 1 8 10 3 1 ...
##  $ nCats                : int  72 73 55 50 32 41 36 31 41 52 ...
##  $ preferredBrand       : Factor w/ 10 levels "Akar","Alekto",..: 10 10 3 10 3 3 3 3 3 3 ...
##  $ nBrands              : int  517 482 126 108 79 98 78 62 99 103 ...
##  $ nPurch               : int  82 88 56 43 18 35 34 12 26 33 ...
##  $ salesLast3Mon        : num  2742 2791 1530 1766 1180 ...
##  $ salesThisMon         : num  1284 1243 683 730 553 ...
##  $ daysSinceLastPurch   : int  1 1 1 1 12 2 2 4 14 1 ...
##  $ meanItemPrice        : num  1.87 1.91 5.84 6.03 10.93 ...
##  $ meanShoppingCartValue: num  33.4 31.7 27.3 41.1 65.6 ...
##  $ customerDuration     : int  821 657 548 596 603 673 612 517 709 480 ...
```


## Now visualize the correlation of the continuous explanatory variables for the past three months with the sales variable of this month. Use the functions cor() and corrplot() and the pipe operator. 

```r
# Plot correlation
?select
```

```
## Help on topic 'select' was found in the following packages:
## 
##   Package               Library
##   dplyr                 /Library/Frameworks/R.framework/Versions/3.5/Resources/library
##   MASS                  /Library/Frameworks/R.framework/Versions/3.5/Resources/library
## 
## 
## Using the first match ...
```

```r
salesData %>% 
  select_if(is.numeric) %>% 
  dplyr::select(-id) %>% 
  cor(use = "complete.obs") %>% 
  corrplot(method = 'square',
           diag = F,
           tl.col = "black")
```

![](Modeling-CLV_R-Markdown_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Correlation for number of items orderd, number of brands, ncats, and sales in last 3 months has positive strong correlation with sales.  
We also observe somewhat positive correlation with customer druration to sales


## Make a box plot to see the distribution of most frequent stores and preferred brand

#### Most frequent stores

```r
ggplot(data = salesData, aes(x = mostFreqStore, y = salesThisMon, color = salesThisMon)) +
  geom_boxplot(alpha = 0.25) +
  geom_jitter(shape = 16, position = position_jitter(0.2), size = 1.5, alpha = 0.7) +
  scale_color_gradientn(name = "", colours = rev(viridis::viridis(20))) +
  guides(color = F)
```

![](Modeling-CLV_R-Markdown_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

#### Most preferred brands

```r
ggplot(data = salesData, aes(x = preferredBrand, y = salesThisMon, color = salesThisMon)) +
  geom_boxplot(alpha = 0.25) +
  geom_jitter(shape = 16, position = position_jitter(0.2), size = 1.5, alpha = 0.7) +
  scale_color_gradientn(name = "", colours = rev(viridis::viridis(20))) +
  guides(color = F) 
```

![](Modeling-CLV_R-Markdown_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


## Linear Regression Model
Now that we have looked at the correlation between variables,
Let's build a linear regression model

Lets start by building a simple linear regression model using sales in the last 3 months as independent varioable since it has a strong positive correlation with sales as observed in above correlation plot

```r
salesSimpleModel = lm(salesThisMon ~ salesLast3Mon, 
                       data = salesData)

# Looking at model summary
summary(salesSimpleModel)
```

```
## 
## Call:
## lm(formula = salesThisMon ~ salesLast3Mon, data = salesData)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -570.18  -68.26    3.21   72.98  605.58 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   99.690501   6.083886   16.39   <2e-16 ***
## salesLast3Mon  0.382696   0.004429   86.40   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 117.5 on 5120 degrees of freedom
## Multiple R-squared:  0.5932,	Adjusted R-squared:  0.5931 
## F-statistic:  7465 on 1 and 5120 DF,  p-value: < 2.2e-16
```

1. The regression coeffecient of 0.38 means for every increase in sales of past 3 months, sales for this month will increase by 0.38 units.
2. And 59.3% of change in sales for this month can be attributed to sales in past 3 months.


## Build Multiple linear regression model

```r
salesModel1 <- lm(salesThisMon ~ . - id, 
                 data = salesData)
summary(salesModel1)
```

```
## 
## Call:
## lm(formula = salesThisMon ~ . - id, data = salesData)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -322.76  -50.76    0.78   50.90  398.79 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -2.585e+02  1.762e+01 -14.673  < 2e-16 ***
## nItems                         1.605e-01  2.709e-02   5.923 3.37e-09 ***
## mostFreqStoreColorado Springs -7.167e+00  4.350e+00  -1.648 0.099503 .  
## mostFreqStoreColumbus          9.579e-01  3.680e+00   0.260 0.794642    
## mostFreqStoreDenver           -8.601e+00  5.130e+00  -1.676 0.093722 .  
## mostFreqStoreHonolulu         -1.588e+01  4.916e+00  -3.231 0.001242 ** 
## mostFreqStoreJersey           -2.169e+01  5.031e+00  -4.311 1.66e-05 ***
## mostFreqStoreOrlando          -1.052e+01  4.492e+00  -2.342 0.019210 *  
## mostFreqStoreSan Diego        -2.009e+01  5.717e+00  -3.514 0.000446 ***
## mostFreqStoreSeattle          -9.784e+00  3.539e+00  -2.765 0.005716 ** 
## mostFreqStoreStockton         -1.176e+02  3.580e+01  -3.286 0.001022 ** 
## mostFreqCatBaby               -3.413e+00  3.513e+00  -0.972 0.331249    
## mostFreqCatBakery             -1.025e+01  5.456e+00  -1.879 0.060339 .  
## mostFreqCatBeverages           3.351e-01  7.008e+00   0.048 0.961867    
## mostFreqCatClothes            -8.527e+00  6.213e+00  -1.372 0.170010    
## mostFreqCatFresh food         -6.372e+00  7.245e+00  -0.880 0.379164    
## mostFreqCatFrozen food        -8.084e+00  3.840e+00  -2.105 0.035332 *  
## mostFreqCatPackaged food      -8.346e-01  4.356e+00  -0.192 0.848063    
## mostFreqCatPets                8.508e+00  7.242e+00   1.175 0.240102    
## mostFreqCatShoes               3.298e+00  3.286e+00   1.004 0.315452    
## nCats                         -7.917e-01  2.345e-01  -3.375 0.000742 ***
## preferredBrandAlekto          -5.590e+00  1.649e+01  -0.339 0.734645    
## preferredBrandBo              -2.505e+01  1.438e+01  -1.742 0.081516 .  
## preferredBrandKatram          -6.264e+01  2.334e+01  -2.684 0.007295 ** 
## preferredBrandKellest         -5.349e+01  2.214e+01  -2.416 0.015713 *  
## preferredBrandMedeia          -2.161e+01  1.556e+01  -1.389 0.164967    
## preferredBrandMoone           -4.166e+01  1.627e+01  -2.561 0.010453 *  
## preferredBrandNilima          -2.888e+01  1.454e+01  -1.986 0.047040 *  
## preferredBrandTanvi            3.135e+01  2.129e+01   1.472 0.141076    
## preferredBrandVeina           -1.861e+01  1.451e+01  -1.282 0.199837    
## nBrands                       -4.804e-02  8.468e-02  -0.567 0.570533    
## nPurch                         4.758e-01  1.513e-01   3.145 0.001669 ** 
## salesLast3Mon                  3.753e-01  8.599e-03  43.652  < 2e-16 ***
## daysSinceLastPurch             1.794e-01  1.524e-01   1.177 0.239322    
## meanItemPrice                  1.793e-01  9.289e-02   1.930 0.053680 .  
## meanShoppingCartValue          2.596e-01  2.618e-02   9.918  < 2e-16 ***
## customerDuration               5.713e-01  7.148e-03  79.927  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 77.34 on 5085 degrees of freedom
## Multiple R-squared:  0.8249,	Adjusted R-squared:  0.8237 
## F-statistic: 665.6 on 36 and 5085 DF,  p-value: < 2.2e-16
```

But one of the problem with multiple regression model is VIF.
+ check VIF from "car" package and get rid of with one pair of variable with VIF valuef of abve 10

**A variance inflation factor greater than 5 hints to multicollinearity, greater than 10 indicates unstable regression estimates.**


```r
# library(car)
car::vif(salesModel1)
```

```
##                            GVIF Df GVIF^(1/(2*Df))
## nItems                11.772600  1        3.431122
## mostFreqStore          1.260469  9        1.012943
## mostFreqCat            1.527348  9        1.023809
## nCats                  8.402073  1        2.898633
## preferredBrand         1.682184  9        1.029316
## nBrands               14.150868  1        3.761764
## nPurch                 3.083952  1        1.756119
## salesLast3Mon          8.697663  1        2.949180
## daysSinceLastPurch     1.585057  1        1.258991
## meanItemPrice          1.987665  1        1.409846
## meanShoppingCartValue  2.247579  1        1.499193
## customerDuration       1.004664  1        1.002329
```


Build another model by removing variables nItems and preferredBrand

```r
salesModel2 <- lm(salesThisMon ~ . - id - nBrands - preferredBrand, 
                 data = salesData)
summary(salesModel2)
```

```
## 
## Call:
## lm(formula = salesThisMon ~ . - id - nBrands - preferredBrand, 
##     data = salesData)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -322.66  -51.26    0.60   51.28  399.10 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -2.828e+02  1.007e+01 -28.079  < 2e-16 ***
## nItems                         1.470e-01  2.093e-02   7.023 2.45e-12 ***
## mostFreqStoreColorado Springs -7.829e+00  4.351e+00  -1.799 0.072047 .  
## mostFreqStoreColumbus          5.960e-01  3.682e+00   0.162 0.871391    
## mostFreqStoreDenver           -9.721e+00  5.133e+00  -1.894 0.058305 .  
## mostFreqStoreHonolulu         -1.604e+01  4.925e+00  -3.257 0.001134 ** 
## mostFreqStoreJersey           -2.215e+01  5.011e+00  -4.420 1.01e-05 ***
## mostFreqStoreOrlando          -1.104e+01  4.500e+00  -2.454 0.014154 *  
## mostFreqStoreSan Diego        -1.985e+01  5.718e+00  -3.472 0.000521 ***
## mostFreqStoreSeattle          -9.573e+00  3.542e+00  -2.702 0.006906 ** 
## mostFreqStoreStockton         -1.129e+02  3.559e+01  -3.171 0.001530 ** 
## mostFreqCatBaby               -3.496e+00  3.469e+00  -1.008 0.313594    
## mostFreqCatBakery             -9.908e+00  5.451e+00  -1.818 0.069188 .  
## mostFreqCatBeverages           9.253e-02  7.024e+00   0.013 0.989489    
## mostFreqCatClothes            -3.828e+00  6.090e+00  -0.629 0.529674    
## mostFreqCatFresh food         -5.935e+00  7.255e+00  -0.818 0.413368    
## mostFreqCatFrozen food        -7.196e+00  3.813e+00  -1.887 0.059179 .  
## mostFreqCatPackaged food      -1.387e+00  4.311e+00  -0.322 0.747746    
## mostFreqCatPets                9.073e+00  7.245e+00   1.252 0.210467    
## mostFreqCatShoes               2.649e+00  3.256e+00   0.814 0.415917    
## nCats                         -9.585e-01  1.956e-01  -4.900 9.90e-07 ***
## nPurch                         5.092e-01  1.513e-01   3.364 0.000773 ***
## salesLast3Mon                  3.782e-01  8.480e-03  44.604  < 2e-16 ***
## daysSinceLastPurch             1.712e-01  1.526e-01   1.122 0.262022    
## meanItemPrice                  2.253e-01  9.168e-02   2.457 0.014034 *  
## meanShoppingCartValue          2.584e-01  2.620e-02   9.861  < 2e-16 ***
## customerDuration               5.708e-01  7.162e-03  79.707  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 77.56 on 5095 degrees of freedom
## Multiple R-squared:  0.8236,	Adjusted R-squared:  0.8227 
## F-statistic: 914.9 on 26 and 5095 DF,  p-value: < 2.2e-16
```

```r
vif(salesModel2)
```

```
##                           GVIF Df GVIF^(1/(2*Df))
## nItems                6.987456  1        2.643380
## mostFreqStore         1.178251  9        1.009154
## mostFreqCat           1.269636  9        1.013351
## nCats                 5.813494  1        2.411119
## nPurch                3.069046  1        1.751869
## salesLast3Mon         8.412520  1        2.900435
## daysSinceLastPurch    1.579426  1        1.256752
## meanItemPrice         1.925494  1        1.387622
## meanShoppingCartValue 2.238410  1        1.496132
## customerDuration      1.002981  1        1.001489
```


## Now, it's time for model validation, model fit, and prediction
*F test is the test used to validate the model*
In our salesModel2, the value of p is less tha 0.05 and hence, the hypothesis of Rsquare equal to 0 is rejected

#### Method to avoid overfitting:
1. AIC() from stats package - useful whe comparing two or more models
2. stepAIC() from MASS package
3. out-of-sample model validation
4. cross-validation

### Predcict the sales and calculate it'e mean on new data set

```r
# load new data set
salesData2_4 = read.csv("/Users/bhavinghoghari/Desktop/Work/Self Learn/R/Marketing Analytics in R/Data Camp_Data sets/salesDataMon2To4.csv")

# use predict () function on new data set
predictSales5 = predict(salesModel2, newdata = salesData2_4)

# calculate mean
mean(predictSales5)
```

```
## [1] 625.1438
```



# Churn Prevention
### Using Logistic Regression


```r
# Load required file
defaultData = read.csv("/Users/bhavinghoghari/Desktop/Work/Self Learn/R/Marketing Analytics in R/Data Camp_Data sets/defaultData.csv", header = T, sep = ";")

glimpse(defaultData)
```

```
## Observations: 18,000
## Variables: 25
## $ ID             <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,…
## $ limitBal       <int> 20000, 120000, 90000, 50000, 50000, 50000, 500000, 100…
## $ sex            <int> 2, 2, 2, 2, 1, 1, 1, 2, 2, 1, 2, 2, 2, 1, 1, 2, 1, 1, …
## $ education      <int> 2, 2, 2, 2, 2, 1, 1, 2, 3, 3, 3, 1, 2, 2, 1, 3, 1, 1, …
## $ marriage       <int> 1, 2, 2, 1, 1, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 3, 2, 1, …
## $ age            <int> 24, 26, 34, 37, 57, 37, 29, 23, 28, 35, 34, 51, 41, 30…
## $ pay1           <int> 2, -1, 0, 0, -1, 0, 0, 0, 0, -2, 0, -1, -1, 1, 0, 1, 0…
## $ pay2           <int> 2, 2, 0, 0, 0, 0, 0, -1, 0, -2, 0, -1, 0, 2, 0, 2, 0, …
## $ pay3           <int> -1, 0, 0, 0, -1, 0, 0, -1, 2, -2, 2, -1, -1, 2, 0, 0, …
## $ pay4           <int> -1, 0, 0, 0, 0, 0, 0, 0, 0, -2, 0, -1, -1, 0, 0, 0, 2,…
## $ pay5           <int> -2, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, -1, -1, 0, 0, 0, 2,…
## $ pay6           <int> -2, 2, 0, 0, 0, 0, 0, -1, 0, -1, -1, 2, -1, 2, 0, 0, 2…
## $ billAmt1       <int> 3913, 2682, 29239, 46990, 8617, 64400, 367965, 11876, …
## $ billAmt2       <int> 3102, 1725, 14027, 48233, 5670, 57069, 412023, 380, 14…
## $ billAmt3       <int> 689, 2682, 13559, 49291, 35835, 57608, 445007, 601, 12…
## $ billAmt4       <int> 0, 3272, 14331, 28314, 20940, 19394, 542653, 221, 1221…
## $ billAmt5       <int> 0, 3455, 14948, 28959, 19146, 19619, 483003, -159, 117…
## $ billAmt6       <int> 0, 3261, 15549, 29547, 19131, 20024, 473944, 567, 3719…
## $ payAmt1        <int> 0, 0, 1518, 2000, 2000, 2500, 55000, 380, 3329, 0, 230…
## $ payAmt2        <int> 689, 1000, 1500, 2019, 36681, 1815, 40000, 601, 0, 0, …
## $ payAmt3        <int> 0, 1000, 1000, 1200, 10000, 657, 38000, 0, 432, 0, 50,…
## $ payAmt4        <int> 0, 1000, 1000, 1100, 9000, 1000, 20239, 581, 1000, 130…
## $ payAmt5        <int> 0, 0, 1000, 1069, 689, 1000, 13750, 1687, 1000, 1122, …
## $ payAmt6        <int> 0, 2000, 5000, 1000, 679, 800, 13770, 1542, 1000, 0, 6…
## $ PaymentDefault <int> 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, …
```


Get some more insights about the variable of interest PaymentDefault by plotting a bar chart of the two levels.

```r
ggplot(data = defaultData, aes(x = PaymentDefault,)) +
  geom_histogram(stat = "count")
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

![](Modeling-CLV_R-Markdown_files/figure-html/unnamed-chunk-13-1.png)<!-- -->


#### Logistic Regression Model
##### How does hypothesis test actually works?
When we draw a curve on graph for 0 and 1, we see the value is towards the end of tail of 0, then the null hypothesis is more like to be proven void.

#### Model Selection
It is important to understand which variables to include in the model.
One of the few method is to use "stepAIC()" from package "mass".
+ This method uses selects and drops all variable simantaniously until the AIC value of model drops to lowest possible value 

```r
# build a logistic regression model
logitModelFull <- glm(PaymentDefault ~ limitBal + sex + education + marriage +
                   age + pay1 + pay2 + pay3 + pay4 + pay5 + pay6 + billAmt1 + 
                   billAmt2 + billAmt3 + billAmt4 + billAmt5 + billAmt6 + payAmt1 + 
                   payAmt2 + payAmt3 + payAmt4 + payAmt5 + payAmt6, 
                 family = binomial, data = defaultData)

# Take a look at the model
summary(logitModelFull)
```

```
## 
## Call:
## glm(formula = PaymentDefault ~ limitBal + sex + education + marriage + 
##     age + pay1 + pay2 + pay3 + pay4 + pay5 + pay6 + billAmt1 + 
##     billAmt2 + billAmt3 + billAmt4 + billAmt5 + billAmt6 + payAmt1 + 
##     payAmt2 + payAmt3 + payAmt4 + payAmt5 + payAmt6, family = binomial, 
##     data = defaultData)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.0893  -0.7116  -0.5615  -0.2794   4.2501  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -5.711e-01  1.505e-01  -3.795 0.000148 ***
## limitBal    -4.825e-07  1.985e-07  -2.431 0.015052 *  
## sex         -8.251e-02  3.880e-02  -2.127 0.033457 *  
## education   -1.217e-01  2.745e-02  -4.434 9.23e-06 ***
## marriage    -1.711e-01  4.016e-02  -4.259 2.05e-05 ***
## age          4.824e-03  2.257e-03   2.137 0.032570 *  
## pay1         5.743e-01  2.221e-02  25.864  < 2e-16 ***
## pay2         5.156e-02  2.552e-02   2.020 0.043336 *  
## pay3         7.811e-02  2.863e-02   2.728 0.006375 ** 
## pay4        -1.191e-02  3.285e-02  -0.363 0.716838    
## pay5         1.080e-01  3.381e-02   3.193 0.001406 ** 
## pay6        -1.956e-02  2.750e-02  -0.711 0.476852    
## billAmt1    -7.948e-06  1.582e-06  -5.023 5.09e-07 ***
## billAmt2     4.911e-06  2.006e-06   2.448 0.014350 *  
## billAmt3     4.203e-07  1.698e-06   0.247 0.804572    
## billAmt4    -1.587e-08  1.872e-06  -0.008 0.993234    
## billAmt5     9.703e-07  2.154e-06   0.451 0.652293    
## billAmt6     6.758e-07  1.591e-06   0.425 0.670955    
## payAmt1     -1.878e-05  3.252e-06  -5.777 7.61e-09 ***
## payAmt2     -6.406e-06  2.364e-06  -2.710 0.006731 ** 
## payAmt3     -3.325e-06  2.401e-06  -1.385 0.166153    
## payAmt4     -3.922e-06  2.342e-06  -1.675 0.093970 .  
## payAmt5     -2.383e-06  2.168e-06  -1.099 0.271635    
## payAmt6     -1.916e-06  1.618e-06  -1.184 0.236521    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 19438  on 17999  degrees of freedom
## Residual deviance: 17216  on 17976  degrees of freedom
## AIC: 17264
## 
## Number of Fisher Scoring iterations: 5
```


Extract the coefficients from the model, then transform them to the odds ratios and round
*By applying the exp() function to the coefficients (coef()) of the logit model, you can get to the effects on the odds (odds ratios).*

```r
# Take a look at the odds ratios
coefsexp <- coef(logitModelFull) %>% exp() %>% round(2)
coefsexp
```

```
## (Intercept)    limitBal         sex   education    marriage         age 
##        0.56        1.00        0.92        0.89        0.84        1.00 
##        pay1        pay2        pay3        pay4        pay5        pay6 
##        1.78        1.05        1.08        0.99        1.11        0.98 
##    billAmt1    billAmt2    billAmt3    billAmt4    billAmt5    billAmt6 
##        1.00        1.00        1.00        1.00        1.00        1.00 
##     payAmt1     payAmt2     payAmt3     payAmt4     payAmt5     payAmt6 
##        1.00        1.00        1.00        1.00        1.00        1.00
```


Checking the odds ratio helps us get a direction of effect of independent variables to dependent variables
> The above result shows that married people are more likely to buy back by factor of 0.84 compared to somebody who is not marrried.


#### Build another model using stepAIC() and see the difference in AIC value
load required package "MASS"

```r
# library(MASS)
# Set trace = 0, as you do not want to get an output for the whole model selection process. Save the result to the object logitModelNew
logitModelNew <- stepAIC(logitModelFull, trace = 0)

summary(logitModelNew)
```

```
## 
## Call:
## glm(formula = PaymentDefault ~ limitBal + sex + education + marriage + 
##     age + pay1 + pay2 + pay3 + pay5 + billAmt1 + billAmt2 + billAmt5 + 
##     payAmt1 + payAmt2 + payAmt3 + payAmt4, family = binomial, 
##     data = defaultData)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.0839  -0.7119  -0.5611  -0.2839   4.1800  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -5.699e-01  1.504e-01  -3.790 0.000151 ***
## limitBal    -5.201e-07  1.954e-07  -2.661 0.007791 ** 
## sex         -8.206e-02  3.878e-02  -2.116 0.034338 *  
## education   -1.212e-01  2.744e-02  -4.418 9.96e-06 ***
## marriage    -1.724e-01  4.014e-02  -4.295 1.75e-05 ***
## age          4.863e-03  2.256e-03   2.156 0.031092 *  
## pay1         5.740e-01  2.218e-02  25.882  < 2e-16 ***
## pay2         4.979e-02  2.552e-02   1.951 0.051048 .  
## pay3         7.197e-02  2.573e-02   2.798 0.005146 ** 
## pay5         8.859e-02  2.249e-02   3.938 8.20e-05 ***
## billAmt1    -8.130e-06  1.580e-06  -5.144 2.69e-07 ***
## billAmt2     5.238e-06  1.775e-06   2.951 0.003165 ** 
## billAmt5     1.790e-06  8.782e-07   2.038 0.041554 *  
## payAmt1     -1.931e-05  3.258e-06  -5.928 3.06e-09 ***
## payAmt2     -6.572e-06  2.092e-06  -3.142 0.001681 ** 
## payAmt3     -3.693e-06  2.187e-06  -1.689 0.091241 .  
## payAmt4     -4.611e-06  2.062e-06  -2.237 0.025306 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 19438  on 17999  degrees of freedom
## Residual deviance: 17220  on 17983  degrees of freedom
## AIC: 17254
## 
## Number of Fisher Scoring iterations: 5
```


The formula is saved in an object so that you don't have to type the whole equation again when you want to use it later.

```r
# Save the formula of the new model (it will be needed for the out-of-sample part) 
formulaLogit <- as.formula(summary(logitModelNew)$call)
formulaLogit
```

```
## PaymentDefault ~ limitBal + sex + education + marriage + age + 
##     pay1 + pay2 + pay3 + pay5 + billAmt1 + billAmt2 + billAmt5 + 
##     payAmt1 + payAmt2 + payAmt3 + payAmt4
```


#### In-Sample model fit and Thresholding
#### Ways to measure the accuracy and validation of model

##### 1. Similar to R square in linear regression, in logistic regression we use Pseudo R square statistics.
This includes:
1. McFadden,
2. Cox & Snell
3. Nagelkerke

Interpretation - For this method,
1. **Reasonable > 0.2** value of 0.2 or higher is resonable
2. **Good if > 0.4** value of 0.4 or higher is considered good
3. **Very Good if > 0.5** value of 0.5 or higher is considered to be very good

Package "descr" and function LogRegR2() gives us a value of Pseudo R2 for above three methods.
_Garbage in, Garbage out_
_model can only be as good as the data_

##### 2. Another way to test the accuracy of model is to use predict() to predict the future values using type = "response" and compare it against the actual values

##### 3. Another test is to use confusion matrix from package "SDMTools:
_note that SDMTools cannot be downloaded from CRAN anymore. Install it instead it via '''{r}remote::install_version("SDMTools", "1.1-221")'''_

##### Checking the accuracy of of model
It is not always correct to check the accuracy or mean of predicted to actual values since the model might accurately predict that more customers would not return but it does also predict that more customers would return while in actual only few customers actually returned. 

Hence, finding the right threshold is necessary to acheive the highest possible payoff even though it means decrease in model accuracy rate.

```r
# calculate prediction using predict() on logitModelFull
defaultData$predFull = predict(logitModelFull, type = "response", na.action = na.exclude)
```


Now build a confusion matrix from package "caret" to see the accuracy of the model
_make sure that the string type is set to factor for actual values and predicted values_

```r
# set a threshold of 0.5 and convert it to integer to match with the actual values class
defaultData$predFull_0.5 = as.integer(ifelse(defaultData$predFull >= 0.5, 1, 0))

# create a confusion matrix using package "caret" and set both string to factor class
# library(caret)
confMatrixModelFull = confusionMatrix(as.factor(defaultData$PaymentDefault), as.factor(defaultData$predFull_0.5))
confMatrixModelFull
```

```
##      [,1]  [,2]
## [1,]    0     0
## [2,]    0 13850
```

Since we calculated the accuracy for confMatrixModelFull, also let's calculate accuracy for logitModelNew which we built above usinf stepAIC

First step is to:
1. Predict the probability using predict function and set type = "response"
2. Set a desired threshold and mutate value to new column
3. Create confusion matrix


```r
# predict the values
defaultData$predNew = predict(logitModelNew, type = "response", na.action = na.exclude)

# set threshold and convert values to 1,0
defaultData$predNew_0.5 = as.integer(ifelse(defaultData$predNew >= 0.5, 1, 0))

# create a confusion matrix using package "caret" and set both string to factor class
# library(caret)
confMatrixModelNew = confusionMatrix(as.factor(defaultData$PaymentDefault), as.factor(defaultData$predNew_0.5))
confMatrixModelNew
```

```
##      [,1]  [,2]
## [1,]    0     0
## [2,]    0 13850
```

From the above confusion matrix:
1. **True Positive** - We correctly predicted that customer would default = 998
2. **True Negative** - We correctly predicted that customer would not default = 13443
3. **False Positive** - We incorrectly predicted that customer would default = 3152
4. **False Negative** - We incorrectly predicted that customer would not default = 407

#### Finding the optimal threshold is important
Imagine you are running a campaign with the aim of preventing customers to default. You can lay out your campaign with the help of your predictions. Thereby, the choice of the threshold is essential for your results. If you know the costs and the rewards of your campaign, you can empirically check which threshold is most reasonable. In this exercise, we are faced with the following scenario:

If a customer does not default due to our campaign, i.e. if we predicted the default correctly (true positive) we are rewarded with 1000€. If however we aim our campaign at a customer who would not have defaulted anyways, i.e. if we falsely predicted the customer (false positive) to default, we are faced with costs of 250€.

From the last exercise we know that the restricted model was the best one. So only calculate the optimal threshold for that model.

```r
# create a confusion matrix for actual vs predicted
confusionMatrix_tableNew_0.5 = confusionMatrix(defaultData$PaymentDefault, defaultData$predNew_0.5)

# Now 
payoff_0.5 = 1000*confusionMatrix_tableNew_0.5[2,2] - 250*confusionMatrix_tableNew_0.5[2,1]
payoff_0.5
```

```
## [1] 896250
```


#### Below we now create 
1. Predicted column for each threshold from 0.1 - 0.5
2. Then create confusion matrix table for each preeicted threshold values
3. Calculate payoff from each table. The value of 1000 is reward for calculating the **True Positve** value and 250 is penalty for **False Positive** values. 
*Value of 1000 and 250 is based on knowing the value of customer. It can be changed according to business*

```r
# create a payoffmatrix for different threshold using loop
payoffmatrix_usingLoopFunction = data.frame(threshold = c(0.1,0.2,0.3,0.4,0.5), 
                                            payoff = c(NA,NA,NA,NA,NA))

# Set up the loop
for(i in 1:5) {
  # Calculate specific confusion matrix with respective threshold
  confMatrix <- confusionMatrix(defaultData$PaymentDefault,
                defaultData$predNew, 
                cutoff = payoffmatrix_usingLoopFunction$threshold[i])
  # Calculate payoff and save it to the corresponding row
  payoffmatrix_usingLoopFunction$payoff[i] <- 1000*confMatrix[2,2] - 250*confMatrix[2,1]
}
payoffmatrix_usingLoopFunction
```

```
##   threshold  payoff
## 1       0.1  994250
## 2       0.2 1440250
## 3       0.3 1575500
## 4       0.4 1328750
## 5       0.5  896250
```
> Threshold of 0.3 seems to be payoff maximum hence, we will use 0.3 as our cutoff/threshold

*Another way of calculating threshold is by creating a predictive column for each threshold*

```r
str(defaultData$predNew_0.2)
```

```
##  NULL
```

```r
defaultData$predNew_0.1 = as.integer(ifelse(defaultData$predNew > 0.1, 1, 0))
defaultData$predNew_0.2 = as.integer(ifelse(defaultData$predNew > 0.2, 1, 0))
defaultData$predNew_0.3 = as.integer(ifelse(defaultData$predNew > 0.3, 1, 0))
defaultData$predNew_0.4 = as.integer(ifelse(defaultData$predNew > 0.4, 1, 0))
defaultData$predNew_0.5 = as.integer(ifelse(defaultData$predNew > 0.5, 1, 0))

# now create table for threshold and calculate the optimal threshold value
confusionMatrix_tableNew_0.1 = table(defaultData$PaymentDefault, defaultData$predNew_0.1)
confusionMatrix_tableNew_0.2 = table(defaultData$PaymentDefault, defaultData$predNew_0.2)
confusionMatrix_tableNew_0.3 = table(defaultData$PaymentDefault, defaultData$predNew_0.3)
confusionMatrix_tableNew_0.4 = table(defaultData$PaymentDefault, defaultData$predNew_0.4)
confusionMatrix_tableNew_0.5 = table(defaultData$PaymentDefault, defaultData$predNew_0.5)

# now calculate payoff for each table 
payoff_0.1 = 1000*confusionMatrix_tableNew_0.1[2,2] - 250*confusionMatrix_tableNew_0.1[2,1]
payoff_0.2 = 1000*confusionMatrix_tableNew_0.2[2,2] - 250*confusionMatrix_tableNew_0.2[2,1]
payoff_0.3 = 1000*confusionMatrix_tableNew_0.3[2,2] - 250*confusionMatrix_tableNew_0.3[2,1]
payoff_0.4 = 1000*confusionMatrix_tableNew_0.4[2,2] - 250*confusionMatrix_tableNew_0.4[2,1]
payoff_0.5 = 1000*confusionMatrix_tableNew_0.5[2,2] - 250*confusionMatrix_tableNew_0.5[2,1]

# create payoffmatrix to compare threshold and its payoff
payoffmatrix = matrix(ncol = 2,list("threshold" = 0.1,0.2,0.3,0.4,0.5,
     "payoff" = payoff_0.1,payoff_0.2,payoff_0.3,payoff_0.4,payoff_0.5))
colnames(payoffmatrix) = c("threshold", "payoff")
payoffmatrix
```

```
##      threshold payoff 
## [1,] 0.1       3786250
## [2,] 0.2       2678750
## [3,] 0.3       1453750
## [4,] 0.4       901250 
## [5,] 0.5       210000
```


From the above calculation, it is seen that the highest payoff is received from setting threshold lowest to 0.1
Hence, setting a threshold to 0.1 for classification would lead to maximum returns

#### Now lets look at Out-of-Sampple Validation and Cross-Validation in order to avoid overfitting
**1. Out-of-Sample Validation**
Start by creating a train and test data set. And then create model on training and calculate prediction on test data set. Then create confusion matrix. Set threshold to 0.1 as calculated only for training data.

If the accuracy is not different from the accuracy of training set, this shows the model is not overfitting the data

**2. Cross-validation:**
First split your data set into four subset randomly. Three of the subset are used as training data.
Build a model on three train data set and use left one as test data. Follow the process and repeate testing model on all of four dataset one by one

cv.glm() function from package "boot" allows to measure cross validation for linear model


```r
# split data into train and test
set.seed(1031)
split1 = sample(1:nrow(defaultData), size = 0.7*nrow(defaultData))
train = defaultData[split1,]
test = defaultData[-split1,]

# create model on train data and 
logitTrainNew <- glm(formulaLogit, family = binomial, data = train) # Modeling
test$predNew <- predict(logitTrainNew, type = "response", newdata = test) # Prediction on test data set

# set threshold of 0.3
test$predNew_0.3_V2 = ifelse(test$predNew >= 0.3, 1, 0)

# Out-of-sample confusion matrix and accuracy
confMatrixModelNew = confusionMatrix(test$PaymentDefault, test$predNew_0.3_V2)
sum(diag(confMatrixModelNew)) / sum(confMatrixModelNew) # Compare this value to the in-sample accuracy
```

```
## [1] 0.785
```


Creating a threshold matrix to compare the payoff amount.
Since we already did it earlier, it wasn't required but we did it just to validate and practise.
Threshold of 0.3 remains valid.

```r
# create a payoffmatrix for different threshold using loop
payoffmatrix_usingLoopFunction_test = data.frame(threshold = c(0.1,0.2,0.3,0.4,0.5),
                                                 payoff = c(NA,NA,NA,NA,NA))

# Set up the loop
for(i in 1:5) {
  # Calculate specific confusion matrix with respective threshold
  confMatrix <- confusionMatrix(test$PaymentDefault, test$predNew, 
                cutoff = payoffmatrix_usingLoopFunction_test$threshold[i])
  # Calculate payoff and save it to the corresponding row
  payoffmatrix_usingLoopFunction_test$payoff[i] <- 1000*confMatrix[2,2] - 250*confMatrix[2,1]
}
payoffmatrix_usingLoopFunction_test
```

```
##   threshold payoff
## 1       0.1 334000
## 2       0.2 459500
## 3       0.3 477250
## 4       0.4 398750
## 5       0.5 255000
```

Upon looking at the pay off for test data, threshold 0.3 seems to be most effective. This is primarily due to high amount of payoff for **True Positive** and comparatively low penalty for **False Negative** prediction

'''r{} ModelMetrics::confusionMatrix()''' can be used to calculate confusion matrix from package ModelMetrics

```r
# load required package
# library(boot)
costAcc = function(r, pi = 0) {
  cm = ModelMetrics::confusionMatrix(r, pi, cutoff = 0.1)
  acc = sum(diag(cm)) / sum(cm)
  return(acc)
}

# Cross validated accuracy for logitModelNe
defaultData_raw = read.csv("/Users/bhavinghoghari/Desktop/Work/Self Learn/R/Marketing Analytics in R/Data Camp_Data sets/defaultData.csv", header = T, sep = ";")
set.seed(1034)
kfold6ModelNew = cv.glm(defaultData_raw, logitModelNew, cost = costAcc, K = 6)$delta[1] # kfold model is performed on original data set with 6 folds
```




#### Now create a payoffmatrix using loop function and confusion.matrix using SDMTools package but the package is no longer supported
needs to figure it out 

```r
# create a payoffmatrix
payoffmatrix_usingLoopFunction = data.frame(threshold = c(0.1,0.2,0.3,0.4,0.5), 
                                            payoff = c(NA,NA,NA,NA,NA))

# Set up the loop
for(i in 1:5) {
  # Calculate specific confusion matrix with respective threshold
  confMatrix <- confusionMatrix(defaultData$PaymentDefault,
                defaultData$predNew, 
                cutoff = payoffmatrix_usingLoopFunction$threshold[i])
  # Calculate payoff and save it to the corresponding row
  payoffmatrix_usingLoopFunction$payoff[i] <- 1000*confMatrix[2,2] - 250*confMatrix[2,1]
}
payoffmatrix_usingLoopFunction
```

```
##   threshold  payoff
## 1       0.1  994250
## 2       0.2 1440250
## 3       0.3 1575500
## 4       0.4 1328750
## 5       0.5  896250
```



```r
# library(pROC)
ROC = roc(defaultData$PaymentDefault, defaultData$predNew)
```

```
## Setting levels: control = 0, case = 1
```

```
## Setting direction: controls < cases
```

```r
plot(ROC, col = "blue")
```

![](Modeling-CLV_R-Markdown_files/figure-html/unnamed-chunk-28-1.png)<!-- -->

```r
pROC::auc(ROC)
```

```
## Area under the curve: 0.7181
```

