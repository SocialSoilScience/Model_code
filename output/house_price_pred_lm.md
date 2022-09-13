# Load Library

``` r
## 加载程序包,使用里面的管道函数
library(dplyr)

## 设置随机数种子
set.seed(202209)
```

# Data Processing

读入数据，生成R数据框。将
`price`、`sqft_living`、`sqft_lot`、`sqft_above`
这四个变量取对数；并计算到 2015 年时房屋的年龄。

``` r
house <- read.csv("./data-raw/ch6_house.csv",
                  colClasses = rep("numeric", 10))

house <- house %>%
  mutate(log_price = log(price)) %>%
  mutate(log_sqft_living = log(sqft_living)) %>%
  mutate(log_sqft_lot = log(sqft_lot)) %>%
  mutate(log_sqft_above = log(sqft_above)) %>%
  mutate(age = 2015-yr_built)
```

使用 `sample()`
函数将数据集随机划分为学习数据集和测试数据集。先抽取学习数据集的观测序号，学习数据集是抽取的观测序号对应的观测。测试数据集是未被抽取到学习数据集的观测。

``` r
id_learning <- sample(1:nrow(house), round(0.7*nrow(house)))
house_learning <- house[id_learning,]
house_testing <- house[-id_learning,]
```

# Fitting

对学习数据集拟合线性模型。因变量是 `log_price`，`log_sqft_living`
等变量均为自变量。

``` r
fit.lm <- lm(log_price ~ log_sqft_living + log_sqft_lot + log_sqft_above + age + bedrooms + bathrooms + floors + condition + grade, data = house_learning)
```

查看建模结果。

``` r
summary(fit.lm)
```

    ## 
    ## Call:
    ## lm(formula = log_price ~ log_sqft_living + log_sqft_lot + log_sqft_above + 
    ##     age + bedrooms + bathrooms + floors + condition + grade, 
    ##     data = house_learning)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.29041 -0.21109  0.01026  0.20526  1.67461 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      7.7428942  0.0735920 105.214  < 2e-16 ***
    ## log_sqft_living  0.5174014  0.0161666  32.004  < 2e-16 ***
    ## log_sqft_lot    -0.0336378  0.0035713  -9.419  < 2e-16 ***
    ## log_sqft_above  -0.0871745  0.0152195  -5.728 1.04e-08 ***
    ## age              0.0060742  0.0001142  53.181  < 2e-16 ***
    ## bedrooms        -0.0438852  0.0035787 -12.263  < 2e-16 ***
    ##  [ reached getOption("max.print") -- omitted 4 rows ]
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3165 on 15119 degrees of freedom
    ## Multiple R-squared:  0.638,  Adjusted R-squared:  0.6377 
    ## F-statistic:  2960 on 9 and 15119 DF,  p-value: < 2.2e-16

模型中各个自变量的系数均显著不为 0；模型的 R 方为 0.6406。

提取模型的系数估计值

``` r
coefficients(fit.lm)
```

    ##     (Intercept) log_sqft_living    log_sqft_lot  log_sqft_above             age        bedrooms       bathrooms          floors       condition           grade 
    ##     7.742894171     0.517401364    -0.033637793    -0.087174490     0.006074193    -0.043885175     0.077396849     0.068048324     0.034325337     0.240901189

提取模型的因变量拟合值。

``` r
yhat <- fitted(fit.lm)
str(yhat)
```

    ##  Named num [1:15129] 13.1 13.2 13.4 13 13.4 ...
    ##  - attr(*, "names")= chr [1:15129] "5338" "13607" "16751" "17709" ...

提取模型的残差。

``` r
resid <- residuals(fit.lm)
str(resid)
```

    ##  Named num [1:15129] -0.0141 -0.308 0.1671 -0.2173 0.443 ...
    ##  - attr(*, "names")= chr [1:15129] "5338" "13607" "16751" "17709" ...

# 模型诊断

将绘图窗口分为 2\*2
的矩阵。指定绘图区域离下边界、左边界、上边界和右边界的距离（单位为文本行数），方便画下所有诊断图。

画模型诊断图。

``` r
par(mfrow=c(2, 2))
par(mar=c(2.5, 2.5, 1.5, 1.5))

library(ggplot2)
plot(fit.lm, which=c(1:4))
```

![](/home/gaoch/Desktop/GitHub/Model_code/output/house_price_pred_lm_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

# Model Optimization

从 Cook 距离图中可以看出，序号为”15871”的观测是异常点。

去除序号为”15871”的观测，重新拟合线性模型

``` r
fit2.lm <- lm(log_price ~ log_sqft_living + log_sqft_lot + log_sqft_above + age + bedrooms + bathrooms + floors + condition + grade,data = house_learning[rownames(house_learning)!="15871",])

par(mfrow=c(2, 2))
par(mar=c(2.5, 2.5, 1.5, 1.5))
plot(fit2.lm, which=c(1:4))
```

![](/home/gaoch/Desktop/GitHub/Model_code/output/house_price_pred_lm_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

使用所得的线性模型对测试数据集进行预测。

``` r
prediction.lm <- predict(fit2.lm, house_testing)
```

`predition.lm` 中含有预测的对数价格，`exp(pred.lm)`
将对数价格转换为预测的价格。将预测价格与真实价格取差值，平方之后平均，再开根号。计算出测试数据集的房屋价格预测的均方根误差。

``` r
rmse.lm <- sqrt(mean((exp(prediction.lm) - house_testing$price)^2))

str(rmse.lm)
```

    ##  num 209576
