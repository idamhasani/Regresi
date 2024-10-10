Analisis Copula Zero Inflated Regression
Memanggil data
library(readxl)
## Warning: package 'readxl' was built under R version 4.1.3
# Memanggil Data
data <- read_xlsx("D:/1. LECTURE/SEMESTER 7/KAPSEL 2/BABAK BARU/Data baru.xlsx")
attach(data)
summary(data)
##        Y1               Y2               Y3               Y4        
##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
##  Median :0.0000   Median :0.0000   Median :0.0000   Median :0.0000  
##  Mean   :0.2308   Mean   :0.1282   Mean   :0.2991   Mean   :0.3162  
##  3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:1.0000  
##  Max.   :2.0000   Max.   :2.0000   Max.   :2.0000   Max.   :2.0000  
##        Y5              X11              X12             X13        
##  Min.   :0.0000   Min.   : 0.000   Min.   :0.000   Min.   : 0.000  
##  1st Qu.:0.0000   1st Qu.: 1.000   1st Qu.:0.000   1st Qu.: 1.000  
##  Median :0.0000   Median : 2.000   Median :0.000   Median : 2.000  
##  Mean   :0.1026   Mean   : 2.359   Mean   :0.641   Mean   : 2.265  
##  3rd Qu.:0.0000   3rd Qu.: 3.000   3rd Qu.:1.000   3rd Qu.: 3.000  
##  Max.   :1.0000   Max.   :23.000   Max.   :6.000   Max.   :15.000  
##       X14              X15              X21             X22       
##  Min.   : 0.000   Min.   :0.0000   Min.   :4.302   Min.   :4.050  
##  1st Qu.: 1.000   1st Qu.:0.0000   1st Qu.:6.100   1st Qu.:5.500  
##  Median : 3.000   Median :1.0000   Median :6.400   Median :5.900  
##  Mean   : 3.496   Mean   :0.9487   Mean   :6.503   Mean   :5.806  
##  3rd Qu.: 4.000   3rd Qu.:2.0000   3rd Qu.:6.900   3rd Qu.:6.350  
##  Max.   :15.000   Max.   :7.0000   Max.   :9.100   Max.   :7.800  
##       X23             X24             X25       
##  Min.   :4.000   Min.   :4.483   Min.   :4.000  
##  1st Qu.:6.350   1st Qu.:6.300   1st Qu.:5.500  
##  Median :6.700   Median :6.610   Median :6.000  
##  Mean   :6.558   Mean   :6.716   Mean   :5.951  
##  3rd Qu.:7.000   3rd Qu.:7.200   3rd Qu.:6.510  
##  Max.   :8.500   Max.   :8.300   Max.   :8.150
Eksplorasi Data
library(ggplot2)
## Warning: package 'ggplot2' was built under R version 4.1.3
vis_y1<-ggplot(data, aes(x=Y1)) + 
  geom_histogram(aes(fill=..count..), binwidth = 1)+
  scale_fill_gradient("Count", low = "#dff9fb", high = "#95afc0")+ggtitle("Frekuensi Banyaknya Data 0 pada Y1 ")

vis_y2<-ggplot(data, aes(x=Y2)) + 
  geom_histogram(aes(fill=..count..), binwidth = 1)+
  scale_fill_gradient("Count", low = "white", high = "#f1c40f")+
  ggtitle("Frekuensi Banyaknya Data 0 pada Y2")

vis_y3<-ggplot(data, aes(x=Y3)) + 
  geom_histogram(aes(fill=..count..), binwidth = 1)+
  scale_fill_gradient("Count", low = "white", high = "#10ac84")+
  ggtitle("Frekuensi Banyaknya Data 0 pada Y1 (Bali)")

vis_y4<-ggplot(data, aes(x=Y4)) + 
  geom_histogram(aes(fill=..count..), binwidth = 1)+
  scale_fill_gradient("Count", low = "white", high = "#eb3b5a")+
  ggtitle("Frekuensi Banyaknya Data 0 pada Y2 (Sulawesi)")

vis_y5<-ggplot(data, aes(x=Y5)) + 
  geom_histogram(aes(fill=..count..), binwidth = 1)+
  scale_fill_gradient("Count", low = "white", high = "#c44569")+
  ggtitle("Frekuensi Banyaknya Data 0 pada Y3 (Papua)")


vis_y1;vis_y2;vis_y3;vis_y4;vis_y5
     
Persiapan
model_Y1 <- glm(Y1~1, family=poisson(link="log"), data = data) 

model_Y2 <- glm(Y2~1, family=poisson(link="log"), data = data) 

model_Y3 <- glm(Y3~1, family=poisson(link="log"), data = data) 

model_Y4 <- glm(Y4~1, family=poisson(link="log"), data = data) 

model_Y5 <- glm(Y5~1, family=poisson(link="log"), data = data) 
Uji Overdispersi
library(AER) 
## Warning: package 'AER' was built under R version 4.1.3
## Loading required package: car
## Warning: package 'car' was built under R version 4.1.3
## Loading required package: carData
## Warning: package 'carData' was built under R version 4.1.3
## Loading required package: lmtest
## Warning: package 'lmtest' was built under R version 4.1.3
## Loading required package: zoo
## Warning: package 'zoo' was built under R version 4.1.3
## 
## Attaching package: 'zoo'
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
## Loading required package: sandwich
## Warning: package 'sandwich' was built under R version 4.1.3
## Loading required package: survival
dispersiontest(model_Y1, alternative="greater") 
## 
##  Overdispersion test
## 
## data:  model_Y1
## z = 0.60367, p-value = 0.273
## alternative hypothesis: true dispersion is greater than 1
## sample estimates:
## dispersion 
##   1.065527
dispersiontest(model_Y2, alternative="greater") 
## 
##  Overdispersion test
## 
## data:  model_Y2
## z = 0.043949, p-value = 0.4825
## alternative hypothesis: true dispersion is greater than 1
## sample estimates:
## dispersion 
##   1.005128
dispersiontest(model_Y3, alternative="greater") 
## 
##  Overdispersion test
## 
## data:  model_Y3
## z = 0.45073, p-value = 0.3261
## alternative hypothesis: true dispersion is greater than 1
## sample estimates:
## dispersion 
##   1.043712
dispersiontest(model_Y4, alternative="greater") 
## 
##  Overdispersion test
## 
## data:  model_Y4
## z = -1.0757, p-value = 0.859
## alternative hypothesis: true dispersion is greater than 1
## sample estimates:
## dispersion 
##  0.8999769
dispersiontest(model_Y5, alternative="greater") 
## 
##  Overdispersion test
## 
## data:  model_Y5
## z = -1.8205, p-value = 0.9657
## alternative hypothesis: true dispersion is greater than 1
## sample estimates:
## dispersion 
##  0.8974359
Uji Excess Zero
library(vcdExtra) 
## Warning: package 'vcdExtra' was built under R version 4.1.3
## Loading required package: vcd
## Warning: package 'vcd' was built under R version 4.1.3
## Loading required package: grid
## Loading required package: gnm
## Warning: package 'gnm' was built under R version 4.1.3
## 
## Attaching package: 'vcdExtra'
## The following object is masked from 'package:carData':
## 
##     Burt
zero.test(Y1)
## Score test for zero inflation
## 
##      Chi-square = 0.58122 
##      df = 1
##      pvalue: 0.44583
zero.test(Y2)
## Score test for zero inflation
## 
##      Chi-square = 0.00789 
##      df = 1
##      pvalue: 0.92924
zero.test(Y3)
## Score test for zero inflation
## 
##      Chi-square = 0.49028 
##      df = 1
##      pvalue: 0.4838
zero.test(Y4)
## Score test for zero inflation
## 
##      Chi-square = 0.47272 
##      df = 1
##      pvalue: 0.49174
zero.test(Y5)
## Score test for zero inflation
## 
##      Chi-square = 0.68205 
##      df = 1
##      pvalue: 0.40888
Scatter Plot
library(car) 

scatterplot(Y1,Y2)
## Warning in smoother(.x, .y, col = col[1], log.x = logged("x"), log.y =
## logged("y"), : could not fit smooth
 
scatterplot(Y2,Y3)
## Warning in smoother(.x, .y, col = col[1], log.x = logged("x"), log.y =
## logged("y"), : could not fit smooth
 
scatterplot(Y3,Y4)
## Warning in smoother(.x, .y, col = col[1], log.x = logged("x"), log.y =
## logged("y"), : could not fit smooth
 
scatterplot(Y4,Y5)
## Warning in smoother(.x, .y, col = col[1], log.x = logged("x"), log.y =
## logged("y"), : could not fit smooth
 
Asosiasi Peubah Dependen
#Spearmen
cor.test(Y1,Y2, method = "spearman")
## Warning in cor.test.default(Y1, Y2, method = "spearman"): Cannot compute exact
## p-value with ties
## 
##  Spearman's rank correlation rho
## 
## data:  Y1 and Y2
## S = 259323, p-value = 0.7608
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##        rho 
## 0.02844632
cor.test(Y2,Y3, method = "spearman")
## Warning in cor.test.default(Y2, Y3, method = "spearman"): Cannot compute exact
## p-value with ties
## 
##  Spearman's rank correlation rho
## 
## data:  Y2 and Y3
## S = 223842, p-value = 0.08217
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##       rho 
## 0.1613781
cor.test(Y3,Y4, method = "spearman")
## Warning in cor.test.default(Y3, Y4, method = "spearman"): Cannot compute exact
## p-value with ties
## 
##  Spearman's rank correlation rho
## 
## data:  Y3 and Y4
## S = 257811, p-value = 0.715
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##        rho 
## 0.03411192
cor.test(Y4,Y5, method = "spearman")
## Warning in cor.test.default(Y4, Y5, method = "spearman"): Cannot compute exact
## p-value with ties
## 
##  Spearman's rank correlation rho
## 
## data:  Y4 and Y5
## S = 222190, p-value = 0.07095
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##      rho 
## 0.167564
#Kendall tau
cor.test(Y1,Y2, method = "kendall")
## 
##  Kendall's rank correlation tau
## 
## data:  Y1 and Y2
## z = 0.30378, p-value = 0.7613
## alternative hypothesis: true tau is not equal to 0
## sample estimates:
##        tau 
## 0.02770821
cor.test(Y2,Y3, method = "kendall")
## 
##  Kendall's rank correlation tau
## 
## data:  Y2 and Y3
## z = 1.7391, p-value = 0.08201
## alternative hypothesis: true tau is not equal to 0
## sample estimates:
##       tau 
## 0.1577055
cor.test(Y3,Y4, method = "kendall")
## 
##  Kendall's rank correlation tau
## 
## data:  Y3 and Y4
## z = 0.3719, p-value = 0.71
## alternative hypothesis: true tau is not equal to 0
## sample estimates:
##        tau 
## 0.03336631
cor.test(Y4,Y5, method = "kendall")
## 
##  Kendall's rank correlation tau
## 
## data:  Y4 and Y5
## z = 1.8047, p-value = 0.07112
## alternative hypothesis: true tau is not equal to 0
## sample estimates:
##       tau 
## 0.1651303
#Pearson
cor.test(Y1,Y2, method = "pearson")
## 
##  Pearson's product-moment correlation
## 
## data:  Y1 and Y2
## t = 0.79433, df = 115, p-value = 0.4286
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.1091267  0.2520231
## sample estimates:
##        cor 
## 0.07386967
cor.test(Y2,Y3, method = "pearson")
## 
##  Pearson's product-moment correlation
## 
## data:  Y2 and Y3
## t = 1.6235, df = 115, p-value = 0.1072
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.0327386  0.3224550
## sample estimates:
##      cor 
## 0.149684
cor.test(Y3,Y4, method = "pearson")
## 
##  Pearson's product-moment correlation
## 
## data:  Y3 and Y4
## t = 0.28655, df = 115, p-value = 0.775
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.1555758  0.2072397
## sample estimates:
##        cor 
## 0.02671159
cor.test(Y3,Y5, method = "pearson")
## 
##  Pearson's product-moment correlation
## 
## data:  Y3 and Y5
## t = 0.22186, df = 115, p-value = 0.8248
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.1614550  0.2014607
## sample estimates:
##       cor 
## 0.0206842
Pemodelan Univariate
a. Bali - Sulawesi (Y3 - Y4)
#Bali - Sulawesi
#Model
model1.1 <- glm(Y3 ~ X13+X14, family=poisson(link="log"), data = data) 
model1.2 <- glm(Y4 ~ X13+X14, family=poisson(link="log"), data = data) 
model1.3 <- glm(Y3 ~ X23+X24, family=poisson(link="log"), data = data) 
model1.4 <- glm(Y4 ~ X23+X24, family=poisson(link="log"), data = data) 

model1.5 <- glm(Y3 ~ X13+X23, family=poisson(link="log"), data = data) 
model1.6 <- glm(Y4 ~ X13+X23, family=poisson(link="log"), data = data) 
model1.7 <- glm(Y3 ~ X14+X24, family=poisson(link="log"), data = data) 
model1.8 <- glm(Y4 ~ X14+X24, family=poisson(link="log"), data = data) 

model1.9 <- glm(Y3 ~ X13+X14+X23+X24, family=poisson(link="log"), data = data) 
model1.10 <- glm(Y4 ~ X13+X14+X23+X24, family=poisson(link="log"), data = data) 
summary(model1.1);summary(model1.2)
## 
## Call:
## glm(formula = Y3 ~ X13 + X14, family = poisson(link = "log"), 
##     data = data)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.2510  -0.7278  -0.6302  -0.5072   2.4461  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -2.05069    0.34462  -5.951 2.67e-09 ***
## X13          0.16908    0.05329   3.173  0.00151 ** 
## X14          0.09600    0.04884   1.965  0.04936 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 101.113  on 116  degrees of freedom
## Residual deviance:  90.667  on 114  degrees of freedom
## AIC: 158.35
## 
## Number of Fisher Scoring iterations: 6
## 
## Call:
## glm(formula = Y4 ~ X13 + X14, family = poisson(link = "log"), 
##     data = data)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.0516  -0.7967  -0.7341   0.7749   2.1347  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.47520    0.31873  -4.628 3.68e-06 ***
## X13          0.02504    0.08100   0.309    0.757    
## X14          0.06939    0.04899   1.416    0.157    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 96.283  on 116  degrees of freedom
## Residual deviance: 94.288  on 114  degrees of freedom
## AIC: 168.74
## 
## Number of Fisher Scoring iterations: 6
summary(model1.3);summary(model1.4)
## 
## Call:
## glm(formula = Y3 ~ X23 + X24, family = poisson(link = "log"), 
##     data = data)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.2720  -0.7811  -0.6646  -0.2340   2.3727  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)   
## (Intercept)  -7.2392     2.3538  -3.076  0.00210 **
## X23           0.7181     0.2427   2.958  0.00309 **
## X24           0.1715     0.2598   0.660  0.50926   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 101.113  on 116  degrees of freedom
## Residual deviance:  90.685  on 114  degrees of freedom
## AIC: 158.37
## 
## Number of Fisher Scoring iterations: 6
## 
## Call:
## glm(formula = Y4 ~ X23 + X24, family = poisson(link = "log"), 
##     data = data)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.1597  -0.7548  -0.6318   0.5067   2.1176  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)   
## (Intercept) -6.98399    2.21626  -3.151  0.00163 **
## X23          0.04112    0.18464   0.223  0.82375   
## X24          0.80707    0.25860   3.121  0.00180 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 96.283  on 116  degrees of freedom
## Residual deviance: 86.116  on 114  degrees of freedom
## AIC: 160.57
## 
## Number of Fisher Scoring iterations: 6
summary(model1.5);summary(model1.6)
## 
## Call:
## glm(formula = Y3 ~ X13 + X23, family = poisson(link = "log"), 
##     data = data)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.0860  -0.7766  -0.6734  -0.3130   2.2719  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)   
## (Intercept) -5.29597    1.82904  -2.895  0.00379 **
## X13          0.08218    0.06142   1.338  0.18092   
## X23          0.56996    0.27528   2.071  0.03840 * 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 101.113  on 116  degrees of freedom
## Residual deviance:  89.538  on 114  degrees of freedom
## AIC: 157.22
## 
## Number of Fisher Scoring iterations: 6
## 
## Call:
## glm(formula = Y4 ~ X13 + X23, family = poisson(link = "log"), 
##     data = data)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.9769  -0.7914  -0.7785   0.9049   2.0391  
## 
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)
## (Intercept) -1.238802   1.366333  -0.907    0.365
## X13          0.032219   0.087529   0.368    0.713
## X23          0.001883   0.220042   0.009    0.993
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 96.283  on 116  degrees of freedom
## Residual deviance: 96.109  on 114  degrees of freedom
## AIC: 170.56
## 
## Number of Fisher Scoring iterations: 6
summary(model1.7);summary(model1.8)
## 
## Call:
## glm(formula = Y3 ~ X14 + X24, family = poisson(link = "log"), 
##     data = data)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.1394  -0.7439  -0.6955  -0.6490   2.3253  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)  
## (Intercept) -1.18840    1.96813  -0.604   0.5460  
## X14          0.09945    0.05780   1.720   0.0853 .
## X24         -0.06073    0.30791  -0.197   0.8436  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 101.113  on 116  degrees of freedom
## Residual deviance:  97.737  on 114  degrees of freedom
## AIC: 165.42
## 
## Number of Fisher Scoring iterations: 6
## 
## Call:
## glm(formula = Y4 ~ X14 + X24, family = poisson(link = "log"), 
##     data = data)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.1937  -0.7364  -0.6323   0.4864   2.0850  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -6.87679    1.94682  -3.532 0.000412 ***
## X14         -0.01493    0.05959  -0.251 0.802169    
## X24          0.83914    0.28879   2.906 0.003664 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 96.283  on 116  degrees of freedom
## Residual deviance: 86.102  on 114  degrees of freedom
## AIC: 160.56
## 
## Number of Fisher Scoring iterations: 6
summary(model1.9);summary(model1.10)
## 
## Call:
## glm(formula = Y3 ~ X13 + X14 + X23 + X24, family = poisson(link = "log"), 
##     data = data)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.1484  -0.7491  -0.6366  -0.3077   2.4889  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)  
## (Intercept) -5.01556    2.83051  -1.772   0.0764 .
## X13          0.10163    0.06496   1.564   0.1177  
## X14          0.07873    0.06121   1.286   0.1984  
## X23          0.49450    0.28305   1.747   0.0806 .
## X24         -0.02048    0.32085  -0.064   0.9491  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 101.113  on 116  degrees of freedom
## Residual deviance:  87.437  on 112  degrees of freedom
## AIC: 159.12
## 
## Number of Fisher Scoring iterations: 6
## 
## Call:
## glm(formula = Y4 ~ X13 + X14 + X23 + X24, family = poisson(link = "log"), 
##     data = data)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.1791  -0.7450  -0.6288   0.4942   2.1565  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)   
## (Intercept) -7.26783    2.53754  -2.864  0.00418 **
## X13          0.01009    0.09509   0.106  0.91549   
## X14         -0.01970    0.06254  -0.315  0.75277   
## X23          0.04722    0.21952   0.215  0.82969   
## X24          0.85005    0.29353   2.896  0.00378 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 96.283  on 116  degrees of freedom
## Residual deviance: 85.996  on 112  degrees of freedom
## AIC: 164.45
## 
## Number of Fisher Scoring iterations: 6
BIC(model1.1);BIC(model1.2);BIC(model1.3)
## [1] 166.6361
## [1] 177.0297
## [1] 166.6536
BIC(model1.4);BIC(model1.5);BIC(model1.6)
## [1] 168.8571
## [1] 165.5063
## [1] 178.8501
BIC(model1.7);BIC(model1.8);BIC(model1.9);BIC(model1.10)
## [1] 173.7054
## [1] 168.8436
## [1] 172.9304
## [1] 178.262
logLik(model1.1);logLik(model1.2);logLik(model1.3);logLik(model1.4)
## 'log Lik.' -76.17477 (df=3)
## 'log Lik.' -81.37159 (df=3)
## 'log Lik.' -76.18352 (df=3)
## 'log Lik.' -77.28527 (df=3)
logLik(model1.5);logLik(model1.6);logLik(model1.7);logLik(model1.8)
## 'log Lik.' -75.60989 (df=3)
## 'log Lik.' -82.28177 (df=3)
## 'log Lik.' -79.70943 (df=3)
## 'log Lik.' -77.27852 (df=3)
logLik(model1.9);logLik(model1.10)
## 'log Lik.' -74.55979 (df=5)
## 'log Lik.' -77.22558 (df=5)
b. Sulawesi - Papua (Y4 - Y5)
model2.1 <- glm(Y4 ~ X14+X15, family=poisson(link="log"), data = data) 
model2.2 <- glm(Y5 ~ X14+X15, family=poisson(link="log"), data = data) 
model2.3 <- glm(Y4 ~ X24+X25, family=poisson(link="log"), data = data) 
model2.4 <- glm(Y5 ~ X24+X25, family=poisson(link="log"), data = data) 

model2.5 <- glm(Y4 ~ X14+X24, family=poisson(link="log"), data = data) 
model2.6 <- glm(Y5 ~ X14+X24, family=poisson(link="log"), data = data) 
model2.7 <- glm(Y4 ~ X15+X25, family=poisson(link="log"), data = data) 
model2.8 <- glm(Y5 ~ X15+X25, family=poisson(link="log"), data = data) 

model2.9 <- glm(Y4 ~ X14+X15+X24+X25, family=poisson(link="log"), data = data) 
model2.10 <- glm(Y5 ~ X14+X15+X24+X25, family=poisson(link="log"), data = data) 
summary(model2.1);summary(model2.2)
## 
## Call:
## glm(formula = Y4 ~ X14 + X15, family = poisson(link = "log"), 
##     data = data)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.1749  -0.7913  -0.7242   0.7489   2.1715  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.48733    0.28149  -5.284 1.27e-07 ***
## X14          0.06518    0.04983   1.308    0.191    
## X15          0.08363    0.11961   0.699    0.484    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 96.283  on 116  degrees of freedom
## Residual deviance: 93.923  on 114  degrees of freedom
## AIC: 168.38
## 
## Number of Fisher Scoring iterations: 6
## 
## Call:
## glm(formula = Y5 ~ X14 + X15, family = poisson(link = "log"), 
##     data = data)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.6482  -0.3995  -0.3141  -0.3131   1.7965  
## 
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -3.018481   0.519743  -5.808 6.34e-09 ***
## X14          0.001539   0.098667   0.016 0.987559    
## X15          0.482483   0.130460   3.698 0.000217 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 54.654  on 116  degrees of freedom
## Residual deviance: 44.309  on 114  degrees of freedom
## AIC: 74.309
## 
## Number of Fisher Scoring iterations: 6
summary(model2.3);summary(model2.4)
## 
## Call:
## glm(formula = Y4 ~ X24 + X25, family = poisson(link = "log"), 
##     data = data)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.2806  -0.7266  -0.6219   0.4490   2.1079  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)   
## (Intercept) -6.21404    2.06574  -3.008  0.00263 **
## X24          0.81038    0.25747   3.148  0.00165 **
## X25         -0.08794    0.17494  -0.503  0.61517   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 96.283  on 116  degrees of freedom
## Residual deviance: 85.913  on 114  degrees of freedom
## AIC: 160.37
## 
## Number of Fisher Scoring iterations: 6
## 
## Call:
## glm(formula = Y5 ~ X24 + X25, family = poisson(link = "log"), 
##     data = data)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.8749  -0.4628  -0.3624  -0.2711   2.3108  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)  
## (Intercept)  -6.1184     3.4112  -1.794   0.0729 .
## X24          -0.1886     0.4468  -0.422   0.6730  
## X25           0.8107     0.3315   2.445   0.0145 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 54.654  on 116  degrees of freedom
## Residual deviance: 48.222  on 114  degrees of freedom
## AIC: 78.222
## 
## Number of Fisher Scoring iterations: 6
summary(model2.5);summary(model2.6)
## 
## Call:
## glm(formula = Y4 ~ X14 + X24, family = poisson(link = "log"), 
##     data = data)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.1937  -0.7364  -0.6323   0.4864   2.0850  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -6.87679    1.94682  -3.532 0.000412 ***
## X14         -0.01493    0.05959  -0.251 0.802169    
## X24          0.83914    0.28879   2.906 0.003664 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 96.283  on 116  degrees of freedom
## Residual deviance: 86.102  on 114  degrees of freedom
## AIC: 160.56
## 
## Number of Fisher Scoring iterations: 6
## 
## Call:
## glm(formula = Y5 ~ X14 + X24, family = poisson(link = "log"), 
##     data = data)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.6762  -0.4604  -0.4335  -0.4125   1.9109  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)
## (Intercept) -0.69996    3.23449  -0.216    0.829
## X14          0.09439    0.10428   0.905    0.365
## X24         -0.28837    0.51559  -0.559    0.576
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 54.654  on 116  degrees of freedom
## Residual deviance: 53.894  on 114  degrees of freedom
## AIC: 83.894
## 
## Number of Fisher Scoring iterations: 6
summary(model2.7);summary(model2.8)
## 
## Call:
## glm(formula = Y4 ~ X15 + X25, family = poisson(link = "log"), 
##     data = data)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.9494  -0.8022  -0.7265   0.7497   2.2062  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)  
## (Intercept)   0.4928     1.3075   0.377   0.7062  
## X15           0.2783     0.1658   1.679   0.0932 .
## X25          -0.3259     0.2426  -1.343   0.1792  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 96.283  on 116  degrees of freedom
## Residual deviance: 93.727  on 114  degrees of freedom
## AIC: 168.18
## 
## Number of Fisher Scoring iterations: 6
## 
## Call:
## glm(formula = Y5 ~ X15 + X25, family = poisson(link = "log"), 
##     data = data)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.6682  -0.4114  -0.3146  -0.3028   1.8649  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)  
## (Intercept) -3.49313    2.79240  -1.251   0.2110  
## X15          0.45216    0.21626   2.091   0.0365 *
## X25          0.08382    0.48071   0.174   0.8616  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 54.654  on 116  degrees of freedom
## Residual deviance: 44.278  on 114  degrees of freedom
## AIC: 74.278
## 
## Number of Fisher Scoring iterations: 6
summary(model2.9);summary(model2.10)
## 
## Call:
## glm(formula = Y4 ~ X14 + X15 + X24 + X25, family = poisson(link = "log"), 
##     data = data)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.4211  -0.7381  -0.5954   0.4551   2.2163  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)   
## (Intercept) -4.90843    2.39732  -2.047  0.04061 * 
## X14         -0.01449    0.06110  -0.237  0.81258   
## X15          0.25231    0.16033   1.574  0.11556   
## X24          0.82596    0.28781   2.870  0.00411 **
## X25         -0.36058    0.25324  -1.424  0.15449   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 96.283  on 116  degrees of freedom
## Residual deviance: 83.658  on 112  degrees of freedom
## AIC: 162.11
## 
## Number of Fisher Scoring iterations: 6
## 
## Call:
## glm(formula = Y5 ~ X14 + X15 + X24 + X25, family = poisson(link = "log"), 
##     data = data)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.7199  -0.4311  -0.3290  -0.2729   2.0547  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)  
## (Intercept) -0.45584    4.18380  -0.109   0.9132  
## X14          0.06687    0.11630   0.575   0.5653  
## X15          0.47971    0.22077   2.173   0.0298 *
## X24         -0.50569    0.53441  -0.946   0.3440  
## X25          0.09076    0.48468   0.187   0.8515  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 54.654  on 116  degrees of freedom
## Residual deviance: 43.400  on 112  degrees of freedom
## AIC: 77.4
## 
## Number of Fisher Scoring iterations: 6
BIC(model2.1);BIC(model2.2);BIC(model2.3)
## [1] 176.6645
## [1] 82.59528
## [1] 168.6544
BIC(model2.4);BIC(model2.5);BIC(model2.6)
## [1] 86.50837
## [1] 168.8436
## [1] 92.18045
BIC(model2.7);BIC(model2.8);BIC(model2.9);BIC(model2.10)
## [1] 176.4685
## [1] 82.56502
## [1] 175.9239
## [1] 91.21052
logLik(model2.1);logLik(model2.2);logLik(model2.3);logLik(model2.4)
## 'log Lik.' -81.18901 (df=3)
## 'log Lik.' -34.15438 (df=3)
## 'log Lik.' -77.18394 (df=3)
## 'log Lik.' -36.11093 (df=3)
logLik(model2.5);logLik(model2.6);logLik(model2.7);logLik(model2.8)
## 'log Lik.' -77.27852 (df=3)
## 'log Lik.' -38.94697 (df=3)
## 'log Lik.' -81.091 (df=3)
## 'log Lik.' -34.13925 (df=3)
logLik(model2.9);logLik(model2.10)
## 'log Lik.' -76.05651 (df=5)
## 'log Lik.' -33.69982 (df=5)
Mencari sebaran univariate Y
Diskrit
library(fitdistrplus)
## Warning: package 'fitdistrplus' was built under R version 4.1.3
## Loading required package: MASS
## Warning: package 'MASS' was built under R version 4.1.3
Y1_coba1 <- fitdist(Y1, discrete = T, "pois")
Y1_coba3 <- fitdist(Y1, discrete = T, "nbinom")
Y1_coba4 <- fitdist(Y1, discrete = T, "geom")

Y2_coba1 <- fitdist(Y2, discrete = T, "pois")
Y2_coba3 <- fitdist(Y2, discrete = T, "nbinom")
Y2_coba4 <- fitdist(Y2, discrete = T, "geom")

Y3_coba1 <- fitdist(Y3, discrete = T, "pois")
Y3_coba3 <- fitdist(Y3, discrete = T, "nbinom")
Y3_coba4 <- fitdist(Y3, discrete = T, "geom")

Y4_coba1 <- fitdist(Y4, discrete = T, "pois")
Y4_coba3 <- fitdist(Y4, discrete = T, "nbinom")
## Warning in sqrt(diag(varcovar)): NaNs produced
## Warning in sqrt(1/diag(V)): NaNs produced
## Warning in cov2cor(varcovar): diag(.) had 0 or NA entries; non-finite result is
## doubtful
Y4_coba4 <- fitdist(Y4, discrete = T, "geom")

Y5_coba1 <- fitdist(Y5, discrete = T, "pois")
Y5_coba3 <- fitdist(Y5, discrete = T, "nbinom")
Y5_coba4 <- fitdist(Y5, discrete = T, "geom")
Y1_coba1$aic;Y1_coba1$bic;Y1_coba1$loglik; summary(Y1_coba1) #Poisson
## [1] 140.7274
## [1] 143.4896
## [1] -69.36369
## Fitting of the distribution ' pois ' by maximum likelihood 
## Parameters : 
##         estimate Std. Error
## lambda 0.2307692 0.04441073
## Loglikelihood:  -69.36369   AIC:  140.7274   BIC:  143.4896
Y1_coba3$aic;Y1_coba3$bic;Y1_coba3$loglik #nbinom
## [1] 142.4508
## [1] 147.9752
## [1] -69.22542
Y1_coba4$aic;Y1_coba4$bic;Y1_coba4$loglik #geom
## [1] 140.9823
## [1] 143.7445
## [1] -69.49117
Y2_coba1$aic;Y2_coba1$bic;Y2_coba1$loglik #Poisson
## [1] 95.01001
## [1] 97.77218
## [1] -46.505
Y2_coba3$aic;Y2_coba3$bic;Y2_coba3$loglik #nbinom
## [1] 97.00834
## [1] 102.5327
## [1] -46.50417
Y2_coba4$aic;Y2_coba4$bic;Y2_coba4$loglik #geom
## [1] 95.4695
## [1] 98.23167
## [1] -46.73475
Y3_coba1$aic;Y3_coba1$bic;Y3_coba1$loglik; summary(Y3_coba1) #Poisson
## [1] 164.7956
## [1] 167.5578
## [1] -81.39779
## Fitting of the distribution ' pois ' by maximum likelihood 
## Parameters : 
##         estimate Std. Error
## lambda 0.2991453 0.05056422
## Loglikelihood:  -81.39779   AIC:  164.7956   BIC:  167.5578
Y3_coba3$aic;Y3_coba3$bic;Y3_coba3$loglik; summary(Y3_coba3) #nbinom
## [1] 166.6608
## [1] 172.1851
## [1] -81.3304
## Fitting of the distribution ' nbinom ' by maximum likelihood 
## Parameters : 
##       estimate  Std. Error
## size 5.2931456 15.48125425
## mu   0.2991559  0.05197533
## Loglikelihood:  -81.3304   AIC:  166.6608   BIC:  172.1851 
## Correlation matrix:
##               size            mu
## size  1.000000e+00 -3.168976e-05
## mu   -3.168976e-05  1.000000e+00
Y3_coba4$aic;Y3_coba4$bic;Y3_coba4$loglik; summary(Y3_coba4) #geom
## [1] 166.0366
## [1] 168.7988
## [1] -82.01831
## Fitting of the distribution ' geom ' by maximum likelihood 
## Parameters : 
##       estimate Std. Error
## prob 0.7697368  0.0341472
## Loglikelihood:  -82.01831   AIC:  166.0366   BIC:  168.7988
Y4_coba1$aic;Y4_coba1$bic;Y4_coba1$loglik; summary(Y4_coba1) #Poisson
## [1] 166.7381
## [1] 169.5003
## [1] -82.36906
## Fitting of the distribution ' pois ' by maximum likelihood 
## Parameters : 
##         estimate Std. Error
## lambda 0.3162393  0.0519889
## Loglikelihood:  -82.36906   AIC:  166.7381   BIC:  169.5003
Y4_coba3$aic;Y4_coba3$bic;Y4_coba3$loglik; summary(Y4_coba3) #nbinom
## [1] 168.7383
## [1] 174.2627
## [1] -82.36916
## Fitting of the distribution ' nbinom ' by maximum likelihood 
## Parameters : 
##          estimate Std. Error
## size 2.305715e+04        NaN
## mu   3.165089e-01 0.05203357
## Loglikelihood:  -82.36916   AIC:  168.7383   BIC:  174.2627 
## Correlation matrix:
##      size  mu
## size    1 NaN
## mu    NaN   1
Y4_coba4$aic;Y4_coba4$bic;Y4_coba4$loglik; summary(Y4_coba4) #geom
## [1] 171.8248
## [1] 174.5869
## [1] -84.91239
## Fitting of the distribution ' geom ' by maximum likelihood 
## Parameters : 
##       estimate Std. Error
## prob 0.7597403 0.03442761
## Loglikelihood:  -84.91239   AIC:  171.8248   BIC:  174.5869
Y5_coba1$aic;Y5_coba1$bic;Y5_coba1$loglik; summary(Y5_coba1) #Poisson
## [1] 80.65441
## [1] 83.41659
## [1] -39.32721
## Fitting of the distribution ' pois ' by maximum likelihood 
## Parameters : 
##         estimate Std. Error
## lambda 0.1025641 0.02960489
## Loglikelihood:  -39.32721   AIC:  80.65441   BIC:  83.41659
Y5_coba3$aic;Y5_coba3$bic;Y5_coba3$loglik; summary(Y5_coba3) #nbinom
## [1] 82.65442
## [1] 88.17876
## [1] -39.32721
## Fitting of the distribution ' nbinom ' by maximum likelihood 
## Parameters : 
##          estimate  Std. Error
## size 7.319947e+05 90.57062957
## mu   1.025711e-01  0.02960693
## Loglikelihood:  -39.32721   AIC:  82.65442   BIC:  88.17876 
## Correlation matrix:
##              size           mu
## size 1.000000e+00 4.763333e-09
## mu   4.763333e-09 1.000000e+00
Y5_coba4$aic;Y5_coba4$bic;Y5_coba4$loglik; summary(Y5_coba4) #geom
## [1] 81.84514
## [1] 84.60731
## [1] -39.92257
## Fitting of the distribution ' geom ' by maximum likelihood 
## Parameters : 
##       estimate Std. Error
## prob 0.9069767 0.02557134
## Loglikelihood:  -39.92257   AIC:  81.84514   BIC:  84.60731
Kontinu
Y1_kon1 <- fitdist(Y1,  "norm")
Y1_kon4 <- fitdist(Y1,  "exp")
Y1_kon6 <- fitdist(Y1,  "logis")

Y2_kon1 <- fitdist(Y2,  "norm")
Y2_kon4 <- fitdist(Y2,  "exp")
Y2_kon6 <- fitdist(Y2,  "logis")

Y3_kon1 <- fitdist(Y3,  "norm")
Y3_kon4 <- fitdist(Y3,  "exp")
Y3_kon6 <- fitdist(Y3,  "logis")

Y4_kon1 <- fitdist(Y4,  "norm")
Y4_kon4 <- fitdist(Y4,  "exp")
Y4_kon6 <- fitdist(Y4,  "logis")

Y5_kon1 <- fitdist(Y5,  "norm")
Y5_kon4 <- fitdist(Y5,  "exp")
Y5_kon6 <- fitdist(Y5,  "logis")
Y1_kon1$aic;Y1_kon1$bic;Y1_kon1$loglik #Normal
## [1] 171.8961
## [1] 177.4205
## [1] -83.94806
Y1_kon4$aic;Y1_kon4$bic;Y1_kon4$loglik #Eksponensial
## [1] -107.1229
## [1] -104.3607
## [1] 54.56144
Y1_kon6$aic;Y1_kon6$bic;Y1_kon6$loglik #Logistik
## [1] 145.6542
## [1] 151.1785
## [1] -70.82709
Y2_kon1$aic;Y2_kon1$bic;Y2_kon1$loglik #Normal
## [1] 96.29761
## [1] 101.822
## [1] -46.1488
Y2_kon4$aic;Y2_kon4$bic;Y2_kon4$loglik #Eksponensial
## [1] -244.665
## [1] -241.9028
## [1] 123.3325
Y2_kon6$aic;Y2_kon6$bic;Y2_kon6$loglik #Logistik
## [1] 39.12195
## [1] 44.6463
## [1] -17.56098
Y3_kon1$aic;Y3_kon1$bic;Y3_kon1$loglik #Normal
## [1] 199.8387
## [1] 205.363
## [1] -97.91933
Y3_kon4$aic;Y3_kon4$bic;Y3_kon4$loglik #Eksponensial
## [1] -46.39725
## [1] -43.63508
## [1] 24.19863
Y3_kon6$aic;Y3_kon6$bic;Y3_kon6$loglik #Logistik
## [1] 185.5231
## [1] 191.0474
## [1] -90.76155
Y4_kon1$aic;Y4_kon1$bic;Y4_kon1$loglik #Normal
## [1] 189.0045
## [1] 194.5288
## [1] -92.50224
Y4_kon4$aic;Y4_kon4$bic;Y4_kon4$loglik #Eksponensial
## [1] -33.39391
## [1] -30.63174
## [1] 17.69695
Y4_kon6$aic;Y4_kon6$bic;Y4_kon6$loglik #Logsitik
## [1] 183.5513
## [1] 189.0756
## [1] -89.77563
Y5_kon1$aic;Y5_kon1$bic;Y5_kon1$loglik #Normal
## [1] 56.93036
## [1] 62.4547
## [1] -26.46518
Y5_kon4$aic;Y5_kon4$bic;Y5_kon4$loglik #Eksponensial
## [1] -296.8805
## [1] -294.1184
## [1] 149.4403
Y5_kon6$aic;Y5_kon6$bic;Y5_kon6$loglik #Logistik
## [1] -6.504071
## [1] -0.9797227
## [1] 5.252035
Pemilihan Model Copula
library(VineCopula) 
## Warning: package 'VineCopula' was built under R version 4.1.3
var_a <- pobs(Y3) 
var_b <- pobs(Y4)
var_c <- pobs(Y5)

selectedCopula1 <- BiCopSelect(var_a, var_b, familyset = 1) #Gaussian
## Warning in cor(x[(x[, 1] > 0) & (x[, 2] > 0), ]): the standard deviation is zero
## Warning in cor(x[(x[, 1] < 0) & (x[, 2] < 0), ]): the standard deviation is zero
selectedCopula2 <- BiCopSelect(var_a, var_b, familyset = 2) #student-t
## Warning in cor(x[(x[, 1] > 0) & (x[, 2] > 0), ]): the standard deviation is zero

## Warning in cor(x[(x[, 1] > 0) & (x[, 2] > 0), ]): the standard deviation is zero
selectedCopula3 <- BiCopSelect(var_a, var_b, familyset = 3) #Clayton
## Warning in cor(x[(x[, 1] > 0) & (x[, 2] > 0), ]): the standard deviation is zero

## Warning in cor(x[(x[, 1] > 0) & (x[, 2] > 0), ]): the standard deviation is zero
selectedCopula4 <- BiCopSelect(var_a, var_b, familyset = 4) #Gumbel
## Warning in cor(x[(x[, 1] > 0) & (x[, 2] > 0), ]): the standard deviation is zero

## Warning in cor(x[(x[, 1] > 0) & (x[, 2] > 0), ]): the standard deviation is zero
selectedCopula5 <- BiCopSelect(var_a, var_b, familyset = 5) #Frank
## Warning in cor(x[(x[, 1] > 0) & (x[, 2] > 0), ]): the standard deviation is zero

## Warning in cor(x[(x[, 1] > 0) & (x[, 2] > 0), ]): the standard deviation is zero
selectedCopula6 <- BiCopSelect(var_a, var_b, familyset = 6) #Survival Joe
## Warning in cor(x[(x[, 1] > 0) & (x[, 2] > 0), ]): the standard deviation is zero

## Warning in cor(x[(x[, 1] > 0) & (x[, 2] > 0), ]): the standard deviation is zero
plot(selectedCopula1, main= "Gaussian Copula")
 
plot(selectedCopula2, main= "Student-t Copula")
 
plot(selectedCopula3, main= "Clayton Copula")
 
plot(selectedCopula4, main= "Gumbel Copula")
 
plot(selectedCopula5, main= "Frank Copula")
 
plot(selectedCopula6, main= "Joe Copula")
 
summary(selectedCopula1)
## Family
## ------ 
## No:    1
## Name:  Gaussian
## 
## Parameter(s)
## ------------
## par:  0.34
## 
## Dependence measures
## -------------------
## Kendall's tau:    0.22 (empirical = 0.03, p value = 0.59)
## Upper TD:         0 
## Lower TD:         0 
## 
## Fit statistics
## --------------
## logLik:  0.72 
## AIC:    0.55 
## BIC:    3.32
summary(selectedCopula2)
## Family
## ------ 
## No:    2
## Name:  t
## 
## Parameter(s)
## ------------
## par:  0.24
## par2: 2
## Dependence measures
## -------------------
## Kendall's tau:    0.15 (empirical = 0.03, p value = 0.59)
## Upper TD:         0.27 
## Lower TD:         0.27 
## 
## Fit statistics
## --------------
## logLik:  4.91 
## AIC:    -5.82 
## BIC:    -0.29
summary(selectedCopula3)
## Family
## ------ 
## No:    3
## Name:  Clayton
## 
## Parameter(s)
## ------------
## par:  2.1
## 
## Dependence measures
## -------------------
## Kendall's tau:    0.51 (empirical = 0.03, p value = 0.59)
## Upper TD:         0 
## Lower TD:         0.72 
## 
## Fit statistics
## --------------
## logLik:  15.45 
## AIC:    -28.91 
## BIC:    -26.14
summary(selectedCopula4)
## Family
## ------ 
## No:    14
## Name:  Survival Gumbel
## 
## Parameter(s)
## ------------
## par:  1.65
## 
## Dependence measures
## -------------------
## Kendall's tau:    0.39 (empirical = 0.03, p value = 0.59)
## Upper TD:         0 
## Lower TD:         0.48 
## 
## Fit statistics
## --------------
## logLik:  7.72 
## AIC:    -13.45 
## BIC:    -10.69
summary(selectedCopula5)
## Family
## ------ 
## No:    5
## Name:  Frank
## 
## Parameter(s)
## ------------
## par:  1.3
## 
## Dependence measures
## -------------------
## Kendall's tau:    0.14 (empirical = 0.03, p value = 0.59)
## Upper TD:         0 
## Lower TD:         0 
## 
## Fit statistics
## --------------
## logLik:  0.29 
## AIC:    1.43 
## BIC:    4.19
summary(selectedCopula6)
## Family
## ------ 
## No:    16
## Name:  Survival Joe
## 
## Parameter(s)
## ------------
## par:  2.99
## 
## Dependence measures
## -------------------
## Kendall's tau:    0.52 (empirical = 0.03, p value = 0.59)
## Upper TD:         0 
## Lower TD:         0.74 
## 
## Fit statistics
## --------------
## logLik:  18.83 
## AIC:    -35.66 
## BIC:    -32.9
selected2Copula1 <- BiCopSelect(var_b, var_c, familyset = 1) #Gaussian
## Warning in cor(x[(x[, 1] > 0) & (x[, 2] > 0), ]): the standard deviation is zero

## Warning in cor(x[(x[, 1] > 0) & (x[, 2] > 0), ]): the standard deviation is zero
selected2Copula2 <- BiCopSelect(var_b, var_c, familyset = 2) #student-t
## Warning in cor(x[(x[, 1] > 0) & (x[, 2] > 0), ]): the standard deviation is zero

## Warning in cor(x[(x[, 1] > 0) & (x[, 2] > 0), ]): the standard deviation is zero
selected2Copula3 <- BiCopSelect(var_b, var_c, familyset = 3) #Clayton
## Warning in cor(x[(x[, 1] > 0) & (x[, 2] > 0), ]): the standard deviation is zero

## Warning in cor(x[(x[, 1] > 0) & (x[, 2] > 0), ]): the standard deviation is zero
selected2Copula4 <- BiCopSelect(var_b, var_c, familyset = 4) #Gumbel
## Warning in cor(x[(x[, 1] > 0) & (x[, 2] > 0), ]): the standard deviation is zero

## Warning in cor(x[(x[, 1] > 0) & (x[, 2] > 0), ]): the standard deviation is zero
selected2Copula5 <- BiCopSelect(var_b, var_c, familyset = 5) #Frank
## Warning in cor(x[(x[, 1] > 0) & (x[, 2] > 0), ]): the standard deviation is zero

## Warning in cor(x[(x[, 1] > 0) & (x[, 2] > 0), ]): the standard deviation is zero
selected2Copula6 <- BiCopSelect(var_b, var_c, familyset = 6) #Survival Joe
## Warning in cor(x[(x[, 1] > 0) & (x[, 2] > 0), ]): the standard deviation is zero

## Warning in cor(x[(x[, 1] > 0) & (x[, 2] > 0), ]): the standard deviation is zero
plot(selected2Copula1, main= "Gaussian Copula", col = "red")
 
plot(selected2Copula2, main= "Student-t Copula", col = "red")
 
plot(selected2Copula3, main= "Clayton Copula", col = "red")
 
plot(selected2Copula4, main= "Gumbel Copula", col = "red")
 
plot(selected2Copula5, main= "Frank Copula", col = "red")
 
plot(selected2Copula6, main= "Joe Copula", col = "red")
 
#=====================================================================
selected2Copula1 <- BiCopSelect(var_b, var_c, familyset = 1) #Gaussian 
## Warning in cor(x[(x[, 1] > 0) & (x[, 2] > 0), ]): the standard deviation is zero
## Warning in cor(x[(x[, 1] < 0) & (x[, 2] < 0), ]): the standard deviation is zero
selected2Copula2 <- BiCopSelect(var_b, var_c, familyset = 2) #Studnet-t
## Warning in cor(x[(x[, 1] > 0) & (x[, 2] > 0), ]): the standard deviation is zero

## Warning in cor(x[(x[, 1] > 0) & (x[, 2] > 0), ]): the standard deviation is zero
selected2Copula3 <- BiCopSelect(var_b, var_c, familyset = 3) #Clayton
## Warning in cor(x[(x[, 1] > 0) & (x[, 2] > 0), ]): the standard deviation is zero

## Warning in cor(x[(x[, 1] > 0) & (x[, 2] > 0), ]): the standard deviation is zero
selected2Copula4 <- BiCopSelect(var_b, var_c, familyset = 4) #Gumbel
## Warning in cor(x[(x[, 1] > 0) & (x[, 2] > 0), ]): the standard deviation is zero

## Warning in cor(x[(x[, 1] > 0) & (x[, 2] > 0), ]): the standard deviation is zero
selected2Copula5 <- BiCopSelect(var_b, var_c, familyset = 5) #Frank
## Warning in cor(x[(x[, 1] > 0) & (x[, 2] > 0), ]): the standard deviation is zero

## Warning in cor(x[(x[, 1] > 0) & (x[, 2] > 0), ]): the standard deviation is zero
selected2Copula6 <- BiCopSelect(var_b, var_c, familyset = 6) #Survival Joe
## Warning in cor(x[(x[, 1] > 0) & (x[, 2] > 0), ]): the standard deviation is zero

## Warning in cor(x[(x[, 1] > 0) & (x[, 2] > 0), ]): the standard deviation is zero
plot(selected2Copula1)
 
plot(selected2Copula2)
 
plot(selected2Copula3)
 
plot(selected2Copula4)
 
plot(selected2Copula5)
 
plot(selected2Copula6)
 
summary(selected2Copula1)
## Family
## ------ 
## No:    1
## Name:  Gaussian
## 
## Parameter(s)
## ------------
## par:  0.62
## 
## Dependence measures
## -------------------
## Kendall's tau:    0.43 (empirical = 0.17, p value < 0.01)
## Upper TD:         0 
## Lower TD:         0 
## 
## Fit statistics
## --------------
## logLik:  8.43 
## AIC:    -14.86 
## BIC:    -12.1
summary(selected2Copula2)
## Family
## ------ 
## No:    2
## Name:  t
## 
## Parameter(s)
## ------------
## par:  0.61
## par2: 2
## Dependence measures
## -------------------
## Kendall's tau:    0.42 (empirical = 0.17, p value < 0.01)
## Upper TD:         0.46 
## Lower TD:         0.46 
## 
## Fit statistics
## --------------
## logLik:  14.27 
## AIC:    -24.55 
## BIC:    -19.02
summary(selected2Copula3)
## Family
## ------ 
## No:    3
## Name:  Clayton
## 
## Parameter(s)
## ------------
## par:  3.15
## 
## Dependence measures
## -------------------
## Kendall's tau:    0.61 (empirical = 0.17, p value < 0.01)
## Upper TD:         0 
## Lower TD:         0.8 
## 
## Fit statistics
## --------------
## logLik:  31.83 
## AIC:    -61.65 
## BIC:    -58.89
summary(selected2Copula4)
## Family
## ------ 
## No:    14
## Name:  Survival Gumbel
## 
## Parameter(s)
## ------------
## par:  2.19
## 
## Dependence measures
## -------------------
## Kendall's tau:    0.54 (empirical = 0.17, p value < 0.01)
## Upper TD:         0 
## Lower TD:         0.63 
## 
## Fit statistics
## --------------
## logLik:  20.34 
## AIC:    -38.67 
## BIC:    -35.91
summary(selected2Copula5)
## Family
## ------ 
## No:    5
## Name:  Frank
## 
## Parameter(s)
## ------------
## par:  5.41
## 
## Dependence measures
## -------------------
## Kendall's tau:    0.48 (empirical = 0.17, p value < 0.01)
## Upper TD:         0 
## Lower TD:         0 
## 
## Fit statistics
## --------------
## logLik:  10.79 
## AIC:    -19.58 
## BIC:    -16.82
summary(selected2Copula6)
## Family
## ------ 
## No:    16
## Name:  Survival Joe
## 
## Parameter(s)
## ------------
## par:  3.92
## 
## Dependence measures
## -------------------
## Kendall's tau:    0.61 (empirical = 0.17, p value < 0.01)
## Upper TD:         0 
## Lower TD:         0.81 
## 
## Fit statistics
## --------------
## logLik:  33.56 
## AIC:    -65.12 
## BIC:    -62.36
Bivariat Copula
a. Bali Sulawesi (Y3 - Y4)
m1 <- Y3 ~ X13+X14
m2 <- Y4 ~ X13+X14

m3 <- Y3 ~ X23+X24
m4 <- Y4 ~ X23+X24

m5 <- Y3 ~ X13+X23
m6 <- Y4 ~ X13+X23

m7 <- Y3 ~ X14+X24
m8 <- Y4 ~ X14+X24

m9 <- Y3 ~ X13+X14+X23+X24
m10 <- Y4 ~ X13+X14+X23+X24
library(bizicount) 
## Warning: package 'bizicount' was built under R version 4.1.3
cp1 <- bizicount(m1,m2,data,
                 cop = "frank",
                 margins = c("pois","pois"),
                 keep = T) 
cp2 <- bizicount(m3,m4,data,
                 cop = "frank",
                 margins = c("pois","pois"),
                 keep = T) 
## Warning in bizicount(m3, m4, data, cop = "frank", margins = c("pois", "pois"), :
## Convergence code 2 try adjusting stepmax. See '?nlm' Details --> Value --> Code
## for more information.
cp3 <- bizicount(m5,m6,data,
                 cop = "frank",
                 margins = c("pois","pois"),
                 keep = T) 
cp4 <- bizicount(m7,m8,data,
                 cop = "frank",
                 margins = c("pois","pois"),
                 keep = T) 
## Warning in bizicount(m7, m8, data, cop = "frank", margins = c("pois", "pois"), :
## Convergence code 2 try adjusting stepmax. See '?nlm' Details --> Value --> Code
## for more information.
cp5 <- bizicount(m9,m10,data,
                 cop = "frank",
                 margins = c("pois","pois"),
                 keep = T) 
summary(cp1);cp1$aic;cp1$bic;cp1$loglik
## Call:
## bizicount(fmla1 = m1, fmla2 = m2, data = data, cop = "frank", 
##     margins = c("pois", "pois"), keep = T)
## 
## ================================================= 
## Count Model: Y3 | 
## ----------------- 
## 
##              Estimate Std. Err. Z value  Pr(>|z|)    
## (Intercept) -2.050288  0.344434 -5.9526 2.639e-09 ***
## X13          0.168832  0.053236  3.1714  0.001517 ** 
## X14          0.096072  0.048829  1.9675  0.049122 *  
## 
## ------------------------------------------------- 
##            Estimate Std. Err. Z value Pr(>|z|)
## dependence -0.13017   1.02842 -0.1266   0.8993
## ------------------------------------------------- 
## 
## Count Model: Y4 | 
## ----------------- 
## 
##              Estimate Std. Err. Z value  Pr(>|z|)    
## (Intercept) -1.477182  0.318949 -4.6314 3.632e-06 ***
## X13          0.025026  0.081021  0.3089    0.7574    
## X14          0.070030  0.049202  1.4233    0.1547    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## =================================================
## [1] 329.0767
## [1] 319.9287
## [1] -157.5383
summary(cp2);cp2$aic;cp2$bic;cp2$loglik
## Call:
## bizicount(fmla1 = m3, fmla2 = m4, data = data, cop = "frank", 
##     margins = c("pois", "pois"), keep = T)
## 
## ================================================= 
## Count Model: Y3 | 
## ----------------- 
## 
##             Estimate Std. Err. Z value Pr(>|z|)   
## (Intercept) -7.24599   2.37015 -3.0572 0.002234 **
## X23          0.71799   0.24260  2.9596 0.003081 **
## X24          0.17255   0.26313  0.6558 0.511983   
## 
## ------------------------------------------------- 
##             Estimate Std. Err. Z value Pr(>|z|)
## dependence -0.028862  1.099900 -0.0262   0.9791
## ------------------------------------------------- 
## 
## Count Model: Y4 | 
## ----------------- 
## 
##              Estimate Std. Err. Z value Pr(>|z|)   
## (Intercept) -6.984980  2.214158 -3.1547 0.001607 **
## X23          0.041263  0.184644  0.2235 0.823166   
## X24          0.807100  0.258239  3.1254 0.001776 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## =================================================
## [1] 320.9369
## [1] 311.7889
## [1] -153.4684
summary(cp3);cp3$aic;cp3$bic;cp3$loglik
## Call:
## bizicount(fmla1 = m5, fmla2 = m6, data = data, cop = "frank", 
##     margins = c("pois", "pois"), keep = T)
## 
## ================================================= 
## Count Model: Y3 | 
## ----------------- 
## 
##              Estimate Std. Err. Z value Pr(>|z|)   
## (Intercept) -5.311654  1.821524 -2.9160 0.003545 **
## X13          0.083020  0.061376  1.3526 0.176170   
## X23          0.571867  0.273929  2.0876 0.036830 * 
## 
## ------------------------------------------------- 
##            Estimate Std. Err. Z value Pr(>|z|)
## dependence  0.36518   1.00648  0.3628   0.7167
## ------------------------------------------------- 
## 
## Count Model: Y4 | 
## ----------------- 
## 
##                Estimate   Std. Err. Z value Pr(>|z|)
## (Intercept) -1.22470279  1.35699250 -0.9025   0.3668
## X13          0.03233454  0.08750462  0.3695   0.7117
## X23         -0.00051239  0.21858406 -0.0023   0.9981
## 
## =================================================
## [1] 329.6511
## [1] 320.5031
## [1] -157.8255
summary(cp4);cp4$aic;cp4$bic;cp4$loglik
## Call:
## bizicount(fmla1 = m7, fmla2 = m8, data = data, cop = "frank", 
##     margins = c("pois", "pois"), keep = T)
## 
## ================================================= 
## Count Model: Y3 | 
## ----------------- 
## 
##              Estimate Std. Err. Z value Pr(>|z|)  
## (Intercept) -1.187913  1.963710 -0.6049  0.54522  
## X14          0.099318  0.057780  1.7189  0.08563 .
## X24         -0.060749  0.307135 -0.1978  0.84321  
## 
## ------------------------------------------------- 
##            Estimate Std. Err. Z value Pr(>|z|)
## dependence 0.058502  1.043114  0.0561   0.9553
## ------------------------------------------------- 
## 
## Count Model: Y4 | 
## ----------------- 
## 
##              Estimate Std. Err. Z value  Pr(>|z|)    
## (Intercept) -6.877205  1.947121 -3.5320 0.0004124 ***
## X14         -0.015096  0.059634 -0.2531 0.8001549    
## X24          0.839285  0.288757  2.9065 0.0036545 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## =================================================
## [1] 327.9728
## [1] 318.8248
## [1] -156.9864
summary(cp5);cp5$aic;cp5$bic;cp5$loglik
## Call:
## bizicount(fmla1 = m9, fmla2 = m10, data = data, cop = "frank", 
##     margins = c("pois", "pois"), keep = T)
## 
## ================================================= 
## Count Model: Y3 | 
## ----------------- 
## 
##              Estimate Std. Err. Z value Pr(>|z|)  
## (Intercept) -5.005542  2.839766 -1.7627  0.07796 .
## X13          0.101647  0.064922  1.5657  0.11742  
## X14          0.078780  0.061180  1.2877  0.19786  
## X23          0.494760  0.283028  1.7481  0.08045 .
## X24         -0.022272  0.324027 -0.0687  0.94520  
## 
## ------------------------------------------------- 
##            Estimate Std. Err. Z value Pr(>|z|)
## dependence 0.041746  1.147128  0.0364    0.971
## ------------------------------------------------- 
## 
## Count Model: Y4 | 
## ----------------- 
## 
##              Estimate Std. Err. Z value Pr(>|z|)   
## (Intercept) -7.268336  2.536318 -2.8657 0.004161 **
## X13          0.010064  0.095038  0.1059 0.915668   
## X14         -0.019798  0.062533 -0.3166 0.751540   
## X23          0.047062  0.219292  0.2146 0.830074   
## X24          0.850315  0.293449  2.8977 0.003760 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## =================================================
## [1] 325.5694
## [1] 311.194
## [1] -151.7847
b. Sulawesi - Papua (Y4 - Y5)
n1 <- Y4 ~ X14+X15
n2 <- Y5 ~ X14+X15 

n3 <- Y4 ~ X24+X25 
n4 <- Y5 ~ X24+X25 

n5 <- Y4 ~ X14+X24 
n6 <- Y5 ~ X14+X24

n7 <- Y4 ~ X15+X25 
n8 <- Y5 ~ X15+X25 

n9 <- Y4 ~ X14+X15+X24+X25 
n10 <- Y5 ~ X14+X15+X24+X25 
cop1 <- bizicount(n1,n2,data,
                 cop = "frank",
                 margins = c("pois","pois"),
                 keep = T) 
cop2 <- bizicount(n3,n4,data,
                 cop = "frank",
                 margins = c("pois","pois"),
                 keep = T) 
cop3 <- bizicount(n5,n6,data,
                 cop = "frank",
                 margins = c("pois","pois"),
                 keep = T) 
cop4 <- bizicount(n7,n8,data,
                 cop = "frank",
                 margins = c("pois","pois"),
                 keep = T) 
cop5 <- bizicount(n9,n10,data,
                 cop = "frank",
                 margins = c("pois","pois"),
                 keep = T) 
summary(cop1);cop1$aic;cop1$bic;cop1$loglik
## Call:
## bizicount(fmla1 = n1, fmla2 = n2, data = data, cop = "frank", 
##     margins = c("pois", "pois"), keep = T)
## 
## ================================================= 
## Count Model: Y4 | 
## ----------------- 
## 
##              Estimate Std. Err. Z value  Pr(>|z|)    
## (Intercept) -1.482126  0.282263 -5.2509 1.514e-07 ***
## X14          0.065415  0.049906  1.3108    0.1899    
## X15          0.071199  0.124537  0.5717    0.5675    
## 
## ------------------------------------------------- 
##            Estimate Std. Err. Z value Pr(>|z|)
## dependence   1.5867    1.5882  0.9991   0.3178
## ------------------------------------------------- 
## 
## Count Model: Y5 | 
## ----------------- 
## 
##              Estimate Std. Err. Z value  Pr(>|z|)    
## (Intercept) -2.990909  0.526260 -5.6833 1.321e-08 ***
## X14          0.014628  0.101605  0.1440  0.885528    
## X15          0.430793  0.137155  3.1409  0.001684 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## =================================================
## [1] 243.642
## [1] 234.494
## [1] -114.821
summary(cop2);cop2$aic;cop2$bic;cop2$loglik
## Call:
## bizicount(fmla1 = n3, fmla2 = n4, data = data, cop = "frank", 
##     margins = c("pois", "pois"), keep = T)
## 
## ================================================= 
## Count Model: Y4 | 
## ----------------- 
## 
##             Estimate Std. Err. Z value Pr(>|z|)   
## (Intercept) -6.06719   1.99730 -3.0377 0.002384 **
## X24          0.82052   0.25621  3.2025 0.001362 **
## X25         -0.12566   0.17043 -0.7373 0.460944   
## 
## ------------------------------------------------- 
##            Estimate Std. Err. Z value Pr(>|z|)  
## dependence   3.7398    2.0843  1.7943  0.07277 .
## ------------------------------------------------- 
## 
## Count Model: Y5 | 
## ----------------- 
## 
##              Estimate Std. Err. Z value Pr(>|z|)   
## (Intercept) -6.941306  3.072428 -2.2592  0.02387 * 
## X24         -0.076261  0.374795 -0.2035  0.83877   
## X25          0.821682  0.318652  2.5786  0.00992 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## =================================================
## [1] 235.7389
## [1] 226.5909
## [1] -110.8694
summary(cop3);cop3$aic;cop3$bic;cop3$loglik
## Call:
## bizicount(fmla1 = n5, fmla2 = n6, data = data, cop = "frank", 
##     margins = c("pois", "pois"), keep = T)
## 
## ================================================= 
## Count Model: Y4 | 
## ----------------- 
## 
##               Estimate  Std. Err. Z value  Pr(>|z|)    
## (Intercept) -6.7677696  1.9241870 -3.5172 0.0004361 ***
## X14         -0.0086061  0.0598492 -0.1438 0.8856612    
## X24          0.8195898  0.2852899  2.8728 0.0040681 ** 
## 
## ------------------------------------------------- 
##            Estimate Std. Err. Z value Pr(>|z|)
## dependence   2.6669    1.6391   1.627   0.1037
## ------------------------------------------------- 
## 
## Count Model: Y5 | 
## ----------------- 
## 
##              Estimate Std. Err. Z value Pr(>|z|)
## (Intercept) -1.114151  2.878259 -0.3871   0.6987
## X14          0.093518  0.104163  0.8978   0.3693
## X24         -0.227449  0.462044 -0.4923   0.6225
## 
## =================================================
## [1] 243.1516
## [1] 234.0037
## [1] -114.5758
summary(cop4);cop4$aic;cop4$bic;cop4$loglik
## Call:
## bizicount(fmla1 = n7, fmla2 = n8, data = data, cop = "frank", 
##     margins = c("pois", "pois"), keep = T)
## 
## ================================================= 
## Count Model: Y4 | 
## ----------------- 
## 
##             Estimate Std. Err. Z value Pr(>|z|)
## (Intercept)  0.33525   1.27420  0.2631   0.7925
## X15          0.25386   0.16958  1.4970   0.1344
## X25         -0.29680   0.23663 -1.2543   0.2097
## 
## ------------------------------------------------- 
##            Estimate Std. Err. Z value Pr(>|z|)
## dependence   1.7603    1.7589  1.0008   0.3169
## ------------------------------------------------- 
## 
## Count Model: Y5 | 
## ----------------- 
## 
##             Estimate Std. Err. Z value Pr(>|z|)
## (Intercept) -4.03687   2.96960 -1.3594   0.1740
## X15          0.37224   0.23373  1.5927   0.1112
## X25          0.18967   0.51213  0.3704   0.7111
## 
## =================================================
## [1] 243.3983
## [1] 234.2503
## [1] -114.6992
summary(cop5);cop5$aic;cop5$bic;cop5$loglik
## Call:
## bizicount(fmla1 = n9, fmla2 = n10, data = data, cop = "frank", 
##     margins = c("pois", "pois"), keep = T)
## 
## ================================================= 
## Count Model: Y4 | 
## ----------------- 
## 
##               Estimate  Std. Err. Z value Pr(>|z|)   
## (Intercept) -4.7425892  2.2746705 -2.0850 0.037073 * 
## X14         -0.0064255  0.0604756 -0.1062 0.915384   
## X15          0.2537043  0.1606617  1.5791 0.114308   
## X24          0.8188000  0.2806038  2.9180 0.003523 **
## X25         -0.3877905  0.2455546 -1.5792 0.114280   
## 
## ------------------------------------------------- 
##            Estimate Std. Err. Z value Pr(>|z|)
## dependence   2.8845    2.2517  1.2811   0.2002
## ------------------------------------------------- 
## 
## Count Model: Y5 | 
## ----------------- 
## 
##              Estimate Std. Err. Z value Pr(>|z|)
## (Intercept) -1.972659  4.195225 -0.4702   0.6382
## X14          0.092459  0.117143  0.7893   0.4299
## X15          0.357361  0.232425  1.5375   0.1242
## X24         -0.418239  0.483341 -0.8653   0.3869
## X25          0.252144  0.512108  0.4924   0.6225
## 
## =================================================
## [1] 239.4107
## [1] 225.0353
## [1] -108.7053
Bivariat Zero Inflated
a. Bali - Sulawesi (Y3 - Y4)
z1 <- Y3 ~ X13+X14|X13+X14
z2 <- Y4 ~ X13+X14|X13+X14

z3 <- Y3 ~ X23+X24|X23+X24
z4 <- Y4 ~ X23+X24|X23+X24

z5 <- Y3 ~ X13+X23|X13+X23
z6 <- Y4 ~ X13+X23|X13+X23

z7 <- Y3 ~ X14+X24|X14+X24
z8 <- Y4 ~ X14+X24|X14+X24

z9 <- Y3 ~ X13+X14+X23+X24|X13+X14+X23+X24
z10 <- Y4 ~ X13+X14+X23+X24|X13+X14+X23+X24
#Pemodelan ZIP
cp.zi.1 <- bizicount(z1,z2,data,
                     cop = "frank",
                     margins = c("zip","zip"),
                     link.zi = c("logit", "logit"),
                     keep = T) 
## Warning in bizicount(z1, z2, data, cop = "frank", margins = c("zip", "zip"), :
## Convergence code 2 try adjusting stepmax. See '?nlm' Details --> Value --> Code
## for more information.
cp.zi.2 <- bizicount(z3,z4,data,
                     cop = "frank",
                     margins = c("zip","zip"),
                     link.zi = c("probit", "probit"),
                     keep = T) 
## Warning in bizicount(z3, z4, data, cop = "frank", margins = c("zip", "zip"), : nlm() was unable to obtain Hessian matrix, so numDeriv::hessian() was used in computing standard errors.
##             Consider reducing 'stepmax' option to nlm to prevent this.
##             See `?nlm` for more details on the 'stepmax' option.
## Warning: Hessian of loglik is not negative definite at convergence point;
## convergence point is not a maximum.
cp.zi.3 <- bizicount(z5,z6,data,
                     cop = "frank",
                     margins = c("zip","zip"),
                     link.zi = c("probit", "probit"),
                     keep = T) 
## Warning in bizicount(z5, z6, data, cop = "frank", margins = c("zip", "zip"), : nlm() was unable to obtain Hessian matrix, so numDeriv::hessian() was used in computing standard errors.
##             Consider reducing 'stepmax' option to nlm to prevent this.
##             See `?nlm` for more details on the 'stepmax' option.

## Warning in bizicount(z5, z6, data, cop = "frank", margins = c("zip", "zip"), : Hessian of loglik is not negative definite at convergence point; convergence point is not a maximum.
## Warning in sqrt(diag(solve(hess.new, tol = tol))): NaNs produced
cp.zi.4 <- bizicount(z7,z8,data,
                     cop = "frank",
                     margins = c("zip","zip"),
                     link.zi = c("probit", "probit"),
                     keep = T) 

cp.zi.5 <- bizicount(z9,z10,data,
                     cop = "frank",
                     margins = c("zip","zip"),
                     link.zi = c("probit", "probit"),
                     keep = T) 
## Warning: Hessian of loglik is not negative definite at convergence point;
## convergence point is not a maximum.
summary(cp.zi.1);cp.zi.1$aic;cp.zi.1$bic;cp.zi.1$loglik
## Call:
## bizicount(fmla1 = z1, fmla2 = z2, data = data, cop = "frank", 
##     margins = c("zip", "zip"), link.zi = c("logit", "logit"), 
##     keep = T)
## 
## ================================================= 
## Count Model: Y3 | 
## ----------------- 
## 
##               Estimate  Std. Err. Z value Pr(>|z|)
## (Intercept) -0.9113089  0.6947910 -1.3116   0.1896
## X13          0.0982510  0.0665306  1.4768   0.1397
## X14         -0.0031543  0.0747580 -0.0422   0.9663
## 
## ++++++++++++++++++++++++ 
## Zero Inflation: Y3 | 
## -------------------- 
## 
##             Estimate Std. Err. Z value Pr(>|z|)  
## (Intercept)  1.59082   1.14410  1.3905  0.16439  
## X13         -0.19873   0.19603 -1.0138  0.31069  
## X14         -0.46310   0.24549 -1.8864  0.05924 .
## 
## ------------------------------------------------- 
##             Estimate Std. Err. Z value Pr(>|z|)
## dependence -0.009675  1.078658  -0.009   0.9928
## ------------------------------------------------- 
## 
## Count Model: Y4 | 
## ----------------- 
## 
##              Estimate Std. Err. Z value  Pr(>|z|)    
## (Intercept) -1.410893  0.344145 -4.0997 4.137e-05 ***
## X13          0.066833  0.100101  0.6677    0.5044    
## X14          0.044737  0.053355  0.8385    0.4018    
## 
## ++++++++++++++++++++++++ 
## Zero Inflation: Y4 | 
## -------------------- 
## 
##             Estimate Std. Err. Z value Pr(>|z|)
## (Intercept)   9.2715  155.1531  0.0598   0.9523
## X13          18.4832   48.9925  0.3773   0.7060
## X14         -92.5362   78.7867 -1.1745   0.2402
## 
## =================================================
## [1] 333.8598
## [1] 316.8707
## [1] -153.9299
summary(cp.zi.2);cp.zi.2$aic;cp.zi.2$bic;cp.zi.2$loglik
## Call:
## bizicount(fmla1 = z3, fmla2 = z4, data = data, cop = "frank", 
##     margins = c("zip", "zip"), link.zi = c("probit", "probit"), 
##     keep = T)
## 
## ================================================= 
## Count Model: Y3 | 
## ----------------- 
## 
##             Estimate Std. Err. Z value Pr(>|z|)  
## (Intercept) -3.59088   3.11468 -1.1529  0.24896  
## X23          0.62134   0.24555  2.5305  0.01139 *
## X24         -0.24386   0.34546 -0.7059  0.48025  
## 
## ++++++++++++++++++++++++ 
## Zero Inflation: Y3 | 
## -------------------- 
## 
##             Estimate Std. Err. Z value Pr(>|z|)
## (Intercept) 50.68551  35.80639  1.4155   0.1569
## X23         -0.55393   0.69069 -0.8020   0.4226
## X24         -7.71218   5.77316 -1.3359   0.1816
## 
## ------------------------------------------------- 
##            Estimate Std. Err. Z value Pr(>|z|)
## dependence  0.78449   1.20335  0.6519   0.5144
## ------------------------------------------------- 
## 
## Count Model: Y4 | 
## ----------------- 
## 
##              Estimate Std. Err. Z value  Pr(>|z|)    
## (Intercept) -6.504189  2.181186 -2.9820 0.0028642 ** 
## X23         -0.090247  0.196653 -0.4589 0.6462957    
## X24          0.866429  0.263080  3.2934 0.0009898 ***
## 
## ++++++++++++++++++++++++ 
## Zero Inflation: Y4 | 
## -------------------- 
## 
##              Estimate Std. Err.  Z value Pr(>|z|)    
## (Intercept) -34.62437  96.72913   -0.358   0.7204    
## X23         -79.01256   0.78504 -100.648   <2e-16 ***
## X24          47.70639   0.70822   67.361   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## =================================================
## [1] 325.2025
## [1] 308.2134
## [1] -149.6012
summary(cp.zi.3);cp.zi.3$aic;cp.zi.3$bic;cp.zi.3$loglik
## Call:
## bizicount(fmla1 = z5, fmla2 = z6, data = data, cop = "frank", 
##     margins = c("zip", "zip"), link.zi = c("probit", "probit"), 
##     keep = T)
## 
## ================================================= 
## Count Model: Y3 | 
## ----------------- 
## 
##              Estimate Std. Err. Z value Pr(>|z|)   
## (Intercept) -5.311555  1.822267 -2.9148 0.003559 **
## X13          0.083022  0.061423  1.3516 0.176493   
## X23          0.571852  0.274126  2.0861 0.036970 * 
## 
## ++++++++++++++++++++++++ 
## Zero Inflation: Y3 | 
## -------------------- 
## 
##               Estimate  Std. Err. Z value Pr(>|z|)
## (Intercept)    1.72090        NaN     NaN      NaN
## X13            0.37947 2841.74464   1e-04   0.9999
## X23           -1.92639        NaN     NaN      NaN
## 
## ------------------------------------------------- 
##            Estimate Std. Err. Z value Pr(>|z|)
## dependence  0.36518   1.00647  0.3628   0.7167
## ------------------------------------------------- 
## 
## Count Model: Y4 | 
## ----------------- 
## 
##                Estimate   Std. Err. Z value Pr(>|z|)
## (Intercept) -1.22452978  1.35326877 -0.9049   0.3655
## X13          0.03233924  0.08748603  0.3697   0.7116
## X23         -0.00054052  0.21802931 -0.0025   0.9980
## 
## ++++++++++++++++++++++++ 
## Zero Inflation: Y4 | 
## -------------------- 
## 
##               Estimate  Std. Err. Z value Pr(>|z|)
## (Intercept)   -0.82331 7517.15305  -1e-04   0.9999
## X13           -0.77973 1383.76028  -6e-04   0.9996
## X23           -1.16029 1914.95020  -6e-04   0.9995
## 
## =================================================
## [1] 341.6511
## [1] 324.662
## [1] -157.8255
summary(cp.zi.4);cp.zi.4$aic;cp.zi.4$bic;cp.zi.4$loglik
## Call:
## bizicount(fmla1 = z7, fmla2 = z8, data = data, cop = "frank", 
##     margins = c("zip", "zip"), link.zi = c("probit", "probit"), 
##     keep = T)
## 
## ================================================= 
## Count Model: Y3 | 
## ----------------- 
## 
##              Estimate Std. Err. Z value Pr(>|z|)  
## (Intercept)  4.781139  2.817784  1.6968  0.08974 .
## X14          0.046855  0.071309  0.6571  0.51114  
## X24         -0.824996  0.422857 -1.9510  0.05106 .
## 
## ++++++++++++++++++++++++ 
## Zero Inflation: Y3 | 
## -------------------- 
## 
##             Estimate Std. Err. Z value Pr(>|z|)  
## (Intercept)  7.41367   4.97599  1.4899  0.13625  
## X14         -0.27794   0.14537 -1.9119  0.05589 .
## X24         -1.03780   0.81572 -1.2722  0.20329  
## 
## ------------------------------------------------- 
##            Estimate Std. Err. Z value Pr(>|z|)
## dependence  0.75707   1.19535  0.6333   0.5265
## ------------------------------------------------- 
## 
## Count Model: Y4 | 
## ----------------- 
## 
##              Estimate Std. Err. Z value  Pr(>|z|)    
## (Intercept) -6.884856  1.948818 -3.5328 0.0004111 ***
## X14         -0.018289  0.059250 -0.3087 0.7575655    
## X24          0.841548  0.288861  2.9133 0.0035759 ** 
## 
## ++++++++++++++++++++++++ 
## Zero Inflation: Y4 | 
## -------------------- 
## 
##              Estimate Std. Err. Z value Pr(>|z|)
## (Intercept)   1.50466 830.06118  0.0018   0.9986
## X14           0.45655 357.49546  0.0013   0.9990
## X24          -1.70207 719.54143 -0.0024   0.9981
## 
## =================================================
## [1] 331.4836
## [1] 314.4945
## [1] -152.7418
summary(cp.zi.5);cp.zi.5$aic;cp.zi.5$bic;cp.zi.5$loglik
## Call:
## bizicount(fmla1 = z9, fmla2 = z10, data = data, cop = "frank", 
##     margins = c("zip", "zip"), link.zi = c("probit", "probit"), 
##     keep = T)
## 
## ================================================= 
## Count Model: Y3 | 
## ----------------- 
## 
##              Estimate Std. Err. Z value Pr(>|z|)
## (Intercept)  0.374234  3.964079  0.0944   0.9248
## X13          0.048142  0.070717  0.6808   0.4960
## X14          0.045297  0.070860  0.6393   0.5227
## X23          0.387773  0.297011  1.3056   0.1917
## X24         -0.606760  0.428800 -1.4150   0.1571
## 
## ++++++++++++++++++++++++ 
## Zero Inflation: Y3 | 
## -------------------- 
## 
##              Estimate Std. Err. Z value Pr(>|z|)
## (Intercept) 11.131357  9.466016  1.1759   0.2396
## X13         -0.135268  0.151646 -0.8920   0.3724
## X14         -0.252757  0.196734 -1.2848   0.1989
## X23         -0.076503  0.471038 -0.1624   0.8710
## X24         -1.548826  1.298524 -1.1928   0.2330
## 
## ------------------------------------------------- 
##            Estimate Std. Err. Z value Pr(>|z|)
## dependence  0.67873   1.24507  0.5451   0.5857
## ------------------------------------------------- 
## 
## Count Model: Y4 | 
## ----------------- 
## 
##               Estimate  Std. Err. Z value Pr(>|z|)   
## (Intercept) -7.2341644  2.5288611 -2.8606 0.004228 **
## X13          0.0091896  0.0948505  0.0969 0.922817   
## X14         -0.0217718  0.0620708 -0.3508 0.725770   
## X23          0.0426961  0.2173203  0.1965 0.844245   
## X24          0.8503782  0.2936986  2.8954 0.003787 **
## 
## ++++++++++++++++++++++++ 
## Zero Inflation: Y4 | 
## -------------------- 
## 
##               Estimate  Std. Err. Z value Pr(>|z|)
## (Intercept)    0.45954 1596.80623  0.0003   0.9998
## X13           -3.05850 1788.58464 -0.0017   0.9986
## X14            3.19384  549.91206  0.0058   0.9954
## X23           -5.25391  476.50494 -0.0110   0.9912
## X24           -1.20981  625.35349 -0.0019   0.9985
## 
## =================================================
## [1] 340.516
## [1] 313.0721
## [1] -149.258
