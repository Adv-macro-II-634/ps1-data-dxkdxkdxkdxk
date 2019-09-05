setwd("~/Desktop/advanced macro")
data = readxl::read_xlsx('SCFP2007.xlsx')
setwd("~/Documents/GitHub/ps1-data-dxkdxkdxkdxk")
install.packages('reldist')
library(reldist)
install.packages('Weighted.Desc.Stat')
library(Weighted.Desc.Stat)
install.packages('acid')
library(acid)
library(xtable)

y = data.frame(data$YY1,data$Y1,data$WGT,data$WAGEINC,
      data$INCOME,data$INCCAT,data$BUSSEFARMINC,data$INTDIVINC,
      data$SSRETINC,data$KGINC,data$TRANSFOTHINC,data$NETWORTH)  ## all variables we need


y[,4:12] = y[,4:12]*0.88  # converting 2016 USD to 2007 USD

## quantiles

# constructing earnings
y$earining = y$data.WAGEINC + 0.863*y$data.BUSSEFARMINC 
e_quantiles = data.frame(wtd.quantile(y$earining/1000,
                                  c(0,0.01,0.05,0.1,0.2,0.4,0.6,0.8,0.9,0.95,0.99,1),
                                  weight = y$data.WGT))
names(e_quantiles)[1] = "quantiles"
e_quantiles$quantiles = round(e_quantiles$quantiles,digits=1) ### earings quantile table 1
xtable(t(e_quantiles))

# constructing income
y$income = y$data.WAGEINC  + y$data.BUSSEFARMINC + y$data.INTDIVINC + y$data.KGINC +
            y$data.SSRETINC + y$data.TRANSFOTHINC 
inc_quantiles = data.frame(wtd.quantile(y$income/1000,
                                      c(0,0.01,0.05,0.1,0.2,0.4,0.6,0.8,0.9,0.95,0.99,1),
                                      weight = y$data.WGT))
names(inc_quantiles)[1] = "quantiles"
inc_quantiles$quantiles = round(inc_quantiles$quantiles,digits=1) ### income quantile table 1
xtable(t(inc_quantiles))

## wealth
w_quantiles = data.frame(wtd.quantile(y$data.NETWORTH/1000,
                                      c(0,0.01,0.05,0.1,0.2,0.4,0.6,0.8,0.9,0.95,0.99,1),
                                      weight = y$data.WGT))
names(w_quantiles)[1] = "quantiles"
w_quantiles$quantiles = round(w_quantiles$quantiles,digits=1) ### wealth quantile table 1
xtable(t(w_quantiles))




## Coefficient of Variation of earning
round(w.cv(y$earining,y$data.WGT),digits = 2)

## Variance of the logs
round(w.var(lg,y$data.WGT),digits = 2)

## Gini index
weighted.gini(y$earining,y$data.WGT)

## top 1%/ lowest 40%
(e_quantiles[12,] - e_quantiles[11,])/(e_quantiles[1,] + e_quantiles[6,])

## Location of mean
mean = w.mean(y$earining,y$data.WGT)
d = wtd.Ecdf(y$earining,y$data.WGT)
ecdf = ecdf(d$x)
ecdf(mean)
## mean/median
w.mean(y$earining,y$data.WGT) / wtd.quantile(y$earining,0.5,weight = y$data.WGT)
