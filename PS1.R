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
install.packages("ineq")
library(ineq)
install.packages('REAT')
library(REAT)

y = data.frame(data$YY1,data$Y1,data$WGT,data$WAGEINC,
      data$INCOME,data$INCCAT,data$BUSSEFARMINC,data$INTDIVINC,
      data$SSRETINC,data$KGINC,data$TRANSFOTHINC,data$NETWORTH)  ## all variables we need


y[,4:12] = y[,4:12]*0.88  # converting 2016 USD to 2007 USD

## quantiles

# constructing earnings
y$earning = y$data.WAGEINC + 0.863*y$data.BUSSEFARMINC 
e_quantiles = data.frame(wtd.quantile(y$earning/1000,
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




## Coefficient of Variation 
round(w.cv(y$earning,y$data.WGT),digits = 2) #earning
round(w.cv(y$income,y$data.WGT),digits = 2) #income
round(w.cv(y$data.NETWORTH,y$data.WGT),digits = 2) #wealth

## Variance of the logs
round(w.var(lg,y$data.WGT),digits = 2)

## Gini index
weighted.gini(y$earning,y$data.WGT)
weighted.gini(y$income,y$data.WGT)
weighted.gini(y$data.NETWORTH,y$data.WGT)

## top 1%/ lowest 40%
t = data.frame(y$earning,y$income,y$data.NETWORTH,y$data.WGT)
t1 = t[order(t$y.earning),]
t1$a = t1$y.data.WGT/sum(t1$y.data.WGT)
t1$b = cumsum(t1$a)
sum(t1$y.earning[t1$b>=0.99])/sum(t1$y.earning[t1$b<=0.4])  # earnings

t2 = t[order(t$y.income),]
t2$a = t2$y.data.WGT/sum(t2$y.data.WGT)
t2$b = cumsum(t2$a)
sum(t2$y.income[t2$b>=0.99])/sum(t2$y.income[t2$b<=0.4])  # income

t3 = t[order(t$y.data.NETWORTH),]
t3$a = t3$y.data.WGT/sum(t3$y.data.WGT)
t3$b = cumsum(t3$a)
sum(t3$y.data.NETWORTH[t3$b>=0.99])/sum(t3$y.data.NETWORTH[t3$b<=0.4])  # income



## Location of mean
mean = w.mean(y$earning,y$data.WGT)
d = wtd.Ecdf(y$earning,y$data.WGT)
ecdf = ecdf(d$x)

## mean/median
w.mean(y$earning,y$data.WGT) / wtd.quantile(y$earning,0.5,weight = y$data.WGT)
w.mean(y$income,y$data.WGT) / wtd.quantile(y$income,0.5,weight = y$data.WGT)
w.mean(y$data.NETWORTH,y$data.WGT) / wtd.quantile(y$data.NETWORTH,0.5,weight = y$data.WGT)




### Lorenz Curves
lorenz(y$earning, weighting = y$data.WGT, z = NULL, na.rm = TRUE,
       lcx = "% of objects", lcy = "% of regarded variable", 
       lctitle = "Lorenz curve of earnings", le.col = "blue", lc.col = "black",
       lsize = 1.5, ltype = "solid", bg.col = "gray95", bgrid = TRUE, 
       bgrid.col = "white", bgrid.size = 2, bgrid.type = "solid",
       lcg = FALSE, lcgn = FALSE, lcg.caption = NULL, lcg.lab.x = 0, 
       lcg.lab.y = 1, add.lc = FALSE, plot.lc = TRUE)

lorenz(y$income, weighting = y$data.WGT, z = NULL, na.rm = TRUE,
       lcx = "% of objects", lcy = "% of regarded variable", 
       lctitle = "Lorenz curve of income", le.col = "blue", lc.col = "black",
       lsize = 1.5, ltype = "solid", bg.col = "gray95", bgrid = TRUE, 
       bgrid.col = "white", bgrid.size = 2, bgrid.type = "solid",
       lcg = FALSE, lcgn = FALSE, lcg.caption = NULL, lcg.lab.x = 0, 
       lcg.lab.y = 1, add.lc = FALSE, plot.lc = TRUE)

lorenz(y$data.NETWORTH, weighting = y$data.WGT, z = NULL, na.rm = TRUE,
       lcx = "% of objects", lcy = "% of regarded variable", 
       lctitle = "Lorenz curve of wealth", le.col = "blue", lc.col = "black",
       lsize = 1.5, ltype = "solid", bg.col = "gray95", bgrid = TRUE, 
       bgrid.col = "white", bgrid.size = 2, bgrid.type = "solid",
       lcg = FALSE, lcgn = FALSE, lcg.caption = NULL, lcg.lab.x = 0, 
       lcg.lab.y = 1, add.lc = FALSE, plot.lc = TRUE)


