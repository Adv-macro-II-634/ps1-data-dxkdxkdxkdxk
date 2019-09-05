setwd("~/Documents/GitHub/ps1-data-dxkdxkdxkdxk")
data = readxl::read_xlsx('SCFP2007.xlsx')
install.packages('reldist')
library(reldist)

y = data.frame(data$YY1,data$Y1,data$WGT,data$WAGEINC,
      data$INCOME,data$INCCAT,data$BUSSEFARMINC,data$INTDIVINC,
      data$SSRETINC,data$KGINC)  ## income

y[,4:10] = y[,4:10]*0.88  # converting 2016 USD to 2007 USD

y$earining = y$data.WAGEINC + 0.863*y$data.BUSSEFARMINC # constructing earning
e_quantiles = data.frame(wtd.quantile(y$earining/1000,
                                  q=c(0,0.01,0.05,0.1,0.2,0.4,0.6,0.8,0.9,0.95,0.99,1),
                                  weight = y$data.WGT))
names(e_quantiles)[1] = "quantiles"
e_quantiles$quantiles = round(e_quantiles$quantiles,digits=2) ### earings quantile table 1

