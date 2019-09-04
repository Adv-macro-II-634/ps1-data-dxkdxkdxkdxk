setwd("~/Desktop/advanced macro")
data = readxl::read_xlsx('SCFP2007.xlsx')

y = data.frame(data$YY1,data$Y1,data$WGT,data$WAGEINC,
      data$INCOME,data$INCCAT,data$BUSSEFARMINC,data$INTDIVINC,
      data$SSRETINC,data$KGINC)  ## income

y[,4:10] = y[,4:10]*0.88  # converting 2016 USD to 2007 USD

y$earining = y$data.INCOME +  0.863*y$data.BUSSEFARMINC # constructing earning
