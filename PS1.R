setwd("~/Desktop/advanced macro")
data = readxl::read_xlsx('SCFP2007.xlsx')

y = data.frame(data$YY1,data$Y1,data$WGT,data$WAGEINC,
      data$INCOME,data$INCCAT,data$BUSSEFARMINC,data$INTDIVINC,
      data$SSRETINC,data$KGINC)  ## income
