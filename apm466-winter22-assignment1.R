
## Fundamental Questions ##
## Question 1
## 1a. Since printing money directly increases the money supply and leads to inflation, governments choose to borrow money to issue bonds rather than create money.

## 1b. Investors expect the Fed to raise the federal funds' rates recently, short-term interest rates will rise more than long-term interest rates, which means the gap of the yield short and long term bond is narrowing and the yield curve is flattening. 

## 1c. Quantitative easing is a monetary policy strategy that a central bank makes a large-scale purchase of financial assets to simulate economy and keep credit flowing.At the beginning of the COVID-19 pandemic, the Federal Reserve on Monday announced it would purchase an unlimited amount of Treasury’s and mortgage-backed securities to support the financial market.

## Question 2
## CAN 0.5  MAR 1
## CAN 1.75 MAR 1
## CAN 2.25 MAR 1
## CAN 1.5  SEP 1
## CAN 1.25 Mar 1
## CAN 0.5  Sep 1
## CAN 0.25 Mar 1
## CAN 1.0  SEP 1
## CAN 1.25 MAR 1
## CAN 0.25 APR 1

## Since we are asked to construct a "0-5 year" yield & spots curves,I chose bonds whose difference between maturity and issue date are greater than five years. Most of the difference is five years, and one particular difference is ten years.

## Question 3
## We plan to extract the main information for analysis. So we want to study things in smaller dimensions and lose as little information as possible.Thus, the variances of the remaining components to be as much as possible. Because the greater the covariance, the greater the difference between the data, the more information is retained. So we want to select a direction with a large variance to reduce. The eigenvector of the covariance matrix represents the projection direction corresponding to the reduction of the original value, and the eigenvalue of the covariance matrix corresponds to the contribution value of the variance under the reduction direction, namely, the information contribution value.


## Question 4

## read the collected data use jrvFinance package
library(jrvFinance)
library(readxl)
## define all 36 bonds
bond<-read_xlsx("/cloud/project/APM466DATA0.xlsx")
## define the selected 10 bonds
selected_bond<-read_xlsx("/cloud/project/APM466DATA1.xlsx")


## redefine the data that will be used in the future calculation
coupon_payment<-as.numeric(selected_bond$COUPON)
maturity_date<-sub("UTC","",selected_bond$`Maturity Date`)
close_price_date<-c("2022-1-10","2022-1-11","2022-1-12","2022-1-13","2022-1-14",
                    "2022-1-17","2022-1-18","2022-1-19","2022-1-20","2022-1-21")
close_price_matrix=matrix(c(selected_bond$'10',selected_bond$'11',selected_bond$'12',
                            selected_bond$'13',selected_bond$'14',selected_bond$'17',
                            selected_bond$'18',selected_bond$'19',
                            selected_bond$'20',selected_bond$'21'),nrow=10, ncol=10, byrow =TRUE)


## Calculating Yield To Maturity using bond.yield() function
ytm_matrix=matrix('numeric',nrow=10,ncol=10)

for (j in c(1:10)){
  close_price=close_price_matrix[,j]
  for(i in c(1:10)){
    ytm_matrix[i,j]<-bond.yield(settle=close_price_date[i], mature=maturity_date[j],coupon=coupon_payment[j],freq=2,close_price[i],convention=c("30/360","ACT/ACT","ACT/360","30/360E"),comp.freq=2, redemption_value = 100 )
  }
}

## plot yield curve
year<-seq(0.5,5,by=0.5)
plot(year,ytm_matrix[1,], type="o",main="5-year yield curve", col="black",xlab="year",ylab="Yield to maturity",ylim=c(0.001,0.02),lwd=1.0)
color<-c("pink","red","orange","yellow","green","blue","purple","powderblue","grey")
for(i in c(2:10)){
  lines(year,ytm_matrix[i,],type="o",col=color[i-1],lwd=1.0)
}

# adding a legend to the yield curve plot
colors<-c("black",color)
legend("topleft",pch=c(15,15),legend=c("2022-1-10","2022-1-11","2022-1-12",
                                       "2022-1-13","2022-1-14","2022-1-17","2022-1-18","2022-1-19",
                                       "2022-1-20","2022-1-21"), col=colors, lty=1.2,cex=0.5)



## spot rate
## calculate the dirty price by bond.TCF() function first
dirtyprice<-matrix('numeric',nrow=10,ncol=10)
for(j in 1:10){
  for(i in 1:10){
    dirtyprice[j,i]=bond.TCF(close_price_date[j],maturity_date[i],coupon_payment[i],
                             freq=2)$accrued+close_price_matrix[j,i]
  }
}

## calculate time to maturity in year fraction by yearFraction() function
year_frac=matrix('numeric',nrow=10,ncol=10)
for (i in c(1:10)){
  for (j in c(1:10)){
    year_frac[i,j]=yearFraction(close_price_date[i],maturity_date[j],freq=2,convention=c("30/360","ACT/ACT","ACT/360","30/360E"))
  }
}

## define spot rate matrix
raw_spot_matrix<-matrix(nrow=10,ncol=10)

## spot rate for bond 1
## calculate the cash flow of bond 1
cf1=bond.TCF(close_price_date[1],maturity_date[1],coupon_payment[1],freq=2,
             redemption_value = 100)$cf
sr1=c()
## calculate the spot rate for bond 1
for (i in 1:10){
  t_1=as.numeric(year_frac[i,1])
  sr1[i]<-log(cf1[1]/as.numeric(dirtyprice[i,1]))/t_1
}

## spot rate for bond 2
cf2=bond.TCF(close_price_date[2],maturity_date[2],coupon_payment[2],freq=2,
             redemption_value = 100)$cf
sr2=c()
for (i in 1:10){
  t_2=as.numeric(year_frac[i,2])
  differ_2=as.numeric(dirtyprice[i,2])-cf2[1]*exp(-1*sr1[i])*t_2
  sr2[i]<-log(cf2[3]/differ_2)/t_2
}

## spot rate for bond 3
cf3=bond.TCF(close_price_date[3],maturity_date[3],coupon_payment[3],freq=2,
             redemption_value = 100)$cf
sr3=c()
for (i in 1:10){
  t_3=as.numeric(year_frac[i,3])
  differ_3=as.numeric(dirtyprice[i,3])-cf3[1]*exp(-1*sr1[i])*t_3-cf3[2]*
    exp(-1*sr2[i])*t_3
  sr3[i]<-log(cf3[5]/differ_3)/t_3
}

## spot rate for bond 4
cf4=bond.TCF(close_price_date[4],maturity_date[4],coupon_payment[4],freq=2,
             redemption_value = 100)$cf

sr4=c()
for (i in 1:10){
  t_4=as.numeric(year_frac[i,4])
  differ_4=as.numeric(dirtyprice[i,4])-cf4[1]*exp(-1*sr1[i])*t_4-cf4[2]*exp(-1*sr2[i])*t_4-cf4[3]*exp(-1*sr3[i])*t_4
  sr4[i]<-log(cf4[6]/differ_4)/t_4
}


## spot rate for bond 5
cf5=bond.TCF(close_price_date[5],maturity_date[5],coupon_payment[5],freq=2,
             redemption_value = 100)$cf
sr5=c()
for (i in 1:10){
  t_5=as.numeric(year_frac[i,5])
  differ_5=as.numeric(dirtyprice[i,5])-cf5[1]*exp(-1*sr1[i])*t_5-cf5[2]*exp(-1*sr2[i])*t_5
  sr5[i]<-log(cf5[5]/differ_5)/t_5
}


## spot rate for bond 6
cf6=bond.TCF(close_price_date[6],maturity_date[6],coupon_payment[6],freq=2,
             redemption_value = 100)$cf
sr6=c()
for (i in 1:10){
  t_6=as.numeric(year_frac[i,6])
  differ_6=as.numeric(dirtyprice[i,6])-cf6[1]*exp(-1*sr1[i])*t_6-cf6[2]*exp(-1*sr2[i])*t_6-cf6[3]*exp(-1*sr3[i])*t_6-cf6[4]*exp(-1*sr4[i])*t_6
  sr6[i]<-log(cf6[7]/differ_6)/t_6
}

## spot rate for bond 7
cf7=bond.TCF(close_price_date[7],maturity_date[7],coupon_payment[7],freq=2,
             redemption_value = 100)$cf

sr7=c()
for (i in 1:10){
  t_7=as.numeric(year_frac[i,7])
  differ_7=as.numeric(dirtyprice[i,7])-cf7[1]*exp(-1*sr1[i])*t_7-cf7[2]*exp(-1*sr2[i])*t_7-cf7[3]*exp(-1*sr3[i])*t_7-cf7[4]*exp(-1*sr4[i])*t_7-cf7[6]*exp(-1*sr6[i])*t_7
  sr7[i]<-log(cf7[8]/differ_7)/t_7
}


## spot rate for bond 8
cf8=bond.TCF(close_price_date[8],maturity_date[8],coupon_payment[8],freq=2,
             redemption_value = 100)$cf

sr8=c()
for (i in 1:10){
  t_8=as.numeric(year_frac[i,8])
  differ_8=as.numeric(dirtyprice[i,8])-cf8[1]*exp(-1*sr1[i])*t_8-cf8[2]*exp(-1*sr2[i])*t_8-cf8[3]*exp(-1*sr3[i])*t_8-cf8[4]*exp(-1*sr4[i])*t_8-cf8[5]*exp(-1*sr6[i])*t_8-cf8[6]*exp(-1*sr7[i])*t_8
  sr8[i]<-log(cf8[9]/differ_8)/t_8
}


## spot rate for bond 9
cf9=bond.TCF(close_price_date[9],maturity_date[9],coupon_payment[9],freq=2,
             redemption_value = 100)$cf
sr9=c()
for (i in 1:10){
  t_9=as.numeric(year_frac[i,9])
  differ_9=as.numeric(dirtyprice[i,9])-cf9[1]*exp(-1*sr1[i])*t_9-cf9[2]*exp(-1*sr2[i])*(t_3-t_2)-cf9[3]*exp(-1*sr3[i])*t_9-cf9[4]*exp(-1*sr4[i])*t_9-cf9[5]*exp(-1*sr6[i])*t_9-cf9[6]*exp(-1*sr7[i])*t_9-cf9[7]*exp(-1*sr8[i])*t_9
  sr9[i]<-log(cf9[10]/differ_9)/t_9
}

## spot rate for bond 10
cf10=bond.TCF(close_price_date[10],maturity_date[10],coupon_payment[10],freq=2,
              redemption_value = 100)$cf
sr10=c()
for (i in 1:10){
  t_10=as.numeric(year_frac[i,10])
  differ_10=as.numeric(dirtyprice[i,10])-cf10[1]*exp(-1*sr1[i])*t_10-cf10[2]*exp(-1*sr2[i])*t_10-cf10[3]*exp(-1*sr3[i])*t_10-cf10[4]*exp(-1*sr4[i])*t_10-cf10[5]*exp(-1*sr6[i])*t_10-cf10[6]*exp(-1*sr7[i])*(t_8-t_7)-cf10[7]*exp(-1*sr8[i])*t_10-cf10[8]*exp(-1*sr9[i])*t_10
  sr10[i]<-log(cf10[11]/differ_10)/t_10
}

raw_spot_matrix=cbind(sr1,sr2,sr3,sr4,sr5,sr6,sr7,sr8,sr9,sr10)


## plot spot rate curve
year<-seq(0.5,5,by=0.5)
plot(year,raw_spot_matrix[1,], type="o",main="5-year spot curve", col="black",xlab="year",ylab="spot rate",ylim=c(0.003,0.06),lwd=1.0)
color<-c("pink","red","orange","yellow","green","blue","purple","powderblue","grey")
for(i in c(2:10)){
  lines(year,raw_spot_matrix[i,],type="o",col=color[i-1],lwd=0.8)
}

# adding a legend to the plot
legend("topleft",pch=c(15,15),legend=c("2022-1-10","2022-1-11","2022-1-12",
                                       "2022-1-13","2022-1-14","2022-1-17","2022-1-18","2022-1-19",
                                       
                                       
                                       ## 1year to 1year forward rate calculation
                                       one_one_forward_rate<-sr1
                                       
                                       
                                       ## 1year to 2year forward rate calculation
                                       one_two_forward_rate<-c()
                                       one_two_forward_rate[1]=((1+sr2[1])^(t_2))/((1+sr1[1])^(t_1))-1
                                       one_two_forward_rate[2]=((1+sr2[2])^(t_2))/((1+sr1[2])^(t_1))-1
                                       one_two_forward_rate[3]=((1+sr2[3])^(t_2))/((1+sr1[3])^(t_1))-1
                                       one_two_forward_rate[4]=((1+sr2[4])^(t_2))/((1+sr1[4])^(t_1))-1
                                       one_two_forward_rate[5]=((1+sr2[5])^(t_2))/((1+sr1[5])^(t_1))-1
                                       one_two_forward_rate[6]=((1+sr2[6])^(t_2))/((1+sr1[6])^(t_1))-1
                                       one_two_forward_rate[7]=((1+sr2[7])^(t_2))/((1+sr1[7])^(t_1))-1
                                       one_two_forward_rate[8]=((1+sr2[8])^(t_2))/((1+sr1[8])^(t_1))-1
                                       one_two_forward_rate[9]=((1+sr2[9])^(t_2))/((1+sr1[9])^(t_1))-1
                                       one_two_forward_rate[10]=((1+sr2[10])^(t_2))/((1+sr1[10])^(t_1))-1
                                       
                                       ## 1year to 3year forward rate calculation
                                       one_three_forward_rate<-c()
                                       one_three_forward_rate[1]=((1+sr3[1])^(t_3))/((1+sr1[1])^(t_1))-1
                                       one_three_forward_rate[2]=((1+sr3[2])^(t_3))/((1+sr1[2])^(t_1))-1
                                       one_three_forward_rate[3]=((1+sr3[3])^(t_3))/((1+sr1[3])^(t_1))-1
                                       one_three_forward_rate[4]=((1+sr3[4])^(t_3))/((1+sr1[4])^(t_1))-1
                                       one_three_forward_rate[5]=((1+sr3[5])^(t_3))/((1+sr1[5])^(t_1))-1
                                       one_three_forward_rate[6]=((1+sr3[6])^(t_3))/((1+sr1[6])^(t_1))-1
                                       one_three_forward_rate[7]=((1+sr3[7])^(t_3))/((1+sr1[7])^(t_1))-1
                                       one_three_forward_rate[8]=((1+sr3[8])^(t_3))/((1+sr1[8])^(t_1))-1
                                       one_three_forward_rate[9]=((1+sr3[9])^(t_3))/((1+sr1[9])^(t_1))-1
                                       one_three_forward_rate[10]=((1+sr3[10])^(t_3))/((1+sr1[10])^(t_1))-1
                                       
                                       
                                       ## 1 year to 4 year forward rate calculation
                                       one_four_forward_rate<-c()
                                       one_four_forward_rate[1]=((1+sr4[1])^(t_4))/((1+sr1[1])^(t_1))-1
                                       one_four_forward_rate[2]=((1+sr4[2])^(t_4))/((1+sr1[2])^(t_1))-1
                                       one_four_forward_rate[3]=((1+sr4[3])^(t_4))/((1+sr1[3])^(t_1))-1
                                       one_four_forward_rate[4]=((1+sr4[4])^(t_4))/((1+sr1[4])^(t_1))-1
                                       one_four_forward_rate[5]=((1+sr4[5])^(t_4))/((1+sr1[5])^(t_1))-1
                                       one_four_forward_rate[6]=((1+sr4[6])^(t_4))/((1+sr1[6])^(t_1))-1
                                       one_four_forward_rate[7]=((1+sr4[7])^(t_4))/((1+sr1[7])^(t_1))-1
                                       one_four_forward_rate[8]=((1+sr4[8])^(t_4))/((1+sr1[8])^(t_1))-1
                                       one_four_forward_rate[9]=((1+sr4[9])^(t_4))/((1+sr1[9])^(t_1))-1
                                       one_four_forward_rate[10]=((1+sr4[10])^(t_4))/((1+sr1[10])^(t_1))-1
                                       
                                       ## combine forward rate of each year as a forward matrix
                                       forward_matrix<-cbind(one_one_forward_rate,one_two_forward_rate,one_three_forward_rate,one_four_forward_rate)
                                       
                                       
                                       ## plot forward curve
                                       year<-c(2,3,4,5)
                                       plot(year,forward_matrix[1,],type="o",main="5-year forward curve",col="black",xlab="year",ylab="forward rate",ylim=c(0.002,0.06),lwd=1)
                                       color<-c("pink","red","orange","yellow","green","blue","purple","powderblue","grey")
                                       for(i in c(2:10)){
                                         lines(year,forward_matrix[i,],type="o",col=color[i-1],lwd=1.0)
                                       }
                                       # adding a legend to the plot
                                       legend("topleft",pch=c(15,15),legend=c("2022-1-10","2022-1-11","2022-1-12",
                                                                              "2022-1-13","2022-1-14","2022-1-17","2022-1-18","2022-1-19",
                                                                              "2022-1-20","2022-1-21"), col=colors,
                                              lty=1,cex=0.5)
                                       
                                       
                                       ## Question 5
                                       ```{r,echo=TRUE}
                                       ## log return of ytm using the giben formula
                                       log_ytm_matrix<-matrix(nrow=9,ncol=5)
                                       for(i in c(1:5)){
                                         for(j in c(1:9)){
                                           log_ytm_matrix[j,i]<-log(as.numeric(ytm_matrix[(j+1),i])/as.numeric(ytm_matrix[j,i]))
                                         }
                                       }
                                       
                                       ## covariance matrix for yield to maturity
                                       ytm_cov<-cov(log_ytm_matrix,log_ytm_matrix)
                                       print(ytm_cov)
                                       
                                       ## covariance matrix for forward rate
                                       forward_cov<-cov(forward_matrix,forward_matrix)
                                       print(forward_cov)
                                       
                                       
                                       
                                       ## Question 6
                                       
                                       # calculate the eigenvalues and eigenvectors for ytm covariance matrices
                                       print(eigen(ytm_cov)$values)
                                       print(eigen(ytm_cov)$vectors)
                                       
                                       # calculate the eigenvalues and eigenvectors for forward covariance matrices
                                       
                                       print(eigen(forward_cov)$values)
                                       print(eigen(forward_cov)$vectors)