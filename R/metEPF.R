# Energy Pattern Factor Toolbox!
#
# This function named 'metEPF'
# Calculate EPF from a daily wind speed data!.
#


metEPF<-function(df,start,end){
  z<-seq(lubridate::mdy(start),lubridate::mdy(end),by="days")
  z<-as.data.frame.POSIXct(z)
  colnames(z)<-c("date")
  df<-data.frame(z,(x1=as.vector(df[1])))
  colnames(df)<-c("date","x1")

  #df1$date<- mdy(df$date)
  #---function to cubed the data
  #---function to calculate monthly mean by month of each year
  #---function to calculate monthly cube mean by month of each year

  df$month<-lubridate::month(df$date)
  df_m<-dplyr::group_by(df,month)

  df_m1<-dplyr::summarize(df_m,mean(x1^3))     # UPPER==>mean cube wind speed(m3/s3)
  df_m2<-dplyr::summarize(df_m,(mean(x1)^3))   # LOWER==>monthly mean wind speed)^3

  EHF<-(df_m1$'mean(x1^3)'/df_m2$'(mean(x1)^3)')

  months<-c("Jan",'feb','mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
  EPF<-data.frame(months=months,EHF)
  EPF$months<-factor(EPF$months,levels=c("Jan",'feb','mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))
  colnames(EPF)<-c("Months","Energy Patttern Factor")
  EPF
}
