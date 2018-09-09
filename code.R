# TAKE HOME ASSESSMENT 2018
####################################
Information <- read.csv("group_50.csv")
sink("useful statistics.txt")

##Replacing "-1" by NA
Information$Ave[Information$Ave==-1] <- NA
Information$Year[Information$Year==-1] <- NA
Information$Month[Information$Month==-1] <- NA
Information$Site_No[Information$Site_No==-1] <- NA
Information$No_Readings[Information$No_Readings==-1] <- NA

##Split Information into two data objects by weather station
Eelde <- Information[Information$Site_No==7,]
Beek <- Information[Information$Site_No==13,]

##################################
##    Task 1: Describe data     ##
##################################

##observation about Beek##################
cat("The summary statistics for the monthly average wind speed in Beek are\n")
cat(summary(Beek$Ave),"\n")
##for convienience, we remove rows with missing wind speed values
complete_Beek <- Beek[complete.cases(Beek$Ave),] 
#max and min wind speed throughout these years
max_Beek <- complete_Beek[complete_Beek$Ave==max(complete_Beek$Ave),]
min_Beek <- complete_Beek[complete_Beek$Ave==min(complete_Beek$Ave),]
cat("The maximum monthly wind speed in Beek from 1950 to 2003 is",max_Beek$Ave,"m/s, in ",month.name[max_Beek$Month],max_Beek$Year,"\n")
cat("The minimum monthly wind speed in Beek from 1950 to 2003 is",min_Beek$Ave,"m/s, in ",month.name[min_Beek$Month],min_Beek$Year,"\n")

# How monthly wind speed changes for each year
plot(Beek[Beek$Year==1950,][,c(2,5)],col="blue",type="l",ylim = c(min_Beek$Ave,max_Beek$Ave),main="The monthly average wind speed in Beek from 1950 to 2003\nEach line represents one year",cex.main=0.9,ylab = "Average wind speed(m/s)")
for (i in 1951:2003 ){
    lines(Beek[Beek$Year==i,]$Month,Beek[Beek$Year==i,]$Ave,col="blue")
}
#compare yearly wind speed in Beek
#remove unrepresentative data (i.e.the data in 2003, as there is only one month which is not representative enough)
complete_Beek2 <- complete_Beek[-length(complete_Beek$Year),] 
year_Beek <- 1961:2002
Ave_mean_Beek <- vector(length=42)
for(i in 1:42){
    Ave_mean_Beek[i] <- mean(complete_Beek2[complete_Beek2$Year==1960+i,]$Ave)
}
Yearlymean_Beek <- data.frame(year = year_Beek,Ave_mean = Ave_mean_Beek)
Yearlymean_Beek_max <- Yearlymean_Beek[max(Yearlymean_Beek$Ave_mean)==Yearlymean_Beek$Ave_mean,]
Yearlymean_Beek_min <- Yearlymean_Beek[min(Yearlymean_Beek$Ave_mean)==Yearlymean_Beek$Ave_mean,]
cat("The maximum yearly average wind speed in Beek is",Yearlymean_Beek_max$Ave_mean,"m/s, in ",Yearlymean_Beek_max$year,"\n","The minimum yearly average wind speed in Beek is",Yearlymean_Beek_min$Ave_mean," m/s, in ",Yearlymean_Beek_min$year,"\n")
cat("The summary statistics for the yearly average wind speed in Beek are\n")
cat(summary(Yearlymean_Beek$Ave_mean),"\n")


##observation about Eelde########################
cat("The summary statistics for the monthly average wind speed in Eelde are\n")
cat(summary(Eelde$Ave),"\n")
##for convienience, we remove rows with missing wind speed values
complete_Eelde <- Eelde[complete.cases(Eelde$Ave),] 
#max and min wind speed
max_Eelde <- complete_Eelde[complete_Eelde$Ave==max(complete_Eelde$Ave),]
min_Eelde <- complete_Eelde[complete_Eelde$Ave==min(complete_Eelde$Ave),]
cat("The maximum monthly wind speed in Eelde from 1950 to 2003 is",max_Eelde$Ave,"m/s, in ",month.name[max_Eelde$Month],max_Eelde$Year,"\n")
cat("The minimum monthly wind speed in Eelde from 1950 to 2003 is",min_Eelde$Ave,"m/s, in ",month.name[min_Eelde$Month],min_Eelde$Year,"\n")

# How monthly wind speed changes for each year
plot(Eelde[Eelde$Year==1950,][,c(2,5)],col="blue",type="l",ylim = c(min_Eelde$Ave,max_Eelde$Ave),main="The monthly average wind speed in Eelde from 1950 to 2003\nEach line represents one year",cex.main=0.9,ylab = "Average wind speed(m/s)")
for (i in 1951:2003 ){
    lines(Eelde[Eelde$Year==i,]$Month,Eelde[Eelde$Year==i,]$Ave,col="red")
}
#compare yearly wind speed in Eelde
complete_Eelde2 <- complete_Eelde[-length(complete_Eelde$Year),]#remove unrepresentative data (i.e.the data in 2003,as there is only one month which is not representative enough)
year_Eelde <- 1961:2002
Ave_mean_Eelde <- vector(length=42)
for(i in 1:42){
    Ave_mean_Eelde[i] <- mean(complete_Eelde2[complete_Eelde2$Year==1960+i,]$Ave)
}
Yearlymean_Eelde <- data.frame(year = year_Eelde,Ave_mean = Ave_mean_Eelde)
Yearlymean_Eelde_max <- Yearlymean_Eelde[max(Yearlymean_Eelde$Ave_mean)==Yearlymean_Eelde$Ave_mean,]
Yearlymean_Eelde_min <- Yearlymean_Eelde[min(Yearlymean_Eelde$Ave_mean)==Yearlymean_Eelde$Ave_mean,]
cat("The maximum mean of yearly wind speed in Eelde is",Yearlymean_Eelde_max$Ave_mean,"m/s, in ",Yearlymean_Eelde_max$year,"\n","The minimum mean of yearly wind speed in Eelde is",Yearlymean_Eelde_min$Ave_mean," m/s, in ",Yearlymean_Eelde_min$year,"\n")
cat("The summary statistics for the yearly average wind speed in Eelde are\n")
cat(summary(Yearlymean_Eelde$Ave_mean),"\n")



##compare Beek and Eelde##############################
#Yearly average wind speed in Beek and Eelde(Boxplot)
combineData <- data.frame(Yearlymean_Beek=Yearlymean_Beek$Ave_mean,Yearlymean_Eelde=Yearlymean_Eelde$Ave_mean)
boxplot(combineData,main="Comparing the spread of yearly average wind speed in Beek and Eelde",xlab = "Ave wind speed(m/s)",cex.main=0.8,horizontal = TRUE,cex.axis=0.9)
#Yearly average wind speed in Beek and Eelde(time seires)
plot(Yearlymean_Beek,type="l",main="The yearly average wind speed in Eelde and Beek from 1950 to 2003(1961-2002)",ylab= "Yearly average wind speed(m/s)",col="red",cex.main = 0.7)
lines(Yearlymean_Eelde,col="blue")
legend(x=1980,y=4.2,legend = c("yearlymean_Beek","yearlymean_Eelde"),col=c("red","blue"),lty = c(1,1))
cat("The range of yearly wind speed from 1959-1976 in Beek is",min(Yearlymean_Beek$Ave_mean[1:16]),"to",max(Yearlymean_Beek$Ave_mean[1:16]),"\n")
cat("The range of yearly wind speed from 1959-1976 in Eelde is",min(Yearlymean_Eelde$Ave_mean[1:16]),"to",max(Yearlymean_Eelde$Ave_mean[1:16]),"\n")
cat("The range of yearly wind speed from 1977-2003 in Beek is",min(Yearlymean_Beek$Ave_mean[17:42]),"to",max(Yearlymean_Beek$Ave_mean[17:42]),"\n")
cat("The range of yearly wind speed from 1977-2003 in Eelde is",min(Yearlymean_Eelde$Ave_mean[17:42]),"to",max(Yearlymean_Eelde$Ave_mean[17:42]),"\n")

##################################
##    Task 2: Data analysis     ##
##################################

######### a. To compare average readings for 1950-1976 and 1977-2003, in Eelde ##############################
#Use Yearlymean_Eelde, which we have computed above.
first_mean <- Yearlymean_Eelde[1:16,] #1950-1976(we start from 1961 due to NAs before 1961)
second_mean <- Yearlymean_Eelde[17:42,]  #1977-2002(due to unrepresenrative value in 2003)

# Assume here two datasets are independent as they were collecetd from different time periods. 
##null hypothesis: there is no difference between the periods 1950 - 1976 and 1977-2003 in Eelde
##alternative hypothesis: there is difference between two periods in Eelde
twoperiods_test<-t.test(x=first_mean$Ave_mean,y=second_mean$Ave_mean,paired = FALSE,alternative = "two.sided",mu=0)
cat ("\nThe p-value for testing the average wind speed for two periods of time in Eelde is",round (twoperiods_test$p.value, 3),"\nThe confidence interval for testing the average wind speed for two periods of time in Eelde is [",twoperiods_test$conf.int,"]\n")
if(twoperiods_test$p.value<0.025){
cat("we reject the null hypothesis that there is no difference between the periods 1950 - 1976 and 1977-2003 in Eelde,","Hence,there is difference between two periods in Eelde\n")
if(twoperiods_test$conf.int[1]>0){cat("The average wind speed in first is estimated to be higher than that in second period\n\n")}
else{cat("The average wind speed in second period is estimated to be higher than that in first period\n\n")}
}else{
    cat("There is no significant evidence to reject the null hypothesis that there is no difference between the periods 1950 - 1976 and 1977-2003 in Eelde \n\n")}
#
#
#
#
#
############ b. To compare average readings for summer(6,7,8) and winter(12,1,2), in Beek#######################
##
Beek_summer <- Beek[Beek$Month==6|Beek$Month==7|Beek$Month==8,]
Beek_winter <- Beek[Beek$Month==12|Beek$Month==1|Beek$Month==2,]
##mainly analyse data from 1962-2002(because data are incomplete in other years)
#remove missing values
Beek_summer <- Beek_summer[complete.cases(Beek_summer$Ave),]
Beek_winter <- Beek_winter[complete.cases(Beek_winter$Ave),]
#remove unrepresentative values i.e 1961.8 and the winter in 2003
Beek_summer <- Beek_summer[-1,]
Beek_winter <- Beek_winter[-c(length(Beek_winter$Year)-1,length(Beek_winter$Year)),]
#remove site number and number of readings
Beek_summer <- Beek_summer[,-c(3,4)]
Beek_winter <- Beek_winter[,-c(3,4)]
######## Analysis about data from 1962-2002#
####summer
#average wind speed of each summer from 1962 to 2002
mean_ave_summer <- vector(length=length(unique(Beek_summer$Year)))
for(i in 1:length(unique(Beek_summer$Year))){
    mean_ave_summer[i] <- mean(Beek_summer$Ave[Beek_summer$Year==1961+i])
}
Beek_summer_mean <- data.frame(Year=unique(Beek_summer$Year),mean_ave_summer)

###winter
##average wind speed of each winter from 1962 to 2002
mean_ave_winter <- vector(length=length(Beek_winter$Ave)/3)
for(i in 0:(length(Beek_winter$Ave)/3)-1){
    mean_ave_winter[i+1] <- mean(Beek_winter$Ave[seq(1+3*i,3+3*i,1)])
}
Beek_winter_mean <- data.frame(Year=unique(Beek_winter$Year)[-1],mean_ave_winter)

###combine summer and winter 
Beek_summer_mean$mean_ave_winter <- Beek_winter_mean$mean_ave_winter
Beek_mean_seasons <- Beek_summer_mean
##Assume that two datasets are dependent because the wind speed is tested at the same station where the wind speed in summer and winter should be relative to each other
##null hypothesis: there is no difference  between the average wind speed in summer and winter
##alternative hypothesis: there is difference between the average wind speed in summer and winter
twoseasons_test <- t.test(Beek_mean_seasons$mean_ave_summer,Beek_mean_seasons$mean_ave_winter,paired = TRUE,alternative = "two.sided",mu=0)
cat ("\nThe p-value for testing the average wind speed for summer and winter is", twoseasons_test$p.value,"\nThe confidence interval for testing the average wind speed for two seasons is[",twoseasons_test$conf.int,"]\n")
if(twoseasons_test$p.value<0.025){
    cat("we reject the null hypothesis that there is no difference between the average wind speed in summer and winter.","Hence,there is difference between the average wind speed in summer and winter")
    if(twoseasons_test$estimate>0){
        cat("\nThe average wind speed in summer is estimated to be higher than that in winter\n\n")
    }
    else{cat("\nThe average wind speed in winter is estimated to be higher than that in summer\n\n")}
}else{ cat("\nThere is no significant evidence to reject the null hypothesis that there is no difference between the average wind speed in summer and winter.\n\n")}

##plot about the yearly average wind speed for both summer and winter in Beek
plot(Beek_winter_mean,type="l",main="The average summer and winter wind speed from 1962 to 2002 in Beek",ylab="mean of wind speed(m/s)",col="red",ylim=c(min(c(Beek_mean_seasons$mean_ave_summer,Beek_mean_seasons$mean_ave_winter)),max(c(Beek_mean_seasons$mean_ave_summer,Beek_mean_seasons$mean_ave_winter))),cex.main = 0.8)
lines(Beek_summer_mean,col="blue")
legend(x=1965,y=6.8,col=c("red","blue"),legend=c("winter","summer"),lty = c(1,1))
#
#
#
#
######### c. To compare average readings for Eelde and Beek over the whole period###########################
# Assume here two datasets are dependent as the wind speed in two places during the same period should be relative to each other 
##null hypothesis: there is no difference in wind speed between the locations
##alternative hypothesis: there is difference in wind speed between two locations
twolocations_test<-t.test(x=Yearlymean_Eelde$Ave_mean,y=Yearlymean_Beek$Ave_mean,paired = TRUE,alternative = "two.sided",mu=0)
cat ("\nThe p-value for testing the average wind speed for two locations is",round (twolocations_test$p.value, 3),"\nThe confidence interval for testing the average wind speed for two locations is[",twolocations_test$conf.int,"]\n")
if(twolocations_test$p.value<0.025){
    cat("we reject the null hypothesis that there is no difference between the locations.","Hence,there is difference between two locations\n")
    if(twolocations_test$estimate>0){
        cat("The average wind speed in Eelde is estimated to be higher than that in Beek\n")
    }
    else{cat("The average wind speed in Beek is estimated to be higher than that in Eelde\n")}
}else{ cat("There is no significant evidence to reject the null hypothesis that there is no difference between the average wind speed in Eelde and Beek \n")}

sink()




