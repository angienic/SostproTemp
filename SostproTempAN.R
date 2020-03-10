#My Edits to Claire's script, with my file locations, etc
#If all files are not in the same folder as R project...
Water<-data.frame(read.csv("C:\\Users\\nicangie\\Documents\\Ang\\Projects_and_Samples\\SOSTPRO\\Sostpro_2018_Temp\\HoboLoggers\\HoboLoggers\\Coastal\\6Coast_Water.csv",
                         header=F,stringsAsFactors=FALSE,
                         colClasses=c(rep("character",4),rep("NULL",5)),
                         na.string=c("","null","NaN","X")))

library(tidyverse)
library(dplyr)
library(reshape2)
library(lubridate)
library(zoo)
library(knitr)
#This works if all .csv files are in same folder as R project. 
#11Coast Sites: 6,14,20,23,24,"29"=>27,East,Griffiths,G,Mother,Spring. 
#12Interior sites: 9,11,15,20,21,22,23,25,29,R1,R2,R3.In LoggerDates: R1,R2,R3
#When processing Coast27(correct name instead of 29C)add Water = Water[,-c(5,6,7)], to remove extra columns

###Read in data and get rows and columns sorted, change the file name and csv output manually
site.name <- "6Coast"

#Water
Water<-data.frame(read.csv("6Coast_Water.csv",
                           header=F,stringsAsFactors=FALSE,
                           colClasses=c(rep("character",4),rep("NULL",5)),
                           na.string=c("","null","NaN","X")))
colnames(Water) <- c("Record", "DateTime", "Temp.C", "Light.Lux")
Water = Water[-2,]
Water = Water[-1,]

Water$DateTime <- strptime(Water$DateTime, "%m/%d/%y %I:%M:%S %p")
Water$DateTime <- as.POSIXct(Water$DateTime, origin="1970-01-01", tz="GMT")
Water$Temp.C <- as.numeric(as.character(Water$Temp.C))
Water$Light.Lux <- as.numeric(as.character(Water$Light.Lux))

#Subset by deployment dates
dates <-read.csv("LoggerDates.csv")
dates$Start <- strptime(dates$Start, "%m/%d/%Y")
dates$End <- strptime(dates$End, "%m/%d/%Y")
dates$Start <- as.POSIXct(dates$Start, origin="1970-01-01", tz="GMT")
dates$End <- as.POSIXct(dates$End, origin="1970-01-01", tz="GMT")

site.date <- dates %>%
  filter(Site == site.name) %>%
  head

int <- interval(site.date$Start, site.date$End)

Water <- Water[Water$DateTime %within% int,]

#Replace light values = 0 (night time) with NA
Water$Light.Lux[Water$Light.Lux==0] <- NA

#Get summary of data
summary(Water)

#By day - show range in hourly variables by day.Function 'cut' divides X into intervals
Water$Day <- cut(Water$DateTime, breaks = "day")

###### Calculate daily max temp
#original ex:WaterMean<-aggregate(cbind(Temp.C, Light.Lux)~Day, Water, mean)

Daily<- group_by(Water,Day) %>% 
        summarize(DayT_Mean = mean(Temp.C),
        DayT_Max = max(Temp.C))

## Add Running means to df (zoo pckg) John asked for 7 days. Gives 38 values
Daily$Tmax_7daymean<-rollmean(Daily$DayT_Max, 7,na.pad = T, align="right") #align =center gives NAs in first 3 and last 3 values, align left=, at the end and so on.

#Subset by new logger-overlapping dates:
#First convert Day to a Date again
Daily$Day <- as.Date(Daily$Day, format = "%Y-%m-%d")

#Continue here, solving subsetting issues this way or creating new Date file with the correct interval
Daily[Daily$Day >= "2018-06-22" & Daily$Day <= "2018-07-17",]
Daily <- Daily %>%
  filter(Day >= as.Date('2018-06-22') & Day <= as.Date('2018-07-17')) #note these intervals are for COAST only

#Add a column of site name, just in case for later merging
Daily$Site<-site.name

#Save as file
setwd("C:\\Users\\nicangie\\Documents\\Ang\\Projects_and_Samples\\SOSTPRO\\Sostpro_2018_Temp\\DailyTMaxOutput")
write.csv(Daily, "6Coast_TMax.csv",row.names = F)
#Remember to set working directory back!
setwd("C:\\Users\\nicangie\\Documents\\Ang\\Projects_and_Samples\\SOSTPRO\\Sostpro_2018_Temp")

#Repeat lines 15-82 for other sites just adjusting file name.


####################### END   ###################

###################################################################
#####################################    INTERIOR STARTS HERE   
###################################################################


#### For interior Sites, make sure change subsetting dates:
#Continue here, solving subsetting issues this way or creating new Date file with the correct interval
Daily <- Daily %>%
  filter(Day >= as.Date('2018-07-06') & Day <= as.Date('2018-08-07')) #








#### Below optional older bits of code in no order, to delete

#Consider
#For merging all dfs at the end
#https://www.r-bloggers.com/how-to-perform-merges-joins-on-two-or-more-data-frames-with-base-r-tidyverse-and-data-table/
#For file locating
library(here)
install.packages("here")
here("data", "file_i_want.csv")

###### Calculate daily max temp
#older, quick and dirty, can't do much with it:
DailyMax<- with(Water, tapply(Temp.C, Day, max))

#Other tried Subsetting methods 
Daily[Daily$Day >= "2018-07-06" & Daily$Day <= "2018-08-07",]#didn't work

##Quick view
h20daymax <- qplot(x=Day, y=Temp.C,
                   data=WaterDayMax, na.rm=TRUE,
                   main="Max.Daily Water Temp",
                   xlab="Date", ylab="Temp (°C)")
h20daymax
ggplot(WaterDayMax, aes(x=Day, y=Temp.C)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)


### Daily mean & max temp
#2step older option, do and rename
DayT_Mean<-aggregate(Temp.C~Day, Water, mean)
DayT_Mean$Mean<-DayT_Mean$Temp.C #renaming result colum
#Option all in one step
DayT_Mean <- setNames(aggregate(Temp.C~Day, Water, mean), c("Date", "TMean"))
### Daily Max Temp
DayT_Max<-setNames(aggregate(Temp.C~Day, Water, max),c("Date","TMax"))

#Renaming column base R version
names(WaterDayMax)[names(WaterDayMax) == "Temp.C"] <- "T.Max"

##Short version of roll means
DayMax7Mean<-rollmean(DayMax$DayMax, 7)

#Renaming columns
WaterDayMax %>% 
  rename(
    DayTMax = Temp.C,
  ) #prints it

### SKIP if all files in same folder 

#Note:colClasses=c(rep("character",4),rep("NULL",5)), tells R to assign first 4 columns as characters, and remove last 5
#In case logger files are in another folder use (notice double slassh!!):
Water<-data.frame(read.csv("C:\\Users\\nicangie\\Documents\\Ang\\Projects and Samples\\SOSTPRO\\Sostpro 2018 Temp\\HoboLoggers\\HoboLoggers\\Coastal\\6Coast_Air.csv",
                           header=F,stringsAsFactors=FALSE,
                           colClasses=c(rep("character",4),rep("NULL",5)),
                           na.string=c("","null","NaN","X")))
colnames(Water) <- c("Record", "DateTime", "Temp.C", "Light.Lux")
Water = Water[-2,]
Water = Water[-1,] #or  Water = Water[-c(2,1),] to remove header rows

#Claire's: calculate total Mean, Min, Max, and CV for all four time series and send to new file.
WaterTemp_Mean <- mean(Water$Temp.C)
WaterTemp_Min <- min(Water$Temp.C)
WaterTemp_Max <- max(Water$Temp.C)
WaterTemp_SD <- sd(Water$Temp.C)
WaterTemp_CV <- (WaterTemp_SD/WaterTemp_Mean)

WaterLight_Mean <- mean(Water$Light.Lux, na.rm=TRUE)
WaterLight_Min <- min(Water$Light.Lux, na.rm=TRUE)
WaterLight_Max <- max(Water$Light.Lux, na.rm=TRUE)
WaterLight_SD <- sd(Water$Light.Lux, na.rm=TRUE)
WaterLight_CV <- (WaterLight_SD/WaterLight_Mean)

DescStats <- cbind(site.name, AirTemp_Mean, AirTemp_Min, AirTemp_Max, AirTemp_SD, AirTemp_CV,
                   AirLight_Mean, AirLight_Min, AirLight_Max, AirLight_SD, AirLight_CV,
                   WaterTemp_Mean, WaterTemp_Min, WaterTemp_Max, WaterTemp_SD, WaterTemp_CV,
                   WaterLight_Mean, WaterLight_Min, WaterLight_Max, WaterLight_SD, WaterLight_CV)  

###Change name of csv file to match site
write.csv(DescStats, file="6Coast_HoboSummary.csv")


####Trying to get descriptive stats from all the .csv files I created with outputs:
#https://www.earthdatascience.org/courses/earth-analytics/automate-science-workflows/use-apply-functions-for-efficient-code-r/

#Try
setwd("C:\\Users\\nicangie\\Documents\\Ang\\Projects and Samples\\SOSTPRO\\Sostpro 2018 Temp\\DailyTMaxOutput")
Testdf1<-read.csv("6Coast_TMax.csv")
Testdf1$Day <- as.Date(Testdf1$Day, format = "%Y-%m-%d")
#If they're still csv files
# create an object with the directory name
the_dir <- "C:/Users/nicangie/Documents/Ang/Projects and Samples/SOSTPRO/Sostpro 2018 Temp/DailyTMaxOutput"

all_Tmax_files <- list.files(the_dir, pattern = "*.csv",
                               full.names = TRUE)

lapply(all_precip_files,
       FUN = summarize_data,
       the_dir = the_dir_ex)
## list()
#summarize_data <- function(6Coast_TMax.csv, the_dir) {
  # open the data, fix the date and add a new column
  Coast6df <- read.csv("6Coast_TMax.csv", header = TRUE, na.string=c("","null","NaN","X")) %>%
    mutate(DATE = as.POSIXct(Day, tz = "GMT", format = "%Y-%m-%d"),
                     Type = as.factor("Clearcut")) #optional, if haven't converted to date format before

   Coast6df_Mean <- mean(Coast6df$DayT_Mean)
    Coast6df_SD <- sd(Coast6df$DayT_Mean)
    Coast6Df_CV <- (Coast6df_SD/Coast6df_Mean)    
    
    
    
#Or If they were already dfs, Make a list of data frames then use lapply to apply the function to them all.go this way (https://stackoverflow.com/questions/22002838/same-function-over-multiple-data-frames-in-r)
df.list <- list(df1,df2,...)
res <- lapply(df.list, function(x) rowMeans(subset(x, select = c(start, stop)), na.rm = TRUE))
# to keep the original data.frame also
res <- lapply(df.list, function(x) cbind(x,"rowmean"=rowMeans(subset(x, select = c(start, stop)), na.rm = TRUE)))

###

