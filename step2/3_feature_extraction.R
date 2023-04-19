
###############clear Rstudio environment variables ####################
rm(list=ls())
gc()

##################libraries#######################################
library(geosphere)
library(dplyr)
library(lubridate)
library(stringr)

#read  config
if(file.exists("config.yml")){
  conf <- config::get(file = "config.yml")
}else{
  conf <- config::get(file = "config_default.yml")
}


print("loading stroke cohort")
###################load stroke cohort #############################
df.stroke.cohort <- read.csv(file  = file.path(getwd(),"data/stroke_cohort.csv"))
df.stroke.cohort <- subset(df.stroke.cohort,select = -c(X))
df.stroke.cohort$admission_datetime <- df.stroke.cohort$admission_date
df.stroke.cohort$admission_date <- as.character(as.Date(df.stroke.cohort$admission_date))

################## checking for missing dates in admission, discharge and recorded date ##############
print(" Handling missing date")
df.stroke.cohort$admission_date <- coalesce(df.stroke.cohort$admission_date, 
                                            df.stroke.cohort$discharge_date, 
                                            df.stroke.cohort$recorded_date)
print(paste("Missing dates count after coalesce:", length(which(is.na(df.stroke.cohort$admission_date)))))

if(length(which(is.na(df.stroke.cohort$admission_date))) > 0){
  df.stroke.cohort <- df.stroke.cohort[-c(which(is.na(df.stroke.cohort$admission_date))),]
}

print(paste("Stroke cohort rows after removing missing dates:", nrow(df.stroke.cohort)))
df.stroke.cohort$admission_date <- as.character(as.Date(df.stroke.cohort$admission_date))

#############################filter admission date between 2015 and 2021################

df.stroke.cohort <- df.stroke.cohort[c(which(as.Date(df.stroke.cohort$admission_date) >= '2015-01-01' & as.Date(df.stroke.cohort$admission_date) <= '2021-12-31' )),]

###################extract latitude and longitude ###################
print("extracting lat and long based on PLZ")
df.plz <- read.csv(file  = file.path(getwd(), "data/plz.csv"))
df.plz$plz <- str_pad(df.plz$plz, 5, pad = "0")
df.stroke.cohort$patient_zip <- as.character(df.stroke.cohort$patient_zip)

#replace rows with NA as postal code with hospital location
df.stroke.cohort$patient_zip[which(is.na(df.stroke.cohort$patient_zip))]<- conf$PLZ
#replace rows that have postal code less than 5 digits with hospital location 
df.stroke.cohort$patient_zip[which(nchar(df.stroke.cohort$patient_zip)!=5)] <- conf$PLZ

df.stroke.cohort <- left_join(df.stroke.cohort,df.plz,c("patient_zip" = "plz"))
if(length(which(is.na(df.stroke.cohort$latitude) | is.na(df.stroke.cohort$longitude) ))>0){
  df.stroke.cohort <- df.stroke.cohort[-c(which(is.na(df.stroke.cohort$latitude) | is.na(df.stroke.cohort$longitude) )),]
  
}
############################stroke daily counts#################################
print("stroke daily counts - three different outputs")
count.info <- df.stroke.cohort%>%
  group_by(Date = as.Date(admission_date))%>%
  summarise(total_count = length(unique(encounter_id))
            ,count_I63 = length(unique(encounter_id[ c(which(startsWith(icd_family,"I63")))]))
            ,count_I61 = length(unique(encounter_id[ c(which(startsWith(icd_family,"I61")))]))
            ,count_I60 = length(unique(encounter_id[ c(which(startsWith(icd_family,"I60")))]))
            ,Ischemic_count = count_I63
            ,Bleeding_count = count_I61 + count_I60
            
  )
count.info.all <- data.table::data.table()
count.info.all$Date <- seq(ymd(min(count.info$Date)),ymd(max(count.info$Date)),by='days')
count.info.all <- left_join(count.info.all,count.info,"Date")
count.info.all[is.na(count.info.all)] <- 0

###################load appropriate conf files#############################
variables <- config::get(file = file.path(getwd(),"conf/WeatherVariables.yml"))
sites <- config::get(file = file.path(getwd(),"conf/sites.yml"))

###############load the weather data downloaded from DWD  and meta index dataframe#######
print("Load weather data and find the appropriate stations for patients")
load(file = file.path(getwd(),"data/weather_data_all_updated.RData"))
weather.data$MESS_DATUM =as.character(weather.data$MESS_DATUM)
load(file = file.path(getwd(),"data/metaIndex.RData"))

#################calculate the distance bin and angle based on clinic location and weather station ######
metaIndex$distance_from_clinic <- apply(X = data.frame(1:nrow(metaIndex)),MARGIN = 1,FUN = function(X){(distHaversine(p1 = c(sites$UMM$clinic.lon,sites$UMM$clinic.lat), p2  = c(metaIndex$lon[X],metaIndex$lat[X]))/1000)})
metaIndex$distance_bin<- cut(metaIndex$distance_from_clinic, breaks=c(0,10,  50, 100, 150, 200, 300,400,500,max(metaIndex$distance_from_clinic)))

metaIndex$angle <- apply(X = data.frame(1:nrow(metaIndex)),MARGIN = 1,FUN = function(X){bearing(p1 = c(sites$UMM$clinic.lat,sites$UMM$clinic.lon),p2 = c(metaIndex$lat[X],metaIndex$lon[X]))})
metaIndex$angle<-(metaIndex$angle + 360) %% 360

arr_4=c("N","NE","E","SE","S","SW","W","NW")
metaIndex$direction <-arr_4[ceiling(metaIndex$angle/45)]

#########################################station which contains all the variables##################################
weather.data <- subset(weather.data,select = -c(snow_pack_height , fresh_snow_depth,precipitation_form,
  precipitation_form,snow_depth,min_precipitation_height ))

#weather.data <- na.locf(weather.data)
station.agg.all.vars <- metaIndex%>%
  group_by(Stations_id)%>%
  mutate(unique_variable_count = length(unique(res_var)))%>%
  filter(unique_variable_count == length(variables))%>%
  select(Stations_id,lat,lon)%>%
  distinct()

########find closest station from the list of station that has all the variables
df.stroke.cohort$closest.from.plz <- station.agg.all.vars$Stations_id[as.data.frame(RANN::nn2(station.agg.all.vars[,c("lat","lon")] ,df.stroke.cohort[,c("latitude","longitude")], k=1))$nn.idx]
df.stroke.cohort$distance <- apply(X = data.frame(1:nrow(df.stroke.cohort)),MARGIN = 1,FUN = function(X){(distHaversine(p1 = c(metaIndex[which(metaIndex$Stations_id ==df.stroke.cohort$closest.from.plz[X])[1],"lon"]
                                                                                                                                       ,metaIndex[which(metaIndex$Stations_id ==df.stroke.cohort$closest.from.plz[X])[1],"lat"])                                                                                                                             , p2  = c(df.stroke.cohort$longitude[X],df.stroke.cohort$latitude[X]))/1000)})
##############################fill for zero dates based on clinic location################################

print("fill for zero dates based on clinic location")
type_1 <- df.stroke.cohort[,c("admission_date","closest.from.plz")]
closest.to.site <- station.agg.all.vars$Stations_id[as.data.frame(RANN::nn2(station.agg.all.vars[,c("lat","lon")] ,data.frame(sites$UMM$clinic.lat,sites$UMM$clinic.lon), k=1))$nn.idx]
zero_dates <- count.info.all$Date[c(which(count.info.all$total_count == 0))]
type_1_zero <- data.table::data.table()
type_1_zero$admission_date <- zero_dates
type_1_zero$admission_date <- as.character(type_1_zero$admission_date)
type_1_zero$closest.from.plz <- closest.to.site

type_1 <- rbind(type_1,as.data.frame(type_1_zero))
type_1 <- type_1[c(order(type_1$admission_date)),]

####################################adding weather#############################################
  print("adding weather and lagged values")
  #####################add current days weather based on the closest station############
  type_1 <- left_join(type_1,weather.data,by = c("admission_date" = "MESS_DATUM" , "closest.from.plz" ="STATIONS_ID"))
  #####################add PT temperature based on site location - station id from sites.yml############
  # read PT temp weather data
  PT.data<-  read.csv(file  = file.path(getwd(),"data/PT_Dwd_Agg.csv"))
  type_1$PT.station <- sites[[conf$site]]$PT.Station.id
  type_1 <- left_join(type_1,PT.data,by = c("admission_date" = "MESS_DATUM" , "PT.station" ="STATIONS_ID"))
  type_1[sapply(type_1, is.infinite)] <- NA
 ################## add lagged weather data based on clinic site ################
  lag_names  <- paste("lag_",seq(1,7),sep = "")
  old_name <- names(weather.data)
  for(i in 1:length(lag_names)){
    type_1$closest.to.site <- closest.to.site
    type_1$admission_date_lag <- as.character(as.Date(type_1$admission_date) - i)
    names(weather.data) <- c(names(weather.data)[1:2],paste("lag_",i,old_name[3:ncol(weather.data)],sep = ""))
    type_1 <- left_join(type_1,weather.data,by = c("admission_date_lag" = "MESS_DATUM" , "closest.to.site" ="STATIONS_ID"))
    #print(i)
  }
  names(weather.data) <- old_name
  
  
#####################Transform patient level to different resolution#################################
 type_1 <- subset(type_1,select = -c(closest.from.plz,PT.station,closest.to.site,admission_date_lag))


##1.###################################Daily################################################ 
  print("Creating daily level data")
  
  daily_level <- type_1%>%
    group_by(admission_date)%>%
    summarise_all(funs(mean), na.rm = TRUE)
  
  daily_level$day_of_month <- day(daily_level$admission_date)
  daily_level$day_of_year <- yday(daily_level$admission_date)
  daily_level$month <- month(daily_level$admission_date)
  daily_level$wday <- as.factor(wday(daily_level$admission_date,week_start =1)) 
  
  
  daily_level$year <- year(daily_level$admission_date)
  daily_level$week_num <-as.integer(format(as.Date(daily_level$admission_date),"%W"))
  
  
  
  week.agg <- count.info.all%>%
                    group_by(year = year(count.info.all$Date)
                            # ,week_num = week(count.info.all$Date)
                             ,week_num = as.integer(format(as.Date(count.info.all$Date),"%W"))
                             )%>%
                    summarize(mean_prior_week_total = mean(total_count,na.rm=TRUE)
                           ,median_prior_week_total = median(total_count,na.rm=TRUE)
                           ,mean_prior_week_ischemic = mean(Ischemic_count,na.rm=TRUE)
                           ,median_prior_week_ischemic = median(Ischemic_count,na.rm=TRUE)
                           ,mean_prior_week_bleeding = mean(Bleeding_count,na.rm=TRUE)
                           ,median_prior_week_bleeding = median(Bleeding_count,na.rm=TRUE)
                           )
  
  
  #join to get mean prior week and median prior week variables
  week.agg$week_num<- week.agg$week_num + 1
  daily_level <- left_join(daily_level,week.agg,c("year" = "year", "week_num"="week_num"))
  week.agg$week_num<- week.agg$week_num - 1
  
  #join to get output variable
  count.info.all$Date <- as.character(count.info.all$Date)
  daily_level <- right_join(daily_level,count.info.all,c("admission_date" = "Date"))
  
  #drop week number which is equal to minimum and min year
  # daily_level<- daily_level[-c(which(daily_level$week_num == min(daily_level$week_num) & 
  #                                      daily_level$year == min(daily_level$year))),] 
  
  
  
  write.csv(daily_level,file = file.path(getwd(),"data/daily_level.csv"),row.names = FALSE)

##2.###################################2 day################################################
  print("Creating two-day level data")
  two_day <- daily_level
  two_day <- subset(two_day,select = -c(day_of_month,day_of_year,month,wday,year,week_num))
  two_day$daycounter<- c(0, rep(1:(nrow(two_day)- 1)%/%2)) + 1
  
  two_day <- subset(two_day,select = -c(admission_date))
  ## check how to aggregate mean prior week counts
  two_day_weather <- subset(two_day,select = -c(total_count,count_I63,count_I61,count_I60
                                                ,Ischemic_count,Bleeding_count))%>%
                group_by(daycounter)%>%
                summarise_all(funs(mean), na.rm = TRUE)
  
  two_day_counts<-  subset(two_day,select = c(daycounter,total_count,count_I63,count_I61,count_I60,Ischemic_count,Bleeding_count))%>%
  group_by(daycounter)%>%
  summarise_all(funs(sum), na.rm = TRUE)
  
  two_day <- left_join(two_day_weather,two_day_counts,"daycounter")
  rm(two_day_counts,two_day_weather)
  write.csv(two_day,file = file.path(getwd(),"data/two_day.csv"),row.names = FALSE)

#####################################site-based################################################
  site.based.df <- count.info.all
  site.based.df$closest.to.site <- closest.to.site
  site.based.df <- left_join(site.based.df,weather.data,by = c("Date" = "MESS_DATUM" , "closest.to.site" ="STATIONS_ID"))
  site.based.df$PT.station <- sites[[conf$site]]$PT.Station.id
  site.based.df <- left_join(site.based.df,PT.data,by = c("Date" = "MESS_DATUM" , "PT.station" ="STATIONS_ID"))
  site.based.df[sapply(site.based.df, is.infinite)] <- NA
  
  ##3.###################################Weekly################################################  
  print("Creating weekly level data")
  weekly <- site.based.df
  weekly$year_weekNum <- paste((strftime(weekly$Date, format = "%Y")),(strftime(weekly$Date, format = "%W")),sep = "_")
  #weekly <- subset(weekly,select = -c(closest.to.site,PT.station,Date,date_lag))
  weekly <- subset(weekly,select = -c(closest.to.site,PT.station,Date))
  
  
  weekly_weather <- subset(weekly,select = -c(total_count,count_I63,count_I61,count_I60,Ischemic_count,Bleeding_count))%>%
    group_by(year_weekNum)%>%
    summarise_all(funs(mean), na.rm = TRUE)
  
  weekly_count <- subset(weekly,select = c(year_weekNum,total_count,count_I63,count_I61,count_I60,Ischemic_count,Bleeding_count))%>%
    group_by(year_weekNum)%>%
    summarise_all(funs(sum), na.rm = TRUE)
  
  weekly <- left_join(weekly_weather,weekly_count,"year_weekNum")
  rm(weekly_count,weekly_weather)
  write.csv(weekly,file = file.path(getwd(),"data/weekly.csv"),row.names = FALSE)

##4.###################################Monthly################################################
  print("Creating monthly level data")
  monthly <- site.based.df
  monthly$year_month <- paste((strftime(monthly$Date, format = "%Y")),(strftime(monthly$Date, format = "%m")),sep = "_")
  #monthly <- subset(monthly,select = -c(closest.to.site,Date,date_lag))
  monthly <- subset(monthly,select = -c(closest.to.site,Date))
  
  
  monthly_weather <- subset(monthly,select = -c(total_count,count_I63,count_I61,count_I60,Ischemic_count,Bleeding_count))%>%
    group_by(year_month)%>%
    summarise_all(funs(mean), na.rm = TRUE)
  
  monthly_count <- subset(monthly,select = c(year_month,total_count,count_I63,count_I61,count_I60,Ischemic_count,Bleeding_count))%>%
    group_by(year_month)%>%
    summarise_all(funs(sum), na.rm = TRUE)
  
  monthly <- left_join(monthly_weather,monthly_count,"year_month")
  rm(monthly_count,monthly_weather)
  write.csv(monthly,file = file.path(getwd(),"data/monthly.csv"),row.names = FALSE)
  print("Feature extraction done")
  #########################################################################################################