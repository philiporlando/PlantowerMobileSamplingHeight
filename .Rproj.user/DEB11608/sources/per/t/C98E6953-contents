# created by Philip Orlando @ Sustainable Atmopsheres Research Lab
# PI Dr. Linda George
# 2018-05-04
# May the 4th be with you!

# Pilot run of our mobile PM sensors at various sampling heights (DustTrak included)


# load the necessary packages
if (!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}

p_load(readr
       ,ggplot2
       ,plyr
       ,dplyr
       ,broom
       ,reshape2
       ,tidyr
       ,stringr
       ,magrittr
       ,rlist
       ,gridExtra
       ,tidyquant
)


# create unanimous time resolution for all data
time_resolution <- "1 sec"
start_time <- "2018-05-04 15:50:00"

# creating a custon not-in function
'%!in%' <- function(x,y)!('%in%'(x,y))


# reads in on TSI DustTrak DRX 8533 file:

# reads in one teensy_pm file:
#fpath <- "./data/teensy_pm/20180504_1.txt"
read_teensy <- function(fpath) {
  x <- read.csv(fpath, header = TRUE, stringsAsFactors = FALSE) 
  x$date <- as.POSIXct(x$date, format = "%Y-%m-%d %H:%M:%S", tz = "US/Pacific")
  # this removes the dupes in the raw data too
  x <- x %>%
    group_by(date = cut(date, breaks = time_resolution)) %>%
    dplyr::summarise_all(funs(mean))
  
  # grab ID from filename and assign to new variable
  file_name <- basename(fpath)
  
  # pick up here later!
  id <- substr(file_name, 10, 10)
  x$id <- paste0("Teensy_", id)
  x$date <- as.POSIXct(x$date, format = "%Y-%m-%d %H:%M:%S", tz = "US/Pacific")
  
  x <- x %>% gather(pollutant, value, -c(date, id))
  return(x)
}


## reads in one TSI DustTrak DRX 8533 file:
read_dtrak<-function(fpath){
  sdate<-read.csv(fpath, header=FALSE, nrow=1, skip=7)
  stime <-read.csv(fpath, header = FALSE, nrow=1, skip=6)  
  startDate<-strptime(paste(sdate$V2, stime$V2), "%m/%d/%Y %H:%M:%S", tz="US/Pacific")
  x<-read.csv(fpath, skip=37, stringsAsFactors = FALSE, header = FALSE)
  names(x)<-c("elapsedtime","pm1","pm2.5","pm4","pm10","total","alarms","errors")
  x$date<-x$elapsedtime+startDate
  x$date <- as.POSIXct(x$date, format = "%m/%d/%Y %H:%M:%S", tz = "US/Pacific")
  x<-x[,-c(1,7,8)]
  x<-x[,c(6,1,2,3,4,5)]
  x$pm1 <- x$pm1 * 1000
  x$pm2.5 <- x$pm2.5 * 1000
  x$pm4 <- x$pm4 * 1000
  x$pm10 <- x$pm10 * 1000
  x$total <- x$total * 1000
  x <- x %>% 
    group_by(date = cut(date, breaks = time_resolution)) %>%
    dplyr::summarize(pm1 = mean(pm1), pm2.5 = mean(pm2.5), pm4 = mean(pm4), 
                     pm10 = mean(pm10), total = mean(total))
  ## manage time zones (necessary when using teensy sensors!)
  x$id <- "DustTrak"
  x$date <- as.POSIXct(x$date, format = "%Y-%m-%d %H:%M:%S", tz = "US/Pacific")
  
  x <- x %>% gather(pollutant, value, -c(date, id))
  
  return(x)
  
}


#fpath <- "./data/gps/20180504_gps.csv"
read_gps <- function(fpath) {
  x <- read.csv(fpath)
  x$datetime <- strptime(paste(x$LOCAL.DATE, x$LOCAL.TIME)
                     ,"%Y/%m/%d %H:%M:%S"
                     ,tz = "US/Pacific")
  x <- x %>% select(date = datetime
                    ,lat = LATITUDE
                    ,lon = LONGITUDE
                    ,speed = SPEED
                    ,elevation = HEIGHT)

  x$date <- as.POSIXct(x$date)
  
  return(x)
  
}



# define relative file paths
teensy_path <- "./data/teensy_pm/"
dtrak_path <- "./data/dusttrak/"
gps_path <- "./data/gps/"

# pulling in a list of files within each directory
teensy_files <- list.files(path = teensy_path
                           ,pattern = "\\.TXT$"
                           ,all.files = FALSE
                           ,full.names = TRUE
                           ,ignore.case = FALSE)

# pulling in a list of files within each directory
dtrak_files <- list.files(path = dtrak_path, pattern = "\\.csv$"
                          ,all.files = FALSE
                          ,full.names = TRUE
                          ,ignore.case = FALSE)


gps_files <- list.files(path = gps_path
                        ,pattern = "\\.csv$"
                        ,all.files = FALSE
                        ,full.names=TRUE
                        ,ignore.case = FALSE)

# reading in our data
dtrak <- ldply(dtrak_files, read_dtrak)
teensy <- ldply(teensy_files, read_teensy)
gps <- ldply(gps_files, read_gps)

# joining all the data into one tidy dataframe
df <- bind_rows(dtrak, teensy) %>% inner_join(gps, by = "date")

# explore time series data before making maps
df %>% ggplot(aes(x = date, y = value, color = as.factor(id))) + 
  geom_jitter(alpha = 0.2) + 
  facet_wrap(~pollutant, scales = "free_y")
  

df %>% filter(pollutant %!in% c("p_0_3_um"
                           ,"p_0_5_um"
                           ,"p_1_0_um"
                           ,"p_2_5_um"
                           ,"p_5_0_um"
                           ,"p_10_0_um")) %>%
  ggplot(aes(x = date, y = value, color = as.factor(pollutant))) + 
  geom_jitter(alpha = 0.2) + 
  facet_wrap(~id, scales = "free_y")




df %>% filter(date >= as.POSIXct(start_time, format = "%Y-%m-%d %H:%M:%S")) %>% 
                filter(pollutant %in% c("pm2_5_atm"
                               ,"pm2.5")) %>%
  # exclude shortlived spikes and zoom in on overall trends 
  filter(value <=300) %>%
  
  # generate our ts plot
  ggplot(aes(x = date, y = value, color = as.factor(id))) + 
  #geom_point(alpha = 0.1, size = 0.9) + 
  #geom_smooth(se = FALSE) + 
  geom_ma(ma_fun = SMA
          ,n = 1
          ,size = 1
          ,aes(linetype = "solid"
               ,alpha = 0.1)) +
  theme_bw()


df %>% filter(pollutant %in% c("pm2.5"
                               ,"pm2_5_atm")) %>%
  ggplot(aes(x = date, y = value, color = as.factor(id))) + 
  geom_jitter(alpha = 0.2) +
  theme_bw()


df %>% filter(pollutant %in% c("pm2.5"
                               ,"pm2_5_atm")) %>%
  filter(id %in% c("DustTrak"
                   ,"Teensy_7"
                   ,"Teensy_8")) %>%
  ggplot(aes(x = date, y = value, color = as.factor(id))) + 
  geom_point(alpha = 0.1) +
  #geom_smooth(method = "loess") +
  theme_bw()

df %>% filter(pollutant %in% c("pm2.5"
                               ,"pm2_5_atm")) %>%
  filter(id %in% c("DustTrak"
                   ,"Teensy_5"
                   ,"Teensy_6")) %>%
  ggplot(aes(x = date, y = value, color = as.factor(id))) + 
  geom_point(alpha = 0.1) +
  #geom_smooth(method = "loess") +
  theme_bw()

df %>% filter(pollutant %in% c("pm2.5"
                               ,"pm2_5_atm")) %>%
  filter(id %in% c("DustTrak"
                   ,"Teensy_3"
                   ,"Teensy_4")) %>%
  ggplot(aes(x = date, y = value, color = as.factor(id))) + 
  geom_point(alpha = 0.1) +
  #geom_smooth(method = "loess") +
  theme_bw()


df %>% filter(pollutant %in% c("pm2.5"
                               ,"pm2_5_atm")) %>%
  filter(id %in% c("DustTrak"
                   ,"Teensy_1"
                   ,"Teensy_2")) %>%
  ggplot(aes(x = date, y = value, color = as.factor(id))) + 
  geom_point(alpha = 0.1) +
  #geom_smooth(method = "loess") +
  theme_bw()

df %>% filter(date >= as.POSIXct(start_time, format = "%Y-%m-%d %H:%M:%S")) %>% 
  filter(pollutant %in% c("pm2_5_atm"
                          ,"pm2.5")) %>%  
  ggplot(aes(x = date, y = value, color = speed)) +
  geom_point(alpha = 0.1, size = 0.9) + 
  #geom_smooth(se = FALSE) + 
  # geom_ma(ma_fun = SMA
  #         ,n = 1
  #         ,size = 1
  #         ,aes(linetype = "solid"
  #              ,alpha = 0.1)) +
  facet_wrap(~id) + 
  theme_bw()
  
df %>% filter(date >= as.POSIXct(start_time, format = "%Y-%m-%d %H:%M:%S")) %>% 
  filter(pollutant %in% c("pm2_5_atm"
                          ,"pm2.5")) %>%  
  ggplot(aes(x = speed, y = value, color = id)) +
  #geom_point(alpha = 0.1, size = 0.9) + 
  geom_smooth(se = FALSE) +
  theme_bw()
