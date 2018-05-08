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
       ,grid
       ,gridExtra
       ,tidyquant
       ,scales
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
  
  # convert to tidy to rbind with dusttrak
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
  
  # convert to tidy format to rbind with teensy data
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

# create a new id variable for sensor height
df$sensor_height <- ifelse(df$id == "DustTrak"
                           ,"DustTrak"
                           ,ifelse(df$id %in% c("Teensy_8","Teensy_7") # Teensy_8 is missing from this set
                                   ,"24"
                                   ,ifelse(df$id %in% c("Teensy_6", "Teeny_5")
                                           ,"18"
                                           ,ifelse(df$id %in% c("Teensy_4", "Teensy_3")
                                                   ,"12"
                                                   ,ifelse(df$id %in% c("Teensy_2", "Teensy_1")
                                                           ,"8"
                                                           ,NA)))))


# define gcs for gps data
wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "

# convert to spatial points dataframe
df_sf <- st_as_sf(df, coords = c("lon", "lat"), crs = wgs84)

df$sensor_height <- factor(df$sensor_height, levels = c("DustTrak", "8", "12", "18", "24"))

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

df %>% filter(pollutant %in% c("pm2.5"
                               ,"pm2_5_atm")) %>%
  ggplot(aes(x = date, y = value, color = as.factor(id))) + 
  geom_jitter(alpha = 0.2) +
  theme_minimal()


df %>% filter(pollutant %in% c("pm2.5"
                               ,"pm2_5_atm")) %>%
  filter(id %in% c("DustTrak"
                   ,"Teensy_7"
                   ,"Teensy_8")) %>%
  ggplot(aes(x = date, y = value, color = as.factor(id))) + 
  geom_point(alpha = 0.1) +
  #geom_smooth(method = "loess") +
  theme_minimal()

df %>% filter(pollutant %in% c("pm2.5"
                               ,"pm2_5_atm")) %>%
  filter(id %in% c("DustTrak"
                   ,"Teensy_5"
                   ,"Teensy_6")) %>%
  ggplot(aes(x = date, y = value, color = as.factor(id))) + 
  geom_point(alpha = 0.1) +
  #geom_smooth(method = "loess") +
  theme_minimal()

df %>% filter(pollutant %in% c("pm2.5"
                               ,"pm2_5_atm")) %>%
  filter(id %in% c("DustTrak"
                   ,"Teensy_3"
                   ,"Teensy_4")) %>%
  ggplot(aes(x = date, y = value, color = as.factor(id))) + 
  geom_point(alpha = 0.1) +
  #geom_smooth(method = "loess") +
  theme_minimal()


df %>% filter(pollutant %in% c("pm2.5"
                               ,"pm2_5_atm")) %>%
  filter(id %in% c("DustTrak"
                   ,"Teensy_1"
                   ,"Teensy_2")) %>%
  ggplot(aes(x = date, y = value, color = as.factor(id))) + 
  geom_point(alpha = 0.1) +
  #geom_smooth(method = "loess") +
  theme_minimal()

df %>% filter(date >= as.POSIXct(start_time, format = "%Y-%m-%d %H:%M:%S")) %>% 
  filter(pollutant %in% c("pm2_5_atm"
                          ,"pm2.5")) %>%  
  ggplot(aes(x = date, y = value, color = speed)) +
  geom_point(alpha = 0.1, size = 0.9) + 
  #geom_smooth(se = FALSE) + 
  # tidyquant::geom_ma(ma_fun = SMA
  #         ,n = 1
  #         ,size = 1
  #         ,aes(linetype = "solid"
  #              ,alpha = 0.1)) +
  facet_wrap(~id) + 
  theme_minimal()
  
pm_smooth_height <- df %>% filter(date >= as.POSIXct(start_time, format = "%Y-%m-%d %H:%M:%S")) %>% 
  filter(pollutant %in% c("pm2_5_atm"
                          ,"pm2.5")) %>%  
  ggplot(aes(x = speed, y = value, color = sensor_height)) +
  #geom_point(alpha = 0.1, size = 0.9) + 
  #geom_smooth(se = FALSE) +
  geom_smooth(method = "loess", se = FALSE) +
  xlab("Speed (mph)") + 
  ylab(expression(~PM[2.5]~mu*g*m^-3)) + 
  theme_minimal()

# ggsave is really slow at this DPI
ggsave(filename = paste0("./figures/", format(Sys.time(), "%Y-%m-%d"), "_pm_speed_height.png"),
       plot = pm_smooth_height,
       scale = 1,
       width = 16,
       height = 10,
       units = "in",
       dpi = 600)


pm_smooth <- df %>% filter(date >= as.POSIXct(start_time, format = "%Y-%m-%d %H:%M:%S")) %>% 
  filter(pollutant %in% c("pm2_5_atm"
                          ,"pm2.5")) %>%  
  ggplot(aes(x = speed, y = value, color = id)) +
  #geom_point(alpha = 0.1, size = 0.9) + 
  #geom_smooth(se = FALSE) +
  geom_smooth(method = "loess", se = FALSE) +
  xlab("Speed (mph)") + 
  ylab(expression(~PM[2.5]~mu*g*m^-3)) + 
  theme_minimal()

# ggsave is really slow at this DPI
ggsave(filename = paste0("./figures/", format(Sys.time(), "%Y-%m-%d"), "_pm_speed_smooth.png"),
       plot = pm_smooth,
       scale = 1,
       width = 16,
       height = 10,
       units = "in",
       dpi = 600)


# make pseudo-continuous color scale for sensor height attribute:
# see canopycontinuum diurnal variation six cities plots from January for adjusted specific factors based on colors
#hue_pal()(6)

#cols <- scales::seq_gradient_pal("green", "red", "Lab")(seq(0,1, length.out = 5))

cols <- terrain.colors(6)
# dropping the 6th color (gray) which doesn't show up well in the plot...
pal = c("DustTrak" = "#00A600FF"
         ,"8" = "#63C600FF"
         ,"12" = "#E6E600FF"
         ,"18" = "#EAB64EFF"
         ,"24" = "#EEB99FFF")


# plot pm time series above vehicle speed and elevation data 
p1 <- df %>% filter(date >= as.POSIXct(start_time, format = "%Y-%m-%d %H:%M:%S")) %>% 
  filter(pollutant %in% c("pm2_5_atm"
                          ,"pm2.5")) %>%
  # exclude shortlived spikes and zoom in on overall trends 
  #filter(value <=300) %>%
  
  # generate our ts plot
  ggplot(aes(x = date
             ,y = value
             ,color = as.factor(sensor_height)
             )) + 
  #geom_point(alpha = 0.1, size = 0.9) + 
  #geom_smooth(se = FALSE) + 
  tidyquant::geom_ma(ma_fun = SMA
                     ,n = 2
                     ,size = 1
                     ,aes(linetype = "solid"
                          ,alpha = 0.1)) +
  #scale_color_gradientn(colours = terrain.colors(5)) +
  #scale_color_manual(colors = terrain.colors(5)) +
  scale_color_manual(values = pal) +
  scale_alpha(guide = 'none') +
  scale_linetype(guide = 'none') +
  xlab("Time") + 
  ylab(expression(~PM[2.5]~mu*g*m^-3)) + 
  guides(color=guide_legend(title="Sensor Height (in)")) +
  theme_minimal()

  p2 <- df %>% filter(date >= as.POSIXct(start_time, format = "%Y-%m-%d %H:%M:%S")) %>% 
  filter(pollutant %in% c("pm2_5_atm"
                          ,"pm2.5")) %>%
  na.omit() %>%
  ggplot(aes(x = date, y = speed, color = elevation)) +
  # exclude shortlived spikes and zoom in on overall trends 
  geom_line() +
  scale_color_gradientn(colours = terrain.colors(9)) + 
  # generate our ts plot
  #geom_point(alpha = 0.1, size = 0.9) + 
  #geom_smooth(se = FALSE) + 
  xlab("Time") +
  ylab(expression(mph)) +
  guides(color=guide_legend(title="elevation (m)")) +
  theme_minimal()

grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))
#grid.arrange(p1, p2)

g1 <- arrangeGrob(p1, p2)

# ggsave is really slow at this DPI
ggsave(filename = paste0("./figures/", format(Sys.time(), "%Y-%m-%d"), "_time_series_pm_speed_elevation.png"),
       plot = g1,
       scale = 1,
       width = 16,
       height = 10,
       units = "in",
       dpi = 600)
