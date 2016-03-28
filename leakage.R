# script to estimate the leakage of our house

# first read in the internal temperature

# location of the daily readings
t_dir <- "../internal-temperature/" 
# the files look like YYYYMM-e.dat
t_files <- list.files(t_dir,pattern="*.dat",full.names=T)

# define the data frame for the for the internal measurments
# we keep the raw time and also mark the type as raw so that
# we nkow where the data comes from.
# 
internal <- matrix(nrow = 0,ncol = 5)
internal <- as.data.frame(internal)
colnames(internal) <- c("times","rawtimes" , "temperature" , "humidity" , "type")

file <- "../internal-temperature/20140330-hum-temp.dat" # for debugging
# read in al data with a loop
for (file in t_files)
{
  test <- read.csv(file,header=F)
  # make the raw type marker
  raw <- rep("raw",nrow(test))
  # convert csv time to a date and store everyting in a temporary data frame
  newInternal <- data.frame(times = strptime(as.character(test$V1),format="%Y-%m-%d %H:%M",tz="GMT"),
                             rawtimes = as.character(test$V1),
                             temperature = test$V2,
                             humidity = test$V3,
                             type = raw)
  # use row bind to add the new data
  internal <- rbind(internal,newInternal)
}

# add the differences between observations
# 
internal$diff <- c(0,diff(internal$times))

## the external data from Benson
# it's all in one file extracted from the Metoffice weather data
external <- read.table('../external-temperature/benson_weather.csv', header = T,sep=",")
# mangle the time to a real date
external$date <- as.POSIXct(external$time,origin="1970-01-01",tz="GMT")
# check the difference between observations
external$diff <- c(0,diff(external$date))
# some observations are repeated (very few)
# we throwo away the those with 0 time difference
externalNZ <- subset(external, diff != 0)


## and now we need the enery usage

# load some common functions
source('common.R')

# where the gas and electricity readings live that we took
# at a dialy basis
dir <- "../daily-readings/"

# the files look like YYYYMM-e/g.dat e for electricity
# and g for gas
e_files <- list.files(dir,pattern="*-e.dat",full.names=T)
g_files <- list.files(dir,pattern="*-g.dat",full.names=T)

# read the files using a common function
eReadings <- readData(e_files)
gReadings <- readData(g_files)


# that data is roughly daily with readings taken in the morning
# We use a linear interpolation to change this to a daily estimated
# observation taken at midnight
gas <- InterpolateData(gReadings)
electric <- InterpolateData(eReadings)

# Calculate the daily gas usage
# 
gas$diff <- c(0,diff(gas$value))
electric$diff <- c(0,diff(electric$value))

### time to do some plotting

library(ggplot2)
library(plotly)

# plotting the daily gas usage is simple
qplot(gas$time,gas$diff)

# but we'd like to see if the performance of our house has changed or
# if we just had mild winters

# It is a fair assumption that short term fluctuations will be cancelled if
# we take weekly averages for temperatures and consumptions

# Our three time series cover different time periods
# we need to find the common time period limited by
# Minday and MaxDay
MinDay <- max(min(internal$times), min(externalNZ$date), min(gas$time))
MaxDay <- min(max(internal$times), max(externalNZ$date), max(gas$time))

# how many weeks are covered?
(MaxDay - MinDay) / 7
# roughly 87 weeks

# Which day of the week do we start and end? We choose the first Monday as start
# and also figure what the last day (a Sunday) is for the observations
weekdays(MinDay + 6  * 86400)
weekdays(MaxDay - 86400)

# Now we define first and last day for the measurements we're going to consider
FirstDay <- as.Date(format(MinDay + 6  * 86400, "%Y-%m-%d")) 
LastDay <- as.Date(format(MaxDay - 86400, "%Y-%m-%d")) 

# next we create a sequence covering all weeks we want to include
dates <- as.POSIXct(seq(FirstDay, LastDay, by = 7))

# now a data frame to hold the weekly gas & electricity usage, the average internal and external temperature
# which we calculate naively as the mean of the existing observations in our
# time series. this s=hould be changed to a mean of equidistant obeservations.
# We also mark the week by the middle of the week
energy <- matrix(nrow=0, ncol=5)
colnames(energy) <- c("day", "internalT", "externalT","gas","electricity")
energy <- as.data.frame(energy)

d <- 3 # debugging the for loop

for (d in 2:length(dates))
{
  # caluculate the external mean temperature of the week
  externalT <- mean(subset(external, date > dates[d-1] & date <= dates[d], select = c(temperature))$temperature)
  # similarly the internal average
  internalT <- mean(subset(internal, times > dates[d-1] & times <= dates[d], select = c(temperature))$temperature)
  # the gas usage during the week in m^3
  gasU <-  sum(subset(gas, time > dates[d-1] & time <= dates[d], select = c(diff))$diff)
  eleU <- sum(subset(electric, time > dates[d-1] & time <= dates[d], select = c(diff))$diff)
  # the middle of the week
  day <- dates[d-1] + 2.5 * 86400
  print(paste(day, internalT, externalT, gasU))
  # add the new data to the heating data frame
  energy <- rbind(energy, c(day,internalT,externalT,gasU,eleU))
}
# reset the colmun names
colnames(energy) <- c("day", "internal", "external","gas","electricity")

# the temperature difference between the in and outside.
energy$diff <- - (energy$external - energy$internal)

energy$date <- as.POSIXct(energy$day,origin="1970-01-01")
# the month of the averages
energy$month <- months(as.POSIXct(energy$day,origin="1970-01-01"))
energy$year <- format(as.POSIXct(energy$day,origin="1970-01-01"),format="%Y")
# convert m^3 to kWh
energy$kWh <- energy$gas * 11.187
#https://www.ukpower.co.uk/home_energy/gas_meter_readings
# and calculate the kWhh/day (whacky unit alert!)
energy$kWhperDay <- energy$kWh /7
# nice labels for the months
months <- c("January" , "February" , "March"   ,  "April" , "May" ,      "June"     , "July",      "August",    "September", "October",   "November",  "December"  )
# which months use the heating system?
heatingSeason <- c("January" , "February" , "March" ,"October",   "November",  "December"  )
# filter out the averages in the heating season
thisHeating <- subset(energy, month %in% heatingSeason)
  

# what is the straight line through those points
sl <- coef(lm(kWhperDay ~ diff , data = na.exclude(thisHeating)))
# sl: (Intercept)        diff 
#      -37.26632    10.09036 

energy$gMonth <- factor(energy$month,levels=months)
# that orders the legend in the right order
# we're missing a week of external temp data, na.exclude()
p <- ggplot( na.exclude(energy), aes(diff, kWhperDay, colour = gMonth, group=gMonth,shape=factor(year) ))
p +  geom_abline(intercept = -37.3, slope = 10.1)
p <- p + geom_point(size=2.5) +  geom_abline(intercept = as.numeric(sl[1]), slope = as.numeric(sl[2])) +  xlab("T_in - T_out /C") + ylab("kWh/day") + theme(legend.position = "right") 
p  + theme(legend.title=element_blank()) + coord_cartesian(xlim=c(3, 16)) + 
  scale_x_continuous(breaks=seq(3, 16, 2))

ggsave("leakage.png", width=8, height=6, dpi=100)
# so, that's the plots showing the leakage of the house


p <- ggplot( na.exclude(energy), aes(internal, kWhperDay, colour = gMonth, group=gMonth,shape=factor(year) ))
p +  geom_abline(intercept = -37.3, slope = 10.1)
p <- p + geom_point(size=2.5) +  xlab("T_in /C") + ylab("kWh/day") + theme(legend.position = "right") 
p  + theme(legend.title=element_blank())   + coord_cartesian(xlim=c(15, 27)) + 
  scale_x_continuous(breaks=seq(15, 26, 2))

ggsave("control.png", width=8, height=6, dpi=100)
# shows when the temperature of the house is controlled by our thermostat

p <- ggplot( na.exclude(energy), aes(external, kWhperDay, colour = gMonth, group=gMonth,shape=factor(year) ))
p +  geom_abline(intercept = -37.3, slope = 10.1)
p <- p + geom_point(size=2.5)  +  xlab("T_out /C") + ylab("kWh/day") + theme(legend.position = "right") 
p  + theme(legend.title=element_blank()) + coord_cartesian(xlim=c(2, 22)) + 
  scale_x_continuous(breaks=seq(2, 22, 2))
ggsave("natural.png", width=8, height=6, dpi=100)
#shows the external temperaure when the thermostat is no longer in control about 15-16 degrees

p <- ggplot( na.exclude(energy), aes(date, electricity/7, colour = gMonth, group=gMonth ))
p +  geom_abline(intercept = -37.3, slope = 10.1)
p <- p + geom_point(size=2.5)  +  xlab("date") + ylab("kWh/day") + theme(legend.position = "right") 
p  + theme(legend.title=element_blank()) 
ggsave("electricity.png", width=8, height=6, dpi=100)


p <- ggplot( na.exclude(energy), aes(diff, electricity/7, colour = gMonth, group=gMonth ))
p +  geom_abline(intercept = -37.3, slope = 10.1)
p <- p + geom_point(size=2.5)  +  xlab("T_in - T_out /C") + ylab("kWh/day") + theme(legend.position = "right") 
p  + theme(legend.title=element_blank()) 
ggsave("electricity-diff.png", width=8, height=6, dpi=100)


#htmlwidgets::saveWidget(as.widget(p), "leakage.html")

