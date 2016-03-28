## First a function to read in data and return a data frame with the data
#  dataFiles is the list of files to read in
readData <- function(dataFiles){
  times <- vector()
  values <- vector()
  for (file in dataFiles)
  {
    newData <- read.csv(file,header=F)
    # convert csv time to a date
    times <- append(times,strptime(as.character(newData$V1),format="%d/%m/%Y %H:%M:%S"))
    values <- append(values,newData$V2)
  }  
  data.df <- data.frame(time=times,reading=values)
  return(data.df)
}



# Now we interpolate the data so that we have a value for each
# midnight. We assume that data is sorted by time and the procdure
# will fail when there are 

InterpolateData <- function(data)
{
  # First how many days are there in the time series
  DayInSec <- 24 * 60 * 60 # aka 86400
  FirstDay <- as.POSIXct(trunc(min(data$time) + DayInSec, units="days"),tz="GMT")
  LastDay <- as.POSIXct(trunc(max(data$time) - DayInSec, units="days"),tz="GMT")
  
  # So, we now have the first and the last day
  # We now need a sequence from the first to the last day
  
  Days <- seq(from=FirstDay,to=LastDay,by=DayInSec)
  
  # now we need to trundle through the original data and find the two measurements
  # just smaller and just bigger for each and interpolate the measured value.
  # this should get rid the problem when there is more than one reading per day
  # missing days
  # It maybe a bit slow as we need to find min/max every single time
  # but the code should be easier to read. 
  
  data$time <- as.POSIXct(data$time,tz="GMT")
  Interpolated <- rep(0,length(Days))
  
  for (i in 1:length(Days))
  {
    
    Left <- max(data$time[data$time < Days[i]])
    Right <- min(data$time[data$time > Days[i]])
    Lindex <- match(Left,data$time)
    Rindex <- match(Right,data$time)
    Lvalue <- data$reading[Lindex]
    Rvalue <- data$reading[Rindex]
    RLtime <- difftime(Right,Left,units="days")
    LNtime <- difftime(Days[i],Left,units="days")
    Slope <- (Rvalue - Lvalue) / as.numeric(RLtime)
    Interpolated[i] <- Lvalue + Slope * LNtime
    #print(paste(Left," ",Days[i] , " ",Right, " ", Lvalue, Interpolated[i], Rvalue, " ",RLtime," ",as.numeric(LNtime)))
    
  }
  interpolation.df <- data.frame(time = Days, value = Interpolated)
  
  return(interpolation.df)
}