readData <- function()
{
    data <- read.csv('ExData_Plotting1/household_power_consumption.txt',sep=';');
    dates <- strptime(data[,1],format="%d/%m/%Y");
    data <- data[dates>="2007-02-01" & dates<="2007-02-02",];
}


plot1 <- function(data,save)
{
  gap<-as.numeric(as.character(data$Global_active_power));
  
  if(save)
  {png('plot1.png', width = 480, height = 480);}
  
  hist(gap,
       breaks=seq(from=0,to=12,by=0.5),
       main='Global Active Power',
       xlab='Global Active Power (kilowatts)',
       ylab='Frequency',
       xlim=c(0,6),
       col='red');
  
  if(save)
  {dev.off();}
  
}