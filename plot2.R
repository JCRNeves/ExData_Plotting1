readData <- function()
{
    data <- read.csv('ExData_Plotting1/household_power_consumption.txt',sep=';');
    dates <- strptime(data[,1],format="%d/%m/%Y");
    data <- data[dates>="2007-02-01" & dates<="2007-02-02",];
}


plot2 <-function(data,save)
{
    gap<-as.numeric(as.character(data$Global_active_power));
    
    if(save)
    {png('plot2.png', width = 480, height = 480);}
    
    plot(1:length(gap),gap,xaxt = "n",
         type='l',
         xlab='',
         ylab='Global Active Power (kilowatts)'
         );
    
    wday<-strptime(data$Date,format='%d/%m/%Y');
    wday<-weekdays(wday);
    tickLoc <- c( match(c('Quinta Feira','Sexta Feira'),wday) , length(gap) );
    axis(1,at=tickLoc,labels = c('Thu','Fri','Sat'));
    
    if(save)
    {dev.off();}
}