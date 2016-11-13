readData <- function()
{
    data <- read.csv('ExData_Plotting1/household_power_consumption.txt',sep=';');
    dates <- strptime(data[,1],format="%d/%m/%Y");
    data <- data[dates>="2007-02-01" & dates<="2007-02-02",];
}

plot3 <-function(data,save)
{
    sub1<-as.numeric(as.character(data$Sub_metering_1));
    
    if(save)
    {png('plot3.png', width = 480, height = 480);}
    
    plot(1:length(sub1),sub1,xaxt = "n",
         type='l',
         xlab='',
         ylab='Energy Sub Metering'
    );
    
    sub2<-as.numeric(as.character(data$Sub_metering_2));
    lines(1:length(sub2),sub2,xaxt = "n",
         type='l',
         col='red'
    );
    
    sub3<-as.numeric(as.character(data$Sub_metering_3));
    lines(1:length(sub3),sub3,xaxt = "n",
          type='l',
          col='blue'
    );

    legend("topright", col = c("black", "red", "blue"), 
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
           lwd=1);
    
    wday<-strptime(data$Date,format='%d/%m/%Y');
    wday<-weekdays(wday);
    tickLoc <- c( match(c('Quinta Feira','Sexta Feira'),wday) , length(sub1) );
    axis(1,at=tickLoc,labels = c('Thu','Fri','Sat'));   
    
    if(save)
    {dev.off();}
}