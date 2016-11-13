readData <- function()
{
    data <- read.csv('ExData_Plotting1/household_power_consumption.txt',sep=';');
    dates <- strptime(data[,1],format="%d/%m/%Y");
    data <- data[dates>="2007-02-01" & dates<="2007-02-02",];
}


plotAux <- function(x,ylabel,data)
{
    
    plot(1:length(x),x,xaxt = "n",
         type='l',
         xlab='datetime',
         ylab=ylabel
    );
    
    wday<-strptime(data$Date,format='%d/%m/%Y');
    wday<-weekdays(wday);
    tickLoc <- c( match(c('Quinta Feira','Sexta Feira'),wday) , length(x) );
    axis(1,at=tickLoc,labels = c('Thu','Fri','Sat'));
}

plot4 <- function(data)
{
    png('plot4.png', width = 480, height = 480);
    
    par(mfrow=c(2,2))
    
    plot2(data,0);
    
    volt<-as.numeric(as.character(data$Voltage));
    plotAux(volt,'Voltage',data);
    
    plot3(data,0);
    
    grp<-as.numeric(as.character(data$Global_reactive_power));
    plotAux(grp,'Global_reactive_power',data);
    
    dev.off();
}