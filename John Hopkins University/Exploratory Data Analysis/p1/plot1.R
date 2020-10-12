#using mydata1 which is filtered 2 day data see main.R for code
hist(as.numeric(mydata1$Global_active_power),breaks=6,col="red",main="Global Active Power", xlab="Global Active Power(kilowatts")
dev.copy(png,"plot1.png")
dev.off()
