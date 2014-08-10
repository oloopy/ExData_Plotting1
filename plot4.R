PlotFigure <- function(file.path = "household_power_consumption.txt"){
    power.consumption <- ReadPowerConsumption(file.path)
    with(data = power.consumption, expr = {
        par(mfrow = c(2, 2))
        # 1
        plot(x = Date, y = Global_active_power, type = "l", col = "black", xlab = "", ylab = "Global Active Power")
        # 2
        plot(x = Date, y = Voltage, type = "l", col = "black", xlab = "datetime", ylab = "Voltage")
        # 3
        plot(x = Date, y = Sub_metering_1, type = "l", col = "black", xlab = "", ylab = "Enengy sub metering")
        lines(x = Date, y = Sub_metering_2, col = "red")
        lines(x = Date, y = Sub_metering_3, col = "blue")
        legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty = rep(x = c(1), 3), col = c("black", "red", "blue"), cex = 0.7)
        # 4
        plot(x = Date, y = Global_reactive_power, type = "l", col = "black", xlab = "datetime")
    })
    ExportPNG("plot4.png")
}

ReadPowerConsumption <- function(file.path){
    # Read
    power.consumption <- read.table(file = file.path, header = T, sep = ";", na.strings = "?", as.is = T)
    
    # Subset (i.e., "1/2/2007" and "2/2/2007")
    power.consumption <- with(data = power.consumption, expr = {
        subset(x = power.consumption, subset = Date == "1/2/2007" | Date == "2/2/2007")
    })
    
    # Covert and combine columns (i.e., $Date and $Time)
    power.consumption$Date <- with(data = power.consumption, expr = {
        strptime(x = paste(power.consumption$Date, power.consumption$Time), 
                 format = "%d/%m/%Y %H:%M:%S")
    })
    power.consumption <- with(data = power.consumption, expr = {
        subset(x = power.consumption, select = -Time)
    })
    
    power.consumption
}

ExportPNG <- function(file.name){
    dev.copy(device = png, file = file.name, width = 480, height = 480)
    dev.off()
}