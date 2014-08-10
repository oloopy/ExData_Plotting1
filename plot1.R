PlotFigure <- function(file.path = "household_power_consumption.txt"){
    power.consumption <- ReadPowerConsumption(file.path)
    with(data = power.consumption, expr = {
        hist(x = Global_active_power, col = "red", xlab = "Global Active Power (kilowatts)",
             main = "Global Active Power")
    })
    ExportPNG("plot1.png")
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