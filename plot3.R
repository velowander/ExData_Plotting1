#Coursera R "Exploratory Data Analysis" Project 1

#Usage: ReadEnergy() -> myEnergy or ReadEnergy() ->> myEnergy to store in the global namespace
ReadEnergy <- function () {
  #helper function to read the energy data table, call from plot[1234].R
  #All 4 plots depend on this function, so if you separate plot[1234].R please make a separate copy
  #of this function or reference it as a library
  #Out: the filtered energy table with only dates from 1 & 2 February, 2007
  #Note: Column 1 (Date represented as character) and Column 2 (time represented as character) are fused together
  #Into a new column 1: timestamp, a POSIXct type. The original Columns 1 and 2 are discarded.
  #The resulting dataset has 8 columns instead of the original 9.
  require("lubridate")
  
  setClass("dmy") #Now dmy is a class I can use in read.table colClasses or anywhere else
  setAs("character","dmy", function(from) as.Date(from, format="%d/%m/%Y") )
  energy <- read.table(file = "household_power_consumption.txt", colClasses = c("dmy", "character", rep("numeric", 7)), 
                       header = TRUE, sep=";", na.strings="?")
  energy <- energy[!is.na(energy$Date) & year(energy$Date) == 2007 & month(energy$Date) == 2 & day(energy$Date) >= 1 & day(energy$Date) <= 2, ]
  #Create a POSIXct object for the date/time which is the new column 1, discard the original separate date & time
  timestamp <- ymd_hms(paste(energy[ , 1], energy[ , 2], sep=" "))
  cbind(timestamp, energy[ , 3:9])
}

plot3 <- function() {
  #We need a 4 column table with the timestamp & 3 submetering values
  if (!exists("energy.plot3")) energy.plot3 <<- ReadEnergy()[, c("timestamp", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")]
  png(file = "plot3.png", width = 480, height = 480, bg = "white")
  plot (energy.plot3[ , "timestamp"], energy.plot3[ , "Sub_metering_1"], col = "black", type = "l", xlab = "", ylab = "Energy sub metering")
  lines(energy.plot3[ , "timestamp"], energy.plot3[ , "Sub_metering_2"], col = "red", type = "l")
  lines(energy.plot3[ , "timestamp"], energy.plot3[ , "Sub_metering_3"], col = "blue", type = "l")
  legend("topright", cex = 0.8, lty = 1, col = c("black", "red", "blue"), legend = colnames(energy.plot3)[2:4])
  dev.off()
}