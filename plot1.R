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


plot1 <- function() {
  #for the first plot, all we need is the global active power for the selected dates, dump others
  if (!exists("energy.plot1")) energy.plot1 <<- ReadEnergy()[, "Global_active_power"]
  
  #deactivate the default x-axis ticks, use axis() for more control
  png(file = "plot1.png", width = 480, height = 480, bg = "white")
  hist(energy.plot1, col="RED", main="Global Active Power", xaxt="n", xlab="Global Active Power (kilowatts)", ylim=c(0,1200))
  axis(1, at=seq(0, 6, 2)) #tick marks at x = 0, 2, 4, 6
  dev.off()
}

#REFERENCES
#Use custom colClasses for reading date in mm/dd/yyyy format
#http://stackoverflow.com/questions/13022299/specify-date-format-for-colclasses-argument-in-read-table-read-csv/13022441#13022441