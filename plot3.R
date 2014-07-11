## Exploratory Data Analysis Course Project 1
## ==========================================

## This function performs the third plot required for the course project 1. The
## function operates in the given directory, unless one is given as a parameter.
## The function's output is the "plot3.png" file in the working directory.
## The data set to read ("household_power_consumption.txt") contains
## 2,075,259 rows and 9 columns:
## 1. Date. dd/mm/yyyy. Estimated size: object.size("dd/mm/yyyy") = 104 bytes
## 2. Time. hh:mm:ss. Estimated size: object.size("hh:mm:ss") = 104 bytes
## 3. Global_active_power. Numeric. Estimated size: 8 bytes
## 4. Global_reactive_power. Numeric. Estimated size: 8 bytes
## 5. Voltage. Numeric. Estimated size: 8 bytes
## 6. Global_intensity. Numeric. Estimated size: 8 bytes
## 7. Sub_metering_1. Numeric. Estimated size: 8 bytes
## 8. Sub_metering_2. Numeric. Estimated size: 8 bytes
## 9. Sub_metering_3. Numeric. Estimated size: 8 bytes
## Total size for a row = 2 * 104 + 7 * 8 = 264 bytes
## Total size of the data set: 2075259 * 264 = 547868376 bytes = 522.49 MB

plot3 <- function(workDir = "")
{
        ## Set the working directory, if given. Otherwise, use the current one
        if (workDir != "")
        {
                setwd(workDir)
        }
        
        ## Read the data set that has a header and separators set to ";" to a
        ## data frame
        df <- read.table("./household_power_consumption.txt",
                         header = TRUE, sep = ";")
        
        ## This data set has missing values identified by the ";" character.
        ## Before subsetting the data frame, the missing data is set to NAs
        df <- as.data.frame(lapply(df, function(x) {replace(x, x == "?", NA)}))
        
        ## NAs are then excluded from the data frame by keeping complete cases
        df <- df[complete.cases(df), ]
        
        ## Because of missing values (";"), numerical data is read as factors.
        ## Factors are then converted to numeric values from the 3rd column (1+2)
        ## to the last (ncol(df))
        for (i in (1+2):ncol(df))
        {
                df[, i] <- as.numeric(as.character(df[, i]))
        }
        
        ## The first two columns of the data frame (Date and Time) are then
        ## binded in order to convert the given format to POSIXct
        df <- cbind(paste(df$Date, df$Time), df[, (1+2):ncol(df)])
        
        ## The binded column is then renamed to "Time"
        colnames(df)[1] <- "Time"
        
        ## The newly binded column "Time" is converted to POSIXct format
        df$Time <- as.POSIXct(strptime(df$Time, format = "%d/%m/%Y %H:%M:%S"))
        
        ## The data frame is then subsetted to the required dates (1st and 2nd
        ## February 2007)
        df <- subset(df, Time >= as.POSIXct('2007-02-01') &
                         Time < as.POSIXct('2007-02-03'))
        
        ## Create a PNG graphics device with a transparent background and
        ## the default width (480) and height (480). Open the plot3.png file in
        ## the working directory
        png(file = "./plot3.png", bg = "transparent")

        ## Plot the line graph with the Date/Time on the X-axis and the 3
        ## Energy sub metering columns on the Y-axis. The axes labels are set to
        ## the requirement. The line graph colors are respectively:
        ## "black" (default) for Sub_metering_1, "red" for Sub_metering_2 and
        ## "blue" for Sub_metering_3. The "topright" legend box has a border
        with(df, plot(Time, Sub_metering_1, type = "l",
                      xlab = "", ylab = "Energy sub metering"))
        with(df, lines(Time, Sub_metering_2, col = "red"))
        with(df, lines(Time, Sub_metering_3, col = "blue"))
        legend("topright", lwd = 1, col = c("black", "red", "blue"),
               legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
        
        ## Shut down the default graphics device
        dev.off()
        
        ## Objects in the environment are then removed using rm()
        rm(list = ls())
}
