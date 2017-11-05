download_data <- function(){
  # The following lines come straight from the Course Lectures
  if(!file.exists("./data")){dir.create("./data")}
  fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  if(!file.exists("./data/household_power_consumption.txt")){
  download.file(fileURL,destfile="./data/dataset.zip",method="curl")
  unzip("./data/dataset.zip",exdir="./data")
  }
}

read_from_datafile <- function(filename1="./data/household_power_consumption.txt"){
  # First read in the first 10 lines from the file
  initialdf <- read.table(filename1, header=TRUE, sep=";", na.strings="?", nrow=10, comment.char="", stringsAsFactors=FALSE)
  iclasses <- sapply(initialdf, class)
  
  # Read the whole dataset
  alldata <- read.table(filename1, header=TRUE, sep=";", na.strings="?",colClasses = iclasses, comment.char="", stringsAsFactors=FALSE)
  
  # The selected dates have Select dates
  selected_dates <- c("1/2/2007","2/2/2007")
  
  # Subset of all rows which have selected_dates
  alldata <- alldata[alldata$Date %in% selected_dates,]
  
  # Merge all date and time columns into a new column of type POSIXlt
  alldata$datetime <- strptime(paste(alldata$Date, alldata$Time), "%d/%m/%Y %H:%M:%S")
  alldata
}

plot_figure3 <- function(){
  # 1. Download the data
  download_data()
  
  # 2. Read the data into a table: alldata
  alldata <- read_from_datafile()
  
  # 3. Construct the plot
  png(filename="plot3.png", width=480, height=480)

  with(alldata, {
    plot(datetime, Sub_metering_1, type="l",xlab="", ylab="Energy sub metering")
    lines(datetime, Sub_metering_2, xlab="", ylab="", col="red")
    lines(datetime, Sub_metering_3, xlab="", ylab="", col="blue")
  })
  
  legend("topright",lwd=c(1, 1, 1), col=c("black","red","blue"), legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
  dev.off()
}