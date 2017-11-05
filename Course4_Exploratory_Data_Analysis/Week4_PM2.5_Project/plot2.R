# Function to download the data file and unzip is
download_data <- function(){
  # The following lines come straight from the Course Lectures
  if(!file.exists("./data")){dir.create("./data")}
  fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  if(!file.exists("./data/dataset.zip")){
  download.file(fileURL,destfile="./data/dataset.zip",method="curl")
  unzip("./data/dataset.zip",exdir="./data")
  # At this point, the data is contained in "./data/UCI HAR Dataset"
  }
}

read_in_nei_data <- function(){
  # This first line will likely take a few seconds. Be patient!
  NEI <- as.data.frame(readRDS("data/summarySCC_PM25.rds"))
  NEI
}

read_in_scc_data <- function(){
  # This first line will likely take a few seconds. Be patient!
  SCC <- as.data.frame(readRDS("data/Source_Classification_Code.rds"))
  SCC
}

get_total_emissions_fips <- function(NEI, selectedfips="24510"){
  # Convert fips to factor variable
  NEI$fips <- as.factor(NEI$fips)
  
  # Group by year
  total_emissions <- NEI %>%
      filter(fips==selectedfips) %>%
      select(Emissions,year) %>%
      group_by(year) %>%
      summarize(Total_Emissions = sum(Emissions, na.rm=TRUE))
  total_emissions
}

make_plot2 <- function(){
  # source dplyr
  library(dplyr)
  
  # First read in data
  NEI <- read_in_nei_data()
  
  # Generate summary
  summary2 <- get_total_emissions_fips(NEI, "24510")
  
  # Plot summary
  options(scipen = 999)
  png(filename="plot2.png", width=480, height=480)
  barplot(summary2$Total_Emissions, names=summary2$year, xlab="Year", ylab="Total PM2.5 Emission(ton)", main="Total PM2.5 Emission for Baltimore City from 1999 to 2008", col="green")
  dev.off()
}
