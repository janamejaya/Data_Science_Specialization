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
  NEI$year <- as.factor(NEI$year)
  
  # Select Baltimore City fips, select type, year, Emissions, group by type and year
  total_emissions <- NEI %>%
      filter(fips==selectedfips) %>%
      select(type, year, Emissions) %>%
      group_by(type, year) %>%
      summarize(Total_Emissions = sum(Emissions, na.rm=TRUE))
  total_emissions
}

make_plot3 <- function(){
  # source dplyr
  library(dplyr)
  library(ggplot2)
  library(RColorBrewer)
  
  # First read in data
  NEI <- read_in_nei_data()
  
  # Generate summary
  summary3 <- get_total_emissions_fips(NEI, "24510")
  
  # Plot summary
  g <- ggplot(summary3, aes(x=year, y=Total_Emissions, fill=year))
  g <- g + geom_bar(stat="identity")
  g <- g + facet_wrap( ~ type, nrow=2)
  g <- g + labs(x="Year", y="Total PM2.5 Emissions(ton)", title="Total PM2.5 Emission of different types for select years in Baltimore City")
  g <- g + scale_fill_brewer(palette="Dark2")
  ggsave("plot3.png", plot=g)
}
