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
  #SCC <- as.data.frame(readRDS("data/Source_Classification_Code.rds"))
  SCC <- readRDS("data/Source_Classification_Code.rds")
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

get_vehicle_scc_index <- function(SCC){
  # As done for Coal Consumption, the EI.Sector column is selected
  # The keyword Mobile adds extra sources that are not motor vehicles,
  # and it is ignored.
  # The keyword "[Vv]ehicle" returns mobile vehicles and is selected
  
  # First index of rows that contain [Vv]ehic in EI.Sector column
  index2 <- which(grepl("[Vv]ehic",SCC$EI.Sector))
  
  # Collect the list of SCC indices
  list_of_scc_indices <- as.data.frame(SCC[index2,]$SCC)
  colnames(list_of_scc_indices) <- c("List")
  list_of_scc_indices
}

get_total_emissions_by_type <- function(NEI, list_of_indices, fipsvalue){
 # select only the rows in REI column SCC which are in list_of_scc_indices
 # and those which correspond to the selected cities fipsvalue
 summary5 <- NEI %>%
             filter(fips==fipsvalue & SCC %in% list_of_indices$List) %>%
             select(year, Emissions) %>%
             group_by(year) %>%
             summarize(Total_Emissions = sum(Emissions, na.rm=TRUE))

 summary5$year = as.factor(summary5$year)
 summary5
}

make_plot5 <- function(){
  # source dplyr
  library(dplyr)
  library(ggplot2)
  library(RColorBrewer)
  
  # First read in NEI data
  NEI <- read_in_nei_data()
  
  # Second, read in the SCC data
  SCC <- read_in_scc_data()

  # Third, identify the list of SCC indices corresponding to coal combustion related emissions
  list_of_scc_indices <- get_vehicle_scc_index(SCC)
  
  # Generate the subset of SCC related to coal combustion and summarize emission
  # information across the US. Since which emission information is required is unclear,
  # I select the total emission from coal combustion related data
  summary5 <- get_total_emissions_by_type(NEI, list_of_scc_indices, "24510")
  summary5$Total_Emissions <- summary5$Total_Emissions/1000
  
  # Plot summary
  g <- ggplot(summary5, aes(x=year, y=Total_Emissions, fill=year))
  g <- g + geom_bar(stat="identity")
  g <- g + xlab("Year")
  g <- g + ylab("Total PM2.5 Emissions from Motor Vehicles (kiloton)")
  #g <- g + coord_cartesian(ylim=c(0,800))
  g <- g + ggtitle("Total PM2.5 Emission from Motor Vehicles over select years in Baltimore City")
  g <- g + scale_fill_brewer(palette="Dark2")
  g <- g + geom_text(aes(label=round(Total_Emissions,2), vjust=-0.2))
  g <- g + guides(fill=FALSE)
  ggsave("plot5.png", plot=g)
}
