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

get_combustion_type_scc_index <- function(SCC){
  # Case 1: columns representing coal combustion related data in SCC
  # Step 1: Identify name of columns in SCC data frame with names(SCC)
  # There are 15 columns.To count the number of distinct instances of Coal or coal 
  # in each column use sum(grepl("[Cc]oal",levels(SCC$Short.Name))). 
  # Turns out that for the 15 columns in SCC, the counts are 
  # (0, 0, 217, 3, 0, 0, 0, 0, 13, 77, 0, 0, 0, 0, 0)
  # So, only Short.Name, EI.Sector, SCC.Level.Three, and SCC.Level.Four contain
  # the word coal or Coal. 
  # Step 2: Note that the counts are for the set of distinct entries
  # enumerated with levels. The actual number of rows with Coal or coal in them 
  # is (239, 99, 181, 126) for the four columns listed above. This can be calculated
  # with length(which(grepl("[Cc]oal",SCC$Short.Name))) or replacing Short.Name with
  # the other possibilities
  # Step 3: Out of these rows, how many contain [Cc]ombustion or [Cc]omb ?
  # length(which(grepl("[Cc]omb|[Cc]ombustion",SCC[ which(grepl("[Cc]oal",SCC$Short.Name)),]
  # $Short.Name))) can be used to find the number of entries in the column for Short.Name
  # that have either ([Cc]omb or [Cc]ombustion) and [Cc]oal in them. The numbers are
  # (91, 99, 0, 8). So, Short.Name, EI.Sector and SCC.Level.Four are best candidates
  # The row indices for these entries can be found with
  # which(grepl("[Cc]omb|[Cc]ombustion",SCC[ which(grepl("[Cc]oal",SCC$Short.Name)),]
  # $Short.Name))
  # Step 4: The contents of only EI.Sector contain "Fuel Comb" and Coal and has the most
  # occurances of [Cc]oal and [CC]omb in them. Since coal is used for fuel combustion
  # as per these names, I select EI.Sector as the most relevant column containing 
  #information on coal combustion.
  # First index of rows that contain [Cc]oal in EI.Sector column
  index2 <- which(grepl("[Cc]oal",SCC$EI.Sector))
  
  # Next, index of rows that contain [Cc]oal that also contain [Cc]omb
  index22 <- which(grepl("[Cc]omb|[Cc]ombustion",SCC[ index2, ]$EI.Sector))
  
  # Collect the list of SCC indices
  list_of_scc_indices <- as.data.frame(SCC[index22,]$SCC)
  colnames(list_of_scc_indices) <- c("List")
  list_of_scc_indices
}

get_total_emissions_by_type <- function(NEI, list_of_indices){
 # select only the rows in REI column SCC which are in list_of_scc_indices
 summary4 <- NEI %>% 
             filter(SCC %in% list_of_indices$List) %>%
             select(year, Emissions) %>%
             group_by(year) %>%
             summarize(Total_Emissions = sum(Emissions, na.rm=TRUE))

 summary4$year = as.factor(summary4$year)
 summary4
}

make_plot4 <- function(){
  # source dplyr
  library(dplyr)
  library(ggplot2)
  library(RColorBrewer)
  
  # First read in NEI data
  NEI <- read_in_nei_data()
  
  # Second, read in the SCC data
  SCC <- read_in_scc_data()

  # Third, identify the list of SCC indices corresponding to coal combustion related emissions
  list_of_scc_indices <- get_combustion_type_scc_index(SCC)
  
  # Generate the subset of SCC related to coal combustion and summarize emission
  # information across the US. Since which emission information is required is unclear,
  # I select the total emission from coal combustion related data
  summary4 <- get_total_emissions_by_type(NEI, list_of_scc_indices)
  summary4$Total_Emissions <- summary4$Total_Emissions/1000
  
  # Plot summary
  g <- ggplot(summary4, aes(x=year, y=Total_Emissions, fill=year))
  g <- g + geom_bar(stat="identity")
  g <- g + xlab("Year")
  g <- g + ylab("Total PM2.5 Emissions from Coal Comsumption (kiloton)")
  g <- g + coord_cartesian(ylim=c(0,800))
  g <- g + ggtitle("Total PM2.5 Emission from Coal Combustion over select years in Baltimore City")
  g <- g + scale_fill_brewer(palette="Dark2")
  g <- g + geom_text(aes(label=round(Total_Emissions,2), vjust=-0.2))
  g <- g + guides(fill=FALSE)
  ggsave("plot4.png", plot=g)
}
