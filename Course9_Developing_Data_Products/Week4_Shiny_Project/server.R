#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  # Specify where to find the data
  #url1 <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00291/airfoil_self_noise.dat"
  #df1 <- read.csv(url(url1),sep='\t',header=FALSE)
  df1 <- read.csv("airfoil_self_noise.dat",sep='\t',header=FALSE)
  
  # Assign colum names
  colnames(df1) <- c("frequency","angle","length","velocity","thickness","pressure")

  # Load the libraries
  library(ggplot2)
  library(dplyr)
  library(broom)
  library(kableExtra)
  
  # Read the number of bootstrap iterations from the input reactively
  # Since we are estimating coefficients based on the user input, we need a reactive statement
  # As fitdf is the key quantity that directly depends on the input, evaluate it here
  out_reg <- reactive({
    nboots <- input$sliderBoot
    # Fit the linear model nboots times
    fitdf <- df1 %>%
      bootstrap(nboots) %>%
      do(tidy(lm(pressure~frequency+angle+length+velocity+thickness,.)))
    
    # Get the 95% confidence interval quantiles, mean, and median values
    alpha = .05
    coeff_stats <- fitdf %>% group_by(term) %>%
      summarize(low=quantile(estimate, alpha / 2),
                mean=mean(estimate),
                median=median(estimate),
                high=quantile(estimate, 1 - alpha / 2)
      )
    return(list( alldata=fitdf, allcoeff=coeff_stats))
  })
  
  # Render a plot for the distribution of each coefficient for the user display
  output$distr_plot <- renderPlot({
    # Get the list of output
    mydat <- out_reg()
    
    # Extract fitdf
    fitdf <- mydat$alldata
    
    # Extract coeff_stats
    coeff_stats <- mydat$allcoeff
    
    g <- ggplot(fitdf, aes(estimate, color=term, fill=term))+ guides(fill=FALSE,color=FALSE)
    g <- g + geom_histogram(bins=30,alpha=0.2) + theme(legend.position="none")
    g <- g + facet_wrap(~ term, scales="free")
    g <- g + geom_vline(data=coeff_stats, aes(xintercept=median, color="black"),linetype="dashed",show.legend=FALSE)
    g <- g + geom_vline(data=coeff_stats, aes(xintercept=mean, color="red"),linetype="dotted",show.legend=FALSE)
    g
  })
  
 # Render a table of the quantiles for each coefficient
 output$coeff_table <- renderTable({
   # Get the list of output
   mydat <- out_reg()
   
   # Extract coeff_stats
   coeff_stats <- mydat$allcoeff
   
   # Fit linear model to data. Append the coefficients from linear model to coeff_stats
   fit1 <- tidy(lm(pressure~frequency+angle+length+velocity+thickness,df1))
   merged_stats <- merge(coeff_stats, fit1, by="term")
   merged_stats <- merged_stats[,c("term","low","mean","median","high","estimate")]
   colnames(merged_stats)[6] <- "lm"
   merged_stats <- tbl_df(merged_stats)
   
   # Calculate the median, mean, and 95% confidence interval values for each coefficient
   merged_stats
 }, hover=TRUE, striped=TRUE, bordered=TRUE, digits=5
 )
 
 # Done
})
