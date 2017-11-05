#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Boostrap Regression: Illustrating the Central Limit Theorem"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h5("The Airfoil dataset contains the measurements for the scaled sound pressure level(decibels) measured
        when an airfoil (wing) is placed in a wind tunnel. The pressure is modeled as a quantity dependent
        on 1. Frequency (Hertzs), 2. Angle of attack (degrees), 3. Chord length (meters), 4. Free-stream
        velocity (meters per second), and 5. Suction side displacement thickness (meters). There are 1503
         measurements in the dataset. Keep in mind that the sound pressure is non-linearly dependent on the variables."),
      h5("Here, a multivariate linear model is fit to the Airfoil dataset. Boostrap sampling with replacement
        is used to calculate the coefficients. Due to the non-linearities in the dataset, the linear model
         is used for illustrative purposes only and has no predictive value"),
      h5("The objective is to understand the effect of varying the number of bootstrap samples and demonstrating
         the implications of the Central Limit Theorem on the basis of:"),
      h5("1. The shape of the distribution of coefficients"),
      h5("2. The mean, median, and 95% confidence interval"),
       sliderInput("sliderBoot",
                   "Select: Number of Bootstrap samples:",
                   min = 10,
                   max = 2500,
                   value = 100),
      submitButton("Submit", icon("refresh"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Coefficients: Distribution",
                 h5("Six coefficients including one intercept are calculated for each bootstrap sample. The
                    distribution of values for each coefficient is dislayed below"),
                 plotOutput("distr_plot"),
                 h5("In the figure above, the vertical lines correspond to the position of the mean (dotted) and median (dashed)."),
                 h5("As number of bootstrap samples increases:"),
                 h5(" 1. The mean and median tend towards each other"),
                 h5(" 2. For each coefficient, the distribution of values tends towards a Gaussian distribution as per the Central Limit Theorem")),
        tabPanel("Coefficients: Summary Values", 
                 h5("The mean and median values for each coefficient are tabulated below, along with
                    the lower and upper 95% confidence interval values, low and high respectively"),
                 tableOutput("coeff_table"),
                 h5("Note the following:"),
                 h5("1. The difference between the mean and median coefficient values tends to vanish
                    as number of bootstrap samples increases. This should be apparent in the distribution
                    of coefficients as well"),
                 h5("2. A simple linear model without bootstrap sampling gives coefficients (in the lm column)
                    that are close to the mean of the distribution. Given the large size of the
                    dataset (1503 observations), this result should not be surprising."))
      )
    )
  )
))
