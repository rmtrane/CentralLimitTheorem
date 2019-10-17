library(shiny)
library(distributions3)
library(plotly)
library(purrr)
library(dplyr)
library(tidyr)
library(shinycssloaders)
# library(promises)
# library(future)
# plan(multiprocess)

ui <- tagList(
  tags$head(
    tags$style(
      HTML(
      "#plot-container {
         position: relative;
       }
       #loading-spinner {
         position: absolute;
         left: 50%;
         top: 50%;
         z-index: -1;
         margin-top: -33px;  /* half of the spinner's height */
         margin-left: -33px; /* half of the spinner's width */
       }
       #plot.recalculating {
         z-index: -2;"
      )
    )
  ),
  navbarPage(
    "Sampling Distributions, and the Central Limit Theorem",
    tabPanel(
      "Introduction",
      radioButtons(inputId = 'real_simulated', 
                   label = "Use real or simulated data?",
                   choices = c('real', 'simulate')),
      conditionalPanel('input.real_simulated == "real"',
                       fileInput(inputId = 'file', 
                                 label = 'Choose file with data to use',
                                 accept = '.csv'),
                       actionButton("upload_data", label = 'Upload!')),
      conditionalPanel('input.real_simulated == "simulate"',
                       selectInput(inputId = 'distribution',
                                   label = 'Choose a distribution',
                                   choices = c('Normal', 'Exponential', 'Binomial', 'Beta', 'Gamma', 
                                               'LogNormal', 'NegativeBinomial', 'Poisson', 'Weibull', 'Logistic')),
                       uiOutput("distribution_parameters"),
                       actionButton(inputId = "generate_pop_data", 
                                    label = "Generate Population Data")),
      uiOutput("select_id"),
      dataTableOutput('dataTable')
    ),
    tabPanel(
      "Sampling Distributions",
      conditionalPanel('input.real_simulated == "real"',
                       uiOutput("select_variable")),
      selectInput(inputId = 'statistic', label = 'Choose Summary Statistic',
                  choices = c('mean', 'var', 'median', 'min', 'max'),
                  selected = 'mean'),
      plotlyOutput("population_distribution") %>% withSpinner(),
      numericInput(inputId = 'sample_size',
                   label = "Sample Size",
                   min = 2,
                   max = 100,
                   value = 15),
      uiOutput("sampling_dist_UI"), # %>% withSpinner(),
      plotlyOutput("sampling_dist")
    )
  )
)