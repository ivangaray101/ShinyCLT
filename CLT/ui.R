library(shiny)
library(ggplot2)

ui = fluidPage(
  titlePanel("Central Limit Theorem", windowTitle = "Central Limit Theorem"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = 'dist',
                  label = 'Distribution',
                  choices = c('Beta' = 'rbeta', 'Binomial' = 'rbinom',
                              'Chi-squared' = 'rchisq', 'Exponential' = 'rexp',
                              'Gamma' = 'rgamma', 'Geometric' = 'rgeom',
                              'Normal' = 'rnorm',
                              'Poisson' = 'rpois','Uniform' ='runif'),
                  selected = 'rnorm'),
      
      # You can choose Histogram or Density with the checkboxes.
      
      checkboxGroupInput(inputId = 'hist_dens_poblacion',
                         label = 'Population Plot :',
                         choices = c('Histograma' = 'hist_plot', 'Density' = 'dens_plot'),
                         selected = c('hist_plot', 'dens_plot'),
                         inline = TRUE),
      
      checkboxGroupInput(inputId = 'hist_dens_mean',
                         label = 'CLT Sample Plot :',
                         choices = c('Histogram' = 'hist_plot', 'Density' = 'dens_plot'),
                         selected = c('hist_plot', 'dens_plot'),
                         inline = TRUE),
      
      sliderInput(inputId = 'pob_size',
                  label = 'Population Size :',
                  value = 200,
                  min = 100,
                  max = 10000),    
      
      sliderInput(inputId = 'sample_size', 
                  label = 'Sample Size :', 
                  value = 30,
                  min = 2, 
                  max = 100),
      
      sliderInput(inputId = 'repeticiones',
                  label = 'Iterations :',
                  value = 200,
                  min = 100,
                  max = 10000),
      
      # Parameters for the distributions.
      
      uiOutput('minmax'), 
      uiOutput('lambda'), 
      uiOutput('n'),      
      uiOutput('p'),      
      uiOutput('a'),      
      uiOutput('b'),     
      uiOutput('mu'),     
      uiOutput('sigma'),  
      uiOutput('df'),     
      
      helpText('UBA - FCEN - Argentina, Buenos Aires'),
      helpText('CLT Shiny App created by :'),
      helpText('Cruz, Sebastian Castaño'),
      helpText('Garay, Iván Alejandro'),
      

    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = 'Graphs',
          textOutput('plot_poblacion_titulo'),
          verbatimTextOutput('plot_poblacion_advertencia'),
          plotOutput('plot_poblacion'),
          
          textOutput('plot_tcl_titulo'),
          verbatimTextOutput('plot_tcl_advertencia'),
          plotOutput('plot_tcl'),
          
        ),
        
        tabPanel(
          title = 'To Remember',
          uiOutput('info')
        )        
      )
      
    )    
    
  )
  
)
