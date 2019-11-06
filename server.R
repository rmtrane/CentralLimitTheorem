server <- function(input, output, session) {
  ## A couple of functions to draw vertical and horizontal lines using plot_ly
  vline <- function(x = 0, color = "red") {
    list(
      type = "line", 
      y0 = 0, 
      y1 = 1, 
      yref = "paper",
      x0 = x, 
      x1 = x, 
      line = list(color = color)
    )
  }
  
  hline <- function(y = 0, color = "blue") {
    list(
      type = "line", 
      x0 = 0, 
      x1 = 1, 
      xref = "paper",
      y0 = y, 
      y1 = y, 
      line = list(color = color)
    )
  }
  
  source("prep_for_histograms.R")
  
  ## Number of samples to create
  max_resamples <- 10000
  
  ## reactiveValues object with defaults where necessary
  RVs <- reactiveValues(data = NULL,
                        input_distr = NA,
                        input_stat = NA,
                        selected_id = 'id',
                        selected_var = 'value',
                        real_simulated = '')
  
  ########################################
  #### Prepare data when real data is used
  observeEvent(input$upload_data, {
    ## Read data
    RVs$data <- readr::read_csv(file = input$file$datapath) %>% 
      mutate(row_number = row_number()) %>% 
      select(row_number, everything())
    
    ## List of all variables available
    RVs$all_vars <- colnames(RVs$data)
    
    ## Keep track of real/simulate status
    RVs$real_simulated <- input$real_simulated
    
    ## "Delete" samples in case a user wants to start over.
    if(!"samples" %in% names(RVs)){
      RVs$samples <- tibble(i = 1)
    }
    
    RVs$samples <- filter(RVs$samples, row_number() < 1)
    
  })
  
  observeEvent(input$use_framingham, {
    ## Read data
    RVs$data <- readr::read_csv(file = './data/framingham.csv') %>% 
      mutate(row_number = row_number()) %>% 
      select(row_number, everything())
    
    ## List of all variables available
    RVs$all_vars <- colnames(RVs$data)
    
    ## Keep track of real/simulate status
    RVs$real_simulated <- input$real_simulated
    
    ## "Delete" samples in case a user wants to start over.
    if(!"samples" %in% names(RVs)){
      RVs$samples <- tibble(i = 1)
    }
    
    RVs$samples <- filter(RVs$samples, row_number() < 1)
  })
  
  #### Prepare data when simulated data is used
  observeEvent(input$distribution, {
    ## Get needed arguments from distribution
    RVs$X_args <- as.list(args(input$distribution))
    ## Remove any NULL arguments
    RVs$X_args <- RVs$X_args[!map_lgl(RVs$X_args, is.null)]
  })
  
  ## Create UI elements that let users specify parameters
  output$distribution_parameters <-
    renderUI({
      if(!is.null(RVs$X_args)){
        lapply(1:length(RVs$X_args),
               function(i)
                 numericInput(inputId = names(RVs$X_args)[i],
                              label = names(RVs$X_args)[i],
                              value = ifelse(is.numeric(RVs$X_args[[i]]),
                                             RVs$X_args[[i]],
                                             1))
        )
      }
    })
  
  ## Plot PDF of distribution chosen
  output$PDF <- renderPlot({
    X <- do.call(eval(parse(text = paste0("function(...) ", input$distribution, "(...)"))),
                 args = setNames(map(names(RVs$X_args), function(x) input[[x]]), 
                                 names(RVs$X_args)))
    plot_pdf(X)
  })

  ## When button "Genrate Population Data" is clicked, simulate a population
  observeEvent(input$generate_pop_data, {
    
    ## Keep track of distribution used
    RVs$input_distr <- input$distribution
    
    ## Keep track of real/simulate status
    RVs$real_simulated <- input$real_simulated
    
    ## Create random variable X that follows the specified distribution.
    RVs$X <- do.call(eval(parse(text = paste0("function(...) ", input$distribution, "(...)"))),
                     args = setNames(map(names(RVs$X_args), function(x) input[[x]]), 
                                     names(RVs$X_args)))
    
    ## Generate 10,000 data points
    RVs$data <- tibble(id = 1:10000,
                       value = random(RVs$X, n = 10000))
    
    ## "Delete" samples in case a user wants to start over.
    if(!"samples" %in% names(RVs)){
      RVs$samples <- tibble(i = 1)
    }
    
    RVs$samples <- filter(RVs$samples, row_number() < 1)
    
    ## List of all variables
    RVs$all_vars <- colnames(RVs$data)
    
    ## Update selected_var 
    RVs$selected_var <- "value"
    
  })
  
  
  
  ##########################################
  #### Prepare data table with population data
  output$dataTable <- renderDataTable({
    if(RVs$real_simulated == input$real_simulated)
      if(input$real_simulated == 'simulate'){
        if(input$distribution == RVs$input_distr & input$generate_pop_data > 0)
          RVs$data
      } else {
        RVs$data
      }
  })
  
  
  ## UI element to select ID variable
  output$select_id <- renderUI({
    if((input$upload_data > 0 | input$use_framingham > 0) & input$real_simulated != "simulate"){
      fluidRow(
        column(
          12,
          selectInput(inputId = "selected_id", 
                      label = "Select variable to use as ID",
                      choices = RVs$all_vars),
          p("Note: if you don't have an ID variable in your data, use the 'row_number' variable created.")
        )
      )
    }
  })
  
  ##########################################
  #### UI elements to select which variable to use
  output$select_variable <- renderUI({
    selectInput(inputId = "selected_var",
                label = 'Select a variable',
                choices = RVs$all_vars[RVs$all_vars != input$selected_id])
  })
  
  ## When selected variable or statistic is changed, or new data uploaded/generated,
  ## update a few things
  calc_stat <- reactive({
    list(input$selected_var,
         input$statistic,
         input$generate_pop_data,
         input$upload_data,
         input$use_framingham)
  })
  
  ## When selected variable, or statistic is changed, delete previously generated samples
  observeEvent({
    input$selected_var
    input$statistic
  }, {
    if(!"samples" %in% names(RVs)){
      RVs$samples <- tibble(i = 1)
    }
    
    RVs$samples <- filter(RVs$samples, row_number() < 1)
  })
  
  observeEvent(calc_stat(), {
    if((input$upload_data + input$use_framingham > 0 & !is.null(input$selected_var)) | input$generate_pop_data > 0 ){
      ## If real data is used, keep track of selected variable
      if(input$real_simulated != 'simulate'){
        RVs$selected_var <- input$selected_var
        RVs$selected_id <- input$selected_id
      }

      ## For debug purposes      
      # print(paste("selected_id:", input$selected_id))
      # print(paste("selected_var:", input$selected_var))
      
      ## Create xaxis
      RVs$xaxis <- list(title = RVs$input_stat, range = range(RVs$data[[RVs$selected_var]], na.rm = T))
      
      ## Create function that simply passes arguments to the specified statistic
      RVs$stat_func <- eval(parse(text = paste0("function(...)", input$statistic, "(...)")))
      
      ## Calculate "true value", i.e. calculate statistic on entire population
      RVs$true_value <- RVs$stat_func(RVs$data[[RVs$selected_var]], na.rm = T)
      
    }
    
  })
  
  
  ## Create histogram of entire population data
  output$population_distribution <- renderPlotly({
    if((input$upload_data + input$use_framingham > 0 & !is.null(input$selected_var)) | input$generate_pop_data > 0){
      plot_ly(data = RVs$data,
              x = as.formula(paste("~", RVs$selected_var)),
              #key = ~id,
              source = 'populationDistribution',
              type = 'histogram',
              histnorm = "probability density") %>% 
        layout(
          title = paste("Distribution of", RVs$selected_var),
          shapes = vline(RVs$true_value)
        )
    }
  })
  
  ## Create samples by sampling from the population
  observeEvent(input$generate_samples, {
    ## First, get subset with only relevant variables, i.e. 'selected_id' and 'selected_var'
    RVs$complete_subset <- RVs$data %>% 
      select(!!sym(RVs$selected_id), !!sym(RVs$selected_var)) %>% 
      filter(complete.cases(.))
    
    ## Update statistic used
    RVs$input_stat <- input$statistic
    
    ## Start resampling
    print("tic")
    tictoc::tic()
    RVs$samples <- map_dfr(1:max_resamples,
                           function(x){
                             tmp <- sample_n(RVs$complete_subset, size = input$sample_size) %>%
                               mutate(i = x)
                             
                             return(tmp)
                           })
    
    ## Calculate summary statistic, and create string with id's in sample (comma separated)
    RVs$statistic_values <- RVs$samples %>% 
      group_by(i) %>% 
      summarise(sum_stat = RVs$stat_func(!!sym(RVs$selected_var)),
                errors = sd(!!sym(RVs$selected_var))*1.96/sqrt(input$sample_size),
                ids_included = paste(!!sym(RVs$selected_id), collapse = ', ')) %>% 
      ungroup() %>% 
      mutate(covers = case_when(RVs$true_value < sum_stat + errors & RVs$true_value > sum_stat - errors ~ "yes",
                               TRUE ~ "no"))
      # mutate(CI_lower = sum_stat - 1.96*sds/sqrt(input$sample_size),
      #        CI_lower = sum_stat + 1.96*sds/sqrt(input$sample_size))
    
    RVs$stat_values_as_datatable <- as.data.table(RVs$statistic_values)
    
    #write_rds(x = RVs$statistic_values, path = "statistic_values.Rds")
    
    RVs$for_histogram <- prep_for_hist(sim_data = RVs$statistic_values, 
                                       sample_size = input$sample_size)
    
    # for (i in 1:length(RVs)){
    #   write_rds(RVs[[names(RVs)[i]]], path=paste0("data/RVs/", names(RVs)[i], ".Rds"))  
    # }
    
    
    print("toc")
    tictoc::toc()
    
  })
  
  ## Sampling Distribution UI:
  ## Either slider to pick N (number of resamples), or button to perform sampling
  output$sampling_dist_UI <- renderUI({
    
    #if(is.null(input$generate_samples) | !is.null(input$generate_samples)){
      if( (sum(RVs$samples$i == 1) == input$sample_size) & 
          (RVs$input_stat == input$statistic) &
          (RVs$real_simulated == input$real_simulated) &
          (nrow(RVs$samples) > 0) ){
        sliderInput(inputId = "N", 
                    label = "Number of samples",
                    min = 1, max = max_resamples, value = 1, step = 1, 
                    animate = animationOptions(interval = 350))
      } else {
        actionButton(inputId = "generate_samples",
                     label = "Let's begin")
      }
    #}
  })

  ## Create histogram of the calculated values of the chosen test statistic
  output$sampling_dist <- renderPlotly({
    
    if(!is.null(input$N) & 
       (sum(RVs$samples$i == 1) == input$sample_size) & 
       (RVs$input_stat == input$statistic) ){ #&
        # (input$generate_samples > 0)
      
      # plot_ly(
      #   data = RVs$stat_values_as_datatable[i <= input$N,],
      #   x = ~sum_stat,
      #   type = 'histogram',
      #   histnorm = 'probability density'
      # ) %>% 
      #   add_trace(
      #     x = ~sum_stat,
      #     y = 0,
      #     type = 'scatter',
      #     mode = 'markers'
      #   ) %>% 
      #   layout(
      #     xaxis = RVs$xaxis,
      #     shapes = list(vline(RVs$true_value),
      #                   vline(x = RVs$stat_values_as_datatable[i == input$N, sum_stat], color = 'blue')),
      #     showlegend = FALSE
      #   )
      
      plot_ly(data = RVs$for_histogram$for_histogram[i == input$N,]) %>% 
        add_trace(
          x = ~bin_center,
          y = ~cumsum/i,
          type = 'bar',
          width = RVs$for_histogram$bin_width
        ) %>% 
        layout(
          bargap = 0,
          xaxis = RVs$xaxis,
          shapes = list(vline(RVs$true_value),
                        vline(x = RVs$stat_values_as_datatable[i == input$N, sum_stat], color = 'blue')),
          showlegend = FALSE
        )
    } 
  })
  
  ## Display 95% CIs IF statistic is 'mean'
  # output$CIs <- renderPlotly({
  #   if(!is.null(input$N) &
  #      (sum(RVs$samples$i == 1) == input$sample_size) &
  #      (RVs$input_stat == input$statistic) &
  #      (RVs$input_stat == "mean")){ #&
  #     #(input$generate_samples > 0)
  # 
  # 
  #     plot_ly(
  #       data = RVs$stat_values_as_datatable[i <= input$N,],
  #       x = ~sum_stat,
  #       y = ~i,
  #       type = 'scatter',
  #       color = ~covers,
  #       error_x = ~list(array = errors)
  #     ) %>%
  #       layout(
  #         # xaxis = RVs$xaxis,
  #         yaxis = list(range = c(max(input$N-50, 0), max(input$N, 50))),
  #         shapes = list(vline(RVs$true_value)),
  #         #               vline(x = filter(RVs$statistic_values, i == input$N)$sum_stat, color = 'blue')),
  #         showlegend = FALSE
  #       )
  #   }
  # })

  ## Create UI element to select and show specific sample
  output$select_sample_to_show <- renderUI({
    numericInput("select_sample_to_show",
                 label = "Select Sample to Show",
                 value = 1,
                 min = 1, max = input$N,
                 step = 1)
  })
  
  observeEvent(input$show_sample, {
    RVs$sample_to_show <- RVs$samples %>% filter(i == input$select_sample_to_show)
  })
  
  output$sample_to_show <- renderDataTable({
    RVs$sample_to_show
  })
  
  
}