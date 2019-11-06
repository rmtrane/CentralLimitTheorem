#sim_data <- read_rds("statistic_values.Rds")

prep_for_hist <- function(sim_data, sample_size){
  
  min_max <- range(sim_data$sum_stat)
  
  es <- str_split(format(min_max, scientific = T), pattern = "e-|e+",simplify = T)[,2] %>% as.numeric
  
  hist_range <- (min_max*10^(es+1)+c(-1,1))/(10^(es+1))
  
  n_unique <- length(unique(sim_data$sum_stat)) 
  bin_width <- case_when(n_unique <= 25 ~ as.double(diff(hist_range)/n_unique),
                         n_unique <= 2*sample_size ~ as.double(diff(hist_range)/(exp(floor(log(n_unique))))),
                         #TRUE ~ diff(hist_range)/(nrow(sim_data)/100))
                         TRUE ~ as.double(diff(hist_range)/(exp(floor(log(n_unique, 2))))))
  
  bin_cuts <- c(hist_range, 
                seq(from = hist_range[1], to = hist_range[2], by = bin_width))
  bin_cuts <- sort(unique(bin_cuts))
  
  tmp <- sim_data %>% 
    select(-ids_included) %>% 
    mutate(bins = cut(sum_stat, breaks = bin_cuts, include.lowest = TRUE))
  
  bins_centers <- tmp %>% select(bins) %>% 
    unique() %>% 
    mutate(bin_start = str_extract(bins, pattern = "\\d+\\.\\d+,") %>% 
             str_remove(pattern = ",") %>% as.numeric,
           bin_end = str_extract(bins, pattern = ",\\d+\\.\\d+") %>% 
             str_remove(pattern = ",") %>% as.numeric,
           bin_center = (bin_end + bin_start)/2,
           bins = paste("bins", as.numeric(bins), sep = "_"))
  
  for_histogram <- tmp %>% select(i, bins, sum_stat) %>% 
    mutate(bins = paste("bins", as.numeric(bins), sep = "_"),
           for_fill = 1) %>% 
    spread(bins, for_fill, fill = 0) %>% 
    mutate_at(vars(contains("bins_")), cumsum) %>% 
    gather(key = 'bins', value = "cumsum", contains("bins_")) %>% 
    left_join(bins_centers) %>% 
    as.data.table()
  
  return(list(for_histogram = for_histogram, bin_width = bin_width))
}


if(FALSE){
plot_ly(data = for_histogram[i == 10000,]) %>%
  add_trace(
    x = ~bin_center,
    y = ~cumsum/i,
    type = 'bar',
    width = bin_width
  ) %>%
  # add_trace(
  #   x = ~sum_stat,
  #   y = 0,
  #   type = 'scatter',
  #   mode = 'markers'
  # ) %>%
  layout(
    bargap = 0,
    xaxis = RVs$xaxis,
    # shapes = list(vline(RVs$true_value),
    #               vline(x = unique(RVs$stat_values_as_datatable[i == input$N, sum_stat]), color = 'blue')),
    showlegend = FALSE
  )
}
