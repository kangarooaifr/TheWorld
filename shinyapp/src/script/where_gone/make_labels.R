

make_labels <- function(data){

  
  # prepare labels
  labels <- sprintf(
    
    # pattern
    "<strong>%s</strong><br><br>%s<br><i>%s</i>",
    
    # result for selected candidate / nuance
    paste(data$country),
    paste(data$city),
    
    paste("I was there.")
    
  ) %>% lapply(htmltools::HTML)
  

  # return
  labels
  
}