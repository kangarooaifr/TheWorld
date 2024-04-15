

route_labels <- function(origin, destination){
  
  # prepare labels
  labels <- sprintf(
    
    # pattern
    "<strong>%s</strong><br><br><strong>%s</strong><br><i>%s</i>",
    
    # result for selected candidate / nuance
    paste(origin),
    paste(destination),
    
    paste("Flying over the raibow.")
    
  ) %>% lapply(htmltools::HTML)
  
}