

# -- function definition
setting_input <- function(setting, ns){
  
  # -- numeric
  if(setting['type'] == "numeric")
  
    numericInput(inputId = ns(setting['name']),
                 label = setting['name'],
                 value = as.numeric(setting['value']),
                 min = 0,
                 max = 5)
  
}
