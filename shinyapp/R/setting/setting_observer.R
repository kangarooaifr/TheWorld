

# -- function definition
setting_observer <- function(name, input){

  # -- return  
  observe(
    setting(name, value = input[[name]]))
  
}
