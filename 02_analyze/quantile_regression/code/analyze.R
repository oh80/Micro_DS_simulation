#library(magrittr)
#install.packages('quantreg')
#library(quantreg)

main <- function(){
  data <- readRDS(file = "01_data/data/data.obj") 
  
  linear_reg_table <- data %>% linear_reg() %>% make_table_lm()
  
  
  return(linear_reg_table)
}


linear_reg <- function(input){
  output <- lm(data=input,
               income~physical_ability)
  return(output)
}


make_table_lm <- function(input){
  summary <- summary(input)
  output <- kableExtra::kable(summary$coefficients[(1:2),(1:2)],
                              format = "latex",
                              digits = 3)
  return(output)
}

main()

