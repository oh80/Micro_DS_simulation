#library(magrittr)


main <-function(){
  data <- readRDS(file = "01_data/data/data.obj")
  
  probit_model <- data %>% probit_reg()
  linear_model <- data %>% linear_reg()
  
  #限界効果と閾値パラメータの推定を行う予定
  
  return(probit_model)
}


probit_reg <- function(input){
  output <- glm(formula = score~physical_ability,
                family = binomial(probit),
                data = input)
  return(output)
}

linear_reg <- function(input){
  output <- lm(formula = score~physical_ability,
               data = input)
  return(output)
}

pm <- main()



