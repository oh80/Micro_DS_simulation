library(magrittr)


main <-function(){
  data <- readRDS(file = "01_data/data/data.obj")
  
  probit_model <- data %>% probit_reg() 
  linear_model <- data %>% linear_reg()
  
  marginal_effect_rable <- data %>% get_values(probit_model,linear_model) %>% 
    get_table()
  
  marginal_effect_rable %>% save_table()
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


get_values <- function(input_data,input_model1,input_model2){
  linear_maginal_effect <- input_model2$coefficients[2]
  
  probit_hidden_param <- c(input_model1$coefficients[1],input_model1$coefficients[2])
  probit_marginal <- dnorm(probit_hidden_param[1]
                           +probit_hidden_param[2]*input_data$physical_ability,
                   mean = 0, sd = 1)*probit_hidden_param[2]
  probit_maginal_mean <- mean(probit_marginal)
  
  marginal_effects <- c(linear_maginal_effect,probit_maginal_mean)
  return(marginal_effects)
}


get_table <- function(input){
  output <- kableExtra::kable(t(input),
                              digits = 5,
                              col.names = c("linear model","probit model"),
                              format = "latex")
  return(output)
}


save_table <- function(input){
  path <- "02_analyze/probit_regression/output/table.txt"
  writeLines(input,path)
}


main()
