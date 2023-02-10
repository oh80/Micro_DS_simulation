library(magrittr)
#install.packages('quantreg')
#library(quantreg)

main <- function(){
  data <- readRDS(file = "01_data/data/data.obj") 
  
  linear_reg_table <- data %>% linear_reg() %>% make_table_lm() 
  
  quantile_reg_25_table <- data %>% quantile_reg(tau = 0.25) %>% make_table_qm()
  quantile_reg_50_table <- data %>% quantile_reg(tau = 0.5) %>% make_table_qm()
  quantile_reg_75_table <- data %>% quantile_reg(tau = 0.75) %>% make_table_qm()
  
  my_plot <- plot_lm_and_rq(data)
  
  save_table(linear_reg_table,
             quantile_reg_25_table,
             quantile_reg_50_table,
             quantile_reg_75_table)
  
  save_my_plot(my_plot)
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


make_table_qm <- function(input){
  summary <- summary(input)
  output <- kableExtra::kable(summary$coefficients,
                              format = "latex",
                              digits = 3)
}


quantile_reg <- function(input,tau){
  output <- quantreg::rq(data = input,
                         formula = income ~ physical_ability,
                         tau = tau)
  return(output)
}


plot_lm_and_rq <- function(input){
  lm_model <- lm(data=input,
                 income~physical_ability)
  
  rq_model_25 <- quantreg::rq(data = input,
                              formula = income ~ physical_ability,
                              tau = 0.25)
  rq_model_50 <- quantreg::rq(data = input,
                              formula = income ~ physical_ability,
                              tau = 0.5)
  rq_model_75 <- quantreg::rq(data = input,
                              formula = income ~ physical_ability,
                              tau = 0.75)
  
  output <- ggplot2::ggplot(data = input,
                            mapping = ggplot2::aes(x=physical_ability,y=income))+
    ggplot2::theme_minimal()+
    ggplot2::geom_point()+
    ggplot2::geom_abline(intercept=lm_model$coefficients[[1]],slope = lm_model$coefficients[[2]],
                         color="red")+
    
    ggplot2::geom_abline(intercept=rq_model_25$coefficients[[1]],slope = rq_model_25$coefficients[[2]],
                         color="cadetblue1")+
    
    ggplot2::geom_abline(intercept=rq_model_50$coefficients[[1]],slope = rq_model_50$coefficients[[2]],
                         color="cadetblue3")+
    
    ggplot2::geom_abline(intercept=rq_model_75$coefficients[[1]],slope = rq_model_75$coefficients[[2]],
                         color="cadetblue4")
  return(output)
}


save_table <- function(input1,input2,input3,input4){
  path1 <- "02_analyze/quantile_regression/output/lm_table.txt"
  path2 <- "02_analyze/quantile_regression/output/rq_table_25.txt"
  path3 <- "02_analyze/quantile_regression/output/rq_table_50.txt"
  path4 <- "02_analyze/quantile_regression/output/rq_table_75.txt"
  
  writeLines(input1,path1)
  writeLines(input2,path2)
  writeLines(input3,path3)
  writeLines(input4,path4)
}

save_my_plot <- function(input){
  my_path <- "02_analyze/quantile_regression/output"
  ggplot2::ggsave(plot = input,
                  filename = "lm_and_rq.pdf",
                  path = my_path)
}


 main()
