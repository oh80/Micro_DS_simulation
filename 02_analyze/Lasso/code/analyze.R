library(magrittr)
#install.packages("glmnet")
#library(glmnet)


main <- function(){
  set.seed(1113)
  data <- readRDS(file = "01_data/data/data.obj")
  
  sampled_data <- data %>% select_col(terms = 20) %>% 
    make_terms(terms = 20) %>% 
    generate_sample(times = 5)
  
  lambda <- sampled_data %>% get_best_lambda()
  
  Lasso_model <- sampled_data %>% Lasso_reg(param = lambda)
  
  my_plot <- sampled_data %>% plot_scatter()
  
  coef_table <- Lasso_model %>% make_coef_table()
  
  save_my_plot(my_plot)
  save_my_table(coef_table)
}


select_col <- function(input,terms){
  output <- tidyr::tibble(z=input$income,
                          x1 = input$physical_ability)
  return(output)
}


make_terms <- function(input,terms){
  multi_term <- data.frame()
  col_name <-c()
  
  for(i in 1:(terms-1)){
    multi_term[(1:length(input$x1)),i] <- input$x1^(i+1)
    col_name[i] <- paste("x",i+1,sep="")
  }
  colnames(multi_term) <- col_name
  output <- dplyr::bind_cols(input,multi_term)

  return(output)
}


generate_sample <- function(input,times){
  output <- list()
  for (i in 1:times){
    output[[i]] <- input[sample(1:nrow(input),100,replace = TRUE),]
  }
  return(output)
}


get_best_lambda <- function(input){
  times <- length(input)
  output <- c()
  
  for (i in 1:times){
      x = input[[i]] %>% dplyr::select(-z) %>% as.matrix()
      y = input[[i]] %>% dplyr::select(z)%>% unlist()
      lasso_cv <- glmnet::cv.glmnet(x = x,y = y,
                            nfold = 5,
                            family = "gaussian", alpha = 1)
      output[i] <- lasso_cv$lambda.min
  }
  return(output)
}


Lasso_reg <- function(input,param){
  output <- list()
  for (i in 1:length(param)) {
    x = input[[i]] %>% dplyr::select(-z) %>% as.matrix()
    y = input[[i]] %>% dplyr::select(z)%>% unlist()
    output[[i]] <- glmnet::glmnet(x = x, y = y,
                                family = "gaussian", lambda = param[i], alpha = 1)
  }
  return(output)
}


plot_scatter <- function(input){
  output <- list()
  for (i in 1:length(input)) {
    output[[i]] <- ggplot2::ggplot(data=input[[i]],
                                   mapping = ggplot2::aes(x=x1,y=z))+
      ggplot2::geom_point()+
      ggplot2::theme_minimal()+
      ggplot2::labs(title = paste0("sample",i,"  ","scatter"))
  }
  return(output)
}


make_coef_table <- function(input){
  output <- list()
  for (i in 1:length(input)){
    output[[i]] <- kableExtra::kable(as.array(input[[i]]$beta),
                                     digits = 3,
                                     col.names = "beta",
                                     format = "latex")
  }
  return(output)
}


save_my_plot <- function(input){
  my_path <- "02_analyze/Lasso/output"
  
  for (i in 1:length(input)) {
    file_name = paste0("/scatter_sample",i)
    ggplot2::ggsave(plot = input[[i]],
                    filename = paste0(file_name,".pdf"),
                    path = my_path)
  }
}


save_my_table <- function(input){
  my_path <- "02_analyze/Lasso/output"
  
  for (i in 1:length(input)){
    file_name = paste0("/Lasso_coef",i,".txt")
    path <- paste0(my_path,file_name)
    writeLines(input[[i]],path)
  }
}

main()
