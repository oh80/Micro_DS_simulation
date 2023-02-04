library(magrittr)
#install.packages("glmnet")
#library(glmnet)


main <- function(){
  set.seed(1113)
  data <- readRDS(file = "01_data/data/data.obj")
  
  sampled_data <- data %>% select_col(terms = 20) %>% 
    make_terms(terms = 20) %>% 
    generate_sample(times = 5)
  
  lambda <- get_best_lambda(sampled_data)
  
  
  
  return(lambda)
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


l <- main()

