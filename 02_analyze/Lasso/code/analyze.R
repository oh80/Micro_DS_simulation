#library(magrittr)

main <- function(){
  set.seed(1113)
  data <- readRDS(file = "01_data/data/data.obj")
  
  sampled_data <- data %>% generate_sample(times = 5)
  
  
  
  return(sampled_data)
}


generate_sample <- function(input,times){
  output <- list()
  for (i in 1:times){
    output[[i]] <- input[sample(1:nrow(input),100),]
  }
  return(output)
}

s <- main()


