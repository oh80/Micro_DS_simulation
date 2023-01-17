main <- function(){
  set.seed(1113)
  physical_ability <- generate_physical_ability(number=100,
                                                mean=3,
                                                variance=2) 
  
  score <- generate_score(physical_ability)
  
  income <- generate_income(physical_ability)
  
  master_data <- master(physical_ability,
                        score,
                        income)
  
  saveRDS(master_data,
          file = "01_data/data/data.obj")
}


generate_physical_ability <- function(number,mean,variance){
  sd <- sqrt(variance)
  output <- exp(rnorm(number,
                  mean,
                  sd))
  return(output)
}


generate_score <- function(input){
  epsilon <- rnorm(length(input),
                   mean = 0,
                   sd = 1)
  hidden_pram <- log(input)+epsilon
  output <- c()
  threshold <- 4
  
  for (i in 1:length(hidden_pram)) {
    if(hidden_pram[i] >= threshold){
      output[i] <- 1
    }else{
      output[i] <- 0
    }
  }
  return(output)
}


generate_income <- function(input){
  epsilon <- rnorm(length(input),
                   mean = 0,
                   sd = 1)
  output <- 0.1*input-0.3*input^3+0.5*input^5-0.7*input^7+epsilon

  return(output)
}


master <- function(input1,input2,input3){
  output <- data.frame("physical_ability" = input1,
                       "score" = input2,
                       "income" = input3)
  return(output)
}


main()
