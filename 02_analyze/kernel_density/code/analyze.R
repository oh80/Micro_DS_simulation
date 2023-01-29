#install.packages("kdensity")
#library("kdensity")
#library(magrittr)
\

main <- function(){
  
  data <- readRDS(file = "01_data/data/data.obj")
  
  density <- data %>% estimate_density_nrd0() %>% 
    plot_density()
  
  density_ucv <- data %>% estimate_density_ucv() %>% 
    plot_density_ucv()
  
  density_bcv <- data %>% estimate_density_bcv() %>% 
    plot_density_bcv()
}


estimate_density_nrd0 <- function(input){
  output <- kdensity(input$physical_ability)
  #default bandwidth is "nrd0"
  return(output)
}


estimate_density_ucv <- function(input){
  output <- kdensity(input$physical_ability,
                     bw = "ucv")
  return(output)
}


estimate_density_bcv <- function(input){
  output <- kdensity(input$physical_ability,
                     bw = "bcv")
  return(output)
}


plot_density <- function(input){
  pdf("02_analyze/kernel_density/output/kernel_density.pdf")
  input %>% plot(main = "kernel_density",
                           ylab = "density",
                           xlab = "physical_ability")
  dev.off()
}

plot_density_ucv <- function(input){
  pdf("02_analyze/kernel_density/output/kernel_density_ucv.pdf")
  input %>% plot(main = "kernel_density",
                 ylab = "density",
                 xlab = "physical_ability")
  dev.off()
}


plot_density_bcv <- function(input){
  pdf("02_analyze/kernel_density/output/kernel_density_bcv.pdf")
  input %>% plot(main = "kernel_density",
                 ylab = "density",
                 xlab = "physical_ability")
  dev.off()
}


main()
