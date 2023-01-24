main <- function(){
  #library(magrittr)
  #library(ggplot2)
  data <- readRDS(file = "01_data/data/data.obj")
  
  histgram_m <- plot_hist_m(data)
  histgram_h <- plot_hist_h(data)
  histgram_l <- plot_hist_l(data)
  
  save_my_plot(histgram_m,histgram_h,histgram_l)
}



plot_hist_m <- function(input_data){
  output <- input_data %>% ggplot2::ggplot(mapping = aes(x=physical_ability))+
    ggplot2::theme_minimal() +
    ggplot2::geom_histogram(binwidth = 20,
                            fill = "cadetblue3",
                            colour = "black")+
    ggplot2::labs(title = "bin-20 histgram")
  return(output)
}


plot_hist_h <- function(input_data){
  output <- input_data %>% ggplot2::ggplot(mapping = aes(x=physical_ability))+
    ggplot2::theme_minimal() +
    ggplot2::geom_histogram(binwidth = 50,
                            fill = "cadetblue1",
                            colour = "black")+
    ggplot2::labs(title = "bin-50 histgram")
  return(output)
}


plot_hist_l <- function(input_data){
  output <- input_data %>% ggplot2::ggplot(mapping = aes(x=physical_ability))+
    ggplot2::theme_minimal() +
    ggplot2::geom_histogram(binwidth = 5,
                            fill = "cadetblue4",
                            colour = "black")+
    ggplot2::labs(title = "bin-5 histgram")
  return(output)
}


save_my_plot <- function(input1,input2,input3){
  
  my_path <- "02_analyze/histgram/output"
  
  ggplot2::ggsave(plot = input1,filename = "hist_m.pdf",
                  path = my_path)
  
  ggplot2::ggsave(plot = input2,filename = "hist_h.pdf",
                  path = my_path)
  
  ggplot2::ggsave(plot = input3,filename = "hist_l.pdf",
                  path = my_path)
}


main()
