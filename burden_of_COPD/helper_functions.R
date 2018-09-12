
costToMill <- function(cost_vector){
  figures <- c()
  for(i in 1:length(cost_vector)){
    figure <- cost_vector[i]/1000000
    if(figure>10){
      figure <- cost_vector[i]/1000000000
      figure <- paste0("$", as.character(round(figure, 2)), " Billion")
    } else if (cost_vector[i]==0) {
      figure <- "No Data"
    } else{
      figure <- paste0("$", as.character(round(figure, 2)), " Million")
    } 
    figures <- c(figures, figure)
  }
  return(figures)
}