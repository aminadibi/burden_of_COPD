
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

provinceConvert <- function(provinces, to, quebec=1){
  short <- c("AB", "BC", "SK", "MB", "ON", "QC", "NL", "NT", "NU", "PE", "YT", "NS", "NB", "QC")
  long <- c("Alberta", "British Columbia", "Saskatchewan", "Manitoba",
            "Ontario", "Québec", "Newfoundland and Labrador", "Northwest Territories",
            "Nunavut", "Prince Edward Island", "Yukon", "Nova Scotia", 
            "New Brunswick", "Quebec")
  
  convert <- c()
  if(to=="toShort"){
    for(p in provinces){
      convert <- c(convert, short[which(long==p)])
    }
  } else {
    for(p in provinces){
      l <- long[which(short==p)]
      if(p=="QC"){l <- l[quebec]}
      convert <- c(convert, l)
    }
  }
  return(convert)
}