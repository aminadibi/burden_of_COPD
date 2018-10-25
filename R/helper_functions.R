
costToMill <- function(cost_vector, char = TRUE){
  figures <- c()
  for(i in 1:length(cost_vector)){
    figure <- cost_vector[i]/1000000
    if(figure>=1000){
      figure <- cost_vector[i]/1000000000
      figure <- round(figure,2)
      figure <- formatC(figure, format = 'f', digits = 1)
      if(char){
        figure <- paste0(as.character(figure), " Billion")
      }
    } else if (cost_vector[i]==0) {
      figure <- "No Data"
    } else{
      figure <- round(figure, 2)
      figure <- formatC(figure, format = 'f', digits = 1)
      if(char){
        figure <- paste0(as.character(figure), " Million")
      }
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

getCost <- function(data, provinces){
  pop <- c()
  prov_new <- c()
  for(i in 1:length(provinces)){
    prov <- provinces[i]
    cost_prov <- subset(data, ((province %in% prov)))
    cost_prov <- cost_prov$value
    if(length(cost_prov)==0){
      cost_prov <- 0
    }
    pop <- c(pop, cost_prov)
    prov_new <- c(prov_new, rep(prov, length(cost_prov)))
  }
  pop <- as.data.frame(pop)
  pop$provinces <- prov_new
  return(pop)
}

customLegend <- function(x){
  return(0)
}

myLabFormat <- function(
  prefix = "", suffix = "", between = " &ndash; ", digits = 3, big.mark = ",",
  transform = identity
) {
  
  formatNum <- function(x) {
    x <- round(transform(x), digits)
   
    if(max(x)>1000000){
      x <- costToMill(x, char=TRUE)
    }

    return(x)
  }
  
  function(type, ...) {
    switch(
      type,
      numeric = (function(cuts) {
        paste0(prefix, formatNum(cuts), suffix)
      })(...), # nolint
      bin = (function(cuts) {
        n <- length(cuts)
        paste0(prefix, formatNum(cuts[-n]), between, formatNum(cuts[-1]), suffix)
      })(...), # nolint
      quantile = (function(cuts, p) {
        n <- length(cuts)
        p <- paste0(round(p * 100), "%")
        cuts <- paste0(formatNum(cuts[-n]), between, formatNum(cuts[-1]))
        # mouse over the legend labels to see the values (quantiles)
        paste0(
          "<span title=\"", cuts, "\">", prefix, p[-n], between, p[-1], suffix,
          "</span>"
        )
      })(...), # nolint
      factor = (function(cuts) {
        paste0(prefix, as.character(transform(cuts)), suffix)
      })(...) # nolint
    )
  }}
  




