source('./R/helper_functions.R')
#source('../R/initialize.R')
library(RColorBrewer)

setClass(
  # Set the name for the class
  "mapData",
  
  # Define the slots
  slots = c(
    col_range = "numeric",
    scale_size = "numeric",
    costYear = "data.frame",
    costAll = "data.frame",
    costYearNoType = "data.frame",
    types = "logical",
    typesList = "character",
    min_pop = "numeric",
    max_pop = "numeric",
    regions = "SpatialPolygonsDataFrame",
    prov_red = "character",
    costDensity = "numeric",
    provinces = "character",
    pal="character",
    palette="character",
    group="character",
    plotLabel = "character",
    digits = "numeric",
    prefix = "character",
    legendLabels = "character"
    
  ),
  
  # Set the default values for the slots. (optional)
  prototype=list(
    col_range = 50,
    scale_size = 5,
    layers = 1
  ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object)
  {
    if((object@x < 0) || (object@y < 0)) {
      return("A negative number for one of the coordinates was given.")
    }
    return(TRUE)
  }
)

setGeneric(name="dataReorder",
           def=function(object, newData, to)
           {
             standardGeneric("dataReorder")
           }
)
setMethod(f="dataReorder",signature="mapData",
          definition=function(object, newData, to="toShort"){
            provinces = object@prov_red
            newProvinces = newData@provinces
            newProvinces = provinceConvert(newProvinces, to=to)
            newOrder = rep(0, length(provinces))
            for (i in 1:length(provinces)){
              newOrder[i] <- which(newProvinces==provinces[i])
            }
            
            return(newOrder)
          }
)
setGeneric(name="getCostDensity",
           def=function(object, dense)
           {
             standardGeneric("getCostDensity")
           }
)
setMethod(f="getCostDensity",signature="mapData",
          definition=function(object, dense){
            
            file <- "./census_data/T10120180918023605.CSV"
            census <- new("censusData")
            census <- readFile(census, file)
            census <- setProvinces(census)
            census <- getPopulation(census)
            newOrder <- dataReorder(object, census, to="toShort")
            population <- census@population[newOrder]
            costAll <- object@costAll
            popAll <- getCost(object@costAll, object@prov_red)
            cd <- c()
            if(dense==TRUE){
              
              for(i in 1:nrow(popAll)){
                prov <- popAll$provinces[i]
                prov <- provinceConvert(prov, to="long", quebec=2)
                cost <- popAll$pop[i]/census@population[prov]
                if(is.na(cost)){cost <- 0}
                cd <- c(cd, cost)
              }}
            else{
              for(i in 1:nrow(popAll)){
                prov <- popAll$provinces[i]
                prov <- provinceConvert(prov, to="long", quebec=2)
                cost <- popAll$pop[i]
                if(is.na(cost)){cost <- 0}
                cd <- c(cd, cost)
              }
              
            }
            popAll$cd <- cd
            sub <- which(popAll$cd==0)
            sub <- popAll[-sub,]
            min_id <- which(sub$cd==min(sub$cd))
            max_id <- which(popAll$cd==max(popAll$cd))
            min_pop <- sub$cd[min_id]
            min_prov <- provinceConvert(sub$provinces[min_id], to="long")
            max_pop <- popAll$cd[max_id]
            max_prov <- provinceConvert(popAll$provinces[max_id], to="long")
            object@min_pop <- min_pop
            object@max_pop <- max_pop
            popYear <- getCost(object@costYear, object@prov_red)
            popYear <- popYear$pop
            if(dense==TRUE){
              object@costDensity <- popYear/population
            } else {
              object@costDensity <- popYear
            }
            return(object)
          }
)

setGeneric(name="setPalette",
           def=function(object, palette)
           {
             standardGeneric("setPalette")
           }
)
setMethod(f="setPalette",signature="mapData",
          definition=function(object, palette="viridis"){
            cat('~~~ mapData: Setting color palette ~~~\n')
            if(names(palette)=="viridis"){
              pal = viridis_pal(option = palette)(object@scale_size)
              pal = rev(pal)
            } else if (names(palette)=="brewer"){
              coul = RColorBrewer::brewer.pal(4, palette)
              pal = colorRampPalette(coul)(25)
            } else if(names(palette)=="custom"){
              coul = c("#e0b8d8", 
                       #"#00A7E1",
                       "#240f20")
              #coul = c("#062F4F", "#813772", "#B82601")
              pal = colorRampPalette(coul)(100)
            }
            object@pal = pal
            cat('mapData: color palette set\n')
            return(object)
          }
)

setMethod(f="initialize", signature="mapData",
          definition=function(.Object,canMap,
                              digits, prefix, group, plotLabel,
                              palette){
            
            .Object@regions <- canMap@regions
            .Object@prov_red <- canMap@prov_red
            .Object@provinces <- canMap@provinces
            .Object@group <- group
            .Object@digits <- digits
            .Object@plotLabel <- plotLabel
            .Object@prefix <- prefix
            .Object <- setPalette(.Object, palette)
            return(.Object) }
)