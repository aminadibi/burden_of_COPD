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
            } else if (names(palette)=="brewer"){
              coul = RColorBrewer::brewer.pal(4, palette)
              pal = colorRampPalette(coul)(25)
              pal = rev(pal)
            }
            object@pal = pal
            cat('mapData: color palette set\n')
            return(object)
          }
)


setClass(
  "createMap",

  # Define the slots
  slots = c(
    mapDataList = "list",
    groups = "character",
    layers = "numeric",
    legendLabels = "character"

  ),

  prototype=list(

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

setGeneric(name="drawMap",
           def=function(object)
           {
             standardGeneric("drawMap")
           }
)
setMethod(f="drawMap",
          signature="createMap",
          definition=function(object){


              m <- leaflet(options=leafletOptions(zoomControl=FALSE),
                           width="50%") %>% setView(lng = -100, lat = 60, zoom = 3)%>%
                addTiles(group="basemap")
                for(i in 1:object@layers){
                  mapLayer <- object@mapDataList[[i]]
                  pal <- leaflet::colorNumeric(
                    mapLayer@pal,
                    domain = range(mapLayer@min_pop, mapLayer@max_pop),
                    na.color="grey")
                  mapLayer@regions$Pop <- mapLayer@costDensity
                  pop <- getCost(mapLayer@costYear, mapLayer@prov_red)
                  prov2 <- provinceConvert(mapLayer@prov_red, to="long")
                  mapLayer@regions$provinces <- prov2
                  #cost_labels <- round(pop$pop, digits=mapLayer@digits)
                  cost_labels <- round(mapLayer@regions$Pop, digits = mapLayer@digits)
                  if(max(cost_labels)>1000000){
                    cost_labels <- costToMill(cost_labels)
                  }
                  #cost_labels <- formatC(cost_labels, big.mark=" ", digits=10)
                  mapLayer@regions$labels <- cost_labels
                  nodata <- which(mapLayer@regions$Pop==0)
                  mapLayer@regions$Pop[nodata] = NA
                  mapLayer@regions$labels[nodata] = "No Data"
                  layerId = sapply(1:length(prov2), function(x){paste0("group",i,x)})
                  layerId2 = object@legendLabels[i]
                  print(layerId2)
                m <- m %>% addPolygons(data=mapLayer@regions, opacity=0.5, fillOpacity=0.8, group=mapLayer@group,
                            color="white", weight=0.8, fillColor=~pal(Pop),layerId = layerId,
                            highlightOptions = highlightOptions(
                              color = "white", opacity = 1, weight = 2, fillOpacity = 1,
                              bringToFront = TRUE, sendToBack = TRUE),
                            popup = paste(mapLayer@regions$provinces, "<br>",
                                          mapLayer@plotLabel, mapLayer@regions$labels, "<br>")) %>%
                addLegend("bottomleft", pal = pal, values=c(mapLayer@min_pop, mapLayer@max_pop),
                          title = object@groups[i], group=object@groups[i],
                          opacity = 1, na.label="No Data", labFormat = myLabFormat(prefix="$",
                                                                                     digits=mapLayer@digits),
                          layerId=layerId2)
    
                }
                m <- m %>% addLayersControl(overlayGroups = c(object@groups),
                                            options = layersControlOptions(collapsed=FALSE)) %>%
                   hideGroup(object@groups[2:object@layers])%>%
  #                 htmlwidgets::onRender("
  #   function(el, x) {
  #     // Navigate the map to the user's location
  #     this.locate({setView: true});
  #   }
  # ")%>%
                  #map.addLayer(console.log(e.layer));
                  
                 htmlwidgets::onRender("
function(el,x){
                  this.on('baselayerchange',
                         function (e) {

                           this.locate({setView: true});
                           this.removeControl(legend1);
                            legend2.addTo(this);
                         })}")

              return(m)
          }
)

setMethod(f="initialize", signature="mapData",
          definition=function(.Object,canMap,
                              digits, group, plotLabel,
                              palette){

            .Object@regions <- canMap@regions
            .Object@prov_red <- canMap@prov_red
            .Object@provinces <- canMap@provinces
            .Object@group <- group
            .Object@digits <- digits
            .Object@plotLabel <- plotLabel
            .Object <- setPalette(.Object, palette)
            return(.Object) }
          )

setMethod(f="initialize", signature="createMap",
          definition=function(.Object, groups, layers, legendLabels,
                              mapDataList){
            .Object@mapDataList <- mapDataList
            .Object@groups <- groups
            .Object@layers <- layers
            .Object@legendLabels <- legendLabels

            return(.Object) }
)


















