source('helper_functions.R')
mapData <- setClass(
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
    provinces = "character"
    
  ),
  
  # Set the default values for the slots. (optional)
  prototype=list(
    col_range = 50,
    scale_size = 2
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

setGeneric(name="drawMap2",
           def=function(object)
           {
             standardGeneric("drawMap2")
           }
)
setMethod(f="drawMap2",signature="mapData",
          definition=function(object){
              
              pal <- leaflet::colorNumeric(
                viridis_pal(option = "D")(object@scale_size), 
                domain = range(object@min_pop, object@max_pop),
                na.color="grey")
              object@regions$Pop <- object@costDensity
              pop <- getCost(object@costYear, object@prov_red)
              print(object@costDensity)
              print(object@min_pop)
              print(object@max_pop)
              prov2 <- provinceConvert(object@prov_red, to="long")
              object@regions$provinces <- prov2
              cost_labels <- round(pop$pop, digits=-5)
              cost_labels <- formatC(cost_labels, big.mark=" ", digits=10)
              object@regions$labels <- cost_labels
              m <- leaflet() %>% setView(lng = -120, lat = 60, zoom = 4)  %>%
                addTiles(group="basemap") %>%
                addPolygons(data=object@regions, opacity=0.5, fillOpacity=0.8, group="cansimp",
                            color="white", weight=0.8, fillColor=~pal(Pop),
                            highlightOptions = highlightOptions(
                              color = "white", opacity = 1, weight = 2, fillOpacity = 1,
                              bringToFront = TRUE, sendToBack = TRUE),
                            popup = paste(object@regions$provinces, "<br>",
                                          "Cost: $", object@regions$labels, "<br>")) %>%
                addLayersControl(overlayGroups = c("basemap", "province", "cansimp")) %>%
                addLegend("bottomleft", pal = pal, values=object@regions$Pop,
                          title = "Cost", group="cansimp",
                          opacity = 1)
              return(m)
          }
)

setGeneric(name="getMap",
           def=function(object)
           {
             standardGeneric("getMap")
           }
)
setMethod(f="getMap",signature="mapData",
          definition=function(object){
              can1<-getData('GADM', country="CAN", level=1) 
              provinces <- unique(can1$NAME_1)
              object@provinces <- provinces
              prov_red <- can1$NAME_1
              mapExtent <- rbind(c(-156, 80), c(-68, 19))
              #newProj <- CRS("+proj=poly +lat_0=0 +lon_0=-100 +x_0=0 
              #+y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
              newProj <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
              mapExtentPr <- spTransform(SpatialPoints(mapExtent, 
                                                       proj4string=CRS("+proj=longlat")),
                                         newProj)
              
              can1Pr <- spTransform(can1, newProj)
              prov_red <- provinceConvert(prov_red, "toShort")
              can2 <- can1Pr[can1Pr$NAME_1 %in% provinces,]
              
              #can_simp <- gSimplify(can2, tol=10000)
              can_simp <- gSimplify(can2, tol=0.1)
              regions <- gBuffer(can_simp, byid=TRUE, width=0)
              regions <- SpatialPolygonsDataFrame(spTransform(regions,
                                                              CRS("+proj=longlat +ellps=sphere +no_defs")),
                                                  data.frame(Region=names(regions),
                                                             row.names=names(regions),
                                                             stringsAsFactors=FALSE))
              object@regions <- regions
              object@prov_red <- prov_red

            return(object)
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
           def=function(object)
           {
             standardGeneric("getCostDensity")
           }
)
setMethod(f="getCostDensity",signature="mapData",
          definition=function(object){
            
            file <- "./census_data/T10120180918023605.CSV"
            census <- censusData()
            census <- readFile(census, file)
            census <- setProvinces(census)
            census <- getPopulation(census)
            newOrder <- dataReorder(object, census, to="toShort")
            population <- census@population[newOrder]
            costAll <- object@costAll
            popAll <- getCost(object@costAll, object@prov_red)
            print(popAll)
            cd <- c()
            for(i in 1:nrow(popAll)){
              prov <- popAll$provinces[i]
              prov <- provinceConvert(prov, to="long", quebec=2)
              print(prov)
              print(popAll$pop[i])
              print(census@population[prov])
              cost <- popAll$pop[i]/census@population[prov]
              if(is.na(cost)){cost <- 0}
              cd <- c(cd, cost)
            }
            print(cd)
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
            object@costDensity <- popYear/population
            print(object@min_pop)
            print(object@max_pop)
            return(object)
          }
)









