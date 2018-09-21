canadaMap <- setClass(
  # Set the name for the class
  "canadaMap",
  
  # Define the slots
  slots = c(

    regions = "SpatialPolygonsDataFrame",
    prov_red = "character",
    provinces = "character"
  ),
  
  # Set the default values for the slots. (optional)
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

setGeneric(name="getMap",
           def=function(object, filename)
           {
             standardGeneric("getMap")
           }
)
setMethod(f="getMap",signature="canadaMap",
          definition=function(object, filename){
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
setMethod(f="initialize", signature="canadaMap",
          definition=function(.Object,filename, initialize){
            
            cat("~~~ canadaMap: initializator ~~~ \n")
            if(initialize==TRUE){
              .Object <- getMap(.Object, filename)
              save(.Object, file=filename)
            } else {
              load(filename)
            }

            return(.Object) }
)








