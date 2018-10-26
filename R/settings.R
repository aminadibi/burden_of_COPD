mapSettings0 <- list(filename = "./static_data/canadaMap.RData",
  palette = c("brewer"="YlGnBu", "brewer"="YlGnBu","brewer"="Greens"),
  layers = 3,
  mapDataList = list("CostDensity"=0,"Cost"=0, "copdNumber"=0),
  groups = c("Cost Density","Overall Cost", "Number of COPD"),
  plotLabels = c("Cost/Person: $", "Cost: $","COPD Cases per Year: "),
  digits = c(-1, -5, 2),
  dense = c(TRUE, FALSE, TRUE),
  legendLabels = c("legend1", "legend2", "legend3")
)
mapSettings1 <- list(filename = "./static_data/canadaMap.RData",
                     palette = c("brewer"="YlGnBu", "brewer"="YlGnBu"),
                     layers = 2,
                     mapDataList = list("CostDensity"=0,"Cost"=0),
                     groups = c("Cost Density","Overall Cost"),
                     plotLabels = c("Cost/Person: $", "Cost: $"),
                     digits = c(-1, -5),
                     dense = c(TRUE, FALSE),
                     legendLabels = c("legend1", "legend2")
)
mapSettings2 <- list(filename = "./static_data/canadaMap.RData",
                    palette = c("brewer"="Greens"),
                    layers = 1,
                    mapDataList = list("copdNumber"=0),
                    groups = c("Number of COPD"),
                    plotLabels = c("COPD Cases per Year: "),
                    digits = c(2),
                    dense = c(TRUE),
                    legendLabels = c("legend3")
)