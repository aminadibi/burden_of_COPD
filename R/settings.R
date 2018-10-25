mapSettings <- list(filename = "./static_data/canadaMap.RData",
  palette = c("brewer"="YlGnBu", "brewer"="YlGnBu","brewer"="Greens"),
  layers = 3,
  mapDataList = list("CostDensity"=0,"Cost"=0, "copdNumber"=0),
  groups = c("Cost Density","Overall Cost", "Number of COPD"),
  plotLabels = c("Cost/Person: $", "Cost: $","COPD Cases per Year: "),
  digits = c(-1, -5, 2),
  dense = c(TRUE, FALSE, TRUE),
  legendLabels = c("legend1", "legend2", "legend3")
)