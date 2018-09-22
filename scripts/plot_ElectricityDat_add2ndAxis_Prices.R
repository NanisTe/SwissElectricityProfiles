dyg <- 
  dyg %>% 
  dyAxis("y2", independentTicks = T,valueRange = c(0,600)) %>%
  dySeries("MaxPrice",axis="y2",color = "red",fillGraph = F,label = "maximaler Strompreis aller CH Nachbarstaaten")
  # dySeries("Italy_EUR_MWh",axis="y2",color = "green",fillGraph = F) %>% 
  # dySeries("France_EUR_MWh",axis="y2",color = "blue",fillGraph = F) %>%
  # dySeries("Phelix_EUR_MWh",axis="y2",color = "black",fillGraph = F) %>%
  # dySeries("Swissix_EUR_MWh",axis="y2",color = "red",fillGraph = F)
