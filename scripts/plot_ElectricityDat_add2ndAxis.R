dyg <- 
  dyg %>% 
  dyAxis("y2", independentTicks = F,valueRange = YvalueRange) %>%
  dySeries("Gesamtverbrauch",axis="y2", label="Gesamtverbrauch",
           color = rgb(0,0,0),strokeWidth = 1.5,fillGraph = F)%>%
  dySeries("Endverbrauch",axis="y2", label="Endverbrauch",
                  color = rgb(1,0,0),strokeWidth = 1.5,fillGraph = F)

