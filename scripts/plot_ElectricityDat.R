cat(stderr(),ls(),"\n")

if(With2ndAxis){
  switch (SecondAxisOption,
    '1' = SwissgridPlot <- PlotData %>% select(Datetime,
                                               ImportEndUse,
                                               SelfProdOwnUse,
                                               ImportNonEndUsed,
                                               NetExport,
                                               SelfProdEndUse) %>% 
      mutate(Endverbrauch=PlotData$Cons_Total_Enduse_ControlBlock) %>% 
      mutate(Gesamtverbrauch=PlotData$Cons_Total_ControlBlock-Endverbrauch) %>% 
      mutate_at(dplyr::vars(-Datetime),funs(./1000)),
    '2' = SwissgridPlot <- PlotData %>% select(Datetime,
                                               ImportEndUse,
                                               SelfProdOwnUse,
                                               ImportNonEndUsed,
                                               NetExport,
                                               SelfProdEndUse,
                                               starts_with("MaxPrice")) %>% 
      mutate_at(dplyr::vars(-Datetime,-MaxPrice),funs(./1000))
  )
}else{
  SwissgridPlot <- PlotData %>% select(Datetime,
                                       ImportEndUse,
                                       SelfProdOwnUse,
                                       ImportNonEndUsed,
                                       NetExport,
                                       SelfProdEndUse) %>% 
    mutate_at(dplyr::vars(-Datetime),funs(./1000))
}


tzone_ <- "Europe/Zurich"
XAxisTitle <- hash::hash()
YAxisTitle <- hash::hash()
XAxisTitle["DE"] <- paste0("Zeit ",tzone_," (dt = 15 min)")
YAxisTitle["DE"] <- enc2utf8("Elektrizitäts-Volumen in MWh")
XAxisTitle["EN"] <- paste0("Time ",tzone_," (dt = 15 min)")
YAxisTitle["EN"] <- "Electricity Volume in MWh"

legendText <- hash::hash()
legendText["DE-1"] <- "Export"
legendText["DE-2"] <- "Importstrom für Pumpen & Verluste"
legendText["DE-3"] <- "Importstrom für Endverbraucher"
legendText["DE-4"] <- "CH Produktion für Pumpen & Verluste"
legendText["DE-5"] <- "CH Produktion für Endverbraucher"

legendText["EN-1"] <- "Export"
legendText["EN-2"] <- "Import for pumps & losses"
legendText["EN-3"] <- "Import for end use"
legendText["EN-4"] <- "CH production for pumps & losses"
legendText["EN-5"] <- "CH production for end use"

text_lines <- hash::hash()
text_lines["DE-1"] <- "Max. Pumpenkapazität CH"
text_lines["DE-2"] <- "Max. Kraftwerkskapazität CH  (ElCom 2016)"

text_lines["EN-1"] <- "Max. pump power CH"
text_lines["EN-2"] <- "Max. capacity of power plants CH (ElCom 2016)"

PlotData_ <- filterXts(SwissgridPlot,Year,tzone=tzone_)
SwissgridXTS <-xts(PlotData_[,-1],order.by = PlotData_$Datetime)

dyg <- 
  dygraph(
    SwissgridXTS,
    main=PlotTitle[[Lang]],
    group=(if(WithGrouping){"G1"}else{NULL})
    ) %>%
  dyAxis("x", label = XAxisTitle[[Lang]], drawGrid = T) %>%
  dyAxis("y", label = enc2utf8(YAxisTitle[[Lang]]), valueRange = YvalueRange) %>%
  dyLegend(width = 300, labelsSeparateLines = T)   %>%
  dyShading(from = PlotData_$Datetime[1], to = tail(PlotData_$Datetime,n=1), color = "ghostwhite") %>% 
  # dyHighlight(highlightSeriesOpts = list(strokeWidth = 0.1),highlightSeriesBackgroundAlpha = 0.2) %>% 
  dySeries("NetExport",
           label=legendText[[paste0(Lang,"-1")]],
           color = "green",strokeWidth = 0)%>%
  dySeries("ImportNonEndUsed",
           label=legendText[[paste0(Lang,"-2")]],
           color = "darkturquoise",fillGraph = T,strokeWidth = 0)%>%
  dySeries("ImportEndUse",
           label=legendText[[paste0(Lang,"-3")]],
           color = "darkorange",strokeWidth = 0,
           strokeBorderWidth = 1, strokeBorderColor = "red")%>%
  dySeries("SelfProdOwnUse",
           label=legendText[[paste0(Lang,"-4")]],
           color = "blue",strokeWidth = 0)%>%
  dySeries("SelfProdEndUse",
           label=legendText[[paste0(Lang,"-5")]],
           color = "purple",strokeWidth = 0) %>%
  dyLimit(limit=440, label = text_lines[[paste0(Lang,"-1")]], labelLoc = c("left", "right"), 
                                                           color = "black", strokePattern = "dashed") %>% 
  dyLimit(limit=4775, label = text_lines[[paste0(Lang,"-2")]], labelLoc = c("left", "right"), 
          color = "black", strokePattern = "dashed")
  # dyCrosshair(direction = "vertical")

if(With2ndAxis){
  switch (SecondAxisOption,
    '1' = source("scripts/plot_ElectricityDat_add2ndAxis.R",encoding = "UTF-8"),
    '2' = source("scripts/plot_ElectricityDat_add2ndAxis_Prices.R",encoding = "UTF-8")
  )
}
