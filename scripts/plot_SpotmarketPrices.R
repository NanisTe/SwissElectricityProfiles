## ## Plot Spot Market Prices ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

MaxDayAheadPreise_df <- 
  DayAheadPreise_df %>%  
  select(Datetime,
         MaxPrice)


dy_dat <- df2xts(filterXts(DayAheadPreise_df,"2010/2016",tzone = "Europe/Zurich") %>% 
                   subset(select=c(Datetime,
                                   MaxPrice)))
dy_dat <- df2xts(DayAheadPreise_df )



dy_dat <- 
  df2xts(
    filterXts(DayAheadPreise_df,"2010/2016",tzone = "Europe/Zurich")%>% 
      subset(select=c(Datetime,
                      Italy_EUR_MWh,
                      France_EUR_MWh,
                      Phelix_EUR_MWh
                      # Swissix_EUR_MWh
                    )
           ) %>%as.data.frame()
  )

(PlotPrices <-
    dygraph(dy_dat,group = "G1",main = "Day Ahead Preise CH und Nachbarstaaten") %>% 
    dyOptions(stepPlot = T,useDataTimezone=T,fillAlpha = 0.2
              # colors=c(,"rgba(255, 0, 0, 0.3)","rgba(255, 0, 0, 0.3)","rgba(255, 0, 0, 0.3)")
              ) %>% 
    dyAxis("y", label = "Preis in EUR/MWh", valueRange = c(-150, 300)) %>% 
    dyAxis("x", label = "Zeit (dt=15min)") %>% 
    dyLegend(width = 300, labelsSeparateLines = T) %>%
    dySeries("France_EUR_MWh",color = "rgba(0, 0, 255, 0.7)") %>%
    dySeries("Phelix_EUR_MWh",color = "rgba(139, 0, 0, 0.7)") %>%
    dySeries("Italy_EUR_MWh",color = "rgba(0, 100, 0, 0.7)")
    # dySeries("Swissix_EUR_MWh",color = "rgba(205, 133, 0, 0.7)")
    
    
)


#   dyEvent(as.POSIXct("2012-02-01 00:00",tz="Europe/Zurich"),label = "- severe cold spell </br>
# - market fears of a possible natural gas supply disruption")
# https://ec.europa.eu/energy/sites/ener/files/documents/qreem_2012_quarter1.pdf

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##