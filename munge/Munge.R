##'### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
##'
##' DATA MUNGING SCRIPT
##' 
##'### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###




##' === === === === === === === === === === === === === === === === === === ===
##' Clean Swissgrid Übersicht data ####
##' === === === === === === === === === === === === === === === === === === ===

SwissgridObjects <- ls(pattern = "EnergieUebersichtCH_20")
SwissgridOverview <- data.frame() 

for (dfname in SwissgridObjects) {
  wb <- get(dfname)
  names(wb)[1] <-"Zeitstempel" #rename first column
  
# because read function makes the datetime automatically UTC but raw data is in Zurich time
  wb$Zeitstempel <- force_tz(wb$Zeitstempel,tz="Europe/Zurich") 
  
  target <- which(duplicated(wb$Zeitstempel))
  if(length(target)>0){
    ##' before correction
    # print(wb$Zeitstempel[lookaround(min(target)-6,8)])
    wb$Zeitstempel[seq(min(target)-6,by=1,length.out = 10)] <-  seq.POSIXt(wb$Zeitstempel[min(target)-6],by="15 min",length.out = 10)
    
##' after correction
    # print(wb$Zeitstempel[lookaround(min(target)-6,8)])
  }
  
  wb$Zeitstempel <- as.POSIXct(round.POSIXt(wb$Zeitstempel),tz="Europe/Zurich",usetz=T)
  wb$Zeitstempel <- with_tz(wb$Zeitstempel,tz="UTC")

# add original headers as description
  attr(wb,"description")<-names(wb)
# define new column names
  names(wb) <- c("Datetime",
                           "Cons_Total_Enduse_ControlBlock",
                           "Prod_Total_ControlBlock",
                           "Cons_Total_ControlBlock",
                           "Net_Outflow_Transm_Grid",
                           "Grid_FeedIn_Transm_Grid",
                           "Control_Energy_Pos_Sec",
                           "Control_Energy_Neg_Sec",
                           "Control_Energy_Pos_Ter",
                           "Control_Energy_Neg_Ter",
                           "Cross_Border_Exchange_CH_AT",
                           "Cross_Border_Exchange_AT_CH",
                           "Cross_Border_Exchange_CH_DE",
                           "Cross_Border_Exchange_DE_CH",
                           "Cross_Border_Exchange_CH_FR",
                           "Cross_Border_Exchange_FR_CH",
                           "Cross_Border_Exchange_CH_IT",
                           "Cross_Border_Exchange_IT_CH",
                           "Transit",
                           "Import",
                           "Export",
                           "Prices_Avg_Pos_Sec_Control_Energy",
                           "Prices_Avg_Neg_Sec_Control_Energy",
                           "Prices_Avg_Pos_Ter_Control_Energy",
                           "Prices_Avg_Neg_Ter_Control_Energy",
                           "Prod_Canton_AG",
                           "Cons_Canton_AG",
                           "Prod_Canton_FR",
                           "Cons_Canton_FR",
                           "Prod_Canton_GL",
                           "Cons_Canton_GL",
                           "Prod_Canton_GR",
                           "Cons_Canton_GR",
                           "Prod_Canton_LU",
                           "Cons_Canton_LU",
                           "Prod_Canton_NE",
                           "Cons_Canton_NE",
                           "Prod_Canton_SO",
                           "Cons_Canton_SO",
                           "Prod_Canton_SG",
                           "Cons_Canton_SG",
                           "Prod_Canton_TI",
                           "Cons_Canton_TI",
                           "Prod_Canton_TG",
                           "Cons_Canton_TG",
                           "Prod_Canton_VS",
                           "Cons_Canton_VS",
                           "Prod_Canton_AI_AR",
                           "Cons_Canton_AI_AR",
                           "Prod_Canton_BL_BS",
                           "Cons_Canton_BL_BS",
                           "Prod_Canton_BE_JU",
                           "Cons_Canton_BE_JU",
                           "Prod_Canton_SZ_ZG",
                           "Cons_Canton_SZ_ZG",
                           "Prod_Canton_OW_NW_UR",
                           "Cons_Canton_OW_NW_UR",
                           "Prod_Canton_GE_VD",
                           "Cons_Canton_GE_VD",
                           "Prod_Canton_SH_ZH",
                           "Cons_Canton_SH_ZH",
                           "Prod_Cantons",
                           "Cons_Cantons",
                           "Prod_Foreign_Territories_SwissControlZone",
                           "Cons_Foreign_Territories_SwissControlZone"
  )
  
  assign(dfname,wb)
  rm(wb)
  SwissgridOverview <- rbind(SwissgridOverview,get(dfname))
}
.project.save.cache(SwissgridOverview,type="munged")

##' === === === === === === === === === === === === === === === === === === ===
##' # Clean Spot Market Price data ####
##' === === === === === === === === === === === === === === === === === === ===

##'
##' ### Read Swissix Spot Prices 
##' 
##' !!!! Misstake in 2015-10-25 merge produces multiple additional rows of daylight saving time
##' !!!! Misstake in 2015-10-25 merge produces multiple additional rows of daylight saving time
##' !!!! Misstake in 2015-10-25 merge produces multiple additional rows of daylight saving time
##' 

df <- SwissixPowerSpotHistory_2015
names(df) <- c("Datetime","Hour","Price.EUR.MWh","Volume.MWh")
df$Datetime <- force_tz(df$Datetime,tz="Europe/Zurich") %>% with_tz(tz="UTC")

df <- df %>% select(Datetime,
                      Swissix_EUR_MWh = Price.EUR.MWh,
                      Swissix_Volume_MWh = Volume.MWh)

spotPrices.Swissix <- df


##' ###  Read Italian Spot Prices 
##' raw data in UTC or CST format is not checked yet. Assumed that it is in UTC format because no missing values 
##' in end of march
##' 
##' !!!! Misstake in 2015-10-25 merge produces multiple additional rows of daylight saving time
##' !!!! Misstake in 2015-10-25 merge produces multiple additional rows of daylight saving time
##' !!!! Misstake in 2015-10-25 merge produces multiple additional rows of daylight saving time
##' 

wb3 <- `PowerPricesItaly_2016-05-25`
wb3 <- rename(wb3,Datetime = Date)

##' take each date in datetime column and create a vector of all dates with each 24 different hours per day
for(i in 1:length(wb3$Datetime)){
  if(i==1) {
    datetimes_ <- as.POSIXct(seq.POSIXt(wb3$Datetime[i],by="hour",length.out = 24),tz="UTC")
  }
  else {
    datetimes_ <- c(datetimes_,as.POSIXct(seq.POSIXt(wb3$Datetime[i],by="hour",length.out = 24),tz="UTC")) 
  }
}
datetimes_ <- with_tz(datetimes_,tz="UTC")

values_ <- c(t(as.matrix(wb3[,-1])))# get all hour columns in one big vector

wb3<- data.frame(Datetime=datetimes_,Italy_EUR_MWh=values_)
spotPrices.Italy <- data.frame(Datetime=datetimes_,Italy_EUR_MWh=values_)
wb3 <- rename(wb3,Col_1 = Datetime)


##' ### Read France Spot Prices 
##' 
##' Price data of France seems to be in CEST/CET standard because of missing values in March
##' !!!! Misstake in 2015-10-25 merge produces multiple additional rows of daylight saving time
##' !!!! Misstake in 2015-10-25 merge produces multiple additional rows of daylight saving time
##' !!!! Misstake in 2015-10-25 merge produces multiple additional rows of daylight saving time

wb4 <- France_spot_all
wb4 <- rename(wb4,Datetime = DTIME)
wb4 <- rename(wb4,France_EUR_MWh=FRANCE)
spotPrices.France <- wb4
wb4 <- rename(wb4,Col_1 = Datetime)

##' ### Read Phelix Spot Prices 
##' 
##' !!!! Misstake in 2015-10-25 merge produces multiple additional rows of daylight saving time
##' !!!! Misstake in 2015-10-25 merge produces multiple additional rows of daylight saving time
##' !!!! Misstake in 2015-10-25 merge produces multiple additional rows of daylight saving time
##' 
wb5 <- PhelixPowerSpotHistory_2015
names(wb5) <- c("Datetime","Hour","Price.EUR.MWh","Volume.MWh")
wb5$Datetime <- force_tz(wb5$Datetime,tz="Europe/Zurich") %>% with_tz(tz="UTC")

wb5 <- wb5 %>% select(Datetime,
                      Phelix_EUR_MWh = Price.EUR.MWh,
                      Phelix_Volume_MWh = Volume.MWh)

spotPrices.Phelix <- wb5

wb5 <- rename(wb5,Col_1 = Datetime)



.project.save.cache(spotPrices.Swissix,spotPrices.Phelix,spotPrices.France,spotPrices.Italy)

##' === === === === === === === === === === === === === === === === === === ===
##' # Clean BAFU Hydrologiedaten ####
##' === === === === === === === === === === === === === === === === === === ===

rhone = `teske_Q_2011_10minMittel-2015`[-c(1:3),-1]
rhone %<>% mutate(V2 = gsub("\\.","-",substr(V2,1,16)))
rhone %<>% mutate(V2 = as.POSIXct(V2,tz = "UTC"))


# 
# 
# datasetnames <- ls(pattern = "teske_Q.*?10minMittel*")
# 
# for (datasetname in datasetnames) {
#   dat <- get(datasetname)
# 
#   # Make from to datatime in column 2 of the dataset 
#   # a single datatime value by splitting the string at the '-' character.
#   dat$V2 <- strsplit(dat$V2, "-") %>% 
#     sapply(FUN=function(x){x[1]}) %>% 
#     strptime(format="%Y.%m.%d %H:%M",tz="UTC") %>% 
#     as.POSIXct()
#   names(dat) <- c("ID.Messstation","Datetime","Volumenstrom_m3_s")
#   dat <- dat %>% 
#     mutate(Volumenstrom_m3_s = as.numeric(Volumenstrom_m3_s))
#   assign(datasetname,
#          dat,
#          envir = .GlobalEnv)
#   if (which(datasetnames==datasetname)==1) {
#     RiverFlows <- dat
#   }
#   else {
#     RiverFlows <- RiverFlows %>% 
#       merge(dat)
#   }
# }


##' === === === === === === === === === === === === === === === === === === ===
##' # Clean BFE Electricity Stats ####
##' === === === === === === === === === === === === === === === === === === ===

BFE_Elec_Stats <- 
  `BFE Produktionstyp Wochenwerte und Monatswerte` %>% 
  rename(Date_Month=`Date Month`) %>% 
  mutate(Date_Month=as.POSIXct(Date_Month)) %>% 
  mutate(Date_Month=force_tz(Date_Month,tzone="Europe/Zurich"))

BFE_Elec_Stats <- 
  BFE_Elec_Stats %>% 
  mutate_at(dplyr::vars(c(Prod_Laufwasser:Store_Change,Store_Capacity)),funs(. * 1E06)) %>% 
  mutate_at(dplyr::vars(c(Store_usage)),funs(. / 100)) %>% 
  as.data.frame()

.project.save.cache(BFE_Elec_Stats,type = "munged")

##' === === === === === === === === === === === === === === === === === === ===
##' # All ElectricityPrices ####
##' === === === === === === === === === === === === === === === === === === ===
##' 
DayAheadPreiseTZzurich_df <- 
  as.data.frame(`PowerPrices_2017-03-13`) %>% 
  select(Datetime,
         Phelix_EUR_MWh,
         Italy_EUR_MWh,
         France_EUR_MWh) %>% 
  mutate(Datetime = lubridate::round_date(Datetime,unit="min")) %>%
  mutate(Datetime = force_tz(Datetime,tz="Europe/Zurich"))

DayAheadPreiseTZutc_df <- 
  as.data.frame(`PowerPrices_2017-03-13`) %>% 
  select(Datetime,
         Swissix_EUR_MWh) %>% 
  mutate(Datetime = lubridate::round_date(Datetime,unit="min"))

DayAheadPreiseTZzurich_df <- DayAheadPreiseTZzurich_df[-which(is.na(DayAheadPreiseTZzurich_df$Datetime)),]

DayAheadPreise_df <- 
  DayAheadPreiseTZzurich_df %>% 
  with_tz(tz="UTC") %>% 
  left_join(DayAheadPreiseTZutc_df, by = "Datetime") %>% 
  mutate(MaxPrice=pmax(Italy_EUR_MWh,Phelix_EUR_MWh, France_EUR_MWh, Swissix_EUR_MWh,na.rm = T),
         MinPrice=pmin(Italy_EUR_MWh,Phelix_EUR_MWh, France_EUR_MWh, Swissix_EUR_MWh,na.rm = T)) %>% 
  # rowwise() %>% 
  # mutate(Mean = mean(Italy_EUR_MWh,Phelix_EUR_MWh, France_EUR_MWh, Swissix_EUR_MWh,na.rm = T),
  #        Median = median(Italy_EUR_MWh,Phelix_EUR_MWh, France_EUR_MWh, Swissix_EUR_MWh,na.rm = T)
  #        ) %>% 
  as.data.frame()

.project.save.cache(DayAheadPreise_df,type = "munged")




