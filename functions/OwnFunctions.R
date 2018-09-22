# USER-DEFINED (OWN) FUNCTIONS 
### ### ### ### ### ### ### ### ### ### ###

p_load("tidyverse")
p_load("xts")
p_load("zoo")
p_load("xlsx")
p_load("lubridate")
p_load("gdata")
p_load("foreach")
p_load("doParallel")
p_load("ggplot2")
p_load("plotly")
p_load("dygraphs")
p_load("tempdisagg")
p_load("stringr")
p_load("stringi")
p_load("htmlwidgets")
p_load("htmltools")
p_load("webshot")
p_load("hash")
p_load("magrittr")
p_load("svglite")
p_load("fields")
p_load("R2HTML")
p_load("viridis")
p_load("scales")
p_load("blogdown")



rename <- dplyr::rename
select <- dplyr::select
summarise <- dplyr::summarise
filter <- dplyr::filter


##' === === === === === === === === === === === === === === === === ===
##' 
##' ## Function for preparing DataFrame for Electrcity Data Plot  ####
##' 
##' === === === === === === === === === === === === === === === === ===

ElDatPlot_Prepare <- function(df){
  ##' Check for correct columns of input data frame 
  str_cols_needed <- c("Prod_Total_ControlBlock",
                       "Cons_Total_Enduse_ControlBlock",
                       "Cons_Total_ControlBlock")
  str_existing <- colnames(df)
  chk_col_str <- str_cols_needed %in% str_existing
  if(!all(chk_col_str)){
    stop(paste0("The following columns are missing in dat '",paste(str_cols_needed[!chk_col_str],collapse="', '"),"'."))
  }
  
  
  ##' Calc in country PRODUCTION providing END USE CONSUMPTION (graph color: magenta) 
  df <- df %>% 
    transform(SelfProdEndUse=pmin(Prod_Total_ControlBlock,
                                  Cons_Total_Enduse_ControlBlock))
  
  ##' Calc the amount of END USE CONSUMPTION provided by IMPORT electricity (graph color: orange) 
  df <- df %>% 
    transform(ImportEndUse= 
                ifelse(Cons_Total_Enduse_ControlBlock>Prod_Total_ControlBlock,
                       Cons_Total_Enduse_ControlBlock-Prod_Total_ControlBlock,
                       0))
  
  ##' Calc the amount of IMPORT electricity provided for NON END USE CONSUMPTION (graph color: yellow) 
  df <- df %>% 
    transform(ImportNonEndUsed = 
                ifelse(Cons_Total_Enduse_ControlBlock>Prod_Total_ControlBlock & Prod_Total_ControlBlock<Cons_Total_ControlBlock,
                       Cons_Total_ControlBlock-Cons_Total_Enduse_ControlBlock,
                       0) + 
                ifelse(Cons_Total_Enduse_ControlBlock<Prod_Total_ControlBlock & Prod_Total_ControlBlock<Cons_Total_ControlBlock,
                       Cons_Total_ControlBlock-Prod_Total_ControlBlock,
                       0)
    )
  
  ##' Calc in country PRODUCTION providing NET EXPORT amount (graph color: green) 
  df <- df %>% 
    transform(NetExport= 
                ifelse(Prod_Total_ControlBlock >= Cons_Total_ControlBlock,
                       Prod_Total_ControlBlock-Cons_Total_ControlBlock,
                       0)
    )
  
  ##' Calc in country PRODUCTION providing in country NON END USE CONSUMPTION (graph color: blue) 
  df <- df %>% 
    transform(SelfProdOwnUse= 
                ifelse(Cons_Total_Enduse_ControlBlock<Prod_Total_ControlBlock &
                         Prod_Total_ControlBlock<=Cons_Total_ControlBlock,
                       Prod_Total_ControlBlock-Cons_Total_Enduse_ControlBlock,
                       0)+
                ifelse(Cons_Total_Enduse_ControlBlock<Prod_Total_ControlBlock &
                         Prod_Total_ControlBlock>Cons_Total_ControlBlock,
                       Cons_Total_ControlBlock-Cons_Total_Enduse_ControlBlock,
                       0)
    )
  
  
  ##' Assumed CO2 content of electricity
  df <- df %>% 
    transform(SwissgridCO2content=
                (28.1 * (SelfProdEndUse+SelfProdOwnUse) +
                   638.4 * (ImportEndUse+ImportNonEndUsed))/
                Cons_Total_ControlBlock)
  
}

##' === === === === === === === === === === === === === === === === ===
##' 
##' ## Function Definition for special Electricity Graph  ####
##' 
##' === === === === === === === === === === === === === === === === ===

# Not completed: Electricity Graph
dy_position<-function(data_final,valueRange,
                      plot_title, 
                      # Primary Axis Options
                      y1_OrderSeriesNames_str=NULL,y1_colors = NULL,
                      y1_axisLabel = NULL,
                      # Secondary Axis Options
                      y2_names=NULL,y2_colors = NULL, 
                      y2_axisLabel=NULL, 
                      # Default Options
                      y1_step=F, y2_step=F,
                      stacked=T, rangePad=0,
                      legendLabels=NA,legendWidth=NULL){
  
  if(!is.null(y1_OrderSeriesNames_str)){
    #reorder necessary so that all y2 are at the right end of the xts. Needed for the multibar plot
    data_final<-reorder_xts(data_final, y2_names,bar_names_ordered = rev(y1_OrderSeriesNames_str)) 
  }

  
  
  dyg <- dygraphs::dygraph(data_final, main=plot_title)
  dyg <- dygraphs::dyAxis(dyg, "x", rangePad=rangePad)
  dyg <- dygraphs::dyAxis(dyg, "y", label = y1_axisLabel,
                          axisLabelWidth = 90,valueRange = valueRange)
  
  y1_names<-colnames(data_final)[!(colnames(data_final) %in%y2_names)]
  
  if (length(y1_names)==1){
    stacked<-T #in this case only stacking works
  }
  
  if (stacked){
    dyg <- dygraphs::dyOptions(dyg,stepPlot=y1_step,stackedGraph = T,useDataTimezone=T,fillAlpha = 0.85)
    for(i in seq_along(y1_names)) {
      dyg <- dygraphs::dySeries(dyg, y1_names[i], axis = "y",
                                strokeWidth = 1.5, stepPlot = y1_step,color = y1_colors[i],
                                label=ifelse(is.na(legendLabels[i]),yes = y1_names[i],no=legendLabels[i]),
                                plotter="  function barChartPlotter(e) {
                                var ctx = e.drawingContext;
                                var points = e.points;
                                var y_bottom = e.dygraph.toDomYCoord(0);
                                
                                ctx.fillStyle = e.color;
                                
                                // Find the minimum separation between x-values.
                                // This determines the bar width.
                                var min_sep = Infinity;
                                for (var i = 1; i < points.length; i++) {
                                var sep = points[i].canvasx - points[i - 1].canvasx;
                                if (sep < min_sep) min_sep = sep;
                                }
                                var bar_width = Math.floor(2.8 / 3 * min_sep);
                                
                                // Do the actual plotting.
                                for (var i = 0; i < points.length; i++) {
                                var p = points[i];
                                var center_x = p.canvasx;
                                
                                //original bar in the middel
                                //ctx.fillRect(center_x - bar_width / 2, p.canvasy,
                                //bar_width, y_bottom - p.canvasy);
                                //original
                                //ctx.strokeRect(center_x - bar_width / 2, p.canvasy,
                                //bar_width, y_bottom - p.canvasy);

                                //bars right of the datetime
                                ctx.fillRect(center_x , p.canvasy,
                                bar_width, y_bottom - p.canvasy);

                                ctx.strokeRect(center_x , p.canvasy,
                                bar_width, y_bottom - p.canvasy);
                                }
    }")
    }
  } else {
    dyg <- dygraphs::dyOptions(dyg,stepPlot=y1_step)
    for(i in seq_along(y1_names)) {
      
      #plotter in function
      dyg <- dygraphs::dySeries(dyg, y1_names[i], axis = "y", strokeWidth   = 1.5, stepPlot = y1_step,color = y1_colors[i],
                                plotter =multibar_combi_plotter(length(y2_names)))
    }
  }
  if(!is.null(y2_names)){
    # put stuff on y2 axis
    dyg <- dygraphs::dyAxis(dyg, "y2", label = y2_axisLabel, independentTicks = F,valueRange = valueRange)
    for(i in seq_along(y2_names)) {
      dyg <- dygraphs::dySeries(dyg, y2_names[i], axis = "y2", strokeWidth = 0.5, stepPlot = y2_step,color = y2_colors[i],
                                label=ifelse(is.na(legendLabels[i+length(y1_names)]),yes = y1_names[i],no=legendLabels[i+length(y1_names)]))
    }
  }
  dyg <- dygraphs::dyLegend(dyg,labelsSeparateLines = T,width = legendWidth)
  return(dyg)
}

#we need to take into account all values and then leave out the ones we do not like
multibar_combi_plotter<-function(num_values){
  #plotter function
  plotter_text<-"function multiColumnBarPlotter(e) {
  // We need to handle all the series simultaneously.
  if (e.seriesIndex !== 0) return;
  
  var g = e.dygraph;
  var ctx = e.drawingContext;
  var sets = e.allSeriesPoints;
  var y_bottom = e.dygraph.toDomYCoord(0);
  
  // Find the minimum separation between x-values.
  // This determines the bar width.
  var min_sep = Infinity;
  for (var j = 0; j < sets.length-%s; j++) {
  var points = sets[j];
  for (var i = 1; i < points.length; i++) {
  var sep = points[i].canvasx - points[i - 1].canvasx;
  if (sep < min_sep) min_sep = sep;
  }
  }
  var bar_width = Math.floor(3.0 / 3 * min_sep/2);
  
  var fillColors = [];
  var strokeColors = g.getColors();
  for (var i = 0; i < strokeColors.length; i++) {
  fillColors.push(strokeColors[i]);
  }
  
  for (var j = 0; j < sets.length-%s; j++) {
  ctx.fillStyle = fillColors[j];
  ctx.strokeStyle = strokeColors[j];
  for (var i = 0; i < sets[j].length; i++) {
  var p = sets[j][i];
  var center_x = p.canvasx;
  var x_left = center_x - (bar_width / 2) * (1 - j/(sets.length-%s-1));
  
  ctx.fillRect(x_left, p.canvasy,
  bar_width/sets.length, y_bottom - p.canvasy);
  
  ctx.strokeRect(x_left, p.canvasy,
  bar_width/sets.length, y_bottom - p.canvasy);
  }
  }
}"

  custom_plotter <- sprintf(plotter_text, num_values, num_values, num_values)
  return(custom_plotter)
  }


reorder_xts<-function(xts_series,line_names, bar_names_ordered = NULL){
  if (is.null(bar_names_ordered) & length(colnames(xts_series)[(colnames(xts_series)%in%bar_names_ordered)])!=length(colnames(xts_series)[!(colnames(xts_series)%in%line_names)])) {
    bar_names<-colnames(xts_series)[!(colnames(xts_series)%in%line_names)]
  }else{
    bar_names <- bar_names_ordered[bar_names_ordered %in% colnames(xts_series)]
  }
  xts_series<-xts_series[,c(bar_names,line_names)]
  return(xts_series)
}




##' === === === === === === === === === === === === === === === === ===
##' 
##' ## Other Function  ####
##' 
##' === === === === === === === === === === === === === === === === ===
myWebshot <- function(FileNames, outputFile = paste0(FileName,".png")){
  # Screen Width & Height for single screen
  scr_width <- system("wmic desktopmonitor get screenwidth", intern=TRUE)[2] %>% as.numeric(.)
  scr_height <- system("wmic desktopmonitor get screenheight", intern=TRUE)[2] %>% as.numeric(.)
  
  for (FileName in FileNames) {
    webshot(FileName, file = paste0(outputFile,".png"),
            cliprect = "viewport",
            expand = c(-100, 200, 200, -100), #top, right, bottom, left
            # selector = "canvas",
            vwidth = scr_width,
            vheight = scr_height,
            # zoom=2,
            eval = "casper.then(function() {
            casper.zoom(4)
  });")
  }
}

filterXts <- function(df, xtsstring, column=1, tzone="UTC") {
  df[,1]<-with_tz(df[,1],tzone = tzone)
  dates.xts <- xts::xts(df[,1], order.by = df[[1]])
  indices <- index(dates.xts[xtsstring]) # TODO: Catch error if index does not exist.
  if (length(indices)==0){
    ret<-df[0,]
  }
  else{
    filtered <- data.frame(Col_1 = indices)
    
    #' Prepare a named character to be used in the by = statement in the join function. 
    #' Its name has to be the column name of the matching column from original
    #' 
    
    join.column <- names(df)[1]
    join.by <- "Col_1"
    names(join.by) <- names(df)[1]
    
    ret<- dplyr::inner_join(df, filtered, by = c(join.by))
  }
  return(ret)
}



df2xts <- function(df){
  # Check first column to be interpreted as date / datetime
  # Format: "YYYY" values / "YYYY-MM" values / "YYYY-MM-DD" values e.g. "2010-01-01"

  chk_class <- class(df[,1])
  
  switch (chk_class[1],
    "character" = {
        #add code which expands the string to a POSIXct convertable string adding "-01" one or two times
        if(all(unique(str_length(df[,1]))==4)){
          df[,1] <- as.POSIXct(str_c(df[,1],"-01-01"))
        }
        if(all(unique(str_length(df[,1]))==7)){
          df[,1] <- as.POSIXct(str_c(df[,1],"-01"))
        }
        else{
          df[,1] <- as.POSIXct(df[,1])
        }
      },
    "numeric" = {
        if(!all(unique(str_length(as.character(df[,1])))==4) &
           !all(stringr::str_detect(as.character(df[,1]),"\\."))){
          stop("Cannot convert to POSIXct. First column of data frame is a numeric but not uniquely in a form of a year with 4 characters.")
        }
        df[,1] <- as.POSIXct(str_c(as.character(df[,1]),"-01-01"))
      }
  )
  
  if(!any(class(as.POSIXct(str_c(as.character(df[,1]),"-01-01")))=="POSIXct")){
    stop("Conversion of first column of data frame to 'POSIXct' not possible")
  }

  
  
  if(ncol(df)>2){
    ret <- xts(df[,-1],order.by = df[,1])
  }else{
    ret <- xts(df[,-1],order.by = df[,1])
    dimnames(ret) <- list(colnames(df)[1],colnames(df)[2])
  }
  return(ret)
}

runAvgLowToHigh <- function(df,sortname) {
  # sortnames in form of c("-a","b")
  
  orderedDf<- df[do.call("order",df[sortname]), ]
  Rmean <- cumsum(orderedDf[,sortname])/seq(along=orderedDf[,sortname])
  
  # df <- filterXts(ElDat_with_Prices[,c("Datetime","Swissix_EUR_MWh")],"2016")
  # orderedDf<- df[order(df$Swissix_EUR_MWh),]
  # Rmean <- cumsum(orderedDf)/seq(along=orderedDf)
  
  return(Rmean)
}




hourly <- function(df){
  return(period.apply(df,endpoints(df,"hours"),mean))
}

lookaround <- function(x,delta){
  return(seq(x-delta,x+delta,1))
}

# Use own color bar
# colramp.own ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
colramp.own <- function(data, cols = "plasma",maxV=max(data)) {
  # Create colorRamp according to value of "data"
  # cols = c("white","red")
  
  n.spdf <- length(data) # data = spdf[,var.name]
  
  if (cols[1] == "plasma") {
    plasma.pal <- plasma(n.spdf)
    colfunc <- colorRamp(plasma.pal)
  } else {
    colfunc <- colorRamp(cols)
  } 
  
  mat.rgb.cols <- colfunc(scales::rescale(data, to = c(0,1),from=c(0,maxV)))
  mat.rgb.cols[is.na(mat.rgb.cols)] <- 0
  col.map <- apply(mat.rgb.cols, 1, function(x) rgb(x[1], x[2], x[3], maxColorValue = 255))
  return(col.map)
}
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

##' Function to plot Colorbar ## ## ## ## ## ## ## ## ## ## 
myColorbar <- function(z, zlim, col = heat.colors(12),
                       breaks, horiz=TRUE, ylim=NULL, xlim=NULL,ylab=NA,cex.ylab=NULL, ...){
  if(!missing(breaks)){
    if(length(breaks) != (length(col)+1)){stop("must have one more break than colour")}
  }
  if(missing(breaks) & !missing(zlim)){
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1)) 
  }
  if(missing(breaks) & missing(zlim)){
    zlim <- range(z, na.rm=TRUE)
    zlim[2] <- zlim[2]+c(zlim[2]-zlim[1])*(1E-3)#adds a bit to the range in both directions
    zlim[1] <- zlim[1]-c(zlim[2]-zlim[1])*(1E-3)
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
  }
  poly <- vector(mode="list", length(col))
  for(i in seq(poly)){
    poly[[i]] <- c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
  }
  xaxt <- ifelse(horiz, "s", "n")
  yaxt <- ifelse(horiz, "n", "s")
  if(horiz){YLIM<-c(0,1); XLIM<-range(breaks)}
  if(!horiz){YLIM<-range(breaks); XLIM<-c(0,1)}
  if(missing(xlim)) xlim=XLIM
  if(missing(ylim)) ylim=YLIM
  # plot(1,1,t="n",ylim=ylim, xlim=xlim, xaxt=xaxt, yaxt=yaxt, xaxs="i", yaxs="i", ...)  #original
  plot(1,1,t="n",ylim=ylim, xlim=xlim, xaxt=xaxt, yaxt="n", xaxs="i", yaxs="i",ylab=NA, ...)
  for(i in seq(poly)){
    if(horiz){
      polygon(poly[[i]], c(0,0,1,1), col=col[i], border=NA)
    }
    if(!horiz){
      polygon(c(0,0,1,1), poly[[i]], col=col[i], border=NA)
    }
  }
}
