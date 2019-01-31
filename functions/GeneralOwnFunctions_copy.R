# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
#
# GENERAL (OWN) FUNCTIONS ----
#
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# GENERAL FUNCTIONS ----
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# qq ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
qq <- function (save="no", ...) {
  # quit without saving
  quit(save=save, ...)
}


# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# TIME SERIES ----
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# filterXts ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
filterXts <- function(df, xtsstring, by_name = NULL) {
  if(is.null(by_name)) by_name=names(df)[1]
  # check if by_name is string and if it is part of colnames(df)
  if(!is.element(by_name,colnames(df))) stop(paste0("Argument by_name= \"",by_name,"\" is not a column name of dataframe df."))
  
  dates.xts <- xts::xts(df[,by_name], order.by = df[[1]])
  indices <- zoo::index(dates.xts[xtsstring])
  filtered <- data.frame(Col_1 = indices)
  
  #' Prepare a named character to be used in the by = statement in the join function. 
  #' Its name has to be the column name of the matching column from original
  #' 
  
  #' Creating a named Vector for inner_join its by argument
  join.by <- "Col_1"
  names(join.by) <- names(df[by_name])
  
  return(dplyr::inner_join(df, filtered, by = c(join.by)))
}
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# SPATIAL DATA (GIS) ----
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# all.same.projection ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
all.same.projection <- function(projection = "+init=epsg:21781") {
  # assing the same projectoin (default: LV03) to all spatial (sp) objects
  
  names.sp.obj_c <- names(which(unlist(eapply(.GlobalEnv, 
                                              function(x) attr(class(x),"package") == "sp"))))
  
  for (i in 1:length(names.sp.obj_c)) {
    print(names.sp.obj_c[i])
    
    eval.parse <- eval(parse(text = names.sp.obj_c[i]))
    
    crs.tmp <- raster::crs(eval.parse)
    
    if (is.na(crs.tmp)) {
      print("*** new crs")
      sp::proj4string(eval.parse) <- raster::crs(projection) # if no crs --> NA
      assign(names.sp.obj_c[i], 
             eval.parse, 
             envir = .GlobalEnv)
    } else if (!identical(crs.tmp, raster::crs(projection))) {
      print("*** change crs")
      assign(names.sp.obj_c[i],
             sp::spTransform(eval.parse, raster::crs(projection)),
             envir = .GlobalEnv)
    } else {
      print("*** identical crs")
    }
  }
}
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# choropleth_own ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
choropleth_own <- function(spdf, var.name, colbar.title, divisor = 1, ...) {
  # choropleth with viridis (plasma) color scale
  
  spdf@data[,var.name] <- spdf@data[,var.name] / divisor
  
  n.spdf <- length(spdf[,var.name])
  plasma.pal <- plasma(n.spdf)
  colfunc <- colorRamp(plasma.pal)
  mat.rgb.cols <- colfunc(scales::rescale(spdf@data[,var.name], to = c(0,1)))
  mat.rgb.cols[is.na(mat.rgb.cols)] <- 0
  col.map.hex <- apply(mat.rgb.cols, 1, function(x) rgb(x[1], x[2], x[3], maxColorValue = 255))
  
  # Zero (0) color b(=black)
  col.map.hex[spdf@data[,var.name] == 0] <- rgb(0.5,0.5,0.5)
  
  # set color for colorbar
  col.bar.scale <- plasma(50)
  col.bar.scale[1] <- rgb(0.5,0.5,0.5)
  
  # Plot
    plot(spdf,
       border = NA,
       col = col.map.hex, ...)
  # add color bar from library(fields)
  image.plot(legend.only = T, 
             zlim = c(0, max(spdf@data[,var.name], na.rm = TRUE)), 
             col = col.bar.scale,
             legend.width = 0.6,
             legend.shrink = 0.4,
             legend.mar = 4.5,
             axis.args = list(cex.axis=1, lwd.ticks = 0.5),
             legend.args = list(text=colbar.title, side=1, font=2, line=2.5, cex=1.25),
             horizontal = T)
}
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---


# sp_join (left_join) ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
sp_join <- function(spdf.obj.1, df.2, by.vars, var.names = "all", force.join = F) {
  # left_join of spatial data frames
  # spdf.obj.1 = join to this spatial data frame
  # df.2 = data frame (spatial or normal) to join to spdf.obj.1
  # by.vars = join by this ID --> e.g c("A" = "B") 
  # --> LINK: https://stackoverflow.com/questions/28399065/dplyr-join-on-by-a-b-where-a-and-b-are-variables-containing-strings
  # var.names = which variables of df.2 to join to spdf.obj.1 --> e.g c("A", "B") (default: "all")
  # force.join = if TRUE data.frames will be join even if any variable name of df.2 already 
  # exists in spdf.obj.1

  # extract by.vars
  by.1 <- names(by.vars)
  by.2 <- unname(by.vars)
  if(is.null(by.1)) { by.1 <- by.2}

  # select variables of df.2 to join (if not "all")
  if (var.names != "all") { df.2 <- df.2[,c(by.2, var.names)] }
  
  # convert df.2 to data.trame
  df.2 <- as.data.frame(df.2)
  
  # check of data.frames have been joined all ready
  if(force.join == F) {
    omit.1 <- which(by.1 == names(spdf.obj.1))
    omit.2 <- which(by.2 == names(df.2))
    
    names.1 <- names(spdf.obj.1)[-omit.1]
    names.2 <- names(df.2)[-omit.2]
    
      if (sum(names.1 %in% names.2) > 0) {
        print("data.frames have been joined already")
        return(spdf.obj.1)
      }
    }

  # join
  spdf.obj.1@data <- dplyr::left_join(spdf.obj.1@data, 
                                      df.2, 
                                      by = by.vars)

  # return 
  return(spdf.obj.1)

}


# colramp.own ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
colramp.own <- function(data, cols = "plasma") {
  # Create colorRamp according to value of "data"
  # cols = c("white","red")
  
  n.spdf <- length(data) # data = spdf[,var.name]
  
  if (cols[1] == "plasma") {
    plasma.pal <- plasma(n.spdf)
    colfunc <- colorRamp(plasma.pal)
  } else {
    colfunc <- colorRamp(cols)
  } 
  
    mat.rgb.cols <- colfunc(scales::rescale(data, to = c(0,1)))
  mat.rgb.cols[is.na(mat.rgb.cols)] <- 0
  col.map <- apply(mat.rgb.cols, 1, function(x) rgb(x[1], x[2], x[3], maxColorValue = 255))
  return(col.map)
}
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---