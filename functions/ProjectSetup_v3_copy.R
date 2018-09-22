### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### -- Project Setup File -- 
#
# Authors: Martin Ruedisueli (ruma), Sinan Teske (tes)
# Last update: 31-08-2018 (ruma)
# (c) Empa, 2018
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# RESTORE WORKSPACE (last saved ---> .Rdata) ----
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
if (file.exists("./.Rdata")) { load("./.Rdata") }


# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# OPERATORS ----
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
'%ni%' <- Negate('%in%')


# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# PROJECT FUNCTIONS ----
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Meta data ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.metaData <- function(data_frame, var_desc = NULL, var_type = NULL, var_units = NULL, 
                              main_desc = "no description", type = "munged", ...) {
  # Source 1: https://cran.r-project.org/web/packages/dataMeta/vignettes/dataMeta_Vignette.html
  # Source 2: https://www.r-bloggers.com/adding-metadata-to-variables/
  # library(dataMeta)
  # library(knitr)
  # library(stringr)
  
  # number of variables/columns
  ncol_df <- ncol(data_frame)
  
  if (is.null(var_desc)) var_desc <- rep("-", ncol_df)
  if (is.null(var_type)) var_type <- rep(0, ncol_df)
  if (is.null(var_units)) var_units <- rep(NA, ncol_df)

    # create variable description
    # desc_data_frame <- data_frame[1,]
    # desc_data_frame <- rbind(desc_data_frame, var_desc, var_type)
    # desc_data_frame[1,] <- NULL
    # 
    # # edit variable description
    # data.entry(desc_data_frame)
    # 
    # # write description and type of varibales
    # var_desc <- desc_data_frame[1,]
    # var_type <- desc_data_frame[2,]
  
  # create linke
  linker <- build_linker(data_frame, variable_description = var_desc, variable_type = var_type)
  
  # create dictionary
  dict <- build_dict(my.data = data_frame, 
                     linker = linker, 
                     option_description = var_units,
                     prompt_varopts = FALSE)
  
  # add main title (main_desc)
  my_new_data <- incorporate_attr(my.data = data_frame, data.dictionary = dict, main_string = main_desc)
  
  # rename columns
  colnames(attributes(my_new_data)$dictionary) <- c("variable_name", "variable_description", "variable_options", "variable_units")
  
  # add further attributes
  more_attr <- list(...)
  attr_names <- names(more_attr)
  for(i in seq_along(more_attr))
  {
    attr(my_new_data, attr_names[i]) <- more_attr[[i]]
  }
  
  # Exporting dictionary only
  attr_all <- attributes(my_new_data)
  
  # Create filename and path
  file.name <- deparse(substitute(data_frame))
  file.path <- paste0("./cache/", type, "/", type, ".", file.name, ".xml")
  file.path_no_ext <- tools::file_path_sans_ext(file.path)
  
  # write txt file
  write("General description:", file.path)
  write(paste("\t", attr_all$main, "\n"), file.path, append = T)
  write("Variables:", file.path, append = T)
  write(paste("\t", attr_all$dictionary$variable_name, 
              "--->", attr_all$dictionary$variable_units,
              "--->", attr_all$dictionary$variable_description,
              "--->", attr_all$dictionary$variable_options), file.path, append = T)
  write("", file.path, append = T)
  write("Last edited:", file.path, append = T)
  write(paste("\t", attr_all$last_edit_date, "\n"), file.path, append = T)
  write("Author:", file.path, append = T)
  write(paste("\t", attr_all$author, "\n"), file.path, append = T)
  
  for(i in seq_along(more_attr)) {
    write(paste0(str_to_title(attr_names[i]),":"), file.path, append = T)
    write(paste("\t", more_attr[[i]], "\n"), file.path, append = T)
  }
  
  # Saving as .rds (dataset with appended dictionary) in cache
  save_it(x = my_new_data, name_of_file = file.path_no_ext)
  
  # return data frame with metadata
  return(my_new_data)
  
}

# EXAMPLE:
# my_data <- .project.metaData(data_frame = my_data,
#                              var_desc = c("Date when report was published",
#                                           "Regional location",
#                                           "Description of regional location",
#                                           "Type of case",
#                                           "A specific code for each data field",
#                                           "The time period of each week",
#                                           "The type of time period",
#                                           "The number of cases per data field type",
#                                           "The unit in which cases are reported"),
#                              var_type = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
#                              var_units = c("cm", "cm", "cm", "cm", "cm", "cm", "cm", "cm", "cm"),
#                              main_desc = "This data set portrays Zika infection related cases as reported by USVI.",
#                              source = "BFE",
#                              source_time = "01.02.2018",
#                              location_Name = "Bern",
#                              location_LV03 = "600000, 300000",
#                              keyword = "A, B, C, D",
#                              copied_from = "BAFU_Strompreise",
#                              type = "munged")

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Convert pdf to png ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.pdf.2.png <- function(file, dpi = 150) {
  # "convert -loop 0 -density 150 -delay 100 C:\\GIT_clones\\BAFU_R_all\\figures\\image87a.pdf \"C:\\GIT_clones\\BAFU_R_all\\figures\\image87a.png\""
  
  proj.main.wd.tmp <- gsub("/", "\\", getwd(), fixed = T)
  proj.main.wd <- gsub("Users\\ruma\\Documents\\GIT_clones_lokal", "GIT_clones", proj.main.wd.tmp, fixed = T)
  
  cmd <- paste("convert -loop 0 -density", dpi, "-delay 100", 
               paste0(proj.main.wd,"\\figures\\", file, ".pdf"),
               paste0("\"", proj.main.wd, "\\figures\\", file, ".png", "\""))

  shell(cmd)
    
}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Copy figures (with meta data .txt) to latex figures folder ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.latex.figures <- function(fig.name, working.dir, ext = ".png") {
  
  file.copy(from = paste0("./figures/", fig.name, ext),
            to = paste0(working.dir, fig.name, ext),
            overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
  
  file.copy(from = paste0("./figures/", fig.name, ".txt"),
            to = paste0(working.dir, fig.name, ".txt"),
            overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
  
}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# install and load multiple R packages. ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# source: https://gist.github.com/stevenworthington/3178163
# 
# usage: 
# packages <- c("ggplot2", "plyr", "reshape2", "RColorBrewer", "scales", "grid")
# ipak(packages)

.project.ipak <- function(pkg){
  
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Write R versions log (.txt) ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.r.ver <- function() {
  
  # get current R version (major and minor)
  r.ver <- paste0(R.version$major, ".", R.version$minor)
  
  if (file.exists("./R-Version-log.txt")){
  # read log file
  log.ver <- read.table("./R-Version-log.txt")$V2
  
  # add new version (with Date Time) if exists
  if (r.ver != tail(log.ver,1)) {
    # initialize log-file
    write(paste0("R-version: ", r.ver, " (", Sys.time(), ")"),
          append = T,
          file = "./R-Version-log.txt")
  }
  } else {
    # initialize log-file
    write(paste0("R-version: ", r.ver, " (", Sys.time(), ")"),
          file = "./R-Version-log.txt")
  }
}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Get Project UUID from main project working directory ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.uuid <- function(working.dir = getwd()) {
  
  # get #PJ-Project file
  txt.files <- list.files(working.dir, pattern = "#PJ_*")
  
  # get UUID
  uuid.prj <- read.table(txt.files)$V1
  
  return(uuid.prj)
  
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Get SHA (UUID) GIT commit of main project working directory ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.git.sha <- function(working.dir = getwd()) {
  
  library(git2r)
  
  repos <- git2r::repository(working.dir)
  sha <- git2r::revparse_single(repo = repos,"HEAD")$sha
  
  return(sha)
  
}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Create metadata file UUID for figures  ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.uuid.metafile <- function(file.name, uuid.prj, uuid.fig, uuid.git) {
  
  write(paste("PRJ-UUID:", uuid.prj, 
              "\nFigure-UUID:", uuid.fig,
              "\nCOMMIT:", uuid.git),
        file = paste0("./figures/", file.name, ".txt"))

}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Clear workspace except for project functions ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.rm.ls <- function() {

  rm(list = setdiff(ls(envir = globalenv()), 
                    lsf.str(pattern = "*project.*", envir = globalenv())), envir = globalenv())
  
}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Copy/Rename "GeneralOwnfunctions.R" from global to local (to folder "functions") ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.update.GenOwnFct <- function() {
  
  # install/load git2r
  library(git2r)
  
  # move (back-up) (locale) GeneralOwnFunctions_copy.R to folder "old GeneralOwnFunctions"
  if( file.exists("./functions/GeneralOwnFunctions_copy.R") ) {
    time.stamp <- format(Sys.time(), format = "%Y-%m-%d-%H-%M-%S", tz = "GMT")
    file.copy(from = "./functions/GeneralOwnFunctions_copy.R",
              to = paste0("./functions/old GeneralOwnFunctions/", 
                          time.stamp, "_GeneralOwnFunctions_copy.R"),
              overwrite = TRUE,  copy.mode = TRUE, copy.date = TRUE)
    file.copy(from = "./functions/GeneralOwnFunctions_copy.txt",
              to = paste0("./functions/old GeneralOwnFunctions/", 
                          time.stamp, "_GeneralOwnFunctions_copy.txt"),
              overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
    print("Copy GeneralOwnFunctions_copy.R to <old> folder completed")
  }
  
  # copy GeneralOwnFunctions.R and rename
  file.copy(from = "../_NewProject/GeneralOwnFunctions.R",
            to = "./functions/GeneralOwnFunctions_copy.R",
            overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)

  # write GIT SHA of current GeneralOwnFunctions.R
  write(paste0("GIT SHA of repo <_NewProject>:\n", .project.git.sha("../_NewProject")), 
        file = "./functions/GeneralOwnFunctions_copy.txt")
  
}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Copy/Rename "ProjectSetup_v2.R" from global to local (to folder "functions") ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.update.ProjectSetup <- function() {
  
  # install/load git2r
  library(git2r)
  
  # move (back-up) (local) ProjectSetup_v3_copy.R to folder "old ProjectSetup"
  if( file.exists("./functions/ProjectSetup_v3_copy.R") ) {
    time.stamp <- format(Sys.time(), format = "%Y-%m-%d-%H-%M-%S", tz = "GMT")
    file.copy(from = "./functions/ProjectSetup_v3_copy.R",
              to = paste0("./functions/old ProjectSetup/", 
                          time.stamp, "_ProjectSetup_v3_copy.R"),
              overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
    file.copy(from = "./functions/ProjectSetup_v3_copy.txt",
              to = paste0("./functions/old ProjectSetup/", 
                          time.stamp, "_ProjectSetup_v3_copy.txt"),
              overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
    print("Copy ProjectSetup_v3_copy.R to <old> folder completed")
  }
  
  # copy ProjectSetup_v2 and rename
  file.copy(from = "../_NewProject/ProjectSetup_v3.R",
            to = "./functions/ProjectSetup_v3_copy.R", 
            overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
  
  # write GIT SHA of current GeneralOwnFunctions.R
  write(paste0("GIT SHA of repo <_NewProject>:\n", .project.git.sha("../_NewProject")), 
        file = "./functions/ProjectSetup_v3_copy.txt")

}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Write caption for munge.R  ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.write.caption <- function(title = "some string", script = "munge", level = 1) {
  
  if (script == "munge") {
      write("# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###", 
            file="./munge/Munge.R", append = T)
      write(paste("#", title, "----"), 
            file="./munge/Munge.R", append = T)
      write("# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###\n\n", 
            file="./munge/Munge.R", append = T)
  }
}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Rename and copy raw data variables (and add _class) ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.rename.var <- function(variable, variable.name, del.raw.flag = F, add.flag = F) {
  # [variable] to be renamed
  # new [variable.name] (string)
  # del.raw.flag = remove raw data variabel from environment (default = TRUE)
  
  # select variable class
  if (add.flag) {
    class.var <- switch (class(variable),
                      "data.frame" = "df",
                      "SpatialPolygonsDataFrame" = "spdf",
                      "SpatialPolygons" = "sp")

    # add class to new variable name
    variable.name <- paste0(variable.name, "_", class.var)
  }
  
  
  # assign variable to new variable name and save globally
  assign(variable.name, variable, envir = .GlobalEnv)
  
  # Print structure of data
  str(variable)

  # remove varialbe from workspace
  if (del.raw.flag) {
    rm(list=deparse(substitute(variable)), envir=.GlobalEnv)
  } 
}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Reset type/class of variable (e.g. string to numeric) ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.retype.var <- function(df, variable, type = "num") {
  # df = data.frame
  # [variable] (colume) to be renamed (string)
  # [type] (eg. numeric, factor, character)

  assign(substitute(df), 
           df[,variable] <- switch(type,
                                  "num" = {as.numeric(df[,variable])},
                                  "fac" = {as.factor(df[,variable])},
                                  "char" = {as.character(df[,variable])}), 
           envir = .GlobalEnv)

}
  

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# Load additional packages --- (deprecated) use pacman::p_load instead
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# .project.library <- function(library_name) {
#   
#   # list packages in checkpoint
#   checkpoint_path <- .libPaths()[1]
#   checkpoint_list <- list.files(checkpoint_path)
# 
#   # install packages (if not installed yet)
#   if (library_name %in% checkpoint_list) {
#       print(paste(library_name, "installed in checkpoint"))
#   } else {install.packages(library_name, lib = .libPaths()[1], dep = T)}
#     
#   # load packages
#   library(library_name, character.only = T)
# 
#   print(paste("Package", library_name, "has been loaded ..."))
#   
# }

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Checkpoint ---
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# .project.checkpoint <- function(checkpoint.date = Sys.Date()-1, copy.flag = F) {
#   # Sourcer: https://cran.r-project.org/web/packages/checkpoint/vignettes/checkpoint.html
#   # checkpoint.date = "today" [default] or e.g "2017-12-18"
#   
#   # start checkpoint library from user library
#   if ("checkpoint" %in% (.packages())) {
#     library("checkpoint")
#   }
# 
#   # initialize Checkpoint
#   checkpoint(snapshotDate = checkpoint.date,
#              checkpointLocation = ".",
#              scanForPackages = FALSE)
#   
#   # remove tmp. files (file4a....)
#   list.file4a <- list.files(.libPaths()[1], pattern = "file*", full.names = T)
#   sapply(list.file4a, function(x) unlink(x, recursive = T, force = T))
#   
#   if(copy.flag) {
#     
#   # list of packages in standard_packages
#   list_of_packages <- list.files("../_NewProject_PackratPackages/standard_packages")
#     
#   # copy standard folders (if not existing)
#     print("*** Copy standard packages ...")
#     file.copy(from = file.path("../_NewProject_PackratPackages/standard_packages",list_of_packages),
#               to = .libPaths()[1],
#               overwrite = TRUE,
#               recursive = TRUE,
#               copy.mode = TRUE,
#               copy.date = TRUE)
#     print("*** Standard packages copied ...")
#   }
#   
# 
#   
# }

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Read raw data ---
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.read.data <- function(refresh.flag = F) {
  # refresh.flag = FALSE --> read only new files
  # refresh.flag = TRUE --> read all files again


  # subfolders in "data" (--> csv, shp, ...)
  subfolders.data <- list.dirs(path = "./data/", full.names = F, recursive = F)
  if (file.exists("./data/acceptedDataTypes.R")) {
    source("./data/acceptedDataTypes.R")
    print(paste0("Accepted data types are: ", paste0(File_Extensions, sep = " ", collapse = "")))
  }
  else {
    File_Extensions <- c("csv", "rds", "shp", "xls", "xlsx")
    warning(paste0("Could not find '",
                   "./data/acceptedDataTypes.R",
                   "'. Used these file extensions instead: ",
                   File_Extensions))
  }

  # read files loop
  for (j in 1:length(subfolders.data)) {
    path.tmp <- paste0("./data/", subfolders.data[j])
    if (!identical(tools::list_files_with_exts(path.tmp,File_Extensions,full.names = F), character(0))) {

    # check for lokal data_read_func.R and load it
      if (file.exists(paste0(path.tmp, "/data_read_func.R"))) {
        source(paste0(path.tmp, "/data_read_func.R"))
      }
      else {
        stop(paste0("data_read_func is missing in ",path.tmp))
      }
    # checking if data_read_func was defined in local data_read_func.R
      if (!exists("data_read_func", mode="function")) {
        stop(paste0("Loading data_read_func function from ", path.tmp,"/data_read_func.R", " failed!"))
      }

      # full path
      list.full <- tools::list_files_with_exts(path.tmp, File_Extensions, full.names = T)

      # only file names (no extention, path, ..)
      list.names <- tools::file_path_sans_ext(tools::list_files_with_exts(path.tmp, File_Extensions,
                                                                          full.names = F))

      # abbriviate file names
      # list.names.abbr <- substring(list.names, 1, 12)

      # read files
      for (i in 1:length(list.full)) {
      # read all files again
        if (refresh.flag) {
          assign(list.names[i],
                 data_read_func(list.full[i]),
                 envir = .GlobalEnv)
          .project.save.cache.raw(list.names[i], type = "raw")  # save .rda to cache
        }
        # read only new files
        else if (!file.exists(paste0("./cache/raw/raw.", list.names[i], ".Rdata"))) {
          assign(list.names[i],
                 data_read_func(list.full[i]),
                 envir = .GlobalEnv)
          .project.save.cache.raw(list.names[i], type = "raw")  # save .rda to cache
        }
        else {
          print(paste0(list.names[i], ".Rdata already exists"))
        }

      }

      if(exists("data_read_func", mode="function")){
        rm("data_read_func", envir = .GlobalEnv)
      }
    }
  }
}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Save munged/analysed variables to cache ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.save.cache <- function(..., type = "munged") {

  # create list
  list <- sapply(substitute(list(...))[-1], deparse) 
  # https://stackoverflow.com/questions/5754367/using-substitute-to-get-argument-name-with

  # save as .rda to cache
  for (i in 1:length(list)) {
    save(list = list[i], file = paste0("./cache/", type, "/", type, ".", list[i], ".Rdata"))
    print(paste(list[i], " saved in cache ..."))
  }

}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Save raw data to cache ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.save.cache.raw <- function(list = names(...), type = "raw") {
  # Source --> https://stackoverflow.com/questions/4675755/how-to-save-with-a-particular-variable-name
  for (i in 1:length(list)) {
    save(list = list[i], file = paste0("./cache/", type, "/", type, ".", list[i], ".Rdata"))
    print(paste(list[i], " saved as .rda ..."))
  }
}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Load variables from cache ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.load.cache <- function(type = "munged", var.name = NULL) {  
  # type = c("raw", "munged", "analysed")
  # var.name = single varialbe to load from cache
  # use Feather
  
  # if (!identical(list.files("./cache/munged"), character(0)) & type == "munged") { 
  #   type = "munged"
  # } else if (!identical(list.files("./cache/analysed"), character(0)) & type == "analysed") {
  #   type = "analysed"
  # } else {
  #   type = "raw"
  # }

  list.rda <- list.files(paste0("./cache/", type),
                                pattern = paste0(type, ".*.Rdata"), 
                                full.names = T)
  
  list.rda.short <- list.files(paste0("./cache/", type),
                         pattern = paste0(type, ".*.Rdata"))
  
  # load single variable [var.name] (string) from cache
  if (!is.null(var.name)){
    load(paste0("./cache/", type, "/", paste0(type, ".", var.name, ".Rdata")), 
         envir = .GlobalEnv)
    print(paste(var.name, "has been loaded ..."))
  # load all variables from cache
  } else { 
      if (length(list.rda)>0) {
        for (i in 1:length(list.rda)) {
          load(list.rda[i], envir = .GlobalEnv)
          print(paste(list.rda.short[i], "has been loaded ..."))
        }
      }
      else {
        warning("Cache is empty. Not loading anything.")
      }
  }
}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Save and load analysed cache (variables) ---
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# .rda.save.load.project <- function(refresh = F, variable, FUN, ...) {
#   list.rda <- list.files("./cache/analysed")
#   if (refresh) {
#     tmp <- FUN(...)
#     assign(variable, tmp, envir = .GlobalEnv)
#     .project.save.rda(variable, type = "analysed")
#   } else if (paste0("analysed.", variable, ".Rdata") %in% list.rda) {
#     load(paste0("./cache/analysed/analysed.", variable, ".Rdata"), envir = .GlobalEnv)
#   } else {
#     tmp <- FUN(...)
#     assign(variable, tmp, envir = .GlobalEnv)
#     .project.save.rda(variable, type = "analysed")
#   }
#   print("variable (saved and) loaded ...")
# }

  
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Source own functions ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.own.fcts <- function () {
  source("./functions/OwnFunctions.R")
  print("Own functions loaded ...")
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Source Munge "data" ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.munge <- function() {
  source("./munge/Munge.R")
  print("data has been munged ...")
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Load packages from local package library ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.load.packages <- function() {
  
  # get current R version (major and minor)
  r.ver <- paste0(R.version$major, ".", R.version$minor)
  
  # get path to current R.version folder
  path.r.ver <- paste0("./local_package_library/", r.ver)

  # set local package library to current R.version
  .libPaths(path.r.ver)
  
  # list packages in local package library
  package.list <- list.files(path.r.ver)
  
  # load packages with pacman
  if (length(package.list)==0){ # TODO: work around to check if lib path has been newly created 
    install.packages("pacman")
  }
  library(pacman)
  p_load(char = package.list)

}


# === === === === === === === === === === === === === === === === === ===
# Set-up (initiate) project ----
# === === === === === === === === === === === === === === === === === ===
project.setup <- function(setup.flag = T, script.flag = T, folder.flag = T, 
                          read.flag = T, munge.flag = T, fct.flag = T, 
                          load.rda.flag = T, script.setup = T, OwnFct.flag = T) {
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  # Set-up Options ----
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  if (setup.flag) {

  # define main folders
  folders <- c("data", "functions", "cache", "figures", "scripts", 
               "munge", "reports", "misc", "tests", "local_package_library")
  
  # define subfolders
  subfolders.data <- c("csv", "rds", "shp","excel")
  subfolders.rda <- c("raw", "munged", "analysed")
  subfolders.fct <- c("old GeneralOwnFunctions", "old ProjectSetup")
  
  }
  
  
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  # Create Folders and Subfolders ----
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  if (folder.flag) {
  print("Create folders ...")
    # Create folder structure (if not existing)
    for (i in 1:length(folders)) {
      if (file.exists(folders[i])) { 
        print(paste("*** ", folders[i], "exists"))
      } else {dir.create(folders[i])}
    }
    
    # Create "data" subfolder structure (if not existing)
    file.copy(from = "../_NewProject/data",
              to = getwd(),
              overwrite = TRUE,
              recursive = TRUE,
              copy.mode = TRUE,
              copy.date = TRUE)
    
    # Create "rda" subfolder structure (if not existing)
    for (i in 1:length(subfolders.rda)) {
      if (file.exists(paste0("./cache/", subfolders.rda[i]))) { 
        print(paste("*** ", subfolders.rda[i], "exists"))
      } else {dir.create(paste0("./cache/", subfolders.rda[i]))}
    }
    
    # Create "functions" subfolder structure (if not existing)
    for (i in 1:length(subfolders.fct)) {
      if (file.exists(paste0("./functions/", subfolders.fct[i]))) { 
        print(paste("*** ", subfolders.fct[i], "exists"))
      } else {dir.create(paste0("./functions/", subfolders.fct[i]))}
    }
  }
  
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  # Copy/Install standard packages ----
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  
  # get R version (major and minor)
  r.ver <- paste0(R.version$major, ".", R.version$minor)
  
  # set local package library
  .libPaths(paste0("./local_package_library/", r.ver))
    
  # copy standard folders (if not existing)
  file.copy(from = paste0("../_NewProject/local_standard_package_library/",
                          r.ver, ".tar"),
            to = "./local_package_library",
            overwrite = TRUE,
            recursive = TRUE,
            copy.mode = TRUE,
            copy.date = TRUE)
  print("*** Standard packages copied ...")
  
  # untar
  untar(paste0("./local_package_library/", r.ver, ".tar"),
        exdir = "./local_package_library")
  print("*** Standard packages untarred ...")
  
  # load standard packages (library(...))
  .project.load.packages()
  print("*** Standard packages loaded ...")

  
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  # Write project UUID file ----
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

  # # get project working directory
  # working.dir <- getwd()
  # 
  # # get # PJ_*.txt file (if it exists)
  # pj.txt.file <- list.files(working.dir, pattern = "#PJ_*")
  # 
  # if (identical(pj.txt.file, character(0))) {
  #     
  # # generate uuid
  # uuid.prj <- UUIDgenerate()
  # 
  # # get project name
  # proj.name <- basename(working.dir)
  # 
  # # write .txt file
  # write(uuid.prj,
  #       file = paste0("./#PJ_", proj.name, "_", uuid.prj, ".txt"))
  # }
  
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  # Write R-version log file ----
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  
  # get R version (major and minor)
  r.ver <- paste0(R.version$major, ".", R.version$minor)
  
  # initialize log-file
  write(paste0("R-version: ", r.ver, " (", Sys.time(), ")"),
                 file = "./R-Version-log.txt")
  # write file
  .project.r.ver()
  

  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  # Create empty script files "Test_Script.R", "OwnFunctions.R", "Munge.R", "Main_Script.R" ----
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  if (script.flag) {
  print("Add .R script files ...")
    
    # Test_Script.R
    if (file.exists("./tests/Test_Script.R")) { 
      print("*** Test_Script.R exists")
    } else {
      file.create("./tests/Test_Script.R")
      write("# SCRIPT FOR TESTING \n### ### ### ### ### ### ### ### ### ### ###\n",
            file=("./tests/Test_Script.R"))
      print("*** Test_Script.R created")}
    
    # Munge.R
    if (file.exists("./munge/Munge.R")) { 
      print("*** Munge.R exists")
    } else {
      file.create("./munge/Munge.R")
      
      write("# DATA MUNGING \n#\n# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###\n", 
            file = "./munge/Munge.R")
      
      .project.write.caption("Notes / ReadMe")
      .project.write.caption("load project and raw data")
      .project.write.caption("rename and delete raw data")
      .project.write.caption("select columns")
      .project.write.caption("filter rows")
      .project.write.caption("change variable types (e.g. as.numeric)")
      .project.write.caption("adjust coordinates (projection)")
      .project.write.caption("do this and that 1")
      .project.write.caption("do this and that 2")
      .project.write.caption("create new data manually")
      .project.write.caption("remove tmp variables")
      .project.write.caption("save munged data to cache")
      
      print("*** Munge.R created")}
    
    # OwnFunctions.R
    if (
      file.exists("./functions/OwnFunctions.R")) { 
      print("*** OwnFunctions.R exists")
    } else {
      file.create("./functions/OwnFunctions.R")
      
      write("### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###",
            file="./functions/OwnFunctions.R")
      write("#\n# ADDITIONAL PACKAGES ####\n#",
            file="./functions/OwnFunctions.R", append = T)
      write("### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###\n",
            file="./functions/OwnFunctions.R", append = T)
      write("# additional packages",
            file="./functions/OwnFunctions.R", append = T)
      write("# p_load(AddPackage)\n\n",
            file="./functions/OwnFunctions.R", append = T)
      # unmask packages (prioritise)
      write("# unmask packages (prioritise)",
            file="./functions/OwnFunctions.R", append = T)
      write("library(dplyr)\n\n\n",
            file="./functions/OwnFunctions.R", append = T)
      
      
      write("### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###",
            file="./functions/OwnFunctions.R", append = T)
      write("#\n# USER-DEFINED (OWN) FUNCTIONS ####\n#",
            file="./functions/OwnFunctions.R", append = T)
      write("### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###\n\n",
            file="./functions/OwnFunctions.R", append = T)
      write("# MyFct ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###\n",
            file="./functions/OwnFunctions.R", append = T)
      
      print("*** OwnFunctions.R created")}
    
    # Main_Script.R
    if (file.exists("./scripts/Main_Script.R")) { 
      print("*** Main_Script.R exists")
    } else {
      file.create("./scripts/Main_Script.R")
      write("### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###",
            file=("./scripts/Main_Script.R"))
      write("#\n# TITLE \n#\n# Author: XY \n# Date: DD/MM/YYY", 
            file=("./scripts/Main_Script.R"), append = T)
      write("### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###",
            file=("./scripts/Main_Script.R"), append = T)
      write("cat(\"\\014\") # clear console", file=("./scripts/Main_Script.R"), append = T) 
      write("project.load() # load project \n \n", 
            file=("./scripts/Main_Script.R"), append = T)
      print("*** Main_Script.R created")
      }
  }
  
  
  

  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  # Copy "GeneralOwnFunctions.R" and "ProjectSetup.R" in to "functions" folder  ----
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  if (OwnFct.flag) {
    .project.update.GenOwnFct()
    .project.update.ProjectSetup()
  }
  
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  # Open "Main_Script.R" file ----
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  if (script.setup) {
    file.edit("./munge/munge.R")
    file.edit("./functions/OwnFunctions.R")
    file.edit("./scripts/Main_Script.R")
  }
  
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  # First commit  ----
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  # working.dir <- getwd()
  # repo <- git2r::repository(working.dir)
  # add(repo, "./c.txt")
  # git2r::commit(repo, message = "First commit", all = T)
  
  
}




# === === === === === === === === === === === === === === === === === ===
# Reload project (not from .Rdata!) ####
# === === === === === === === === === === === === === === === === === ===
project.load <- function(type = "munged", update_functions = F) {
  
  # check for new r-version and add to log (if changed)
  .project.r.ver()
  
  # remove workspace expect project functions
  .project.rm.ls()
  
  # get current R version (major and minor)
  r.ver <- paste0(R.version$major, ".", R.version$minor)
  
  # set .libPaths to current R.version
  path.r.ver <- paste0("./local_package_library/", r.ver)
  
  # set local package library
  if (dir.exists(path.r.ver)){
    .libPaths(path.r.ver)
  }
  else{
    dir.create(path.r.ver)
    .libPaths(path.r.ver)
    
    # stop(paste0("The path ",path.r.ver,"does not exist. Consider installing the right version of R or creating a library folder with correct R version number."))
  }
  
  # update ProjectSetup.R and GeneralOwnFunctions
  if (update_functions) {
    .project.update.GenOwnFct()
    .project.update.ProjectSetup()
  }
  
  # source "GeneralOwnFunctions_copy.R"
  source("./functions/ProjectSetup_v3_copy.R")
  
  # source "GeneralOwnFunctions_copy.R"
  source("./functions/GeneralOwnFunctions_copy.R")
  
  # load packages
  .project.load.packages()
  print("Packages loaded")
  
  # load own functions and additional packages therein
  .project.own.fcts()
  
  # load cached datasets
  .project.load.cache(type = type)
  
  # prioritise (masked) functions (e.g dplyr --> select)
  # .project.prioritise.fcts()
  
  # Open files
  # file.edit("./functions/OwnFunctions.R")
  # file.edit("./munge/Munge.R")
  # file.edit("./scripts/Main_Script.R")
  
  # cat("\014") # clear console
  print("*** Project has been successfully loaded ...")
  
  # print warnings
  warnings()
  
}

# project.reload <- function(type = "all") {
#   
#   # remove workspace expect project functions
#   rm(list = setdiff(ls(envir = globalenv()), 
#                     lsf.str(pattern = "*project*", envir = globalenv())), envir = globalenv())
#   
#   .project.own.fcts()
#   
#   switch (type,
#     "all" = {.project.load.cache(type = "raw")
#              .project.load.cache(type = "munged")
#              .project.load.cache(type = "analysed")},
#     "munged" = {.project.load.cache(type = type)},
#     "raw" = {.project.load.cache(type = type)}
#     
#   )
#   
#   # packrat::on()
#   # .project.load.packages()
# 
#   
#   # cat("\014") # clear console
#   print("*** Project has been successfully reloaded ...")
#   Sys.sleep(2)
#   # cat("\014") # clear console
#   
# }


# === === === === === === === === === === === === === === === === === ===
# Re-Evaluate data (e.g with updated data) ####
# === === === === === === === === === === === === === === === === === ===
.project.newdata <- function(refresh.flag = F, munge.flag = F) {

  .project.read.data(refresh.flag = refresh.flag)
  print("*** New data has been read ...")
  
  if (munge.flag) {
    .project.munge()
    print("*** New data has been munged ...")
  } 

}
