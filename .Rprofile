### -- Auto Start-up File -- ###


# === === === === === === === === === === === === === === === === === ===
# Start ####
# === === === === === === === === === === === === === === === === === ===
rm(list = ls()) # clear workspace variables
cat("\014") # clear console
print("Auto start-up project ...")

# === === === === === === === === === === === === === === === === === ===
# R Options ####
# === === === === === === === === === === === === === === === === === ===
options(digits = 3)
options(stringsAsFactors = F)
options(java.parameters = "-Xmx8000m")
#options(encoding = "UTF-8")
options(Ncpus = 4)


# === === === === === === === === === === === === === === === === === ===
# Source "ProjectSetup.R from "../_NewProject_PackratPackages/packrat" ----
# === === === === === === === === === === === === === === === === === ===
if (file.exists("./functions/ProjectSetup_v3_copy.R")) {
  source("./functions/ProjectSetup_v3_copy.R")
  print("Loading Project Functions from local copy")
} else { # for initalisation
  source("../_NewProject/ProjectSetup_v3.R")
  print("Loading Project Functions from NewProject_Rprofile")
}



# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# End auto start-up ####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
print("Auto start-up done ...")


### -- End Auto Start-up -- ###