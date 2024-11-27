# sources every R-Script and loads every needed library to make every function in this repository available
list.of.packages <- c("tidyverse", "lubridate", "openair", "httr", "jsonlite", "rlang", "rdwd", "rJava", "ggpubr", "OSMscale","RColorBrewer","forestmangr","viridis","threadr","xfun","data.table","remotes","segmented")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages,chooseCRANmirror(ind=37))
#remotes::install_github("skgrange/threadr")

lapply(list.of.packages, require, character.only = TRUE)

source(".\\src\\Eval_functions\\accuracy.R")
source(".\\src\\Eval_functions\\performance.R")
source(".\\src\\Eval_functions\\correlation.R")
source(".\\src\\Eval_functions\\precision.R")
source(".\\src\\Eval_functions\\RH_test.R")

# all functions to get the data of the devices
source(".\\src\\getDataFunctions\\getData.R")
source(".\\src\\getDataFunctions\\buildReferenceData.R")



# function that provides the intersection of column names from two data sets excluding the date column
match_parameters <- function(reference_data, test_data) {
    int_names <- intersect(names(reference_data), names(test_data))
    parameters <- int_names[-which(int_names == "date")]
    return(parameters)
}

lookup_table <- function(unit){
        if (unit == "ppb"){
                dict <- c(
                        "CO2" = "CO2 in ppm",
                        "CO" = "CO in ppb",
                        "NO" = "NO in ppb",
                        "NO2" = "NO2 in ppb",
                        "SO2" = "SO2 in ppb",
                        "O3" = "O3 in ppb",
                        "air_temp" = "temperature in °C",
                        "RH" = "RH in %",
                        "abs_hum" = "a in g m-3",
                        "PM01" = "PM1 in µg m-3",
                        "PM02.5" = "PM2.5 in µg m-3",
                        "PM10" = "PM10 in µg m-3",
                        "PM04" = "PM 4 in µg m-3",
                        "PMtot" = "PM total in µg m-3",
                        "VOC" = "VOC in ppb",
                        "VOC_P3" = "VOC(P3) in ppb")
        } else if (unit == "microgramm"){
                dict <- c(
                        "CO2" = "CO2 in ppm",
                        "NO" = "NO in µg m-3",
                        "NO2" = "NO2 in µg m-3",
                        "SO2" = "SO2 in µg m-3",
                        "O3" = "O3 in µg m-3",
                        "air_temp" = "temperature in °C",
                        "RH" = "RH in %",
                        "abs_hum" = "a in g m-3",
                        "PM01" = "PM1 in µg m-3",
                        "PM02.5" = "PM2.5 in µg m-3",
                        "PM10" = "PM10 in µg m-3",
                        "PM04" = "PM 4 in µg m-3",
                        "PMtot" = "PM total in µg m-3",
                        "VOC" = "VOC in mg m-3",
                        "VOC_P3" = "VOC(P3) in mg m-3")
        }
        return(dict)
}

lookup <- lookup_table("ppb")
