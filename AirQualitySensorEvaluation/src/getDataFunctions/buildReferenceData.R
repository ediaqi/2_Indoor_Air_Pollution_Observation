# Method that builds a reference data set based on different sources each part can be filled by one of the getData.R functions

reference_data <- function(meteo = NULL, co2 = NULL,  gases = NULL, pm = NULL, avg.time = "5 min"){
    reference <- NULL
    if(length(meteo)!=0){
        meteo <- meteo %>% dplyr::select(date, RH, air_temp)   
    }
    if(length(co2)!=0){
        co2 <- co2 %>% dplyr::select(date, CO2)   
    }
    if(length(gases)!=0){
        gases <- gases %>% dplyr::select(date, dplyr::any_of(c("VOC","VOC_P3","NO2","NO","O3","SO","SO2","CO")))   
    }
    if(length(pm)!=0){
        pm <- pm %>% dplyr::select(date, dplyr::any_of(c("PM01", "PM02.5", "PM10","period")))   
    }
    list <- list(meteo, co2, gases, pm) 
    list <- list[which(unlist(lapply(list,FUN = function(x) length(x)))!=0)]
    
    reference <- list %>% reduce(full_join, by = "date") 
    return(reference %>% openair::timeAverage(., avg.time=avg.time, start = floor_date(.$date[1],"day")))
}