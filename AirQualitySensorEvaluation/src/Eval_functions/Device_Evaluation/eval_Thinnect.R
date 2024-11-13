### analog to this code one can investigate also the first period of measurements at TROPOS (office 1st round - only a few instruemtns available)

### gets data from respective folder (here: Office_2nd_round from 04.12.2023 to 20.12.2023)
files_thinnect <- list.files("data\\Thinnect\\Office_2nd_round",full.names=T)
data_thinnect <- data.frame()

for(f in files_thinnect){
  tmp <- read_delim(f,id="device_id",col_names = T,delim = ",")%>%
    mutate(device_id = substr(basename(f),1,4))%>%
    mutate(date = as.POSIXct(strptime(Time,"%H:%M %Y-%m-%d",tz="UTC"),tz="UTC")-3600)%>% # adjust time by 1 hour (3600 sec; default timezone offset)
    dplyr::select(-Time)%>%
    mutate(var = ifelse(str_detect(Type,"RH"),"RH",ifelse(str_detect(Type,"t"),"air_temp","CO2")))%>%
    dplyr::select(-Type)%>%
    rename(val = "Value") %>% dplyr::select(-Room)
  data_thinnect <- data_thinnect %>% bind_rows(.,tmp)
}
# transforms data (different format than in the Lab. test rounds)
data_thinnect <- data_thinnect %>% unique() %>% tidyr::spread(.,var,val) %>%
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH))

#makes reference data
reference <- reference_data(
    meteo = getPalasData("data\\AQ_Guard\\2nd_office",
       start = "2023-12-04 00:00:00 UTC",
        end = "2023-12-20 00:00:00 UTC"
    ),
    gases = getPalasData("data\\AQ_Guard\\2nd_office",
       start = "2023-12-04 00:00:00 UTC",
        end = "2023-12-20 00:00:00 UTC"
    ),
    co2 =  getPalasData("data\\AQ_Guard\\2nd_office",
       start = "2023-12-04 00:00:00 UTC",
        end = "2023-12-20 00:00:00 UTC"
    ), 
    pm = getPalasData("data\\AQ_Guard\\2nd_office",
        start = "2023-12-04 00:00:00 UTC",
        end = "2023-12-20 00:00:00 UTC"
    ),
    avg.time = "1 min") %>%
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH)) %>% 
    dplyr::filter(PM02.5 < 25) # all data below 25 µq for compariosn with LAS and Wings sensors

# Precision
parameters <- names(data_thinnect %>% dplyr::select(-device_id,-date))
# 15 min
sink("output\\Thinnect_office_period\\Precision_Thinnect_20231204-20241220_15min.txt")
for (p in parameters) {
    precision(data_thinnect, parameter = p, start = "2023-12-04", end = "2023-12-20", avg.time = "15 min", devicetype = "Thinnect_office_period_PMbelow25µg")
}
sink(file = NULL)
# 1 hour
sink("output\\Thinnect_office_period\\Precision_Thinnect_20231204-20241220_1hour.txt")
for (p in parameters) {
    precision(data_thinnect, parameter = p, start = "2023-12-04", end = "2023-12-20", avg.time = "1 hour", devicetype = "Thinnect_office_period_PMbelow25µg")
}
sink(file = NULL)

lookup <- lookup_table("microgramm")
parameters <- match_parameters(reference, data_thinnect)

# overall performance of THinnect devices  15 min averages
for (p in parameters) {
    print(paste0("Parameter: ", p))
    performance(data_thinnect, reference = reference, parameter = p,  start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "15 min", devicetype = "Thinnect_office_period_PMbelow25µg")
}
# accuracy
# 15 min
for (p in parameters) {
    print(paste0("Parameter: ", p))
    accuracy(data_thinnect, reference = reference, parameter = p,  start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "15 min", devicetype = "Thinnect_office_period_PMbelow25µg",reference_device = "Palas")
}
# 1 hour
for (p in parameters) {
    print(paste0("Parameter: ", p))
    accuracy(data_thinnect, reference = reference, parameter = p,  start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "1 hour", devicetype = "Thinnect_office_period_PMbelow25µg",reference_device = "Palas")
}

# correlation against reference 
#15 min 
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_thinnect, reference = reference, parameter = p,  start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "15 min", devicetype = "Thinnect_office_period_PMbelow25µg",reference_device = "Palas")
}

# 1 hour
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_thinnect, reference = reference, parameter = p,  start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "1 hour", devicetype = "Thinnect_office_period_PMbelow25µg",reference_device = "Palas")
}

#Correlation of Thinnect and Reference during office experiment (CO2 sensor dependence to meteorological conditions)
lookup <- lookup_table("ppb")
parameters <- "CO2"
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_T(data_thinnect, reference = reference, parameter = p,  start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "15 min", devicetype = "Thinnect_office_period_PMbelow25µg",reference_device = "Palas")
}
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_RH(data_thinnect, reference = reference, parameter = p,  start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "15 min", devicetype = "Thinnect_office_period_PMbelow25µg",reference_device = "Palas")
}
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_abshum(data_thinnect, reference = reference, parameter = p,  start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "15 min", devicetype = "Thinnect_office_period_PMbelow25µg",reference_device = "Palas")
}

## Summary of Performance for all parameters for 15 min and 1 hour averages
summary_performance(
    df_test = data_thinnect, 
    df_ref = reference, 
    "Thinnect_office_period_below25µg", #device name / experiment, here all the period from the chamber experiment
    "Palas", #reference device: Here calibrated Palas
    start = "2023-12-04 00:00:00 UTC", # start of period of interest
    end = "2023-12-20 00:00:00 UTC", # and end of period of interest
    avg.time = "1 hour")

summary_performance(
    df_test = data_thinnect, 
    df_ref = reference, 
    "Thinnect_office_period_PMbelow25µg", #device name / experiment, here all the period from the chamber experiment
    "Palas", #reference device: Here calibrated Palas
    start = "2023-12-04 00:00:00 UTC", # start of period of interest
    end = "2023-12-20 00:00:00 UTC", # and end of period of interest
    avg.time = "15 min")





# Chamber tests
## RH and T during VOC test
## Experiment conducted on 29.02. 01.03.2024 local time
## Reference is Vaisala, chamber for RH and T

reference <- reference_data(
    meteo = getChamberData(
        start = "2024-02-29",
        end = "2024-03-02",
        devicetype = "chamber",
        hours_difference_to_local_time = 0
    ),
    gases = getPalasData("data\\AQ_Guard\\",
        start = "2024-02-29",
        end = "2024-03-02"
    ),
    co2 = vaisalaCO2(
        start = "2024-02-29",
        end = "2024-03-02"
    ),
    avg.time = "1 min"
) %>%
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH)) %>%
    filter(between(date, as.POSIXct("2024-02-29 10:06:00 UTC", tz = "UTC"), as.POSIXct("2024-03-01 08:30:00 UTC", tz = "UTC")))

data_thinnect <- getThinnectData(path="data\\Thinnect\\LabTests",start="2024-02-01", end="2024-03-31") %>%
    filter(between(date, as.POSIXct("2024-02-29 10:06:00 UTC", tz = "UTC"), as.POSIXct("2024-03-01 08:30:00 UTC", tz = "UTC"))) %>%
    mutate(device_id = as.character(device_id)) %>%
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH)) %>% mutate(date=date+3600)%>%
    mutate(O3 = O3*1000) # ppm in ppb

# reference data based on PALAS AQ-Guard
reference_aq <- reference_data(
    meteo = getPalasData("data\\AQ_Guard\\",
        start = "2024-02-29",
        end = "2024-03-02"
    ),
    gases = getPalasData_VOCmg("data\\AQ_Guard\\",
        start = "2024-02-29",
        end = "2024-03-02"
    ),
    co2 = getPalasData("data\\AQ_Guard\\",
        start = "2024-02-29",
        end = "2024-03-02"
    ),
    avg.time = "1 min"
) %>%
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH)) %>%
    filter(between(date, as.POSIXct("2024-02-29 10:06:00 UTC", tz = "UTC"), as.POSIXct("2024-03-01 08:30:00 UTC", tz = "UTC")))


# Precision of sensors
# 15 min
sink("output\\Thinnect_only_VOC_period\\Precision_Thinnect_20240229-20240301_15min_chamber.txt")
parameters <- names(data_thinnect %>% dplyr::select(-device_id, -date))
for (p in parameters) {
    precision(data_thinnect,
        parameter = p, start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC",
        avg.time = "15 min",
        devicetype = "Thinnect_chamber_meteo_during_VOCtest"
    )
}
sink(file = NULL)

# 1 hour
sink("output\\Thinnect_only_VOC_period\\Precision_Thinnect_20240229-20240301_1h_chamber.txt")
parameters <- names(data_thinnect %>% dplyr::select(-device_id, -date))
for (p in parameters) {
    precision(data_thinnect,
        parameter = p, start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC",
        avg.time = "1 hour",
        devicetype = "Thinnect_chamber_meteo_during_VOCtest"
    )
}
sink(file = NULL)

# Correlation against Chameber for RH, T, Palas (Gases), and Vaisala sensor (CO2)

# 15 min
lookup <- lookup_table("ppb")
parameters <- match_parameters(reference_data = reference, test_data = data_thinnect)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_thinnect,
        reference = reference,
        parameter = p, start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC",
        avg.time = "15 min",
        devicetype = "Thinnect_chamber_meteo_during_VOCtest", reference_device = "ChamberMetPalasGasVaisalaCo2"
    )
}

# 1 hour
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_thinnect,
        reference = reference,
        parameter = p, start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC",
        avg.time = "1 hour",
        devicetype = "Thinnect_chamber_meteo_during_VOCtest", reference_device = "ChamberMetPalasGasVaisalaCo2"
    )
}

# Correlation and only Palas ist reference
# 15 min
lookup <- lookup_table("ppb")
parameters <- match_parameters(reference_data = reference_aq, test_data = data_thinnect)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_thinnect, reference = reference_aq, parameter = p, start = "2024-02-29 10:06:00 UTC", end = "2024-03-01 08:30:00 UTC", avg.time = "15 min ", 
    devicetype = "Thinnect_chamber_meteo_during_VOCtest", reference_device = "Palas")
}

###### Evaluation of CO2 performance
# Experiment conducted on 26.02.2024 14:58 to 27.02.2024 10:00 local time
# Reference is Vaisala, chamber for RH and T
reference <- reference_data(
    meteo = getChamberData(
        start = "2024-02-26",
        end = "2024-02-28",
        devicetype = "chamber",
        hours_difference_to_local_time = 0
    ),
    co2 = vaisalaCO2(
        start = "2024-02-26",
        end = "2024-02-28"
    ),
    avg.time = "1 min"
) %>%
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH)) %>%
    dplyr::filter(between(date, as.POSIXct("2024-02-26 14:58:00 UTC", tz = "UTC"), as.POSIXct("2024-02-27 10:00:00 UTC", tz = "UTC")))

#Thinnect sensor data
data_thinnect <- getThinnectData(path="data\\Thinnect\\LabTests",start="2024-02-01", end="2024-03-31") %>%
    filter(between(date, as.POSIXct("2024-02-26 14:58:00 UTC", tz = "UTC"), as.POSIXct("2024-02-27 10:00:00 UTC", tz = "UTC")))%>%
    mutate(device_id = as.character(device_id)) %>% mutate(date=date+3600) #%>%
    #mutate(VOC = VOC*1000) # mg in to µg


# Overall accuracy of Thinnect sensor against Vaisala and chamber
parameters <- match_parameters(reference_data = reference, test_data = data_thinnect)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    accuracy(data_thinnect, reference = reference, parameter = p, start = "2024-02-26 14:58:00 UTC", end = "2024-02-27 10:00:00 UTC", avg.time = "15 min", devicetype = "Thinnect_only_CO2_period", reference_device = "ChamberAndVaisala")
}
# Correlation against Vaisala and AQ-Guard (15 min)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_thinnect, reference = reference, parameter = p, start = "2024-02-26 14:58:00 UTC", end = "2024-02-27 10:00:00 UTC", avg.time = "15 min", devicetype = "Thinnect_only_CO2_period", reference_device = "ChamberAndVaisala")
}
# Correlation against Vaisala and AQ-Guard (1 hour)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_thinnect, reference = reference, parameter = p, start = "2024-02-26 14:58:00 UTC", end = "2024-02-27 10:00:00 UTC", avg.time = "1 hour", devicetype = "Thinnect_only_CO2_period", reference_device = "ChamberAndVaisala")
}

summary_performance(data_thinnect, df_ref = reference, start = "2024-02-26 14:58:00 UTC", end = "2024-02-27 10:00:00 UTC", avg.time = "15 min", devicetype = "Thinnect_only_CO2_period", referencetype = "ChamberAndVaisala")
summary_performance(data_thinnect, df_ref = reference, start = "2024-02-26 14:58:00 UTC", end = "2024-02-27 10:00:00 UTC", avg.time = "1 hour", devicetype = "Thinnect_only_CO2_period", referencetype = "ChamberAndVaisala")

# Reference Data AQ-Guard
reference_aq <- reference_data(
    meteo = getPalasData("data\\AQ_Guard\\",
        start = "2024-02-12",
        end = "2024-03-01",
        plot = FALSE
    ),
    co2 = getPalasData("data\\AQ_Guard\\",
        start = "2024-02-12",
        end = "2024-03-01",
        plot = FALSE
    ),
    gases = getPalasData_VOCmg("data\\AQ_Guard\\",
        start = "2024-02-12",
        end = "2024-03-01",
        plot = TRUE
    ),
    avg.time = "1 min"
) %>%
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH)) %>%
    dplyr::filter(between(date, as.POSIXct("2024-02-26 14:58:00 UTC", tz = "UTC"), as.POSIXct("2024-02-27 10:00:00 UTC", tz = "UTC")))


# Correlation against AQ-Guard
lookup <- lookup_table("microgramm")
parameters <- match_parameters(reference_data = reference_aq, test_data = data_thinnect)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_thinnect, reference = reference_aq, parameter = p, start = "2024-02-26 14:58:00 UTC", end = "2024-02-27 10:00:00 UTC", avg.time = "15 min", devicetype = "Thinnect_only_CO2_period", reference_device = "Palas")
}

#Correlation against Vaisala (CO2) and Chamber
parameters <- match_parameters(reference_data = reference, test_data = data_thinnect)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_thinnect, reference = reference, parameter = p, start = "2024-02-26 14:58:00 UTC", end = "2024-02-27 10:00:00 UTC", avg.time = "15 min", devicetype = "Thinnect_only_CO2_period", reference_device = "ChamberAndVaisala")
}

#Correlation of Thinnect and Reference during CO2 experiment (CO2 sensor dependence to meteorologcal conditions)
parameters <- "CO2"
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_RH(data_thinnect, reference = reference, parameter = p, start = "2024-02-26 14:58:00 UTC", end = "2024-02-27 10:00:00 UTC", avg.time = "15 min", devicetype = "Thinnect_only_CO2_period", reference_device = "ChamberAndVaisala")
}

for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_abshum(data_thinnect, reference = reference, parameter = p, start = "2024-02-26 14:58:00 UTC", end = "2024-02-27 10:00:00 UTC", avg.time = "15 min", devicetype = "Thinnect_only_CO2_period", reference_device = "ChamberAndVaisala")
}

for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_T(data_thinnect, reference = reference, parameter = p, start = "2024-02-26 14:58:00 UTC", end = "2024-02-27 10:00:00 UTC", avg.time = "15 min", devicetype = "Thinnect_only_CO2_period", reference_device = "ChamberAndVaisala")
}


###### Evaluation of VOC performance
# Note: VOC is measured by a non-characterized indoor air quality sensor (PALAS AQ-Guard)
# Experiment conducted on 29.02.2024 10:06 to 01.03.2024 08:30 local time

reference <- reference_data(
    meteo = getChamberData(
        start = "2024-02-29",
        end = "2024-03-02",
        devicetype = "chamber",
        hours_difference_to_local_time = 0),
    gases = getPalasData_VOCmg("data\\AQ_Guard\\",
        start = "2024-02-29",
        end = "2024-03-02"),avg.time = "1 min") %>% 
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH)) %>%
    filter(between(date, as.POSIXct("2024-02-29 10:06:00 UTC", tz = "UTC"), as.POSIXct("2024-03-01 08:30:00 UTC", tz = "UTC")))

data_thinnect <- getThinnectData(path="data\\Thinnect\\LabTests",start="2024-02-29", end="2024-03-02",plot=T) %>%
    filter(between(date, as.POSIXct("2024-02-29 10:06:00 UTC", tz = "UTC"), as.POSIXct("2024-03-01 08:30:00 UTC", tz = "UTC"))) %>%
    mutate(device_id = as.character(device_id))%>%
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH)) %>% mutate(date=date+3600)%>%
    mutate(O3 = O3*1000) # ppm in ppb

# Precision during VOC test 
# 15 min
sink("output\\Thinnect_only_VOC_period\\Precision_Thinnect_20231204-20241220_15min.txt")
for (p in parameters) {
    precision(data_thinnect, parameter = p, start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC", avg.time = "15 min", devicetype = "Thinnect_only_VOC_period")
}
sink(file = NULL)

# 1 hour
sink("output\\Thinnect_only_VOC_period\\Precision_Thinnect_20231204-20241220_1hour.txt")
for (p in parameters) {
    precision(data_thinnect, parameter = p, start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC", avg.time = "1 hour", devicetype = "Thinnect_only_VOC_period")
}
sink(file = NULL)

# 15 min
parameters <- "VOC"
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_thinnect,
        reference = reference, parameter = p,
        start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC",
        avg.time = "15 min",
        devicetype = "Thinnect_only_VOC_period", reference_device = "Palas"
    )
}

# 1 hour
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_thinnect,
        reference = reference, parameter = p,
        start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC",
        avg.time = "1 hour",
        devicetype = "Thinnect_only_VOC_period", reference_device = "Palas"
    )
}
# 15 min RH dependency
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_RH(data_thinnect,
        reference = reference, parameter = p,
        start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC",
        avg.time = "15 min",
        devicetype = "Thinnect_only_VOC_period", reference_device = "Palas"
    )
}
# 15 min temperature dependency
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_T(data_thinnect,
        reference = reference, parameter = p,
        start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC",
        avg.time = "15 min",
        devicetype = "Thinnect_only_VOC_period", reference_device = "Palas"
    )
}

# 15 min abs. humidity dependency
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_abshum(data_thinnect,
        reference = reference, parameter = p,
        start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC",
        avg.time = "15 min",
        devicetype = "Thinnect_only_VOC_period", reference_device = "Palas"
    )
}

### summary of the performance during the VOC experiment
summary_performance(data_thinnect,
    df_ref = reference %>% dplyr::select(date, VOC),
    start = "2024-02-29 10:06:00 UTC",
    end = "2024-03-01 08:30:00 UTC", avg.time = "15 min", devicetype = "Thinnect_only_VOC_period", referencetype = "Palas"
)



# reads in result overview table
results <- readr::read_delim("data\\results.csv",col_names=T,delim=";") %>% dplyr::filter(device_type=="Thinnect")

# plots parameters of linear mod as a summery for each parameter
for (p in unique(results$parameter)){
    res <- results%>%gather(var,val,-device_id,-experiment,-device_type,-reference,-avg,-parameter)%>%dplyr::filter(parameter == p)
    plt <- ggplot(res, aes(x=experiment, y=val, fill=device_id,color=device_id,alpha=reference)) +
    geom_bar(stat="identity", position=position_dodge(1),width=0.6) +
    ggplot2::facet_grid(var ~ avg, scales = "free") +
    theme_bw(16) +
    labs(title=paste0("Bar Plot by averaging period and stat. accuracy parameter\nfor ",lookup[p]), x="Experiment", y="Value") +
    theme(axis.text.x = element_text(angle=45, hjust=1))+scale_alpha_manual(values=c(1,0.5))+
    scale_fill_manual(breaks=unique(results$device_id) %>% sort(),values=c(brewer.pal(length(unique(results$device_id))-1,"Dark2"),"gray4"))+
    scale_color_manual(breaks=unique(results$device_id) %>% sort(),values=c(brewer.pal(length(unique(results$device_id))-1,"Dark2"),"gray4"))
    
    ggsave(plot=plt,filename=paste0("Thinnect_stat_param_",p,".png"),width = 30 ,height =30, units="cm",dpi=300)
}


#### Testing from here on 
ggplot(results %>% dplyr::filter(device_id!="mean")%>%gather(var,val,-device_id,-experiment,-device_type,-reference,-avg,-parameter)
%>%dplyr::filter(reference == "Palas AQ-Guard"))+barplot(aes(x=experiment,y=val,fill=device_id),position=position_dodge(),stat="identity")+
facet_wrap(var~parameter)


res_aq <- results %>% dplyr::filter(device_id!="mean")%>%gather(var,val,-device_id,-experiment,-device_type,-reference,-avg,-parameter)%>%dplyr::filter(reference == "Palas AQ-Guard")

res_tropos <- results%>%dplyr::filter(device_id!="mean")%>%gather(var,val,-device_id,-experiment,-device_type,-reference,-avg,-parameter)%>%dplyr::filter(reference == "Tropos")

res_vaisala <- results%>%dplyr::filter(device_id!="mean")%>%gather(var,val,-device_id,-experiment,-device_type,-reference,-avg,-parameter)%>%dplyr::filter(reference == "GMP-251")

ggplot(res_aq,aes(x=interaction(avg,experiment),y=val,fill=device_id))+geom_bar(stat="identity",position=position_dodge(),width=0.2)+facet_grid(var~parameter,scales="free")+theme_bw(16)
res <- results%>%gather(var,val,-device_id,-experiment,-device_type,-reference,-avg,-parameter) %>% dplyr::filter(parameter == "CO2")


## just some testing with difference references
# AQ-Guard
ggplot(res_aq, aes(x=experiment, y=val, fill=device_id,color=device_id,alpha=avg)) +
  geom_bar(stat="identity", position=position_dodge(1),width=0.6) +
  ggplot2::facet_grid(var ~ parameter, scales = "free_y") +
  theme_bw(16) +
  labs(title="Faceted Bar Plot by Parameter and Averaging Period", x="Experiment", y="Value") +
  theme(axis.text.x = element_text(angle=45, hjust=1))+scale_alpha_manual(values=c(1,0.5))

#Tropos devices
ggplot(res_tropos, aes(x=experiment, y=val, fill=device_id,color=device_id,alpha=avg)) +
  geom_bar(stat="identity", position=position_dodge(1),width=0.6) +
  ggplot2::facet_grid(var ~ parameter, scales = "free_y") +
  theme_bw(16) +
  labs(title="Faceted Bar Plot by Parameter and Averaging Period", x="Experiment", y="Value") +
  theme(axis.text.x = element_text(angle=45, hjust=1))+scale_alpha_manual(values=c(1,0.5))

#Vaisala GMP-251
ggplot(res_vaisala, aes(x=experiment, y=val, fill=device_id,color=device_id,alpha=avg)) +
  geom_bar(stat="identity", position=position_dodge(1),width=0.6) +
  ggplot2::facet_grid(var ~ parameter, scales = "free_y") +
  theme_bw(16) +
  labs(title="Faceted Bar Plot by Parameter and Averaging Period", x="Experiment", y="Value") +
  theme(axis.text.x = element_text(angle=45, hjust=1))+scale_alpha_manual(values=c(1,0.5)) +
  annotate("text", x = 1:6, y = - 400,
           label = rep(c("Variety 1", "Variety 2"), 2)) +
  annotate("text", c(1.5, 3.5), y = - 800, label = c("Avg", "Experiment")) +
  theme_classic() +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
       axis.title.x = element_blank(),
       axis.text.x = element_blank())