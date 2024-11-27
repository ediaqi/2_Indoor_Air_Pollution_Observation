#### Evaluation of Wings sensors
#### 2nd office period

#Wings data indoor + outdoor
ids_i <- c(17, 18, 19)
ids_o <- c(1602, 1603, 1604)

df_i <- NULL
for (id in ids_i) {
    data_wings <- getWingsData(start = "2023-12-04", end = "2023-12-21", type = "i", id = id) %>% mutate(type = "indoor")
    df_i <- bind_rows(df_i, data_wings)
}
df_i <- df_i %>% mutate(device_id = as.character(device_id))%>%
        openair::timeAverage(., avg.time = "5 min", start.date = floor_date(.$date[1], "days") - days(1), type = "device_id") %>% 
        dplyr::filter(PM02.5 <= 25) %>%
        mutate(abs_hum = threadr::absolute_humidity(air_temp, RH))

df_o <- NULL
for (id in ids_o) {
    data_wings <- getWingsData(start = "2023-12-04", end = "2023-12-21", type = "o", id = id) %>% mutate(type = "outdoor")
    df_o <- bind_rows(df_o, data_wings)
}

df_o <- df_o %>% mutate(device_id = as.character(device_id)) %>%
        openair::timeAverage(., avg.time = "10 min", start.date = floor_date(.$date[1], "days") - days(2), type = "device_id") %>% 
        dplyr::filter(PM02.5 <= 25) %>%
        mutate(abs_hum = threadr::absolute_humidity(air_temp, RH))

# Precision
# indoor
#5 min
parameters <- names(df_i %>% dplyr::select(-device_id, -date))
sink("output\\Precision_Wings_i_20231204_20231220_5min.txt")
for (p in parameters) {
    precision(df_i, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "5 min", devicetype=paste0("Wings_Indoor_office_period_PMbelow25µg"))
}
sink(file = NULL)

# 1 hour
sink("output\\Precision_Wings_i_20231204_20231220_1hour.txt")
parameters <- names(df_i %>% dplyr::select(-device_id, -date))
for (p in parameters) {
    precision(df_i, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "1 hour", devicetype=paste0("Wings_Indoor_office_period_PMbelow25µg"))
}
sink(file = NULL)


#outdoor
# 10 min
parameters <- names(df_o %>% dplyr::select(-device_id, -date))
sink("output\\Precision_Wings_o_20231204_20231220_10min.txt")
for (p in parameters) {
    precision(df_o, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "10 min", devicetype=paste0("Wings_Outdoor_office_period_PMbelow25µg"))
}
sink(file = NULL)

# 1 hour
sink("output\\Precision_Wings_o_20231204_20231220_1hour.txt")
for (p in parameters) {
    precision(df_o, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "1 hour", devicetype=paste0("Wings_Outdoor_office_period_PMbelow25µg"))
}
sink(file = NULL)

###performance accuracy and correlation 
# reference daza AQ-Guard
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
    avg.time = "1 min"
) %>% 
    filter(PM02.5 <= 25) %>% # PM below 25 µg
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH))


### Indoor
df_i <- df_i %>% mutate(device_id = as.character(device_id)) 

# 5 min
lookup <- lookup_table("ppb")
parameters <- match_parameters(reference_data = reference, test_data = df_i)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    accuracy(df_i, reference = reference, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "5 min", devicetype = "Wings_Indoor_office_period_PMbelow25µg", reference_device = "Palas")
}

# 1 hour
for (p in parameters) {
    print(paste0("Parameter: ", p))
    accuracy(df_i, reference = reference, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "1 hour", devicetype = "Wings_Indoor_office_period_PMbelow25µg", reference_device = "Palas")
}

# 5 min
lookup <- lookup_table("ppb")
parameters <- match_parameters(reference_data = reference %>% dplyr::select(-VOC_P3), test_data = df_i)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    performance(df_i, reference = reference, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "5 min", devicetype = "Wings_Indoor_office_period_PMbelow25µg")
}

# 1 hour
lookup <- lookup_table("ppb")
parameters <- match_parameters(reference_data = reference %>% dplyr::select(-VOC_P3), test_data = df_i)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    performance(df_i, reference = reference, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "1 hour", devicetype = "Wings_Indoor_office_period_PMbelow25µg")
}

# 5 min
lookup <- lookup_table("ppb")
parameters <- match_parameters(reference_data = reference %>% dplyr::select(-VOC_P3), test_data = df_i)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_i, reference = reference, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "5 min", devicetype = "Wings_Indoor_office_period_PMbelow25µg")
}

# 1 hour
lookup <- lookup_table("ppb")
parameters <- match_parameters(reference_data = reference %>% dplyr::select(-VOC_P3), test_data = df_i)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_i, reference = reference, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "1 hour", devicetype = "Wings_Indoor_office_period_PMbelow25µg")
}


#### Outdoor
df_o <- df_o %>% mutate(device_id = as.character(device_id)) 

#10 min
lookup <- lookup_table("ppb")
parameters <- match_parameters(reference_data = reference , test_data = df_o)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    accuracy(df_o, reference = reference, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", 
    avg.time = "10 min", 
    devicetype = "Wings_Outdoor_office_period_PMbelow25µg", reference_device = "Palas")
}

# 1 hour
for (p in parameters) {
    print(paste0("Parameter: ", p))
    accuracy(df_o, reference = reference, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC",
     avg.time = "1 hour", 
     devicetype = "Wings_Outdoor_office_period_PMbelow25µg", reference_device = "Palas")
}

#Performance
#10 min
lookup <- lookup_table("ppb")
parameters <- match_parameters(reference_data = reference %>% dplyr::select(-VOC_P3), test_data = df_o)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    performance(df_o, reference = reference, parameter = p, start = "2023-12-04 00:00:00 UTC", 
    end = "2023-12-20 00:00:00 UTC", avg.time = "10 min", 
    devicetype = "Wings_Outdoor_office_period_PMbelow25µg")
}

# 1 hour
for (p in parameters) {
    print(paste0("Parameter: ", p))
    performance(df_o, reference = reference, parameter = p, start = "2023-12-04 00:00:00 UTC", 
    end = "2023-12-20 00:00:00 UTC", 
    avg.time = "1 hour", 
    devicetype = "Wings_Outdoor_office_period_PMbelow25µg")
}

#Correlation
#10 min
lookup <- lookup_table("ppb")
parameters <- match_parameters(reference_data = reference %>% dplyr::select(-VOC_P3), test_data = df_o)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_o, reference = reference, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "10 min",
     devicetype = "Wings_Outdoor_office_period_PMbelow25µg")
}

# 1 hour
lookup <- lookup_table("ppb")
parameters <- match_parameters(reference_data = reference %>% dplyr::select(-VOC_P3), test_data = df_o)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_o, reference = reference, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", 
    avg.time = "1 hour", devicetype = "Wings_Outdoor_office_period_PMbelow25µg")
}

### Met. influence correaltion against RH, Temp, abs. hum
#### indoor 
##### 5 min

lookup <- lookup_table("ppb")
parameters <- match_parameters(reference_data = reference %>% dplyr::select(-VOC_P3, -RH, -air_temp, - abs_hum), test_data = df_i)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_abshum(df_i, reference = reference, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC",
     avg.time = "5 min", devicetype = "Wings_Indoor_office_period_PMbelow25µg", reference_device = "Palas")
}

parameters <- match_parameters(reference_data = reference %>% dplyr::select(-VOC_P3, -RH, -air_temp), test_data = df_i)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_T(df_i, reference = reference, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC",
     avg.time = "5 min", devicetype = "Wings_Indoor_office_period_PMbelow25µg", reference_device = "Palas")
}

for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_RH(df_i, reference = reference, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC",
     avg.time = "5 min", devicetype = "Wings_Indoor_office_period_PMbelow25µg", reference_device = "Palas")
}

#### outdoor
# 10 min
lookup <- lookup_table("ppb")
parameters <- match_parameters(reference_data = reference %>% dplyr::select(-VOC_P3, -RH, -air_temp, - abs_hum), test_data = df_o)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_abshum(df_o, reference = reference, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC",
     avg.time = "10 min", devicetype = "Wings_Outdoor_office_period_PMbelow25µg", reference_device = "Palas")
}

for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_T(df_o, reference = reference, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC",
     avg.time = "10 min", devicetype = "Wings_Outdoor_office_period_PMbelow25µg", reference_device = "Palas")
}

for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_RH(df_o, reference = reference, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC",
     avg.time = "10 min", devicetype = "Wings_Outdoor_office_period_PMbelow25µg", reference_device = "Palas")
}


### Rooftop experiment to perform gasperformance check (only outdoor) indoor no O3 sensor

# Wings data (outdoor)
df_o <- NULL
for (id in ids_o) {
    data_wings <- getWingsData(start = "2024-03-13", end = "2024-03-17", type = "o", id = id) %>% mutate(type = "outdoor")
    df_o <- bind_rows(df_o, data_wings)
}

df_o <- df_o %>% mutate(device_id = as.character(device_id)) %>%
        openair::timeAverage(., avg.time = "10 min", start.date = floor_date(.$date[1], "days") - days(2), type = "device_id") %>%
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH))

# reference data
reference <- reference_data(
    meteo = getPalasData("data\\AQ_Guard\\",
        start = "2024-03-13 14:00:00 UTC",
        end = "2024-03-16 14:00:00 UTC"
    ),
    gases = getPalasData("data\\AQ_Guard\\",
        start = "2024-03-13 14:00:00 UTC",
        end = "2024-03-16 14:00:00 UTC"
    ),
    co2 = getPalasData("data\\AQ_Guard\\",
        start = "2024-03-13 14:00:00 UTC",
        end = "2024-03-16 14:00:00 UTC"
    ),
    pm=getPalasData("data\\AQ_Guard\\",
        start = "2024-03-13 14:00:00 UTC",
        end = "2024-03-16 14:00:00 UTC"
    ),
    avg.time = "1 min"
)%>%
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH))

# reference gases from TROPOS devices
reference_chem <- getChemGases(file = "data\\Gases_chem_DAQ\\chem_gases.dat")

#reference station from UBA
data_uba_lpz_mitte <- all_data_uba_station(station = "DESN025", begin = "2024-03-13", ends = "2024-03-18")

# reference with chemistry data
reference_chem <- reference_data(
    meteo = getPalasData("data\\AQ_Guard\\",
        start = "2024-03-13 14:00:00 UTC",
        end = "2024-03-16 14:00:00 UTC"
    ),
    gases = getChemGases(file = "data\\Gases_chem_DAQ\\chem_gases.dat"),
    co2 = getPalasData("data\\AQ_Guard\\",
        start = "2024-03-13 14:00:00 UTC",
        end = "2024-03-16 14:00:00 UTC"
    ),
    pm=getPalasData("data\\AQ_Guard\\",
        start = "2024-03-13 14:00:00 UTC",
        end = "2024-03-16 14:00:00 UTC"
    ),
    avg.time = "1 min"
)%>%
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH))


## Precision
# 10 min 
sink("output\\Wings_Outdoor_rooftop\\Precision_Wings_Outdoor_20240313-160000_20240316-140000_10min.txt")
parameters <- names(df_o %>% dplyr::select(-device_id, -date))
for (p in parameters) {
    precision(df_o, parameter = p, start = "2024-03-13 16:00:00 UTC", end = "2024-03-16 14:00:00 UTC", avg.time = "10 min", devicetype = "Wings_Outdoor_rooftop")
}
sink(file = NULL)

#1 hour
sink("output\\Wings_Outdoor_rooftop\\Precision_Wings_Outdoor_20240313-160000_20240316-140000_1h.txt")
parameters <- names(df_o %>% dplyr::select(-device_id, -date))
for (p in parameters) {
    precision(df_o, parameter = p, start = "2024-03-13 16:00:00 UTC", end = "2024-03-16 14:00:00 UTC", avg.time = "1 hour", devicetype = "Wings_Outdoor_rooftop")
}
sink(file = NULL)

# Correaltion agaianst AQ-Guard
# 10 min
parameters <- match_parameters(reference_data = reference %>% dplyr::select(-VOC_P3), test_data = df_o)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_o %>% mutate(device_id = as.character(device_id)), reference = reference, parameter = p, 
    start = "2024-03-13 17:00:00 UTC", end = "2024-03-16 14:00:00 UTC", 
    avg.time = "10 min", devicetype = "Wings_Outdoor_rooftop", reference_device = "all_AQ-Guard")
}

# 1 hour
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_o%>%mutate(device_id = as.character(device_id)), reference = reference, parameter = p, 
    start = "2024-03-13 17:00:00 UTC", end = "2024-03-16 14:00:00 UTC", 
    avg.time = "1 hour", devicetype = "Wings_Outdoor_rooftop", reference_device = "all_AQ-Guard")
}

# Correlation against chem. tropos
# 10 min
parameters <- match_parameters(reference_data = reference_chem, test_data = df_o)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_o%>%mutate(device_id = as.character(device_id)),
        reference = reference_chem,
        parameter = p, start = "2024-03-13 17:00:00 UTC", end = "2024-03-16 14:00:00 UTC", avg.time = "10 min", devicetype = "Wings_Outdoor_rooftop", reference_device = "Chem-Tropos"
    )
}

# 1 hour
parameters <- match_parameters(reference_data = reference_chem, test_data = df_o)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_o%>%mutate(device_id = as.character(device_id)),
        reference = reference_chem,
        parameter = p, start = "2024-03-13 17:00:00 UTC", end = "2024-03-16 14:00:00 UTC", avg.time = "1 hour", devicetype = "Wings_Outdoor_rooftop", reference_device = "Chem-Tropos"
    )
}

# againts measurement station of german environmental agency
lookup <- lookup_table("microgramm")
parameters <- match_parameters(reference_data = data_uba_lpz_mitte, test_data = df_o)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_o %>% mutate(device_id = as.character(device_id)) %>% 
    mutate(
    O3 = O3 * 48 / (22.41 * ((air_temp + 273.15) / 273.15) * (1013 / pressure)),
    NO2 = NO2 * 46.0055 / (22.41 * ((air_temp + 273.15) / 273.15) * (1013 / pressure)),
    SO2 = SO2 * 64.07 / (22.41 * ((air_temp + 273.15) / 273.15) * (1013 / pressure)),
    NO = NO * 30.01 / (22.41 * ((air_temp + 273.15) / 273.15) * (1013 / pressure))),
        reference = data_uba_lpz_mitte %>%
            mutate(date = date + 3600), # to local time
        parameter = p, start = "2024-03-13 17:00:00 UTC", end = "2024-03-16 14:00:00 UTC", avg.time = "1 hour", devicetype = "Wings_Outdoor_rooftop", reference_device = "Lpz-Mitte"
    )
}

#RH - T check on rooftop
#Correlation with a, RH, temp. against Palas AQ-Guard
parameters <- match_parameters(reference_data = reference, test_data = df_o%>%dplyr::select(-RH,-air_temp,-abs_hum))
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_abshum(df_o %>% mutate(device_id = as.character(device_id)),
        reference = reference,
        parameter = p, start = "2024-03-13 17:00:00 UTC", end = "2024-03-16 14:00:00 UTC", avg.time = "10 min", devicetype = "Wings_Outdoor_rooftop", reference_device = "Palas_AQ-Guard"
    )
}

for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_RH(df_o%>%mutate(device_id = as.character(device_id)),
        reference = reference,
        parameter = p, start = "2024-03-13 17:00:00 UTC", end = "2024-03-16 14:00:00 UTC", avg.time = "10 min", devicetype = "Wings_Outdoor_rooftop", reference_device = "Palas_AQ-Guard"
    )
}

for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_T(df_o%>%mutate(device_id = as.character(device_id)),
        reference = reference,
        parameter = p, start = "2024-03-13 17:00:00 UTC", end = "2024-03-16 14:00:00 UTC", avg.time = "10 min", devicetype = "Wings_Outdoor_rooftop", reference_device = "Palas_AQ-Guard"
    )
}
# Rh and T variance against chem. from TROPOS (RH, T from Palas)
parameters <- match_parameters(reference_data = reference_chem, test_data = df_o%>%dplyr::select(-RH,-air_temp,-abs_hum))
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_RH(df_o%>%mutate(device_id = as.character(device_id)),
        reference = reference_chem,
        parameter = p, start = "2024-03-13 17:00:00 UTC", end = "2024-03-16 14:00:00 UTC", avg.time = "10 min", devicetype = "Wings_Outdoor_rooftop", reference_device = "Chem-tropos"
    )
}

for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_T(df_o%>%mutate(device_id = as.character(device_id)),
        reference = reference_chem,
        parameter = p, start = "2024-03-13 17:00:00 UTC", end = "2024-03-16 14:00:00 UTC", avg.time = "10 min", devicetype = "Wings_Outdoor_rooftop", reference_device = "Chem-tropos"
    )
}


##### RH, and T test at chamber 

###### Evaluation of RH,T performance
# Experiment conducted on 29.02. 01.03.2024 local time
# Reference is Vaisala, chamber for RH and T

# Reference data
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
    avg.time = "1 min") %>% 
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH)) %>%
    filter(between(date, as.POSIXct("2024-02-29 10:06:00 UTC", tz = "UTC"), as.POSIXct("2024-03-01 08:30:00 UTC", tz = "UTC")))

# Wings data
ids_i <- c(17, 18, 19)
ids_o <- c(1602, 1603, 1604)

df_i <- NULL
for (id in ids_i) {
    data_wings <- getWingsData(start = "2024-02-29",
        end = "2024-03-02", type = "i", id = id)
    df_i <- bind_rows(df_i, data_wings)
}
df_i <- df_i %>% mutate(device_id = as.character(device_id))%>%
        openair::timeAverage(., avg.time = "5 min", start.date = floor_date(.$date[1], "days") - days(1), type = "device_id") %>%
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH)) %>%
    filter(between(date, as.POSIXct("2024-02-29 10:06:00 UTC", tz = "UTC"), as.POSIXct("2024-03-01 08:30:00 UTC", tz = "UTC")))

df_o <- NULL
for (id in ids_o) {
    data_wings <- getWingsData(start = "2024-02-29",
        end = "2024-03-02", type = "o", id = id) 
    df_o <- bind_rows(df_o, data_wings)
}

df_o <- df_o %>% mutate(device_id = as.character(device_id)) %>%
        openair::timeAverage(., avg.time = "10 min", start.date = floor_date(.$date[1], "days") - days(2), type = "device_id") %>%
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH)) %>%
    filter(between(date, as.POSIXct("2024-02-29 10:06:00 UTC", tz = "UTC"), as.POSIXct("2024-03-01 08:30:00 UTC", tz = "UTC")))

# Reference Palas data
reference_aq <- reference_data(
    meteo = getPalasData("data\\AQ_Guard\\",
        start = "2024-02-29",
        end = "2024-03-02"
    ),
    gases = getPalasData("data\\AQ_Guard\\",
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



# Outdoor
# Precision 10 min - not really useful
sink("output\\Wings_Outdoor_only_VOC_period\\Precision_Wings_Outdoor_20240229-20240301_10min_chamber.txt")
parameters <- names(df_o %>% dplyr::select(-device_id, -date))
for (p in parameters) {
    precision(df_o,
        parameter = p, start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC",
        avg.time = "10 min",
        devicetype = "Wings_Outdoor_chamber_meteo_during_VOCtest"
    )
}
sink(file = NULL)

# Precision 1 hour
sink("output\\Wings_Outdoor_only_VOC_period\\Precision_Wings_Outdoor_20240229-20240301_1hour_chamber.txt")
parameters <- names(df_o %>% dplyr::select(-device_id, -date))
for (p in parameters) {
    precision(df_o,
        parameter = p, start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC",
        avg.time = "1 hour",
        devicetype = "Wings_Outdoor_chamber_meteo_during_VOCtest"
    )
}
sink(file = NULL)

# Indoor
# Precision 5 min
sink("output\\Wings_Indoor_only_VOC_period\\Precision_Wings_Indoor_20240229-20240301_5min_chamber.txt")
parameters <- names(df_i %>% dplyr::select(-device_id, -date))
for (p in parameters) {
    precision(df_i,
        parameter = p, start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC",
        avg.time = "5 min",
        devicetype = "Wings_Indoor_chamber_meteo_during_VOCtest"
    )
}
sink(file = NULL)

# 1 hour
sink("output\\Wings_Indoor_only_VOC_period\\Precision_Wings_Indoor_20240229-20240301_1hour_chamber.txt")
parameters <- names(df_i %>% dplyr::select(-device_id, -date))
for (p in parameters) {
    precision(df_i,
        parameter = p, start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC",
        avg.time = "1 hour",
        devicetype = "Wings_Indoor_chamber_meteo_during_VOCtest"
    )
}
sink(file = NULL)

### Correlation (a, RH, T, CO2)
# Indoor - Reference Chamber, Palas, Vaisala
# 5 min
lookup <- lookup_table("ppb")
parameters <- c("CO2", "RH", "air_temp", "abs_hum")

for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_i%>%mutate(device_id = as.character(device_id)),
        reference = reference,
        parameter = p, start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC",
        avg.time = "5 min ",
        devicetype = "Wings_Indoor_chamber", reference_device = "chamber"
    )
}

# 5 min
# reference AQ-Guard
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_i %>% mutate(device_id = as.character(device_id)),
        reference = reference_aq,
        parameter = p, start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC",
        avg.time = "5 min ",
        devicetype = "Wings_Indoor_chamber", reference_device = "Palas"
    )
}

# 1 hour
# Reference Chamber, Palas, Vaisala
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_i %>% mutate(device_id = as.character(device_id)),
        reference = reference,
        parameter = p, start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC",
        avg.time = "1 hour ",
        devicetype = "Wings_Indoor_chamber", reference_device = "chamber"
    )
}

# 1 hour
# reference AQ-Guard
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_i%>%mutate(device_id = as.character(device_id)),
        reference = reference_aq,
        parameter = p, start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC",
        avg.time = "1 hour",
        devicetype = "Wings_Indoor_chamber", reference_device = "Palas"
    )
}

### Outdoor
# 10 min -actually useles
# Reference Chamber
lookup <- lookup_table("ppb")
parameters <- c("RH","air_temp","abs_hum")

for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_o %>% mutate(device_id = as.character(device_id)),
        reference = reference,
        parameter = p, start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC",
        avg.time = "10 min",
        devicetype = "Wings_Outdoor_chamber", reference_device = "chamber"
    )
}

# 1 hour
# Reference AQ-Guard
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_o %>% mutate(device_id = as.character(device_id)),
        reference = reference_aq,
        parameter = p, start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC",
        avg.time = "10 min",
        devicetype = "Wings_Outdoor_chamber", reference_device = "Palas"
    )
}


# 1 hour
# Reference Chamber
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_o%>%mutate(device_id = as.character(device_id)),
        reference = reference,
        parameter = p, start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC",
        avg.time = "1 hour",
        devicetype = "Wings_Outdoor_chamber", reference_device = "chamber"
    )
}

# Reference AQ-Guard
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_o %>% mutate(device_id = as.character(device_id)),
        reference = reference_aq,
        parameter = p, 
        start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC",
        avg.time = "1 hour",
        devicetype = "Wings_Outdoor_chamber", reference_device = "Palas"
    )
}

###### Evaluation of CO2 performance
# Experiment conducted on 26.02.2024 14:58 to 27.02.2024 10:00 local time
# Reference is Vaisala, chamber for RH and T or Palas AQ-Guard 
# only indoor devices considered since Outdoor devices dont have CO2 sensor

#Chamber + Vaisala are reference
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
    dplyr::filter(between(date, 
    as.POSIXct("2024-02-26 14:58:00 UTC", tz = "UTC"), 
    as.POSIXct("2024-02-27 10:00:00 UTC", tz = "UTC")))


#Palas reference
reference_aq <- reference_data(
    meteo = getPalasData("data\\AQ_Guard\\",
        start = "2024-02-12",
        end = "2024-03-01",
        plot = TRUE
    ),
    co2 = getPalasData("data\\AQ_Guard\\",
        start = "2024-02-12",
        end = "2024-03-01",
        plot = FALSE
    ),
    avg.time = "1 min"
) %>%
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH)) %>%
    dplyr::filter(between(date, 
    as.POSIXct("2024-02-26 14:58:00 UTC", tz = "UTC"), 
    as.POSIXct("2024-02-27 10:00:00 UTC", tz = "UTC")))


# Reads in data of test devices (at least three)
ids_i <- c(17, 18, 19)

df_i <- NULL
for (id in ids_i) {
    data_wings <- getWingsData(start = "2024-02-26",
        end = "2024-02-28", type = "i", id = id,plt=TRUE)
    df_i <- bind_rows(df_i, data_wings)
}

df_i <- df_i %>% mutate(device_id = as.character(device_id))%>%
        openair::timeAverage(., avg.time = "5 min", 
        start.date = floor_date(.$date[1], "days") - days(1), 
        type = "device_id") %>%
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH)) %>%
    dplyr::filter(between(date, 
    as.POSIXct("2024-02-26 14:58:00 UTC", tz = "UTC"), 
    as.POSIXct("2024-02-27 10:00:00 UTC", tz = "UTC"))) %>% 
    mutate(device_id = as.character(device_id))

### Correlation against reference (only CO2)
parameters <- "CO2"
# 5 min
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_i, reference = reference, parameter = p, 
    start = "2024-02-26 14:58:00 UTC", 
    end = "2024-02-27 10:00:00 UTC", 
    avg.time = "5 min", 
    devicetype = "Wings_Indoor_only_CO2_period", 
    reference_device = "Vaisala_GMP251")
}
# 1 hour
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_i, reference = reference, parameter = p, 
    start = "2024-02-26 14:58:00 UTC", 
    end = "2024-02-27 10:00:00 UTC", 
    avg.time = "1 hour", 
    devicetype = "Wings_Indoor_only_CO2_period", 
    reference_device = "Vaisala_GMP251")
}

#Unit to unit variation - Precision
for (p in parameters) {
    print(paste0("Parameter: ", p))
    precision(df_i, parameter = p, 
    start = "2024-02-26 14:58:00 UTC", 
    end = "2024-02-27 10:00:00 UTC", 
    avg.time = "5 min", 
    devicetype = "Wings_Indoor_only_CO2_period")
}

#Accuracy against reference
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_i, reference = reference_aq, 
    parameter = p, 
    start = "2024-02-26 14:58:00 UTC", 
    end = "2024-02-27 10:00:00 UTC", 
    avg.time = "5 min", 
    devicetype = "Wings_Indoor_only_CO2_period", 
    reference_device = "Palas")
}

#### Dependence to temp
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_T(df_i, reference = reference, 
    parameter = p, 
    start = "2024-02-26 14:58:00 UTC", 
    end = "2024-02-27 10:00:00 UTC", 
    avg.time = "5 min", 
    devicetype = "Wings_Indoor_only_CO2_period", 
    reference_device = "Vaisala")
}

#### Dependence to RH
parameters <- "CO2"
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_RH(df_i, reference = reference, 
    parameter = p, 
    start = "2024-02-26 14:58:00 UTC", 
    end = "2024-02-27 10:00:00 UTC", 
    avg.time = "5 min", 
    devicetype = "Wings_Indoor_only_CO2_period", 
    reference_device = "Vaisala")
}

#### Dependence to a
parameters <- "CO2"
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_abshum(df_i, reference = reference, 
    parameter = p, 
    start = "2024-02-26 14:58:00 UTC", 
    end = "2024-02-27 10:00:00 UTC", 
    avg.time = "5 min", 
    devicetype = "Wings_Indoor_only_CO2_period", 
    reference_device = "Vaisala")
}


##VOC (only indoor)
###### Evaluation of VOC performance
# Note: VOC is measured by a non-characterized indoor air quality sensor (PALAS AQ-Guard)
# Experiment conducted on 29.02.2024 10:06 to 01.03.2024 08:30 local time

### Times series test

##### VOC dependence to RH an T (29.02.2024 10:06 - 01.03.2024 08:30)
# data of devices
ids_i <- c(17, 18, 19)
ids_o <- c(1602, 1603, 1604)
df_i <- NULL
df_o <- NULL
for (id in ids_i) {
    data_wings <- getWingsData(start = "2024-02-29", end = "2024-03-01", type = "i", id = id) %>% mutate(type = "indoor")
    df_i <- bind_rows(df_i, data_wings)
}

for (id in ids_o) {
    data_wings <- getWingsData(start = "2024-02-29", end = "2024-03-01", type = "o", id = id) %>% mutate(type = "outdoor")
    df_o <- bind_rows(df_o, data_wings)
}
df_i <- df_i %>%
    mutate(device_id = as.character(device_id)) %>%
    openair::timeAverage(., avg.time = "5 min", type = "device_id") %>%
    filter(as.Date(date) == "2024-02-29") %>%
    filter(between(date, as.POSIXct("2024-02-29 10:06:00 UTC", tz = "UTC"), as.POSIXct("2024-03-01 08:30:00 UTC", tz = "UTC"))) %>%
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH))

# gas reference
gases <- getPalasData("data\\AQ_Guard\\",
        start = "2024-02-29",
        end = "2024-03-02") %>%
        openair::timeAverage(.,avg.time="5 min") %>%
        filter(as.Date(date)=="2024-02-29") %>%
        filter(between(date, as.POSIXct("2024-02-29 10:06:00 UTC", tz = "UTC"), as.POSIXct("2024-03-01 08:30:00 UTC", tz = "UTC"))) %>% 
        mutate(abs_hum = threadr::absolute_humidity(air_temp, RH))

VOC <- ggplot()+
geom_line(data=reference,aes(x=date,y=VOC/10,col="Reference / 10"))+
geom_line(data=df_i,aes(x=date,y=VOC,col="DUT",lty=device_id))+
theme_bw(16) +
ylab("VOC in ppb") + labs(col="",lty="")+scale_color_manual(breaks=c("DUT","Reference / 10"),values=c("red3","black"))


RH <- ggplot(reference%>% mutate(abs_hum=threadr::absolute_humidity(air_temp,RH)))+geom_line(aes(x=date,y=RH,col="Reference"))+
geom_line(data=gases,aes(x=date,y=RH,col="RH(VOC reference)"))+
theme_bw(16)+
geom_line(data=df_i,aes(x=date,y=RH,col="DUT",lty=device_id)) + ylab("RH in %")+labs(col="",lty="")

abs_hum <- ggplot(reference%>% mutate(abs_hum=threadr::absolute_humidity(air_temp,RH)))+geom_line(aes(x=date,y=abs_hum,col="Reference"))+
geom_line(data=gases,aes(x=date,y=abs_hum,col="a(VOC reference)"))+
theme_bw(16)+
geom_line(data=df_i,aes(x=date,y=abs_hum,col="DUT",lty=device_id)) + 
ylab("a in g m-3")+labs(col="",lty="")+
scale_color_manual(breaks=c("a(VOC reference)","DUT","Reference"),values=c("#4545ff","red3","black"))

temp <- ggplot(reference)+
geom_line(aes(x=date,y=air_temp,col="Reference"))+
geom_line(data=gases,aes(x=date,y=air_temp,col="T(VOC reference)"))+
theme_bw(16)+
geom_line(data=df_i,aes(x=date,y=air_temp,col="DUT",lty=device_id))+
ylab("Temperature in °C")+labs(col="",lty="")+
scale_color_manual(breaks=c("T(VOC reference)","DUT","Reference"),values=c("#4545ff","red3","black"))

# Variation of VOC from Wings indoor device to Rh and T changes
cowplot::plot_grid(temp,RH,VOC, nrow=3,align="v") %>% ggsave(.,filename="RH_T_VOC_chamber_Wings_indoor_20240229.png",width=20,height=20,units="cm",dpi=150)
# Variation of VOC from Wings indoor device to abs. hum. and T changes
cowplot::plot_grid(temp,abs_hum,VOC, nrow=3,align="v") %>% ggsave(.,filename="T_abs_hum_VOC_chamber_Wings_indoor_20240229.png",width=20,height=20,units="cm",dpi=150)

correlation_along_x(data=df_i, reference = reference, avg.time = "5 min","2024-02-29 10:06:00 UTC", "2024-03-01 08:30:00 UTC", parameter = "VOC", alongWhat = "abs_hum", devicetype = "Wings_indoor_VOC_only_period",reference_device = "Reference")


# Correaltion test
# reference data
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
    co2 = getPalasData("data\\AQ_Guard\\",
        start = "2024-02-29",
        end = "2024-03-02"
    ),
    pm = getPalasData("data\\AQ_Guard\\",
        start = "2024-02-29",
        end = "2024-03-02"
    ),
    avg.time = "1 min") %>% 
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH)) %>%
    filter(between(date, as.POSIXct("2024-02-29 10:06:00 UTC", tz = "UTC"), as.POSIXct("2024-03-01 08:30:00 UTC", tz = "UTC")))

#Wings indoor data
ids_i <- c(17, 18, 19)
df_i <- NULL
for (id in ids_i) {
    data_wings <- getWingsData(start = "2024-02-29",
        end = "2024-03-02", type = "i", id = id, plt = FALSE)
    df_i <- bind_rows(df_i, data_wings)
}
df_i <- df_i %>% mutate(device_id = as.character(device_id))%>%
        openair::timeAverage(., avg.time = "5 min", 
        start.date = floor_date(.$date[1], "days") - days(1), 
        type = "device_id") %>% 
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH)) %>%
    filter(between(date, 
    as.POSIXct("2024-02-29 10:06:00 UTC", tz = "UTC"), 
    as.POSIXct("2024-03-01 08:30:00 UTC", tz = "UTC"))) %>% 
    mutate(device_id = as.character(device_id)) %>% dplyr::filter(PM02.5 < 11)

# Correlation of PMx, VOC to meteo
# 5 min
parameters <- c("VOC", "PM01", "PM02.5", "PM10")
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_i,
        reference = reference, parameter = p,
        start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC",
        avg.time = "5 min",
        devicetype = "Wings_Indoor_only_VOC_period", reference_device = "Palas"
    )
}
#1hour
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_i,
        reference = reference, parameter = p,
        start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC",
        avg.time = "1 hour",
        devicetype = "Wings_Indoor_only_VOC_period", reference_device = "Palas"
    )
}

# 5 min a, T, RH
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_RH(df_i,
        reference = reference, parameter = p,
        start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC",
        avg.time = "5 min",
        devicetype = "Wings_Indoor_only_VOC_period", reference_device = "Palas"
    )
}

for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_T(df_i,
        reference = reference, parameter = p,
        start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC",
        avg.time = "5 min",
        devicetype = "Wings_Indoor_only_VOC_period", reference_device = "Palas"
    )
}

for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_abshum(df_i, reference = reference, parameter = p,
        start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC",
        avg.time = "5 min",
        devicetype = "Wings_Indoor_only_VOC_period", reference_device = "Palas")
}

# Performacne only for VOC indiir
parameters <- "VOC"
for (p in parameters) {
    print(paste0("Parameter: ", p))
    performance(df_i,
        reference = reference, parameter = p, start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC", avg.time = "5 min", devicetype = "Wings_Indoor_only_VOC_period"
    )
}

## Summary of performance for all parameters 5 min and 1 hour
summary_performance(df_i,
    df_ref = reference %>% dplyr::select(date, VOC, PM10, PM02.5, PM01),
    start = "2024-02-29 10:06:00 UTC",
    end = "2024-03-01 08:30:00 UTC", avg.time = "5 min", devicetype = "Wings_Indoor_only_VOC_period", referencetype = "Palas"
)

summary_performance(df_i,
    df_ref = reference %>% dplyr::select(date, VOC, PM01, PM02.5, PM10),
    start = "2024-02-29 10:06:00 UTC",
    end = "2024-03-01 08:30:00 UTC", avg.time = "1 hour", devicetype = "Wings_Indoor_only_VOC_period", referencetype = "Palas"
)

# precision of indoor 
# 5 min
sink("output\\Precision_Wings_Indoor_VOC_Period_20240229-20240301_5min.txt")
parameters <- names(df_i %>% dplyr::select(-device_id, -date))
for (p in parameters) {
    precision(df_i,
        parameter = p,
        start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC", avg.time = "5 min",
        devicetype = "Wings_Indoor_only_VOC_period"
    )
}
sink(file = NULL)

# 1 hour
sink("output\\Precision_Wings_Indoor_VOC_Period_20240229-20240301_1hour.txt")
parameters <- names(df_i %>% dplyr::select(-device_id, -date))
for (p in parameters) {
    precision(df_i,
        parameter = p,
        start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC", avg.time = "1 hour",
        devicetype = "Wings_Indoor_only_VOC_period"
    )
}
sink(file = NULL)

##PM (both)
# reference data
reference <- reference_data(
    meteo = getPalasData("data\\AQ_Guard\\",
        start = "2024-02-14",
        end = "2024-03-17"
    ),
    gases = getPalasData("data\\AQ_Guard\\",
        start = "2024-02-14",
        end = "2024-03-17"
    ),
    co2 = getPalasData("data\\AQ_Guard\\",
        start = "2024-02-14",
        end = "2024-03-17"
    ),
    getPalasData("data\\AQ_Guard\\",
        start = "2024-02-14",
        end = "2024-03-17"
    ),
    #pm = read_csv("data\\TSI_3330_OPS\\chamber_PM.csv",col_names=T),
    avg.time = "1 min"
) %>%
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH)) %>%
    filter(between(date, as.POSIXct("2024-02-14 09:00:00 UTC", tz = "UTC"), 
    as.POSIXct("2024-02-16 15:30:00 UTC", tz = "UTC")))


# reference from TROPOS device (MPSS-OPSS)
reference_mpss <- reference_data(
    meteo = getPalasData("data\\AQ_Guard\\",
        start = "2024-02-14",
        end = "2024-03-17"
    ),
    gases = getPalasData("data\\AQ_Guard\\",
        start = "2024-02-14",
        end = "2024-03-17"
    ),
    co2 = getPalasData("data\\AQ_Guard\\",
        start = "2024-02-14",
        end = "2024-03-17"
    ),
    pm = read_csv("data\\TSI_3330_OPS\\chamber_PM.csv",col_names=T),
    avg.time = "5 min"
) %>%
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH)) %>%
    filter(between(date, as.POSIXct("2024-02-14 09:00:00 UTC", tz = "UTC"), as.POSIXct("2024-02-16 15:30:00 UTC", tz = "UTC")))


#indoor data
df_i <- NULL
for (id in ids_i) {
    data_wings <- getWingsData(start = "2024-02-14",
        end = "2024-02-17", type = "i", id = id, plt = FALSE)
    df_i <- bind_rows(df_i, data_wings)
}

df_i <- df_i %>% mutate(device_id = as.character(device_id))%>%
        openair::timeAverage(., avg.time = "5 min", 
        start.date = floor_date(.$date[1], "days") - days(1), 
        type = "device_id") %>% 
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH)) %>%
    filter(between(date, as.POSIXct("2024-02-14 09:00:00 UTC", tz = "UTC"), as.POSIXct("2024-02-16 15:30:00 UTC", tz = "UTC"))) %>% 
    mutate(device_id = as.character(device_id))


# outdoor data
df_o <- NULL
for (id in ids_o) {
    data_wings <- getWingsData(start = "2024-02-14",
        end = "2024-02-17", type = "o", id = id, plt = FALSE)
    df_o <- bind_rows(df_o, data_wings)
}

df_o <- df_o%>% mutate(device_id = as.character(device_id))%>%
        openair::timeAverage(., avg.time = "10 min", 
        start.date = floor_date(.$date[1], "days") - days(1), 
        type = "device_id") %>% 
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH)) %>%
    filter(between(date, as.POSIXct("2024-02-14 09:00:00 UTC", tz = "UTC"), as.POSIXct("2024-02-16 15:30:00 UTC", tz = "UTC"))) %>% 
    mutate(device_id = as.character(device_id))


# precsion during smoldering aroma stick experiment
# 5 min 
sink("output\\Wings_Indoor_only_PM_period\\Precision_Wings_Indoor_20240214-20240216_5min_chamber.txt")
parameters <- names(df_i %>% dplyr::select(-device_id, -date))
for (p in parameters) {
    precision(df_i %>% dplyr::filter(!as.Date(date)=="2024-02-15"),
        parameter = p, start = "2024-02-14 09:00:00 UTC",
        end = "2024-02-16 15:30:00 UTC",
        avg.time = "5 min",
        devicetype = "Wings_Indoor_chamber_during_PM_incence1_and_2"
    )
}
sink(file = NULL)

#1 hour
sink("output\\Wings_Indoor_only_PM_period\\Precision_Wings_Indoor_20240214-20240216_1hour_chamber.txt")
parameters <- names(df_i %>% dplyr::select(-device_id, -date))
for (p in parameters) {
    precision(df_i %>% dplyr::filter(!as.Date(date)=="2024-02-15"),
        parameter = p, start = "2024-02-14 09:00:00 UTC",
        end = "2024-02-16 15:30:00 UTC",
        avg.time = "1 hour",
        devicetype = "Wings_Indoor_chamber_during_PM_incence1_and_2"
    )
}
sink(file = NULL)

# outdoor 10 min
sink("output\\Wings_Outdoor_only_PM_period\\Precision_Wings_Outdoor_20240214-20240216_10min_chamber.txt")
parameters <- names(df_o %>% dplyr::select(-device_id, -date))
for (p in parameters) {
    precision(df_o%>% dplyr::filter(!as.Date(date)=="2024-02-15"),
        parameter = p, start = "2024-02-14 09:00:00 UTC",
        end = "2024-02-16 15:30:00 UTC",
        avg.time = "10 min",
        devicetype = "Wings_Outdoor_chamber_during_PM_incence1_and_2"
    )
}
sink(file = NULL)

# outdoor 1 hour
sink("output\\Wings_Outdoor_only_PM_period\\Precision_Wings_Outdoor_20240214-20240216_1hour_chamber.txt")
parameters <- names(df_o %>% dplyr::select(-device_id, -date))
for (p in parameters) {
    precision(df_o%>% dplyr::filter(!as.Date(date)=="2024-02-15"),
        parameter = p, start = "2024-02-14 09:00:00 UTC",
        end = "2024-02-16 15:30:00 UTC",
        avg.time = "1 hour",
        devicetype = "Wings_Outdoor_chamber_during_PM_incence1_and_2"
    )
}
sink(file = NULL)

#### Palas Ref Indoor

# 5 min
lookup <- lookup_table("ppb")
parameters <- match_parameters(reference_data = reference%>%dplyr::filter(as.Date(date)!="2024-02-15"), test_data = df_i%>%dplyr::filter(as.Date(date)!="2024-02-15"))
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_i%>%dplyr::filter(!as.Date(date)=="2024-02-15"), reference = reference%>%dplyr::filter(!as.Date(date)=="2024-02-15"), parameter = p, 
    start = "2024-02-14 11:00:00 UTC", end = "2024-02-16 15:30:00 UTC", 
    avg.time = "5 min ", devicetype = "Wings_Indoor_chamber_PM_incense", reference_device = "Palas")
}

# 1 hour
lookup <- lookup_table("ppb")
parameters <- match_parameters(reference_data = reference%>%dplyr::filter(as.Date(date)!="2024-02-15"), test_data = df_i%>%dplyr::filter(as.Date(date)!="2024-02-15"))
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_i%>%dplyr::filter(!as.Date(date)=="2024-02-15"), reference = reference%>%dplyr::filter(!as.Date(date)=="2024-02-15"), parameter = p, 
    start = "2024-02-14 11:00:00 UTC", end = "2024-02-16 15:30:00 UTC", 
    avg.time = "1 hour", devicetype = "Wings_Indoor_chamber_PM_incense", reference_device = "Palas")
}

#Reference MPSS + OPSS
# 5 min
parameters <- match_parameters(reference_data = reference_mpss%>%dplyr::filter(as.Date(date)!="2024-02-15"), test_data = df_i%>%dplyr::filter(as.Date(date)!="2024-02-15"))
# parameters <- c("PM02.5","PM10")
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_i%>%dplyr::filter(!as.Date(date)=="2024-02-15"), reference = reference_mpss%>%dplyr::filter(!as.Date(date)=="2024-02-15"), parameter = p, 
    start = "2024-02-14 11:00:00 UTC", end = "2024-02-16 15:30:00 UTC", 
    avg.time = "5 min ", devicetype = "Wings_Indoor_chamber_PM_incense", reference_device = "MPSS")
}
# 1 hour
lookup <- lookup_table("ppb")
parameters <- match_parameters(reference_data = reference_mpss%>%dplyr::filter(as.Date(date)!="2024-02-15"), test_data = df_i%>%dplyr::filter(as.Date(date)!="2024-02-15"))
# parameters <- c("PM02.5","PM10")
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_i%>%dplyr::filter(!as.Date(date)=="2024-02-15"), reference = reference_mpss%>%dplyr::filter(!as.Date(date)=="2024-02-15"), parameter = p, 
    start = "2024-02-14 11:00:00 UTC", end = "2024-02-16 15:30:00 UTC", 
    avg.time = "1 hour", devicetype = "Wings_Indoor_chamber_PM_incense", reference_device = "MPSS")
}


# Outdoor
# 10 min
parameters <- match_parameters(reference_data = reference%>%dplyr::filter(as.Date(date)!="2024-02-15"), test_data = df_o %>% dplyr::filter(as.Date(date)!="2024-02-15"))
# parameters <- c("PM02.5","PM10")
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_o%>%dplyr::filter(!as.Date(date)=="2024-02-15"), reference = reference%>%dplyr::filter(!as.Date(date)=="2024-02-15"), parameter = p, 
    start = "2024-02-14 11:00:00 UTC", end = "2024-02-16 15:30:00 UTC", 
    avg.time = "10 min ", devicetype = "Wings_Outdoor_chamber_PM_incense", reference_device = "Palas")
}

# 1 hour
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_o%>%dplyr::filter(!as.Date(date)=="2024-02-15"), reference = reference%>%dplyr::filter(!as.Date(date)=="2024-02-15"), parameter = p, 
    start = "2024-02-14 11:00:00 UTC", end = "2024-02-16 15:30:00 UTC", 
    avg.time = "1 hour", devicetype = "Wings_Outdoor_chamber_PM_incense", reference_device = "Palas")
}


#Reference MPSS + OPSS
# 5 min
parameters <- match_parameters(reference_data = reference_mpss%>%dplyr::filter(as.Date(date)!="2024-02-15"), test_data = df_o%>%dplyr::filter(as.Date(date)!="2024-02-15"))
# parameters <- c("PM02.5","PM10")
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_o %>% dplyr::filter(!as.Date(date)=="2024-02-15"), reference = reference_mpss%>%dplyr::filter(!as.Date(date)=="2024-02-15"), parameter = p, 
    start = "2024-02-14 11:00:00 UTC", end = "2024-02-16 15:30:00 UTC", 
    avg.time = "10 min ", devicetype = "Wings_Outdoor_chamber_PM_incense", reference_device = "MPSS")
}


# 1 hour
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_o%>%dplyr::filter(!as.Date(date)=="2024-02-15"), reference = reference_mpss%>%dplyr::filter(!as.Date(date)=="2024-02-15"), parameter = p, 
    start = "2024-02-14 11:00:00 UTC", end = "2024-02-16 15:30:00 UTC", 
    avg.time = "1 hour", devicetype = "Wings_Outdoor_chamber_PM_incense", reference_device = "MPSS")
}


# RH
# 5 min - REF Palas
parameters <- match_parameters(reference_data = reference%>%dplyr::filter(as.Date(date)!="2024-02-15"),test_data = df_i%>%dplyr::filter(as.Date(date)!="2024-02-15")%>%dplyr::select(-air_temp,-RH,-abs_hum))
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_RH(df_i%>%dplyr::filter(!as.Date(date)=="2024-02-15"), reference = reference %>% dplyr::filter(!as.Date(date)=="2024-02-15"), parameter = p, 
    start = "2024-02-14 11:00:00 UTC", end = "2024-02-16 15:30:00 UTC", 
    avg.time = "5 min ", devicetype = "Wings_Indoor_chamber_PM_incense", reference_device = "Palas")
}

# RH
# 5 min - REF MPSS
parameters <- match_parameters(reference_data = reference_mpss%>%dplyr::filter(as.Date(date)!="2024-02-15"),test_data = df_i%>%dplyr::filter(as.Date(date)!="2024-02-15")%>%dplyr::select(-air_temp,-RH,-abs_hum))
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_RH(df_i%>%dplyr::filter(!as.Date(date)=="2024-02-15"), reference = reference_mpss %>% dplyr::filter(!as.Date(date)=="2024-02-15"), parameter = p, 
    start = "2024-02-14 11:00:00 UTC", end = "2024-02-16 15:30:00 UTC", 
    avg.time = "5 min", devicetype = "Wings_Indoor_chamber_PM_incense", reference_device = "MPSS")
}

# Outdoor
# RH
# 10 min - Ref Palas
parameters <- match_parameters(reference_data = reference%>%dplyr::filter(as.Date(date)!="2024-02-15"), test_data = df_o%>%dplyr::filter(as.Date(date)!="2024-02-15")%>%dplyr::select(-air_temp,-RH,-abs_hum))

for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_RH(df_o%>%dplyr::filter(!as.Date(date)=="2024-02-15"), reference = reference %>% dplyr::filter(!as.Date(date)=="2024-02-15"), parameter = p, 
    start = "2024-02-14 11:00:00 UTC", end = "2024-02-16 15:30:00 UTC", 
    avg.time = "10 min ", devicetype = "Wings_Outdoor_chamber_PM_incense", reference_device = "Palas")
}

# RH
# 5 min - Ref Mpss
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_RH(df_o%>%dplyr::filter(!as.Date(date)=="2024-02-15"), reference = reference_mpss %>% dplyr::filter(!as.Date(date)=="2024-02-15"), parameter = p, 
    start = "2024-02-14 11:00:00 UTC", end = "2024-02-16 15:30:00 UTC", 
    avg.time = "10 min", devicetype = "Wings_Outdoor_chamber_PM_incense", reference_device = "MPSS")
}

### Office second round
# reference AQ-Guard from PALAS
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
    avg.time = "5 min"
) %>%  mutate(abs_hum=threadr::absolute_humidity(air_temp,RH)) 

#indoor and outdoor data
ids_i <- c(17, 18, 19)
ids_o <- c(1602, 1603, 1604)
df_i <- NULL
df_o <- NULL
for (id in ids_i) {
    data_wings <- getWingsData(start = "2023-12-01", end = "2024-03-01", type = "i", id = id, plt=F) %>% mutate(type = "indoor")
    df_i <- bind_rows(df_i, data_wings)
}

for (id in ids_o) {
    data_wings <- getWingsData(start = "2023-12-01", end = "2024-03-01", type = "o", id = id, plt=F) %>% mutate(type = "outdoor")
    df_o <- bind_rows(df_o, data_wings)
}

reference <- reference%>%filter(PM02.5 <= 25) # filter period to make it comparibale against other experiments


df_i <- df_i%>%filter(PM02.5<=25)
df_o <- df_o%>%filter(PM02.5<=25)

lookup <- lookup_table("microgramm")

##### Precision
#### Indoor
# 5 min
parameters <- match_parameters(reference_data = reference, test_data = df_i)
sink("output\\Wings_indoor_office_period\\Precision_Wings_20231204-20231220_5min_PM2.5below25µg.txt")
parameters <- names(df_i %>% dplyr::select(-device_id, -date))
for (p in parameters) {
    precision(df_i, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "5 min", devicetype = "Wings_indoor_office_period")
}
sink(file = NULL)

# 1 hour
parameters <- match_parameters(reference_data = reference, test_data = df_i)
sink("output\\Wings_indoor_office_period\\Precision_Wings_20231204-20231220_1hour_PM2.5below25µg.txt")
parameters <- names(df_i %>% dplyr::select(-device_id, -date))
for (p in parameters) {
    precision(df_i, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "1 hour", devicetype = "Wings_indoor_office_period")
}
sink(file = NULL)


# Outdoor
# 1 hour
parameters <- match_parameters(reference_data = reference, test_data = df_o)
sink("output\\Wings_outdoor_office_period\\Precision_WingsO_20231204-20231220_1hour_PM2.5below25µg.txt")
parameters <- names(df_o %>% dplyr::select(-device_id, -date))
for (p in parameters) {
    precision(df_o, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "1 hour", devicetype = "Wings_outdoor_office_period")
}
sink(file = NULL)

### Accuarcy
### indoor
lookup <- lookup_table("microgramm")
parameters <- match_parameters(reference_data = reference, test_data = df_i)

for (p in parameters) {
    print(paste0("Parameter: ", p))
    accuracy(df_i, reference = reference, parameter = p,  start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "1 hour", devicetype = "Wings_indoor_office_PMbelow25µg",reference_device = "Palas")
}
# outdoor

parameters <- match_parameters(reference_data = reference, test_data = df_o)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    accuracy(df_o, reference = reference, parameter = p,  start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "1 hour", devicetype = "Wings_outdoor_office_PMbelow25µg",reference_device = "Palas")
}

#### Correlation
# Indoor
lookup <- lookup_table("ppb")
parameters <- match_parameters(reference_data = reference, test_data = df_i)

for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_i, reference = reference, parameter = p,  start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "1 hour", devicetype = "Wings_indoor_office_PMbelow25µg",reference_device = "Palas")
}

#Outdoor
lookup <- lookup_table("microgramm")
parameters <- match_parameters(reference_data=reference,test_data = df_o)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_o, reference = reference, parameter = p,  start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "1 hour", devicetype = "Wings_outdoor_office_PMbelow25µg",reference_device = "Palas")
}

#Summary performance
#Indoor
summary_performance(
    df_test = df_i, 
    df_ref = reference, 
    "Wings_indoor_office_period_PMbelow25µg", #device name / experiment, here all the period from the chamber experiment
    "Palas", #reference device: Here calibrated Palas
    start = "2023-12-04 00:00:00 UTC", # start of period of interest
    end = "2023-12-20 00:00:00 UTC", # and end of period of interest
    avg.time = "1 hour")

#Outdoor
summary_performance(
    df_test = df_o, 
    df_ref = reference, 
    "Wings_outdoor_office_period_PMbelow25µg", #device name / experiment, here all the period from the chamber experiment
    "Palas", #reference device: Here calibrated Palas
    start = "2023-12-04 00:00:00 UTC", # start of period of interest
    end = "2023-12-20 00:00:00 UTC", # and end of period of interest
    avg.time = "1 hour")

##### room test - ARD

results <- readr::read_delim("data\\result_room_ATD_2.65.csv", col_names = T, delim = ",") %>% 
spread(var,val) %>% 
rename(device_id="device") %>% mutate(date=date+3600)

#indoor and outdoor data
ids_i <- c(17, 18, 19)
ids_o <- c(1602, 1603, 1604)
df_i <- NULL
df_o <- NULL
for (id in ids_i) {
    data_wings <- getWingsData(start = "2024-03-22", end = "2024-03-23", type = "i", id = id, plt=F,api_key = api) %>% mutate(type = "indoor")
    df_i <- bind_rows(df_i, data_wings)
}

for (id in ids_o) {
    data_wings <- getWingsData(start = "2024-03-22", end = "2024-03-23", type = "o", id = id, plt=F,api_key = api) %>% mutate(type = "outdoor")
    df_o <- bind_rows(df_o, data_wings)
}

palas <- getPalasData("data\\AQ_Guard\\",
        start = "2024-03-22",
        end = "2024-03-23",
        plot = FALSE
    ) %>% 
    openair::timeAverage(avg.time="1 min",start="2024-03-22 00:00:00",statistic = "mean",data.thresh = 0) %>% 
    mutate(device_id="Palas_13265")%>%mutate(date=date-2*3600)

parameters <- names(df_i%>%dplyr::select(-device_id,-date,-type))
#### Precision
#### indoor
## 5 min
sink("output\\Wings_indoor_room_period\\Precision_Wings_indoor_202403221200-202403221700_5min.txt")
for(p in parameters){
   precision(testdata = df_i%>%mutate(device_id=as.character(device_id)), start="2024-03-22 13:00:00 UTC",
    end="2024-03-22 17:00:00 UTC",parameter=p,avg.time="5 min",devicetype="Wings_indoor_room_test_ATD2.65")
}
sink(file=NULL)
## 1 hour
sink("output\\Wings_indoor_room_period\\Precision_Wings_indoor_202403221200-202403221700_1hour.txt")
for(p in parameters){
   precision(testdata = df_i%>%mutate(device_id=as.character(device_id)), start="2024-03-22 13:00:00 UTC",
    end="2024-03-22 17:00:00 UTC",parameter=p,avg.time="1 hour",devicetype="Wings_indoor_room_test_ATD2.65")
}
sink(file=NULL)

#### outdoor
parameters <- names(df_o%>%dplyr::select(-device_id,-date,-type))
## 10 min
sink("output\\Wings_outdoor_room_period\\Precision_Wings_outdoor_202403221300-202403221700_10min.txt")
for(p in parameters){
   precision(testdata = df_o%>%mutate(device_id=as.character(device_id)), start="2024-03-22 12:00:00 UTC",
    end="2024-03-22 16:00:00 UTC",parameter=p,avg.time="10 min",devicetype="Wings_outdoor_room_test_ATD2.65")
}
sink(file=NULL)
## 1 hour
sink("output\\Wings_outdoor_room_period\\Precision_Wings_outdoor_202403221300-202403221700_1hour.txt")
for(p in parameters){
   precision(testdata = df_o%>%mutate(device_id=as.character(device_id)), start="2024-03-22 13:00:00 UTC",
    end="2024-03-22 17:00:00 UTC",parameter=p,avg.time="1 hour",devicetype="Wings_outdoor_room_test_ATD2.65")
}
sink(file=NULL)

### Correlation with reference and Palas
## Indoor - vs MPSS_APSS
# 3 min
parameters <- match_parameters(df_i%>%dplyr::select(-device_id),results)
for(p in parameters){
   correlation(test_data = df_i%>%mutate(device_id=as.character(device_id)),reference=results, start="2024-03-22 12:00:00 UTC",
    end="2024-03-22 16:00:00 UTC",parameter=p,avg.time="3 min",devicetype="Wings_indoor_room_test",reference_device="MPSS_APSS_2.65")
}

#15 min
parameters <- match_parameters(df_i%>%dplyr::select(-device_id),results)
for(p in parameters){
   correlation(test_data = df_i%>%mutate(device_id=as.character(device_id)),reference=results, start="2024-03-22 12:00:00 UTC",
    end="2024-03-22 16:00:00 UTC",parameter=p,avg.time="15 min",devicetype="Wings_indoor_room_test",reference_device="MPSS_APSS_2.65")
}

## Indoor - vs Palas
# 3 min
parameters <- match_parameters(df_i%>%dplyr::select(-device_id),palas)
for(p in parameters){
   correlation(test_data = df_i%>%mutate(device_id=as.character(device_id)),reference=palas, start="2024-03-22 12:00:00 UTC",
    end="2024-03-22 16:00:00 UTC",parameter=p,avg.time="5 min",devicetype="Wings_indoor_room_test",reference_device="Palas")
}

#15 min
parameters <- match_parameters(df_i%>%dplyr::select(-device_id),results)
for(p in parameters){
   correlation(test_data = df_i%>%mutate(device_id=as.character(device_id)),reference=palas, start="2024-03-22 12:00:00 UTC",
    end="2024-03-22 16:00:00 UTC",parameter=p,avg.time="1 hour",devicetype="Wings_indoor_room_test",reference_device="Palas")
}





## Outdoor
# 15 min
parameters <- match_parameters(df_i%>%dplyr::select(-device_id),results)
for(p in parameters){
   correlation(test_data = df_i%>%mutate(device_id=as.character(device_id)),reference=results, start="2024-03-22 12:00:00 UTC",
    end="2024-03-22 16:00:00 UTC",parameter=p,avg.time="3 min",devicetype="Wings_indoor_room_test",reference_device="MPSS_APSS_2.65")
}

#15 min
parameters <- match_parameters(df_i%>%dplyr::select(-device_id),results)
for(p in parameters){
   correlation(test_data = df_i%>%mutate(device_id=as.character(device_id)),reference=results, start="2024-03-22 12:00:00 UTC",
    end="2024-03-22 16:00:00 UTC",parameter=p,avg.time="15 min",devicetype="Wings_indoor_room_test",reference_device="MPSS_APSS_2.65")
}


parameters <- match_parameters((ls_data %>% dplyr::select(date,PM02.5,PM10)),palas)
for(p in parameters){
   correlation(test_data = ls_data%>%mutate(device_id=as.character(device_id)),reference=palas, start="2024-03-22 12:00:00 UTC",
    end="2024-03-22 16:00:00 UTC",parameter=p,avg.time="5 min",devicetype="LS-PID_room_test",reference_device="Palas")
}

for(p in parameters){
   correlation(test_data = ls_data%>%mutate(device_id=as.character(device_id)),reference=palas, start="2024-03-22 12:00:00 UTC",
    end="2024-03-22 16:00:00 UTC",parameter=p,avg.time="1 hour",devicetype="LS-PID_room_test",reference_device="Palas")
}

#### room test - NH42SO4

results_amsulf <- readr::read_delim("data\\result_room_NH42SO4_1.77.csv", col_names = T, delim = ",") %>% 
spread(var,val) %>% 
rename(device_id="device")

palas <- getPalasData("data\\AQ_Guard\\",
        start = "2024-03-25",
        end = "2024-03-28",
        plot = FALSE
    )%>%dplyr::select(date,PM01,PM02.5,PM10) %>% 
    openair::timeAverage(avg.time="5 min",start="2024-03-25 14:00:00",statistic = "mean",data.thresh = 0) %>% 
    mutate(device_id="Palas_13265") %>% mutate(date=date-2*3600)


ls_data <- getLSData(path = "data\\LS_PID\\Download", datasource = "device",
        start = "2024-03-25",
        end = "2024-03-28",hours_difference_to_local_time = 1)%>%
        openair::timeAverage(avg.time="5 min",start.date = "2024-03-25 00:00:00",type="device_id")

        
parameters <- names(ls_data%>%dplyr::select(-device_id,-date))
sink("output\\LS_room_period\\Precision_LAS_202403221200-202403221600_5min_AMSulf.txt")
for(p in parameters){
   precision(testdata = ls_data%>%mutate(device_id=as.character(device_id)), start="2024-03-26 10:00:00 UTC",
    end="2024-03-26 16:00:00 UTC",parameter=p,avg.time="5 min",devicetype="LS-PID_room_test_AmSulf")
}
sink(file=NULL)

parameters <- match_parameters(ls_data %>% dplyr::select(-device_id),results_amsulf)
for(p in parameters){
   correlation(test_data = ls_data%>%mutate(device_id=as.character(device_id)),reference=results_amsulf, start="2024-03-26 10:00:00 UTC",
    end="2024-03-26 16:00:00 UTC",parameter=p,avg.time="5 min",devicetype="LS-PID_room_test",reference_device="MPSS_APSS_Amsulf_1.77")
}

for(p in parameters){
   correlation(test_data = ls_data%>%mutate(device_id=as.character(device_id)),reference=results_amsulf, start="2024-03-26 10:00:00 UTC",
    end="2024-03-26 16:00:00 UTC",parameter=p,avg.time="15 min",devicetype="LS-PID_room_test",reference_device="MPSS_APSS_Amsulf_1.77")
}

parameters <- match_parameters((ls_data %>% dplyr::select(date,PM02.5,PM10)),palas)
for(p in parameters){
   correlation(test_data = ls_data%>%mutate(device_id=as.character(device_id)),reference=palas, start="2024-03-26 10:00:00 UTC",
    end="2024-03-26 16:00:00 UTC",parameter=p,avg.time="5 min",devicetype="LS-PID_room_test_Amsulf",reference_device="Palas")
}

for(p in parameters){
   correlation(test_data = ls_data%>%mutate(device_id=as.character(device_id)),reference=palas, start="2024-03-26 10:00:00 UTC",
    end="2024-03-26 16:00:00 UTC",parameter=p,avg.time="1 hour",devicetype="LS-PID_room_test_Amsulf",reference_device="Palas")
}












####### Zagreb test
#### Evaluation of Wings sensors (indoor and outdoor)
#### Reference LAS and "reference" indoor and outdoor devices from WINGS ID316 and ID402, respectively

ids_i <- 1:15    # investugated devices 
ids_o <- 103:111 # investigated outdoor devices

# get data for this period 
df_i <- NULL
for (id in ids_i) {
    data_wings <- getWingsData(start = "2024-06-18", end = "2024-07-06", type = "i", id = id) %>% mutate(type = "indoor")
    df_i <- bind_rows(df_i, data_wings)
    }

# process, filter and abs. hum of indoor data devices
df_i <- df_i %>% 
        mutate(date = date - lubridate::hours(2)) %>% 
        mutate(device_id = as.character(device_id)) %>%
        filter(between(date, as.POSIXct("2024-06-18 11:00:00 UTC", tz = "UTC"), as.POSIXct("2024-07-05 15:50:00", tz = "UTC"))) %>%
        openair::timeAverage(., avg.time = "15 min", start.date = floor_date(.$date[1], "days") - days(1), type = "device_id") %>%
        mutate(abs_hum = threadr::absolute_humidity(air_temp, RH)) %>% filter(PM01!=0)

# same for outdoor
df_o <- NULL
for (id in ids_o) {
    data_wings <- getWingsData(start = "2024-06-18", end = "2024-07-06", type = "o", id = id) %>% mutate(type = "outdoor")
    df_o <- bind_rows(df_o, data_wings)
}

df_o <- df_o %>% 
        mutate(date=date-lubridate::hours(2)) %>% 
        mutate(device_id = as.character(device_id)) %>%
        filter(between(date, as.POSIXct("2024-06-18 11:00:00 UTC", tz = "UTC"), as.POSIXct("2024-07-05 15:50:00", tz = "UTC"))) %>% 
        openair::timeAverage(., avg.time = "1 hour", start.date = floor_date(.$date[1], "days") - days(2), type = "device_id") %>% 
        mutate(abs_hum=threadr::absolute_humidity(air_temp,RH)) 

reference_o <- NULL
    data_wings <- getWingsData(start = "2024-06-18", end = "2024-07-06", type = "o", id = 402) %>% mutate(type = "outdoor")
    reference_o <- bind_rows(reference_o, data_wings)

reference_o <- reference_o %>% 
                mutate(date = date-lubridate::hours(2)) %>%
                dplyr::select(-device_id) %>%
                filter(between(date, as.POSIXct("2024-06-18 11:00:00 UTC", tz = "UTC"), as.POSIXct("2024-07-05 15:50:00", tz = "UTC"))) %>% 
                openair::timeAverage(., avg.time = "1 hour", start.date = floor_date(.$date[1], "days") - days(2)) %>%
                mutate(abs_hum = threadr::absolute_humidity(air_temp, RH))

# reference device from LAS
reference_las <- getLSData(data = "archive", start = "2024-07-01 00:00:00 UTC", end = "2024-07-06 00:00:00 UTC", plot = T, hours_difference_to_local_time = -0) %>% 
                 mutate(abs_hum = threadr::absolute_humidity(air_temp, RH)) %>%
                 dplyr::filter(between(date, as.POSIXct("2024-07-03 12:00:00 UTC",tz="UTC"), as.POSIXct("2024-07-05 14:00:00 UTC",tz="UTC")))

# reference device from WINGS (indoor)
reference_i <- NULL
    data_wings <- getWingsData(start = "2024-06-18", end = "2024-07-06", type = "i", id = 316) %>% mutate(type = "indoor")
    reference_i <- bind_rows(reference_i, data_wings)

# filtering, averaging, prcoessing, abs. hum
reference_i <- reference_i %>% 
                mutate(date = date - lubridate::hours(2)) %>%
                dplyr::select(-device_id) %>%
                filter(between(date, as.POSIXct("2024-06-18 11:00:00 UTC", tz = "UTC"), as.POSIXct("2024-07-05 15:50:00", tz = "UTC"))) %>% 
                openair::timeAverage(., avg.time = "15 min", start.date = floor_date(.$date[1], "days") - days(2)) %>%
                mutate(abs_hum = threadr::absolute_humidity(air_temp, RH))



#### Indoor
##### Precision

# 15 min (base time resolution is 15 min)
parameters <- names(df_i %>% dplyr::select(-device_id, -date))
sink("output\\Wings_indoor_Zagreb_period\\Precision_Wings_indoor_20240618-20240707_Zagreb_15min_pm01_nonequal_0.txt")
for (p in parameters) {
    precision(df_i, parameter = p, start = "2024-06-18 00:00:00 UTC", end = "2024-07-07 00:00:00 UTC", avg.time = "15 min", devicetype = "Wings_indoor_Zagreb_period")
}
sink(file = NULL)

# 1 hour
parameters <- names(df_i %>% dplyr::select(-device_id, -date))
sink("output\\Wings_indoor_Zagreb_period\\Precision_Wings_indoor_20240618-20240707_Zagreb_1hour_pm01_nonequal_0.txt")

for (p in parameters) {
    precision(df_i, parameter = p, start = "2024-06-18 00:00:00 UTC", end = "2024-07-07 00:00:00 UTC", avg.time = "1 hour", devicetype = "Wings_indoor_Zagreb_period")
}
sink(file = NULL)

### Accuracy
### against LAS

lookup <- lookup_table("ppb")
parameters <- match_parameters(reference_data = reference_las, test_data = df_i)[-(1)]

#Accuracy agaianst LAS for
# 15 min and
for (p in parameters) {
    print(paste0("Parameter: ", p))
    accuracy(df_i %>% mutate(device_id=as.factor(device_id)), reference = reference_las, parameter = p,  start = "2024-07-03 12:00:00 UTC", end="2024-07-05 14:00:00 UTC", avg.time = "15 min", devicetype = "Wings_indoor_Zagreb", reference_device = "LS_169")
}

# 1 hour
for (p in parameters) {
    print(paste0("Parameter: ", p))
    accuracy(df_i %>% mutate(device_id=as.factor(device_id)), reference = reference_las, parameter = p,  start = "2024-07-03 12:00:00 UTC", end="2024-07-05 14:00:00 UTC", avg.time = "1 hour", devicetype = "Wings_indoor_Zagreb", reference_device = "LS_169")
}

# Correlation against LAS
#15 min
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_i, reference = reference_las, parameter = p,  start = "2024-07-03 12:00:00 UTC", end = "2024-07-05 14:00:00 UTC", avg.time = "15 min", devicetype = "Wings_indoor_Zagreb", reference_device = "LS_169")
}
#1 hour
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_i, reference = reference_las, parameter = p,  start = "2024-07-03 12:00:00 UTC", end = "2024-07-05 14:00:00 UTC", avg.time = "1 hour", devicetype = "Wings_indoor_Zagreb", reference_device = "LS_169")
}

# Summary of device performance
#15 min
summary_performance(
    df_test = df_i, 
    df_ref = reference_las, 
    "Wings_indoor_Zagreb", #device name / experiment, here all the period from the chamber experiment
    "LS_169", #reference device: Here calibrated Palas
    start="2024-07-03 12:00:00 UTC", # start of period of interest
    end="2024-07-05 14:00:00 UTC", # and end of period of interest
    avg.time = "15 min")

#1 hour
summary_performance(
    df_test = df_i, 
    df_ref = reference_las, 
    "Wings_indoor_Zagreb", #device name / experiment, here all the period from the chamber experiment
    "LS_169", #reference device: Here calibrated Palas
    start="2024-07-03 12:00:00 UTC", # start of period of interest
    end="2024-07-05 14:00:00 UTC", # and end of period of interest
    avg.time = "1 hour")

### against Wings_indoor_reference
lookup <- lookup_table("ppb")
parameters <- match_parameters(reference_data = reference_i, test_data = df_i)

# Accuracy
# 15 min
for (p in parameters) {
    print(paste0("Parameter: ", p))
    accuracy(df_i %>% mutate(device_id = as.factor(device_id)), reference = reference_i, parameter = p,  start = "2024-06-18 00:00:00 UTC", end = "2024-07-07 00:00:00 UTC", avg.time = "15 min", devicetype = "Wings_indoor_Zagreb", reference_device = "WINGS_i_316")
}

# 1 hour
for (p in parameters) {
    print(paste0("Parameter: ", p))
    accuracy(df_i %>% mutate(device_id = as.factor(device_id)), reference = reference_i, parameter = p,  start = "2024-06-18 00:00:00 UTC", end = "2024-07-07 00:00:00 UTC", avg.time = "1 hour", devicetype = "Wings_indoor_Zagreb", reference_device = "WINGS_i_316")
}
# Correlation
#15 min
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_i, reference = reference_i, parameter = p,  start = "2024-06-18 00:00:00 UTC", end = "2024-07-07 00:00:00 UTC", avg.time = "15 min", devicetype = "Wings_indoor_Zagreb", reference_device = "WINGS_i_316")
}
#1 hour
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(df_i, reference = reference_i, parameter = p,  start = "2024-06-18 00:00:00 UTC", end = "2024-07-07 00:00:00 UTC", avg.time = "1 hour", devicetype = "Wings_indoor_Zagreb", reference_device = "WINGS_i_316")
}

# Summary of performance against Wings indoor reference
# 15 min
summary_performance(
    df_test = df_i, 
    df_ref = reference_i, 
    "Wings_indoor_Zagreb", #device name / experiment, here all the period from the chamber experiment
    "WINGS_i_316", #reference device: Here calibrated Palas
    start="2024-06-18 00:00:00 UTC", # start of period of interest
    end="2024-07-07 00:00:00 UTC", # and end of period of interest
    avg.time = "15 min")

# 1 hour
summary_performance(
    df_test = df_i, 
    df_ref = reference_i, 
    "Wings_indoor_Zagreb", #device name / experiment, here all the period from the chamber experiment
    "WINGS_i_316", #reference device: Here calibrated Palas
    start="2024-06-18 00:00:00 UTC", # start of period of interest
    end="2024-07-07 00:00:00 UTC", # and end of period of interest
    avg.time = "1 hour")



# Outdoor device Evaluation
# Precision
# 1 hour
parameters <- names(df_o %>% dplyr::select(-device_id, -date))
sink("output\\Wings_outdoor_Zagreb_period\\Precision_Wings_outdoor_20240618-20240707_Zagreb_1hour.txt")
for (p in parameters) {
    precision(df_o, parameter = p, start = "2024-06-18 00:00:00 UTC", end = "2024-07-07 00:00:00 UTC", avg.time = "1 hour", devicetype = "Wings_outdoor_Zagreb_period")
}
sink(file = NULL)

### Accuracy
#reference LAS
lookup <- lookup_table("ppb")
parameters <- match_parameters(reference_data = reference_las, test_data = df_o)[-1]
for (p in parameters) {
    print(paste0("Parameter: ", p))
    accuracy(df_o %>% mutate(device_id = as.character(device_id)), reference = reference_las, parameter = p,  start = "2024-07-03 12:00:00 UTC", end = "2024-07-05 14:00:00 UTC", avg.time = "1 hour", devicetype = "Wings_outdoor_Zagreb", reference_device = "LS_169")
}
# Summary Performance
summary_performance(
    df_test = df_o, 
    df_ref = reference_las, 
    "Wings_outdoor_Zagreb", #device name / experiment, here all the period from the chamber experiment
    "LS_169", #reference device: Here calibrated Palas
    start="2024-07-03 12:00:00 UTC", # start of period of interest
    end="2024-07-05 14:00:00 UTC", # and end of period of interest
    avg.time = "1 hour")

#reference Wings_outdoor ID402
# Acccuracy 1 h
lookup <- lookup_table("ppb")
parameters <- match_parameters(reference_data = reference_o, test_data = df_o %>% dplyr::select(-device_id))
for (p in parameters) {
    print(paste0("Parameter: ", p))
    accuracy(df_o %>% mutate(device_id=as.character(device_id)), reference = reference_o, parameter = p,  start = "2024-06-18 00:00:00 UTC", end="2024-07-06 00:00:00 UTC", avg.time = "1 hour", devicetype = "Wings_outdoor_ferrara", reference_device = "Wings_o_402")
}

#Summary performance 1 hour
summary_performance(
    df_test = df_o, 
    df_ref = reference_o, 
    "Wings_outdoor_ferrara", #device name / experiment, here all the period from the chamber experiment
    "Wings_o_402", #reference device: Here calibrated Palas
    start = "2024-06-18 00:00:00 UTC", # start of period of interest
    end="2024-07-06 00:00:00 UTC", # and end of period of interest
    avg.time = "1 hour")


# Result tables read for Zagreb or Leipzig intercompariosn
#results <- readr::read_delim("data\\results.csv",col_names=T,delim=";",na="") %>% dplyr::filter(location_type=="indoor",device_type=="Wings")
results <- readr::read_delim("data\\results_zagreb.csv",col_names=T,delim=";",na="") %>% dplyr::filter(location_type=="outdoor",reference=="Wings_402")

labels <- sort(unique(results$device_id))[1:4]
#labels <- sort(unique(results$device_id))[1:9]
for (p in unique(results$parameter)){
    res <- results %>% gather(var,val,-device_id,-experiment,-device_type,-reference,-avg,-parameter,-location_type) %>% 
    dplyr::filter(parameter == p) %>% mutate(val = as.numeric(val))
    plt <- ggplot(res, aes(x=experiment, y=val, fill=device_id,color=device_id,alpha=reference)) +
    geom_bar(stat="identity", position=position_dodge(1),width=0.6) +
    ggplot2::facet_grid(var ~ avg, scales = "free") +
    theme_bw(16) +
    labs(title=paste0("Bar Plot by averaging period and stat. accuracy parameter\nfor ",lookup[p]), x="Experiment", y="Value") +
    theme(axis.text.x = element_text(angle=45, hjust=1),
    panel.background = element_rect(fill = "lightgray",
                                colour = "lightgray",
                                size = 0.5, linetype = "solid"))+
    scale_alpha_manual(values=c(1,0.5))+
    scale_fill_manual(breaks=c(labels,"mean"),values=c("red4","darkgreen","blue4","gray4"))+
    scale_color_manual(breaks=c(labels,"mean"),values=c("red4","darkgreen","blue4","gray4"))
    #scale_fill_manual(breaks=c(labels,"mean"),values=c(RColorBrewer::brewer.pal(9,"Set1"),RColorBrewer::brewer.pal(1,"Dark2"),"gray4"))+
    #scale_color_manual(breaks=c(labels,"mean"),values=c(RColorBrewer::brewer.pal(9,"Set1"),RColorBrewer::brewer.pal(1,"Dark2"),"gray4"))
    #scale_fill_manual(breaks=c(labels,"mean"),values=c(RColorBrewer::brewer.pal(9,"Set1"),"gray4"))+
    #scale_color_manual(breaks=c(labels,"mean"),values=c(RColorBrewer::brewer.pal(9,"Set1"),"gray4"))
    
    ggsave(plot=plt,filename=paste0("plt/Wings_indoor_leipzig_stat_param_",p,".png"),width = 30 ,height =30, units="cm",dpi=300)
}

# ggplot(results %>% dplyr::filter(device_id!="mean")%>%gather(var,val,-device_id,-experiment,-device_type,-reference,-avg,-parameter)%>%dplyr::filter(reference == "Palas AQ-Guard"))+barplot(aes(x=experiment,y=val,fill=device_id),position=position_dodge(),stat="identity")+facet_wrap(var~parameter)


# res_aq <- results %>% dplyr::filter(device_id!="mean")%>%gather(var,val,-device_id,-experiment,-device_type,-reference,-avg,-parameter)%>%dplyr::filter(reference == "Palas AQ-Guard")

# res_tropos <- results%>%dplyr::filter(device_id!="mean")%>%gather(var,val,-device_id,-experiment,-device_type,-reference,-avg,-parameter)%>%dplyr::filter(reference == "Tropos")

# res_vaisala <- results%>%dplyr::filter(device_id!="mean")%>%gather(var,val,-device_id,-experiment,-device_type,-reference,-avg,-parameter)%>%dplyr::filter(reference == "GMP-251")

# ggplot(res_aq,aes(x=interaction(avg,experiment),y=val,fill=device_id))+geom_bar(stat="identity",position=position_dodge(),width=0.2)+facet_grid(var~parameter,scales="free")+theme_bw(16)


# res <- results%>%gather(var,val,-device_id,-experiment,-device_type,-reference,-avg,-parameter)%>%dplyr::filter(parameter == "CO2")



# ggplot(res_aq, aes(x=experiment, y=val, fill=device_id,color=device_id,alpha=avg)) +
#   geom_bar(stat="identity", position=position_dodge(1),width=0.6) +
#   ggplot2::facet_grid(var ~ parameter, scales = "free_y") +
#   theme_bw(16) +
#   labs(title="Faceted Bar Plot by Parameter and Averaging Period", x="Experiment", y="Value") +
#   theme(axis.text.x = element_text(angle=45, hjust=1))+scale_alpha_manual(values=c(1,0.5))

# ggplot(res_tropos, aes(x=experiment, y=val, fill=device_id,color=device_id,alpha=avg)) +
#   geom_bar(stat="identity", position=position_dodge(1),width=0.6) +
#   ggplot2::facet_grid(var ~ parameter, scales = "free_y") +
#   theme_bw(16) +
#   labs(title="Faceted Bar Plot by Parameter and Averaging Period", x="Experiment", y="Value") +
#   theme(axis.text.x = element_text(angle=45, hjust=1))+scale_alpha_manual(values=c(1,0.5))

# ggplot(res_vaisala, aes(x=experiment, y=val, fill=device_id,color=device_id,alpha=avg)) +
#   geom_bar(stat="identity", position=position_dodge(1),width=0.6) +
#   ggplot2::facet_grid(var ~ parameter, scales = "free_y") +
#   theme_bw(16) +
#   labs(title="Faceted Bar Plot by Parameter and Averaging Period", x="Experiment", y="Value") +
#   theme(axis.text.x = element_text(angle=45, hjust=1))+scale_alpha_manual(values=c(1,0.5)) +
# annotate("text", x = 1:6, y = - 400,
#            label = rep(c("Variety 1", "Variety 2"), 2)) +
#   annotate("text", c(1.5, 3.5), y = - 800, label = c("Avg", "Experiment")) +
#   theme_classic() +
#   theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
#        axis.title.x = element_blank(),
#        axis.text.x = element_blank())

##### further testing

ids_i <- c(17, 18, 19)
ids_o <- c(1602, 1603, 1604)
df_i <- NULL
df_o <- NULL
for (id in ids_i) {
    data_wings <- getWingsData(start = "2023-12-01", end = "2024-03-01", type = "i", id = id) %>% mutate(type = "indoor")
    df_i <- bind_rows(df_i, data_wings)
}

for (id in ids_o) {
    data_wings <- getWingsData(start = "2023-12-01", end = "2024-03-01", type = "o", id = id) %>% mutate(type = "outdoor")
    df_o <- bind_rows(df_o, data_wings)
}

sink("output\\Precision_Wings_o_20240212-20240216.txt")
parameters <- names(df_o %>% dplyr::filter(type == "outdoor") %>% dplyr::select(-device_id, -date, -type))
for (p in parameters) {
    precision(df_o %>% dplyr::filter(type == "outdoor"), parameter = p, start = "2024-02-12", end = "2024-02-17", avg.time = "20 min",devicetype=paste0("Wings_Outdoor"))
}
sink(file = NULL)

sink("output\\Precision_Wings_i_20240212-20240216.txt")
parameters <- names(df %>% filter(type == "indoor") %>% select(-device_id, -date, -type))
for (p in parameters) {
    precision(df_i %>% filter(type == "indoor"), parameter = p, start = "2024-02-12", end = "2024-02-17", avg.time = "20 min",devicetype=paste0("Wings_Indoor"))
}
sink(file = NULL)


data_palas <- getPalasData("data\\AQ_Guard\\",start="2024-02-12",end="2024-02-17") %>%
    as_tibble(.) %>%
    select(date, contains("PM"), CO2, air_temp, RH, VOC, air_pressure)


sink("output\\Accuracy_Wings_o_20240216-20240216.txt")
match_parameters(t_palas_dl_prc,df_o)
for (p in parameters) {
        #print(paste0("Parameter: ", p))
        accuracy(df_o %>% select(-type), reference = t_palas_dl_prc, parameter = p, start = "2024-02-16",end="2024-02-16", avg.time = "20 min",devicetype=paste0("Wings_Outdoor"))
    }
sink(file=NULL)

sink("output\\Accuracy_Wings_i_20240216-20240216.txt")
match_parameters(t_palas_dl_prc,df_i)
for (p in parameters) {
        #print(paste0("Parameter: ", p))
        accuracy(df_i %>% select(-type), reference = t_palas_dl_prc, parameter = p, start = "2024-02-16", end="2024-02-16", avg.time = "5 min",devicetype=paste0("Wings_Indoor"))
    }
sink(file=NULL)


match_parameters(data_palas%>%select(-device_id),df_i)
for (p in parameters) {
        #print(paste0("Parameter: ", p))
        correlation(df_i %>% select(-type), reference = data_palas%>%select(-device_id), parameter = p, start = "2024-02-12", end="2024-02-16", avg.time = "10 min",devicetype=paste0("Wings_Indoor"),reference_device = "Palas")
    }


##### First Arizon road dust test evaluations

##### ATD
ids_i <- c(17, 18, 19)
ids_o <- c(1602, 1603, 1604)
df_i <- NULL
df_o <- NULL
for (id in ids_i) {
    data_wings <- getWingsData(start = "2024-03-20", end = "2024-03-22", type = "i", id = id) %>% mutate(type = "indoor")
    df_i <- bind_rows(df_i, data_wings)
}

for (id in ids_o) {
    data_wings <- getWingsData(start = "2024-03-20", end = "2024-03-22", type = "o", id = id) %>% mutate(type = "outdoor")
    df_o <- bind_rows(df_o, data_wings)
}

sink("output\\Precision_Wings_i_20240321-20240321_ATD.txt")
parameters <- names(df_i %>% filter(type == "indoor") %>% select(-device_id, -date, -type))
for (p in parameters) {
    precision(df_i %>% filter(type == "indoor"), parameter = p, start = "2024-03-21 13:00:00 UTC", end = "2024-03-21 17:00:00 UTC", avg.time = "10 min",devicetype=paste0("Wings_Indoor"))
}
sink(file = NULL)

sink("output\\Precision_Wings_o_20240321-20240321_ATD.txt")
parameters <- names(df_o %>% filter(type == "outdoor") %>% select(-device_id, -date, -type))
for (p in parameters) {
    precision(df_o %>% filter(type == "outdoor"), parameter = p, start = "2024-03-21 13:00:00 UTC", end = "2024-03-21 17:00:00 UTC", avg.time = "10 min",devicetype=paste0("Wings_Outdoor"))
}
sink(file = NULL)


sink("output\\Precision_Wings_o_20240320-20240320_ATD.txt")
parameters <- names(df_o %>% filter(type == "outdoor") %>% select(-device_id, -date, -type))
for (p in parameters) {
    precision(df_o %>% filter(type == "outdoor"), parameter = p, start = "2024-03-20 00:00:00 UTC", end = "2024-03-21 00:00:00 UTC", avg.time = "10 min",devicetype=paste0("Wings_Outdoor"))
}
sink(file = NULL)