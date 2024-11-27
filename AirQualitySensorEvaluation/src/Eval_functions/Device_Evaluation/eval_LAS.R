# This script evaluates some experiemnts conducted with the LabService Analytica device
# Scripts and functions needed are stored in the dedicated R-Scripts


################# Begin of Evaluation #######################

#### Rooftoop experiment to check gas measurement performance

data_ls <- getLSData(path = "data\\LS_PID\\Download", datasource = "device", start = "2024-03-13 14:00:00 UTC", end = "2024-03-16 14:00:00 UTC", plot = FALSE)

reference <- reference_data(
    meteo = getPalasData("data\\AQ_Guard\\",
        start = "2024-03-13 14:00:00 UTC",
        end = "2024-03-16 14:00:00 UTC"
    ),
    gases = getPalasData("data\\AQ_Guard\\",
        start = "2024-03-13 14:00:00 UTC",
        end = "2024-03-16 14:00:00 UTC"
    ),
    getPalasData("data\\AQ_Guard\\",
        start = "2024-03-13 14:00:00 UTC",
        end = "2024-03-16 14:00:00 UTC"
    ),
    avg.time = "5 min"
) 
# Precision of LS device for 5 min ...
sink("output\\LS_rooftop\\Precision_LAS_20240313-160000_20240316-140000_5min.txt")
parameters <- names(data_ls %>% dplyr::select(-device_id, -date))
for (p in parameters) {
    precision(data_ls, parameter = p, start = "2024-03-13 16:00:00 UTC", end = "2024-03-16 14:00:00 UTC", avg.time = "5 min", devicetype = "LS_rooftop")
}
sink(file = NULL)

# ... and 1 hour averages
sink("output\\LS_rooftop\\Precision_LAS_20240313-160000_20240316-140000_1hour.txt")
parameters <- names(data_ls %>% dplyr::select(-device_id, -date))
for (p in parameters) {
    precision(data_ls, parameter = p, start = "2024-03-13 16:00:00 UTC", end = "2024-03-16 14:00:00 UTC", avg.time = "1 hour", devicetype = "LS_rooftop")
}
sink(file = NULL)

# Correlation
# match parameters
parameters <- match_parameters(reference, data_ls)
# Correlation against reference devices (here AQ-Guard)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_ls, reference = reference, parameter = p, start = "2024-03-13 17:00:00 UTC", end = "2024-03-16 14:00:00 UTC", avg.time = "5 min", devicetype = "LS_rooftop", reference_device = "all_AQ-Guard")
}

### try to identify breakpoints in the correlation of PM2.5
performance_segmented(data = data_ls, reference = reference, parameter = "PM02.5", avg.time = "5 min", devicetype = "LS_rooftop", start = "2024-03-13", end = "2024-03-16")


#### comparison with ambient station data among city (here UBA, access via API v3.0.0)
# data of LS device
data_ls <- getLSData(path = "data\\LS_PID\\Download", datasource = "device", start = "2024-03-13 14:00:00 UTC", end = "2024-03-16 14:00:00 UTC", plot = FALSE)
data_ls <- data_ls %>% mutate(
    O3 = O3 * 48 / (22.41 * ((air_temp + 273.15) / 273.15) * (1013 / air_pressure)),
    NO2 = NO2 * 46.0055 / (22.41 * ((air_temp + 273.15) / 273.15) * (1013 / air_pressure))
) # conversion of ppb into µg/m3

# data of German Environment agency station number DESN059 (leipzig west)
data_uba_lpz_west <- all_data_uba_station(station = "DESN059", begin = "2024-03-13", ends = "2024-03-18")

lookup <- lookup_table("microgramm") # requires microgram units
# match parameters of both data sets
parameters <- match_parameters(reference_data = data_uba_lpz_west, test_data = data_ls)

# correlation with station
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_ls,
        reference = data_uba_lpz_west %>%
            mutate(date = date + 3600), # conversion to local time
        parameter = p, start = "2024-03-13 17:00:00 UTC", end = "2024-03-16 14:00:00 UTC", avg.time = "1 hour", devicetype = "LS_rooftop", reference_device = "Lpz-West"
    )
}

# data of German Environment agency station number DESN025 (leipzig mitte)
data_uba_lpz_mitte <- all_data_uba_station(station = "DESN025", begin = "2024-03-13", ends = "2024-03-18")
# match parameters of both data sets
parameters <- match_parameters(reference_data = data_uba_lpz_mitte, test_data = data_ls)
# correlation with station
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_ls,
        reference = data_uba_lpz_mitte %>%
            mutate(date = date + 3600), # conversion to local time
        parameter = p, start = "2024-03-13 17:00:00 UTC", end = "2024-03-16 14:00:00 UTC", avg.time = "1 hour", devicetype = "LS_rooftop", reference_device = "Lpz-Mitte"
    )
}

# data of German Environment agency station number DESN077 (leipzig Lützener-Straße)
data_uba_lpz_ltz <- all_data_uba_station(station = "DESN077", begin = "2024-03-13", ends = "2024-03-18")
# match parameters of both data sets
parameters <- match_parameters(reference_data = data_uba_lpz_ltz, test_data = data_ls)
# correlation with station
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_ls,
        reference = data_uba_lpz_ltz %>%
            mutate(date = date + 3600), # to local time
        parameter = p, start = "2024-03-13 17:00:00 UTC", end = "2024-03-16 14:00:00 UTC", avg.time = "1 hour", devicetype = "LS_rooftop", reference_device = "Lpz-Ltz"
    )
}

# data of German Environment agency station number DESN080 (leipzig Schkeuditz)
data_uba_lpz_skz <- all_data_uba_station(station = "DESN080", begin = "2024-03-13", ends = "2024-03-18")
# match parameters of both data sets
parameters <- match_parameters(reference_data = data_uba_lpz_skz, test_data = data_ls)
# correlation with station
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_ls,
        reference = data_uba_lpz_skz %>%
            mutate(date = date + 3600), # to local time
        parameter = p, start = "2024-03-13 17:00:00 UTC", end = "2024-03-16 14:00:00 UTC", avg.time = "1 hour", devicetype = "LS_rooftop", reference_device = "Lpz-Skz"
    )
}

# comparison against gas sensors @ tropos
### rooftop gases DAQ chem
lookup <- lookup_table("ppb") # reference measures ppb

data_ls <- getLSData(path = "data\\LS_PID\\Download", datasource = "device", start = "2024-03-13 17:00:00 UTC", end = "2024-03-16 14:00:00 UTC", plot = FALSE)
# data of gas reference @tropos 
reference_chem <- getChemGases(file = "data\\Gases_chem_DAQ\\chem_gases.dat")
# other reference by AQ-Guard
reference_aq <- reference_data(
    meteo = getPalasData("data\\AQ_Guard\\",
        start = "2024-03-13 14:00:00 UTC",
        end = "2024-03-16 14:00:00 UTC"
    ),
    gases = getPalasData("data\\AQ_Guard\\",
        start = "2024-03-13 14:00:00 UTC",
        end = "2024-03-16 14:00:00 UTC"
    ),
    pm = getPalasData("data\\AQ_Guard\\",
        start = "2024-03-13 14:00:00 UTC",
        end = "2024-03-16 14:00:00 UTC"
    ),
    co2 = getPalasData("data\\AQ_Guard\\",
        start = "2024-03-13 14:00:00 UTC",
        end = "2024-03-16 14:00:00 UTC"
    ),
    avg.time = "1 min"
)

#plot meteorological condtions during experiment measured by AQ-Guard
ggplot(reference_aq %>% mutate(abs_hum = threadr::absolute_humidity(air_temp = air_temp, rh = RH)) %>% dplyr::select(date, RH, abs_hum, air_temp) %>% gather(var, val, -date)) +
    geom_line(aes(x = date, y = val, col = var)) +
    facet_wrap(. ~ var, scales = "free", nrow = 3, labeller = as_labeller(lookup)) +
    theme_bw(16) +
    xlab("date") +
    ylab("") +
    theme(legend.position = "none")
ggsave(plot = last_plot(), filename = "meteorological_rooftop.png", width = 20, height = 16, dpi = 450, units = "cm")

#summary of those
reference %>%
    dplyr::select(RH, abs_hum, air_temp) %>%
    summarise(across(where(is.numeric),
        .fns =
            list(
                min = min,
                median = median,
                mean = mean,
                stdev = sd,
                q25 = ~ quantile(., 0.25),
                q75 = ~ quantile(., 0.75),
                max = max
            )
    ))

#Correlation against AQ-Guard 5 min averages
parameters <- match_parameters(reference_data = reference_aq, test_data = data_ls)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_ls,
        reference = reference_aq,
        parameter = p, start = "2024-03-13 17:00:00 UTC", end = "2024-03-16 14:00:00 UTC", avg.time = "5 min", devicetype = "LS_rooftop", reference_device = "Palas"
    )
}
# ... and 1 hour averages
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_ls,
        reference = reference_aq,
        parameter = p, start = "2024-03-13 17:00:00 UTC", end = "2024-03-16 14:00:00 UTC", avg.time = "1 hour", devicetype = "LS_rooftop", reference_device = "Palas"
    )
}
#Correlation against chem. sensors at TROPOS 5 min averages
parameters <- match_parameters(reference_data = reference_chem, test_data = data_ls)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_ls,
        reference = reference_chem,
        parameter = p, start = "2024-03-13 17:00:00 UTC", end = "2024-03-16 14:00:00 UTC", avg.time = "1 hour", devicetype = "LS_rooftop", reference_device = "Chem-Tropos"
    )
}
# ... and 1 hour averages
parameters <- match_parameters(reference_data = reference, test_data = data_ls)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_ls,
        reference = reference_chem,
        parameter = p, start = "2024-03-13 17:00:00 UTC", end = "2024-03-16 14:00:00 UTC", avg.time = "5 min", devicetype = "LS_rooftop", reference_device = "Chem-Tropos"
    )
}

# summary of all 4 correlation tests
sum_palas_5min <- summary_performance(data_ls, df_ref = reference_aq, start = "2024-03-13 17:00:00 UTC", end = "2024-03-16 14:00:00 UTC", avg.time = "5 min", devicetype = "LS_rooftop", referencetype = "Palas")
sum_chem_5min <- summary_performance(data_ls, df_ref = reference_chem, start = "2024-03-13 17:00:00 UTC", end = "2024-03-16 14:00:00 UTC", avg.time = "5 min", devicetype = "LS_rooftop", referencetype = "Chem-Tropos")
sum_palas_1hour <- summary_performance(data_ls, df_ref = reference_aq, start = "2024-03-13 17:00:00 UTC", end = "2024-03-16 14:00:00 UTC", avg.time = "1 hour", devicetype = "LS_rooftop", referencetype = "Palas")
sum_chem_1hour <- summary_performance(data_ls, df_ref = reference_chem, start = "2024-03-13 17:00:00 UTC", end = "2024-03-16 14:00:00 UTC", avg.time = "1 hour", devicetype = "LS_rooftop", referencetype = "Chem-Tropos")


# dependency to meteorological conditons
# against temperature 5 min average
parameters <- match_parameters(reference_data = reference_aq, test_data = data_ls %>% dplyr::select(-air_temp))
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_T(data_ls,
        reference = reference_aq,
        parameter = p, start = "2024-03-13 17:00:00 UTC", end = "2024-03-16 14:00:00 UTC", avg.time = "5 min", devicetype = "LS_rooftop", reference_device = "Palas_AQ-Guard"
    )
}
# against temperature 1 hour average
parameters <- match_parameters(reference_data = reference_aq, test_data = data_ls)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_T(data_ls,
        reference = reference_aq,
        parameter = p, start = "2024-03-13 17:00:00 UTC", end = "2024-03-16 14:00:00 UTC", avg.time = "1 hour", devicetype = "LS_rooftop", reference_device = "Palas_AQ-Guard"
    )
}
# against RH
parameters <- match_parameters(reference_data = reference_aq, test_data = data_ls %>% dplyr::select(-air_temp, -RH))
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_RH(data_ls,
        reference = reference_aq,
        parameter = p, start = "2024-03-13 17:00:00 UTC", end = "2024-03-16 14:00:00 UTC", avg.time = "5 min", devicetype = "LS_rooftop", reference_device = "Palas_AQ-Guard"
    )
}
## summarises performance of device against AQ-Guard 
parameters <- match_parameters(reference_data = reference, test_data = data_ls)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    performance(
        data = data_ls, reference = reference_aq,
        parameter = p, start = "2024-03-13 17:00:00 UTC", end = "2024-03-16 14:00:00 UTC", avg.time = "1 hour", devicetype = "LS_rooftop"
    )
}

## test of correlation of reference and station along the city of Leipzig
data_uba_lpz_west <- all_data_uba_station(station = "DESN059", begin = "2024-03-01", ends = "2024-03-18")
parameters <- match_parameters(reference_data = reference, test_data = data_uba_lpz_west)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_uba_lpz_west %>% mutate(device_id = "Lpz.-West", O3 = O3 / 1.96, NO2 = NO2 / 1.88),
        reference = reference,
        parameter = p, start = "2024-03-13 17:00:00 UTC", end = "2024-03-16 14:00:00 UTC", avg.time = "1 hour", devicetype = "UBA_WEST", reference_device = "DAQ_chem_TROPOS"
    )
}

# summary of that correlation
data_uba_lpz_west <- all_data_uba_station(station = "DESN059", begin = "2024-03-01", ends = "2024-03-18")
parameters <- match_parameters(reference_data = reference, test_data = data_uba_lpz_west)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    performance(data_uba_lpz_west %>% mutate(device_id = "Lpz.-West", O3 = O3 / 1.96, NO2 = NO2 / 1.88),
        reference = reference,
        parameter = p, start = "2024-03-13 17:00:00 UTC", end = "2024-03-16 14:00:00 UTC", avg.time = "1 hour", devicetype = "UBA_WEST"
    )
}

### Measurements in office
data_ls <- getLSData(path = "data\\LS_PID\\Download", datasource = "device", start = "2023-12-04 00:00:00 UTC", end = "2024-12-20 00:00:00 UTC", plot = FALSE)

## Reference AQ-Guard
reference <- reference_data(
    meteo = getPalasData("data\\AQ_Guard\\2nd_office",
        start = "2023-12-04 00:00:00 UTC",
        end = "2023-12-20 00:00:00 UTC"
    ),
    gases = getPalasData("data\\AQ_Guard\\2nd_office",
        start = "2023-12-04 00:00:00 UTC",
        end = "2023-12-20 00:00:00 UTC"
    ),
    co2 = getPalasData("data\\AQ_Guard\\2nd_office",
        start = "2023-12-04 00:00:00 UTC",
        end = "2023-12-20 00:00:00 UTC"
    ),
    pm = getPalasData("data\\AQ_Guard\\2nd_office",
        start = "2023-12-04 00:00:00 UTC",
        end = "2023-12-20 00:00:00 UTC"
    ),
    avg.time = "1 min"
) %>% mutate(abs_hum = threadr::absolute_humidity(air_temp, RH)) # %>%
# filter(between(date, as.POSIXct("2024-02-29 10:06:00 UTC", tz = "UTC"), as.POSIXct("2024-03-01 08:30:00 UTC", tz = "UTC"))) %>%

# meteorological overview over that period
ggplot(reference %>% dplyr::select(date, RH, abs_hum, air_temp) %>% gather(var, val, -date)) +
    geom_line(aes(x = date, y = val, col = var)) +
    facet_wrap(. ~ var, scales = "free", nrow = 3, labeller = as_labeller(lookup)) +
    theme_bw(16) +
    xlab("date") +
    ylab("") +
    theme(legend.position = "none")
ggsave(plot = last_plot(), filename = "meteorological_office2nd.png", width = 20, height = 16, dpi = 450, units = "cm")

# summary of meteorological conditions
reference %>%
    dplyr::select(RH, abs_hum, air_temp) %>%
    summarise(across(where(is.numeric),
        .fns =
            list(
                min = min,
                median = median,
                mean = mean,
                stdev = sd,
                q25 = ~ quantile(., 0.25),
                q75 = ~ quantile(., 0.75),
                max = max
            )
    ))

# Voc in µg / m³ from Palas
lookup <- lookup_table("microgramm")

#Precision 
# 5 min

parameters <- match_parameters(reference_data = reference, test_data = data_ls)
sink("output\\LS_office_period\\Precision_LAS_20231204-20231220_5min.txt")
parameters <- names(data_ls %>% dplyr::select(-device_id, -date))
for (p in parameters) {
    precision(data_ls, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "5 min", devicetype = "LS_office_period")
}
sink(file = NULL)

# 1 hour

sink("output\\LS_office_period\\Precision_LAS_20231204-20231220_1hour.txt")
parameters <- names(data_ls %>% dplyr::select(-device_id, -date))
for (p in parameters) {
    precision(data_ls, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "1 hour", devicetype = "LS_office_period")
}
sink(file = NULL)

# below 25 µg to exclude bursts of NaCl
data_ls <- data_ls %>%
    filter(PM02.5 <= 25) %>%
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH))
reference <- reference %>% filter(PM02.5 <= 25)
reference <- reference %>%
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH))

# 5 min

sink("output\\LS_office_period\\Precision_LAS_20231204-20231220_5min_PM2.5below25µg.txt")
parameters <- names(data_ls %>% dplyr::select(-device_id, -date, -VOC_P3))
for (p in parameters) {
    precision(data_ls, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "5 min", devicetype = "LS_office_period_PMbelow25µg")
}
sink(file = NULL)

# 1 hour
sink("output\\LS_office_period\\Precision_LAS_20231204-20231220_1hour_PM2.5below25µg.txt")
parameters <- names(data_ls %>% dplyr::select(-device_id, -date))
for (p in parameters) {
    precision(data_ls, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "1 hour", devicetype = "LS_office_period_PMbelow25µg")
}
sink(file = NULL)

# Summarises performance of LS device
# 5 min
lookup <- lookup_table("microgramm")
parameters <- match_parameters(reference_data = reference %>% dplyr::select(-VOC_P3), test_data = data_ls)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    performance(data_ls, reference = reference, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "5 min", devicetype = "LS_office_period_PMbelow25µg")
}

# 1 hour
parameters <- match_parameters(reference_data = reference %>% dplyr::select(-VOC_P3), test_data = data_ls)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    performance(data_ls, reference = reference, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "1 hour", devicetype = "LS_office_period_PMbelow25µg")
}

### estimates accuracy for LAS devices (all at once)
# 5 min
parameters <- match_parameters(reference_data = reference %>% dplyr::select(-VOC_P3), test_data = data_ls)
# parameters <- c("PM02.5","PM10")
for (p in parameters) {
    print(paste0("Parameter: ", p))
    accuracy(data_ls, reference = reference, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "5 min", devicetype = "LS_office_period_PMbelow25µg", reference_device = "Palas")
}

# 1 hour

parameters <- match_parameters(reference_data = reference %>% dplyr::select(-VOC_P3), test_data = data_ls)
# parameters <- c("PM02.5","PM10")
for (p in parameters) {
    print(paste0("Parameter: ", p))
    accuracy(data_ls, reference = reference, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "1 hour", devicetype = "LS_office_period_PMbelow25µg", reference_device = "Palas")
}

# 1 hour
parameters <- match_parameters(reference_data = reference %>% dplyr::select(-VOC_P3), test_data = data_ls)
# parameters <- c("PM02.5","PM10")
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_ls, reference = reference, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "1 hour", devicetype = "LS_office_period_PMbelow25µg", reference_device = "Palas")
}

# 5 min
parameters <- match_parameters(reference_data = reference %>% dplyr::select(-VOC_P3), test_data = data_ls)
# parameters <- c("PM02.5","PM10")
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_ls, reference = reference, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "5 min", devicetype = "LS_office_period_PMbelow25µg", reference_device = "Palas")
}

#summarises performance (all parameters) 5 min and 1 hour
sum_5min <- summary_performance(
    df_test = data_ls,
    df_ref = reference,
    "LS_office_period_PM2.5_below25µg", # device name / experiment, here all the period from the chamber experiment
    "Palas", # reference device: Here calibrated Palas
    start = "2023-12-04 00:00:00 UTC", # start of period of interest
    end = "2023-12-20 00:00:00 UTC", # and end of period of interest
    avg.time = "5 min"
)

sum_1hour <- summary_performance(
    df_test = data_ls,
    df_ref = reference,
    "LS_office_period_PM2.5_below25µg", # device name / experiment, here all the period from the chamber experiment
    "Palas", # reference device: Here calibrated Palas
    start = "2023-12-04 00:00:00 UTC", # start of period of interest
    end = "2023-12-20 00:00:00 UTC", # and end of period of interest
    avg.time = "1 hour"
)

## All office period in dependence to abs. humidity
lookup <- lookup_table("ppb")
parameters <- match_parameters(reference_data = reference %>% dplyr::select(-VOC_P3, -RH, -air_temp), test_data = data_ls)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_abshum(data_ls, reference = reference, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "5 min", devicetype = "LS_office_period_PMbelow25µg", reference_device = "Palas")
}

## All office period in dependence to rel. humidity
lookup <- lookup_table("ppb")
parameters <- match_parameters(reference_data = reference %>% dplyr::select(-VOC_P3, -RH, -air_temp, -abs_hum), test_data = data_ls)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_RH(data_ls, reference = reference, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "5 min", devicetype = "LS_office_period_PMbelow25µg", reference_device = "Palas")
}

## All office period in dependence to temp
lookup <- lookup_table("ppb")
parameters <- match_parameters(reference_data = reference %>% dplyr::select(-VOC_P3, -RH, -air_temp, -abs_hum), test_data = data_ls)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_T(data_ls, reference = reference, parameter = p, start = "2023-12-04 00:00:00 UTC", end = "2023-12-20 00:00:00 UTC", avg.time = "5 min", devicetype = "LS_office_period_PMbelow25µg", reference_device = "Palas")
}

#### test abs. hum agreement of reference and LAS device
abs_hum_corr <- data_ls %>%
    openair::timeAverage(., "5 min", type = "device_id") %>%
    dplyr::select(date, device_id, abs_hum) %>%
    full_join(., reference %>% openair::timeAverage(., "5 min") %>% dplyr::select(date, abs_hum), suffix = c("test", "ref"), by = "date")

ggplot(abs_hum_corr) +
    geom_point(aes(x = abs_humref, y = abs_humtest, col = device_id))

#### test for CO2 measurements during working hours (concentration)
reference_workhours <- reference %>%
    filter(between(hour(date), 8, 18)) %>%
    filter(!wday(date) %in% c(6, 7))
ls_workhours <- data_ls %>%
    filter(between(hour(date), 8, 18)) %>%
    filter(!wday(date) %in% c(6, 7))

ggplot(ls_workhours) +
    geom_point(aes(x = date, y = CO2, col = device_id)) +
    ggtitle("CO2 in ppm (LS (non-cal.)") +
    theme_bw(16) +
    labs(col = "") +
    theme(legend.position = "none") +
    scale_y_log10() #+geom_hline(aes(yintercept = 986,col="mean"))

ggplot(reference_workhours) +
    geom_point(aes(x = date, y = CO2, col = "CO2")) +
    ggtitle("CO2 in ppm (Palas AQ-Guard (non-cal.)") +
    theme_bw(16) +
    labs(col = "") +
    theme(legend.position = "none") + 
    scale_y_log10() + 
    geom_hline(aes(yintercept = 986, col = "mean"))


##### Chamber experiments

###### Evaluation of CO2 performance conducted with inflow of pure CO2
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

#reference only measurements from AQ-Guard
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
    avg.time = "1 min"
) %>%
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH)) %>%
    dplyr::filter(between(date, as.POSIXct("2024-02-26 14:58:00 UTC", tz = "UTC"), as.POSIXct("2024-02-27 10:00:00 UTC", tz = "UTC")))

#LAS data
data_ls <- getLSData(path = "data\\LS_PID\\Download", datasource = "device", start = "2024-02-26", end = "2024-02-28") %>%
    filter(between(date, as.POSIXct("2024-02-26 14:58:00 UTC", tz = "UTC"), as.POSIXct("2024-02-27 10:00:00 UTC", tz = "UTC")))

# summarises the performance (statistical linearity parameters -- see function) for 5 min and 1 hours averages
summary_performance(data_ls, df_ref = reference, start = "2024-02-26", end = "2024-02-28", avg.time = "5 min", devicetype = "LS_only_CO2_period", referencetype = "Vaisala-GMP251")
summary_performance(data_ls, df_ref = reference, start = "2024-02-26", end = "2024-02-28", avg.time = "1 hour", devicetype = "LS_only_CO2_period", referencetype = "Vaisala-GMP251")

# precision
parameters <- names(data_ls %>% dplyr::select(-device_id, -date, -VOC_P3))
sink("output\\LS_only_CO2_period\\Precision_LAS_2024-02-26_145800-2024-02-27_100000.txt")
for (p in parameters) {
    precision(data_ls, parameter = p, start = "2024-02-26 14:58:00 UTC", end = "2024-02-27 10:00:00 UTC", 
    avg.time = "5 min", devicetype = "LS_only_CO2_period")
}
sink(file = NULL)

# only CO2 as parameter considers 5 min averages
parameters <- "CO2"

#Corelation with reference
#Vaisala
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_ls, reference = reference, parameter = p, start = "2024-02-26 14:58:00 UTC", end = "2024-02-27 10:00:00 UTC", avg.time = "5 min", devicetype = "LS_only_CO2_period", reference_device = "Vaisala_GMP251")
}
#Correlation in dependence to RH against Vaisala
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_RH(data_ls, reference = reference, parameter = p, start = "2024-02-26 14:58:00 UTC", end = "2024-02-27 10:00:00 UTC", avg.time = "5 min", devicetype = "LS_only_CO2_period", reference_device = "Vaisala_GMP251")
}
#Correlation in dependence to abs. hum. against Vaisala
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_abshum(data_ls, reference = reference, parameter = p, start = "2024-02-26 14:58:00 UTC", end = "2024-02-27 10:00:00 UTC", avg.time = "5 min", devicetype = "LS_only_CO2_period", reference_device = "Vaisala_GMP251")
}

#Correlation in dependence to temperature against Vaisala
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_T(data_ls, reference = reference, parameter = p, start = "2024-02-26 14:58:00 UTC", end = "2024-02-27 10:00:00 UTC", avg.time = "5 min", devicetype = "LS_only_CO2_period", reference_device = "Vaisala_GMP251")
}

#against AQ-Guard
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_RH(data_ls, reference = reference_aq, parameter = p, start = "2024-02-26 14:58:00 UTC", end = "2024-02-27 10:00:00 UTC", avg.time = "5 min", devicetype = "LS_only_CO2_period", reference_device = "AQ-Guard")
}
#### linear parameters given out during correlation function call


###### Evaluation of VOC performance (acetone from nail polish)
# Note: VOC is measured by a non-characterized indoor air quality sensor (PALAS AQ-Guard) chamber sensors act as reference for RH, T
# Experiment conducted on 29.02.2024 10:06 to 01.03.2024 08:30 local time

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
    avg.time = "1 min"
) %>%
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH)) %>%
    filter(between(date, as.POSIXct("2024-02-29 10:06:00 UTC", tz = "UTC"), as.POSIXct("2024-03-01 08:30:00 UTC", tz = "UTC")))

# LAS data
data_ls <- getLSData(path = "data\\LS_PID\\Download", datasource = "device", start = "2024-02-29", end = "2024-03-02") %>%
    filter(between(date, as.POSIXct("2024-02-29 10:06:00 UTC", tz = "UTC"), as.POSIXct("2024-03-01 08:30:00 UTC", tz = "UTC"))) %>%
    openair::timeAverage(., avg.time = "5 min", type = "device_id") %>%
    mutate(device_id = as.character(device_id))

# tbale with only gas measurements (VOC)
gases <- getPalasData("data\\AQ_Guard\\",
    start = "2024-02-29",
    end = "2024-03-02"
) %>%
    openair::timeAverage(., avg.time = "5 min") %>%
    filter(between(date, as.POSIXct("2024-02-29 10:06:00 UTC", tz = "UTC"), as.POSIXct("2024-03-01 08:30:00 UTC", tz = "UTC"))) %>%
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH))

# Precison of LAS device during that period
sink("output\\Precision_LAS_VOC_Period_20240229-20240301.txt")
parameters <- names(data_ls %>% select(-device_id, -date))
for (p in parameters) {
    precision(data_ls,
        parameter = p,
        start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC", avg.time = "5 min",
        devicetype = "LS_only_VOC_period"
    )
}
sink(file = NULL)

# Correlation of PM_x and VOC only
parameters <- c("VOC", "PM02.5", "PM10")
#correlation with reference 5 min average
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_ls,
        reference = reference, parameter = p,
        start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC",
        avg.time = "5 min",
        devicetype = "LS_only_VOC_period", reference_device = "Palas"
    )
}
#correlation with reference 1 hour average
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_ls,
        reference = reference, parameter = p,
        start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC",
        avg.time = "1 hour",
        devicetype = "LS_only_VOC_period", reference_device = "Palas"
    )
}
#dependency to RH
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_RH(data_ls,
        reference = reference, parameter = p,
        start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC",
        avg.time = "5 min",
        devicetype = "LS_only_VOC_period", reference_device = "Palas"
    )
}
# dependency to temperature
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_T(data_ls,
        reference = reference, parameter = p,
        start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC",
        avg.time = "5 min",
        devicetype = "LS_only_VOC_period", reference_device = "Palas"
    )
}
# dependency to abs. humidity
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_abshum(data_ls,
        reference = reference, parameter = p,
        start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC",
        avg.time = "5 min",
        devicetype = "LS_only_VOC_period", reference_device = "Palas"
    )
}
# general performance 
parameters <- "VOC"
for (p in parameters) {
    print(paste0("Parameter: ", p))
    performance(data_ls,
        reference = reference, parameter = p, start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC", avg.time = "5 min", devicetype = "LS_only_VOC_period"
    )
}
# summarises the performance against a reference
summary_performance(data_ls,
    df_ref = reference %>% dplyr::select(date, VOC, PM10, PM02.5),
    start = "2024-02-29 10:06:00 UTC",
    end = "2024-03-01 08:30:00 UTC", avg.time = "5 min", devicetype = "LS_only_VOC_period", referencetype = "Palas"
)

# plot for time series of VOC
VOC <- ggplot() +
    geom_line(data = reference, aes(x = date, y = VOC / 10, col = "Reference / 10")) +
    geom_line(data = data_ls, aes(x = date, y = VOC, col = "DUT", lty = device_id)) +
    theme_bw(16) +
    ylab("VOC in ppb") +
    labs(col = "", lty = "") +
    scale_color_manual(breaks = c("DUT", "Reference / 10"), values = c("red3", "black")) ## +
# theme(legend.position="bottom",legend.direction="horizontal")#+geom_line(data=gases,aes(x=date,y=VOC_mgm3*1000))

# plot for time series of humidity
RH <- ggplot(reference %>% mutate(abs_hum = threadr::absolute_humidity(air_temp, RH))) +
    geom_line(aes(x = date, y = RH, col = "Reference")) +
    geom_line(data = gases, aes(x = date, y = RH, col = "RH(VOC reference)")) +
    theme_bw(16) +
    geom_line(data = data_ls, aes(x = date, y = RH, col = "DUT", lty = device_id)) +
    ylab("RH in %") +
    labs(col = "", lty = "")

# plot for time series of abs. humidty
abs_hum <- ggplot(reference %>% mutate(abs_hum = threadr::absolute_humidity(air_temp, RH))) +
    geom_line(aes(x = date, y = abs_hum, col = "Reference")) +
    geom_line(data = gases, aes(x = date, y = abs_hum, col = "a(VOC reference)")) +
    theme_bw(16) +
    geom_line(data = data_ls, aes(x = date, y = abs_hum, col = "DUT", lty = device_id)) +
    ylab("a in g m-3") +
    labs(col = "", lty = "") +
    scale_color_manual(breaks = c("a(VOC reference)", "DUT", "Reference"), values = c("#4545ff", "red3", "black"))

# plot for time series of temperature
temp <- ggplot(reference) +
    geom_line(aes(x = date, y = air_temp, col = "Reference")) +
    geom_line(data = gases, aes(x = date, y = air_temp, col = "T(VOC reference)")) +
    theme_bw(16) +
    geom_line(data = data_ls, aes(x = date, y = air_temp, col = "DUT", lty = device_id)) +
    ylab("Temperature in °C") +
    labs(col = "", lty = "") +
    scale_color_manual(breaks = c("T(VOC reference)", "DUT", "Reference"), values = c("#4545ff", "red3", "black"))

# plot of VOC, RH, and temperature
cowplot::plot_grid(temp, RH, VOC, nrow = 3, align = "v") %>% ggsave(., filename = "RH_T_VOC_chamber_LAS.png", width = 20, height = 20, units = "cm", dpi = 150)
# plot of VOC, abs. humidty, and temperature
cowplot::plot_grid(temp, abs_hum, VOC, nrow = 3, align = "v") %>% ggsave(., filename = "T_abs_hum_VOC_chamber_LAS.png", width = 20, height = 20, units = "cm", dpi = 150)


###### Evaluation of RH, T measurement performance (also some other metrices were evaluated)
# Experiment conducted on 08.02.2024 07:16 to 08.02.2024 18:30 local time (dedicated test, testing of chamber)
# Reference is Vaisala, chamber for RH and T

reference_chamber <- reference_data(
    meteo = getChamberData(
        start = "2024-02-08",
        end = "2024-02-10",
        devicetype = "chamber",
        hours_difference_to_local_time = 0
    ),
    avg.time = "1 min"
) %>%
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH)) %>%
    dplyr::filter(between(date, as.POSIXct("2024-02-08 07:16:00 UTC", tz = "UTC"), as.POSIXct("2024-02-08 18:30:00 UTC", tz = "UTC")))

data_ls <- getLSData(path = "data\\LS_PID\\Download", datasource = "device", start = "2024-02-08", end = "2024-02-10") %>%
    dplyr::filter(between(date, as.POSIXct("2024-02-08 07:16:00 UTC", tz = "UTC"), as.POSIXct("2024-02-08 18:30:00 UTC", tz = "UTC"))) %>%
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH))

# 5 min
lookup <- lookup_table("microgramm")
parameters <- match_parameters(reference_data = reference_chamber, test_data = data_ls)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_ls, reference = reference_chamber, parameter = p, start = "2024-02-08 07:16:00 UTC", end = "2024-02-08 18:30:00 UTC", avg.time = "5 min ", devicetype = "LS_chamber", reference_device = "chamber")
}

## Evaluation of RH, T performance
# during Experiment conducted on 29.02.and 01.03.2024 local time (during VOC experiment)
# Reference is Vaisala GMP-251, chamber sensors for RH and T

# Load data from folder
data_ls <- getLSData(path = "data\\LS_PID\\Download", datasource = "device", start = "2024-02-29", end = "2024-03-02") %>%
    filter(between(date, as.POSIXct("2024-02-29 10:06:00 UTC", tz = "UTC"), as.POSIXct("2024-03-01 08:30:00 UTC", tz = "UTC"))) %>%
    openair::timeAverage(., avg.time = "5 min", type = "device_id") %>%
    mutate(device_id = as.character(device_id)) %>%
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH))

# make reference data 
reference <- reference_data(
    meteo = getChamberData( #chamber sensors RH, T
        start = "2024-02-29",
        end = "2024-03-02",
        devicetype = "chamber",
        hours_difference_to_local_time = 0
    ),
    gases = getPalasData("data\\AQ_Guard\\", #Palas as gas sensor reference
        start = "2024-02-29",
        end = "2024-03-02"
    ),
    co2 = vaisalaCO2(   #Vaiasala GMP-251 as CO2 sensor reference
        start = "2024-02-29",
        end = "2024-03-02"
    ),
    avg.time = "1 min"
) %>%
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH)) %>%
    filter(between(date, as.POSIXct("2024-02-29 10:06:00 UTC", tz = "UTC"), as.POSIXct("2024-03-01 08:30:00 UTC", tz = "UTC")))

# Precision estimate
parameters <- names(data_ls %>% dplyr::select(-device_id, -date, -VOC_P3))
sink("output\\LS_only_VOC_period\\Precision_LAS_20240229-20240301_5min_chamber.txt")
for (p in parameters) {
    precision(data_ls,
        parameter = p, start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC",
        avg.time = "5 min",
        devicetype = "LS_chamber_meteo_during_VOCtest"
    )
}
sink(file = NULL)

# Correlation with "reference"
# 5 min averages

lookup <- lookup_table("ppb")
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_ls,
        reference = reference,
        parameter = p, start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC",
        avg.time = "5 min ",
        devicetype = "LS_chamber", reference_device = "chamber"
    )
}

# 1 hour averages
lookup <- lookup_table("ppb")
parameters <- match_parameters(reference_data = reference, test_data = data_ls)
# parameters <- c("PM02.5","PM10")
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_ls,
        reference = reference,
        parameter = p, start = "2024-02-29 10:06:00 UTC",
        end = "2024-03-01 08:30:00 UTC",
        avg.time = "1 hour",
        devicetype = "LS_chamber", reference_device = "chamber"
    )
}

# Reference AQ-Guard
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

# Correlation with AQ-Guard

lookup <- lookup_table("ppb")
parameters <- match_parameters(reference_data = reference_aq, test_data = data_ls)
# parameters <- c("PM02.5","PM10")

# 5 min averages
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_ls, reference = reference_aq, parameter = p, start = "2024-02-29 10:06:00 UTC", end = "2024-03-01 08:30:00 UTC", avg.time = "5 min ", devicetype = "LS_chamber", reference_device = "Palas")
}

## PM experiments within chamber
# PM sensor evalualtion with smoldering incense sticks on 14. and 16.02.2024 (15.02.2024 is excluded from data)

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
    pm = read_csv("data\\TSI_3330_OPS\\chamber_PM.csv", col_names = T), # PM data based on sice distribution (density incense 1.1 g/cm³)
    avg.time = "5 min"
) %>%
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH)) %>%
    filter(between(date, as.POSIXct("2024-02-14 09:00:00 UTC", tz = "UTC"), as.POSIXct("2024-02-16 15:30:00 UTC", tz = "UTC")))

data_ls <- getLSData(path = "data\\LS_PID\\Download", datasource = "device", start = "2024-02-14", end = "2024-02-17") %>%
    filter(between(date, as.POSIXct("2024-02-14 09:00:00 UTC", tz = "UTC"), as.POSIXct("2024-02-16 15:30:00 UTC", tz = "UTC"))) %>%
    openair::timeAverage(., avg.time = "5 min", type = "device_id") %>%
    mutate(device_id = as.character(device_id)) %>%
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH))

# Precision
# 5 min averages
sink("output\\LS_only_PM_period\\Precision_LAS_20240214-20240216_5min_chamber.txt")
parameters <- names(data_ls %>% dplyr::select(-device_id, -date, -VOC_P3))
for (p in parameters) {
    precision(data_ls %>% dplyr::filter(!as.Date(date) == "2024-02-15"),
        parameter = p, start = "2024-02-14 09:00:00 UTC",
        end = "2024-02-16 15:30:00 UTC",
        avg.time = "5 min",
        devicetype = "LS_chamber_during_PM_incence1_and_2"
    )
}
sink(file = NULL)

# Correlation with "reference MPSS + OPSS"
# 5 min averages
lookup <- lookup_table("microgramm")
parameters <- c("PM02.5", "PM10")
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_ls %>% dplyr::filter(!as.Date(date) == "2024-02-15"),
        reference = reference %>% dplyr::filter(!as.Date(date) == "2024-02-15"), parameter = p,
        start = "2024-02-14 11:00:00 UTC", end = "2024-02-16 15:30:00 UTC",
        avg.time = "5 min ", devicetype = "LS_chamber_PM_incense", reference_device = "TROPOS"
    )
}
# 1 hour averages
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_ls %>% dplyr::filter(!as.Date(date) == "2024-02-15"),
        reference = reference %>% dplyr::filter(!as.Date(date) == "2024-02-15"), parameter = p,
        start = "2024-02-14 11:00:00 UTC", end = "2024-02-16 15:30:00 UTC",
        avg.time = "1 hour", devicetype = "LS_chamber_PM_incense", reference_device = "TROPOS"
    )
}

# Dependency to meterological parameters
# 5 min averages
## Correlation considering change in RH
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_RH(data_ls %>% dplyr::filter(!as.Date(date) == "2024-02-15"),
        reference = reference %>% dplyr::filter(!as.Date(date) == "2024-02-15"), parameter = p,
        start = "2024-02-14 11:00:00 UTC", end = "2024-02-16 15:30:00 UTC",
        avg.time = "5 min ", devicetype = "LS_chamber_PM_incense", reference_device = "TROPOS"
    )
}

## Correlation considering change in temperature
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_T(data_ls %>% dplyr::filter(!as.Date(date) == "2024-02-15"),
        reference = reference %>% dplyr::filter(!as.Date(date) == "2024-02-15"), parameter = p,
        start = "2024-02-14 11:00:00 UTC", end = "2024-02-16 15:30:00 UTC",
        avg.time = "5 min ", devicetype = "LS_chamber_PM_incense", reference_device = "TROPOS"
    )
}

## Correlation considering change in abs. humidity
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_abshum(data_ls %>% dplyr::filter(!as.Date(date) == "2024-02-15"),
        reference = reference %>% dplyr::filter(!as.Date(date) == "2024-02-15"), parameter = p,
        start = "2024-02-14 11:00:00 UTC", end = "2024-02-16 15:30:00 UTC",
        avg.time = "5 min ", devicetype = "LS_chamber_PM_incense", reference_device = "TROPOS"
    )
}

# Against Palas (Palas AQ-Guard is considered as reference device)
reference_aq <- reference_data(
    meteo = getPalasData("data\\AQ_Guard\\",
        start = "2024-02-14",
        end = "2024-03-17"
    ),
    pm = getPalasData("data\\AQ_Guard\\",
        start = "2024-02-14",
        end = "2024-03-17"
    ),
    avg.time = "1 min"
) %>%
    filter(between(date, as.POSIXct("2024-02-14 09:00:00 UTC", tz = "UTC"), as.POSIXct("2024-02-16 15:30:00 UTC", tz = "UTC"))) %>%
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH))

# Correlation with reference device

lookup <- lookup_table("ppb") # units in ppb
# intersects observed parameters
parameters <- match_parameters(
    reference_data = reference_aq %>% dplyr::filter(as.Date(date) != "2024-02-15"),
    test_data = data_ls %>% dplyr::filter(as.Date(date) != "2024-02-15")
)

# 5 min averages
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_ls %>% dplyr::filter(!as.Date(date) == "2024-02-15"),
        reference = reference_aq %>% dplyr::filter(!as.Date(date) == "2024-02-15"), parameter = p,
        start = "2024-02-14 11:00:00 UTC", end = "2024-02-16 15:30:00 UTC",
        avg.time = "5 min ", devicetype = "LS_chamber_PM_incense", reference_device = "Palas"
    )
}

# 1 hour  averages
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_ls %>% dplyr::filter(!as.Date(date) == "2024-02-15"),
        reference = reference_aq %>% dplyr::filter(!as.Date(date) == "2024-02-15"), parameter = p,
        start = "2024-02-14 11:00:00 UTC", end = "2024-02-16 15:30:00 UTC",
        avg.time = "1 hour", devicetype = "LS_chamber_PM_incense", reference_device = "Palas"
    )
}

## Correlation in dependency with rel. humidity
# 5 min averages
lookup <- lookup_table("microgramm")
parameters <- c("PM02.5", "PM10")
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_RH(data_ls %>% dplyr::filter(!as.Date(date) == "2024-02-15"),
        reference = reference_aq %>% dplyr::filter(!as.Date(date) == "2024-02-15"), parameter = p,
        start = "2024-02-14 11:00:00 UTC", end = "2024-02-16 15:30:00 UTC",
        avg.time = "5 min ", devicetype = "LS_chamber_PM_incense", reference_device = "Palas"
    )
}

## Correlation in dependency with temperature
# 5 min averages
lookup <- lookup_table("microgramm")
parameters <- c("PM02.5", "PM10")
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_T(data_ls %>% dplyr::filter(!as.Date(date) == "2024-02-15"),
        reference = reference_aq %>% dplyr::filter(!as.Date(date) == "2024-02-15"), parameter = p,
        start = "2024-02-14 11:00:00 UTC", end = "2024-02-16 15:30:00 UTC",
        avg.time = "5 min ", devicetype = "LS_chamber_PM_incense", reference_device = "Palas"
    )
}

## Correlation in dependency with abs. humidity
# 5 min averages
lookup <- lookup_table("microgramm")
parameters <- c("PM02.5", "PM10")
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_abshum(data_ls %>% dplyr::filter(!as.Date(date) == "2024-02-15"),
        reference = reference_aq %>% dplyr::filter(!as.Date(date) == "2024-02-15"), parameter = p,
        start = "2024-02-14 11:00:00 UTC", end = "2024-02-16 15:30:00 UTC",
        avg.time = "5 min ", devicetype = "LS_chamber_PM_incense", reference_device = "Palas"
    )
}

#### Produces summarising plot the results of the experiments
# loads data
results <- readr::read_delim("data\\results.csv", col_names = T, delim = ";")

# plot result statistical parameter along averageing period  - created for each observed parameter
for (p in unique(results$parameter)) {
    res <- results %>%
        gather(var, val, -device_id, -experiment, -device_type, -reference, -avg, -parameter) %>%
        dplyr::filter(parameter == p)
    plt <- ggplot(res, aes(x = experiment, y = val, fill = device_id, color = device_id, alpha = reference)) +
        geom_bar(stat = "identity", position = position_dodge(1), width = 0.6) +
        ggplot2::facet_grid(var ~ avg, scales = "free") +
        theme_bw(16) +
        labs(title = paste0("Bar Plot by averaging period and stat. accuracy parameter\nfor ", lookup[p]), x = "Experiment", y = "Value") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_alpha_manual(values = c(1, 0.5)) +
        scale_fill_manual(breaks = c("149", "150", "151", "mean"), values = c("red4", "green4", "blue3", "gray4")) +
        scale_color_manual(breaks = c("149", "150", "151", "mean"), values = c("red4", "green4", "blue3", "gray4"))

    ggsave(plot = plt, filename = paste0("LS_stat_param_", p, ".png"), width = 30, height = 30, units = "cm", dpi = 300)
}

#################### END of evaluation ########################



### Testbench for plots
ggplot(results %>% 
dplyr::filter(device_id != "mean") %>% 
gather(var, val, -device_id, -experiment, -device_type, -reference, -avg, -parameter)
 %>% dplyr::filter(reference == "Palas AQ-Guard")) +
    barplot(aes(x = experiment, y = val, fill = device_id), position = position_dodge(), stat = "identity") +
    facet_wrap(var ~ parameter)

res_aq <- results %>%
    dplyr::filter(device_id != "mean") %>%
    gather(var, val, -device_id, -experiment, -device_type, -reference, -avg, -parameter) %>%
    dplyr::filter(reference == "Palas AQ-Guard")

res_tropos <- results %>%
    dplyr::filter(device_id != "mean") %>%
    gather(var, val, -device_id, -experiment, -device_type, -reference, -avg, -parameter) %>%
    dplyr::filter(reference == "Tropos")

res_vaisala <- results %>%
    dplyr::filter(device_id != "mean") %>%
    gather(var, val, -device_id, -experiment, -device_type, -reference, -avg, -parameter) %>%
    dplyr::filter(reference == "GMP-251")

ggplot(res_aq, aes(x = interaction(avg, experiment), y = val, fill = device_id)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.2) +
    facet_grid(var ~ parameter, scales = "free") +
    theme_bw(16)

res <- results %>%
    gather(var, val, -device_id, -experiment, -device_type, -reference, -avg, -parameter) %>%
    dplyr::filter(parameter == "CO2")

# Results plotted for period and parameter (reference AQ-Guard)
ggplot(res_aq, aes(x = experiment, y = val, fill = device_id, color = device_id, alpha = avg)) +
    geom_bar(stat = "identity", position = position_dodge(1), width = 0.6) +
    ggplot2::facet_grid(var ~ parameter, scales = "free_y") +
    theme_bw(16) +
    labs(title = "Faceted Bar Plot by Parameter and Averaging Period", x = "Experiment", y = "Value") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_alpha_manual(values = c(1, 0.5))

# Results plotted for period and parameter (reference Tropos (PM))
ggplot(res_tropos, aes(x = experiment, y = val, fill = device_id, color = device_id, alpha = avg)) +
    geom_bar(stat = "identity", position = position_dodge(1), width = 0.6) +
    ggplot2::facet_grid(var ~ parameter, scales = "free_y") +
    theme_bw(16) +
    labs(title = "Faceted Bar Plot by Parameter and Averaging Period", x = "Experiment", y = "Value") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_alpha_manual(values = c(1, 0.5))


# Results plotted for period and parameter (reference vaisala - CO2)
ggplot(res_vaisala, aes(x = experiment, y = val, fill = device_id, color = device_id, alpha = avg)) +
    geom_bar(stat = "identity", position = position_dodge(1), width = 0.6) +
    ggplot2::facet_grid(var ~ parameter, scales = "free_y") +
    theme_bw(16) +
    labs(title = "Faceted Bar Plot by Parameter and Averaging Period", x = "Experiment", y = "Value") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_alpha_manual(values = c(1, 0.5)) + 
    annotate("text",
        x = 1:6, y = -400,
        label = rep(c("Variety 1", "Variety 2"), 2)
    ) +
    annotate("text", c(1.5, 3.5), y = -800, label = c("Avg", "Experiment")) +
    theme_classic() +
    theme(
        plot.margin = unit(c(1, 1, 4, 1), "lines"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()
    )


##### This part was created to test the functions and get a grasp on the data

# This part is not needed
# data_ls <- getLSData(path = "data\\LS_PID\\Archive\\", datasource = "archive")

# data_ls <- data_ls %>%
#     select(-contains("ppb")) %>%
#     rename("date" = "timestamp(UTC)") %>%
#     filter(date >= as.POSIXct("2024-02-08 12:30:00 UTC", tz = "UTC"))

# sink("output\\Precision_LAS_20240208-20240301.txt")
# parameters <- names(data_ls %>% select(-device_id, -date))
# for (p in parameters) {
#     precision(data_ls, parameter = p, start = "2024-02-12", end = "2024-02-17", avg.time = "20 min", devicetype = "LS")
# }
# sink(file = NULL)



# Chamber experiments at LIM (Leipzig Institute for Meteorology)

data_chamber <- getChamberData(start = "2024-02-08", end = "2024-02-16", devicetype = "chamber_lim")
sink("output\\Accuracy_LAS_20240213-20240213_5min.txt")
parameters <- match_parameters(data_chamber, data_ls)
for (p in parameters) {
    # print(paste0("Parameter: ", p))
    accuracy(data_ls, reference = data_chamber, parameter = p, start = "2024-02-13", end = "2024-02-13", avg.time = "5 min", devicetype = "LS")
}
sink(file = NULL)

parameters <- match_parameters(data_palas %>% select(-device_id), data_ls)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_ls, reference = data_palas, parameter = p, start = "2024-02-12", end = "2024-02-16", avg.time = "20 min", devicetype = "LS")
}

parameters <- "PM01" # match_parameters(data_palas %>% select(-device_id), data_ls)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_ls, reference = data_palas, parameter = p, start = "2024-02-12", end = "2024-02-16", avg.time = "20 min", devicetype = "LS")
}

parameters <- match_parameters(data_chamber, data_ls)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_ls, reference = data_chamber, parameter = p, start = "2024-02-12", end = "2024-02-12", avg.time = "5 min", devicetype = "LS", reference_device = "Chamber_LIM_5min")
}

data_CO2 <- vaisalaCO2(start = "2024-02-08", end = "2024-02-16")
parameters <- match_parameters(data_CO2, data_ls)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_ls, reference = data_CO2, parameter = p, start = "2024-02-12", end = "2024-02-16", avg.time = "5 min", devicetype = "LS", reference_device = "Vaisala_GMP251")
}


parameters <- match_parameters(t_palas_dl_prc %>% select(-device_id), data_ls)
sink("output\\LS\\Perfomance_LAS_20240212-20240216_adjustedWithCorrFrom.txt")
for (p in parameters) {
    print(paste0("Parameter: ", p))
    performance(data_ls, reference = t_palas_dl_prc %>% mutate(RH = (RH - 4.4) / 0.81, air_temp = (air_temp - 2.09) / 0.952) %>% select(-device_id), parameter = p, start = "2024-02-12", end = "2024-02-16", avg.time = "20 min", devicetype = "LS_ref_updated")
}
sink(file = NULL)

performance_of_device <- summary_performance(data_ls, data_palas %>% select(-device_id), "LS", "Palas", start = "2024-02-12", end = "2024-02-12", avg.time = "10 min")
performance_of_device_update <- summary_performance(data_ls, data_palas %>% select(-device_id) %>% mutate(RH = (RH - 4.4) / 0.81, air_temp = (air_temp - 2.09) / 0.952) %>% select(date, RH, air_temp), "LS", "Palas_Corrected", start = "2024-02-12", end = "2024-02-12", avg.time = "10 min")
performance_of_device <- summary_performance(data_ls, data_chamber, "LS", "Chamber", start = "2024-02-12", end = "2024-02-12", avg.time = "10 min")


###################
### Reference is cal. Palas
reference <- reference_data(
    meteo = getPalasData("data\\AQ_Guard\\",
        start = "2024-02-08",
        end = "2024-03-01",
        plot = FALSE
    ) %>% mutate(RH = (RH - 4.31) / 0.815, air_temp = (air_temp - 2.24) / 0.949),
    co2 = getPalasData("data\\AQ_Guard\\",
        start = "2024-02-08",
        end = "2024-03-01",
        plot = FALSE
    ) %>% mutate(CO2 = (CO2 + 37.1) / 0.96),
    gases = getPalasData("data\\AQ_Guard\\",
        start = "2024-02-08",
        end = "2024-03-01",
        plot = FALSE
    ),
    pm = getPalasData("data\\AQ_Guard\\",
        start = "2024-02-08",
        end = "2024-03-01",
        plot = FALSE
    ),
    avg.time = "5 min"
) %>%
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH))

data_ls <- getLSData(path = "data\\LS_PID\\Download", datasource = "device", start = "2024-02-12", end = "2024-03-01", plot = FALSE)

summary_performance(data_ls, reference, "LS_all_period", "Palas_cal", start = "2024-02-12 10:00:00 UTC", end = "2024-03-01 10:00:00 UTC", avg.time = "5 min")

sink("output\\LS_all_period\\Precision_LAS_20240212-20240301_5min.txt")
parameters <- names(data_ls %>% dplyr::select(-device_id, -date))
for (p in parameters) {
    precision(data_ls, parameter = p, start = "2024-02-12 10:00:00 UTC", end = "2024-03-01 10:00:00 UTC", avg.time = "5 min", devicetype = "LS_all_period")
}
sink(file = NULL)

# data_palas <- getPalasData("data\\AQ_Guard\\", start = "2024-02-08", end = "2024-02-16") # %>% # VOC of Palas in PPB to normalize with ppm of LAS x/1000: ppb * 1000 = 1 ppm

sink("output\\LS_all_period\\Accuracy_LAS_20240212-20240301_5min.txt")
parameters <- match_parameters(reference, data_ls)
for (p in parameters) {
    # print(paste0("Parameter: ", p))
    accuracy(data_ls, reference = reference, parameter = p, start = "2024-02-12 10:00:00 UTC", end = "2024-03-01 10:00:00 UTC", avg.time = "5 min", devicetype = "LS_all_period", reference_device = "Palas_cal")
}
sink(file = NULL)

parameters <- match_parameters(reference, data_ls)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_ls, reference = reference, parameter = p, start = "2024-02-12 10:00:00 UTC", end = "2024-03-01 10:00:00", avg.time = "5 min", devicetype = "LS_all_period", reference_device = "Palas_cal")
}

parameters <- "PM02.5" # match_parameters(reference, data_ls)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    bias_by_RH(data_ls, reference = reference, parameter = p, start = "2024-02-12 10:00:00 UTC", end = "2024-03-01 10:00:00", avg.time = "5 min", devicetype = "LS_all_period", reference_device = "Palas_cal")
}


# Performance PM2.5 < 25 µg
###################
### Reference is cal. Palas
reference <- reference_data(
    pm = getPalasData("data\\AQ_Guard\\",
        start = "2024-02-08",
        end = "2024-03-01",
        plot = FALSE
    ),
    avg.time = "5 min"
)

parameters <- match_parameters(reference, data_ls)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    performance(data_ls, reference = reference, parameter = p, start = "2024-02-12 10:00:00 UTC", end = "2024-03-01 10:00:00 UTC", avg.time = "5 min", devicetype = "LS_all_period_below_25µg")
}

summary_performance(data_ls, reference, "LS_all_period_below_25µg", "Palas_cal", start = "2024-02-12 10:00:00 UTC", end = "2024-03-01 10:00:00 UTC", avg.time = "5 min")

# Performance PM2.5 > 25 µg
###################
### Reference is cal. Palas
reference <- reference_data(
    pm = getPalasData("data\\AQ_Guard\\",
        start = "2024-02-12",
        end = "2024-03-01",
        plot = FALSE
    ),
    avg.time = "5 min"
) %>% filter(PM02.5 > 25)

parameters <- match_parameters(reference, data_ls)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_ls, reference = reference, parameter = p, start = "2024-02-12 10:00:00 UTC", end = "2024-03-01 10:00:00 UTC", avg.time = "5 min", devicetype = "LS_all_period_above_25µg", reference_device = "Palas_cal")
}

summary_performance(data_ls, reference, "LS_all_period_above_25µg", "Palas_cal", start = "2024-02-12 10:00:00 UTC", end = "2024-03-01 10:00:00 UTC", avg.time = "5 min")

#### Breakpoint PM data LAS
###################
### Reference is cal. Palas
reference <- reference_data(
    pm = getPalasData("data\\AQ_Guard\\",
        start = "2024-02-12",
        end = "2024-03-01",
        plot = FALSE
    ),
    avg.time = "5 min"
)

parameters <- match_parameters(reference, data_ls)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    performance_segmented(data_ls, reference = reference, parameter = p, start = "2024-02-12", end = "2024-03-01", avg.time = "5 min", devicetype = "LS_all_period_breakpoint")
}

# all period breakpoint at 14 µg @ PM2.5
reference <- reference_data(
    pm = getPalasData("data\\AQ_Guard\\",
        start = "2024-02-12",
        end = "2024-03-01",
        plot = FALSE
    ),
    avg.time = "5 min"
) %>% filter(PM02.5 <= 14)

parameters <- match_parameters(reference, data_ls)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_ls,
        reference = reference, parameter = p,
        start = "2024-02-12 10:00:00 UTC", end = "2024-03-01 10:00:00 UTC", avg.time = "5 min", devicetype = "LS_all_period_above_25µg", reference_device = "Palas_cal"
    )
}

##### room test - ARD
results <- readr::read_delim("data\\result_room_ATD_2.65.csv", col_names = T, delim = ",") %>% 
spread(var,val) %>% 
rename(device_id="device")

ls_data <- getLSData(path = "data\\LS_PID\\Download", datasource = "device",
        start = "2024-03-22",
        end = "2024-03-23")%>%
        openair::timeAverage(avg.time="3 min",start.date = "2024-03-22 00:00:00",type="device_id")

parameters <- names(ls_data%>%dplyr::select(-device_id,-date))

sink("output\\LS_room_period\\Precision_LAS_202403221200-202403221600_5min.txt")
for(p in parameters){
   precision(testdata = ls_data%>%mutate(device_id=as.character(device_id)), start="2024-03-22 12:00:00 UTC",
    end="2024-03-22 16:00:00 UTC",parameter=p,avg.time="5 min",devicetype="LS-PID_room_test_ATD2.65")
}
sink(file=NULL)


parameters <- match_parameters(ls_data%>%dplyr::select(-device_id),results)
for(p in parameters){
   correlation(test_data = ls_data%>%mutate(device_id=as.character(device_id)),reference=results, start="2024-03-22 12:00:00 UTC",
    end="2024-03-22 16:00:00 UTC",parameter=p,avg.time="15 min",devicetype="LS-PID_room_test",reference_device="MPSS_APSS_2.65")
}

palas <- getPalasData("data\\AQ_Guard\\",
        start = "2024-03-22",
        end = "2024-03-23",
        plot = FALSE
    ) %>% 
    openair::timeAverage(avg.time="3 min",start="2024-03-22 00:00:00",statistic = "mean",data.thresh = 0) %>% 
    mutate(device_id="Palas_13265")%>%mutate(date=date-2*3600)

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