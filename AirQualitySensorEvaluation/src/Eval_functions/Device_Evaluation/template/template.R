### to do: Create test data folder

# get test data, here from LAS from local data storage
test_data <- getLSData(
    path = "data\\LS_PID\\Download", 
    datasource = "device", # data directly downloaded from device, "archive" indicates downloaded from cloud plattform
    start = "2024-02-08 00:00:00", 
    end = "2024-03-01 00:00:00", 
    plot = FALSE)

# build a reference data set here data from PALAS AQ-Guard data from local storage is taken
# choose any of the getData functions if another device is your "Master" device
reference <- reference_data(
    meteo = getPalasData("data\\AQ_Guard\\",
        start = "2024-02-08 00:00:00",
        end = "2024-03-01 00:00:00",
        plot = FALSE
    ) %>% mutate(
        RH = (RH - 4.31) / 0.815, # reference RH calibrated 
        air_temp = (air_temp - 2.24) / 0.949 # air temp calibrated with correlation 
    ), # Rh and T calibrated based on previous correlation
    co2 = getPalasData("data\\AQ_Guard\\",
        start = "2024-02-08 00:00:00",
        end = "2024-03-01 00:00:00",
        plot = FALSE
    ) %>% mutate(CO2 = (CO2 + 37.1) / 0.96), # CO2 calibrated based on previous correlation against Vaisala GMP251 reference
    gases = getPalasData("data\\AQ_Guard\\",
        start = "2024-02-08 00:00:00",
        end = "2024-03-01 00:00:00",
        plot = FALSE
    ),
    pm = getPalasData("data\\AQ_Guard\\",
        start = "2024-02-08 00:00:00",
        end = "2024-03-01 00:00:00",
        plot = FALSE
    ),
    avg.time = "5 min" # time resolution is 5 minutes
) %>%
    mutate(abs_hum = threadr::absolute_humidity(air_temp, RH)) # create abs humidity


##########################################################################
# summarises performance (metrices for single units against a reference) #
##########################################################################

summary_performance(
    test_data, 
    reference, 
    "LS_all_period", #device name / experiment, here all the period from the chamber experiment
    "Palas_cal", #reference device: Here calibrated Palas
    start = "2024-02-12 10:00:00 UTC", #start
    end = "2024-03-01 10:00:00 UTC", # and end of period of interest
    avg.time = "5 min") # averaging time

###########################################
# write the precision of a set of devices #
###########################################

parameters <- names(test_data %>% dplyr::select(-device_id, -date)) # all parameters except device_id and data
sink("output\\LS_all_period\\Precision_LAS_20240212-20240301_5min.txt") # where to store the result
for (p in parameters) {
    precision(
        test_data, 
        parameter = p, 
        start = "2024-02-12 10:00:00 UTC", 
        end = "2024-03-01 10:00:00 UTC", avg.time = "5 min", devicetype = "LS_all_period")
}
sink(file = NULL)

# write the accuary of a set of devices 
parameters <- match_parameters(reference, test_data) # compares parameters of tables for accuracy test
sink("output\\LS_all_period\\Accuracy_LAS_20240212-20240301_5min.txt") # were to store the result
for (p in parameters) {
    # print(paste0("Parameter: ", p))
    accuracy(test_data, reference = reference, parameter = p, start = "2024-02-12 10:00:00 UTC", end = "2024-03-01 10:00:00 UTC", avg.time = "5 min", devicetype = "LS_all_period", reference_device = "Palas_cal")
}
sink(file = NULL)

#plots correlation of parameter list, correlation with abs hum, RH, and T possible
parameters <- match_parameters(reference, test_data)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(test_data, reference = reference, 
    parameter = p, 
    start = "2024-02-12 10:00:00 UTC", 
    end = "2024-03-01 10:00:00",
     avg.time = "5 min", 
     devicetype = "LS_all_period", 
     reference_device = "Palas_cal")
}

#############################################################################
# plots bias of a given parameter or parameter list, here PM2.5, against RH #
#############################################################################

parameters <- "PM02.5" # match_parameters(reference, data_ls)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    bias_by_RH(dtest_data, reference = reference, parameter = p, start = "2024-02-12 10:00:00 UTC", end = "2024-03-01 10:00:00", avg.time = "5 min", devicetype = "LS_all_period", reference_device = "Palas_cal")
}

#########################################################
# evaluates breakpoint of linear dependecy to reference #
#########################################################

parameters <- "PM02.5" # single parameter or list with match_parameters(reference, test_data)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    performance_segmented(test_data, reference = reference, parameter = p, start = "2024-02-12", end = "2024-03-01", avg.time = "10 min", devicetype = "LS_all_period_breakpoint")
}

### result would be a break point of 17.6 µg m-3

#"Parameter: PM02.5"
# # A tibble: 15 x 6
#    device_id    term        estimate std.error statistic    p.value
#    <chr>        <chr>          <dbl>     <dbl>     <dbl>      <dbl>
#  1 LS0623020149 (Intercept)   -0.539    0.0872     -6.18  7.49e- 10
#  2 LS0623020149 x              1.29     0.0415     30.9   2.07e-174
#  3 LS0623020149 U1.x          -0.837    0.0416    -20.1   1.19e- 82
#  4 LS0623020149 psi1.x         0        0.947       0     1   e+  0
#  5 LS0623020149 breakpoint    17.6     NA          NA    NA
#  6 LS0623020150 (Intercept)   -0.384    0.0844     -4.55  5.60e-  6
#  7 LS0623020150 x              1.18     0.0402     29.4   6.97e-160
#  8 LS0623020150 U1.x          -0.742    0.0403    -18.4   2.00e- 70
#  9 LS0623020150 psi1.x         0        1.04        0     1   e+  0
# 10 LS0623020150 breakpoint    17.6     NA          NA    NA
# 11 LS0623020151 (Intercept)   -0.345    0.0822     -4.20  2.80e-  5
# 12 LS0623020151 x              1.11     0.0392     28.4   2.28e-151
# 13 LS0623020151 U1.x          -0.692    0.0393    -17.6   4.28e- 65
# 14 LS0623020151 psi1.x         0        1.08        0     1   e+  0
# 15 LS0623020151 breakpoint    17.6     NA          NA    NA


# test if 17.6 fits
#plots correlation of parameter list, correlation with abs hum, RH, and T possible
parameters <- "PM02.5"#match_parameters(reference, test_data)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    performance(test_data, reference = reference %>% 
    filter(PM02.5 < 17.6), 
    parameter = p, 
    start = "2024-02-12", 
    end = "2024-03-01",
     avg.time = "5 min", 
     devicetype = "LS_all_period_ref_below17.6")
}

#plots correlation of parameter list, correlation with abs hum, RH, and T possible
parameters <- "PM02.5" # match_parameters(reference, test_data)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(test_data %>% filter(PM02.5 < 17.6), reference = reference, 
    parameter = p, 
    start = "2024-02-12 10:00:00 UTC", 
    end = "2024-03-01 10:00:00",
     avg.time = "5 min", 
     devicetype = "LS_all_period_pm025_below_17.6", # name the device type - ideal to seperate investiagtions also according your own needs, e.g., PM2.5 below xx µg m³
     reference_device = "Palas_cal")
}
