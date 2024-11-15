# Quick and easy evaluation of the assumed "reference" - all matching data points of PALAS and chamebr considered

#Palas data for 12.02.2024 to 01.03.2024
data_palas <- getPalasData("data\\AQ_Guard\\", start = "2024-02-12", end = "2024-03-01") %>%
    as_tibble(.) %>%
    dplyr::select(date, contains("PM"), CO2, air_temp, RH, VOC, air_pressure, device_id) %>%
    mutate(VOC = VOC / 1000) # %>% # VOC of Palas in PPB to normalize with ppm of LAS x/1000: ppb * 1000 = 1 ppm

# chamber data
data_chamber <- getChamberData(
    start = "2024-02-12",
    end = "2024-03-01",
    devicetype = "chamber_lim"
)

#CO2 Vaiasla reference
CO2_vaisala <- vaisalaCO2(path = "data\\Vaisala-GMP251", start = "2024-02-12", end = "2024-03-01")

# finding intersection columns
parameters <- match_parameters(
    data_chamber,
    data_palas
)

#plot
ggplot(data=data_palas %>% filter(as.Date(date)=="2024-02-26"))+ # play around with the date
geom_line(aes(x=date,y=VOC*1000,col="VOC [ppm]"))+geom_line(aes(x=date,y=RH*50,col="RH"))+
geom_line(aes(x=date,y=air_temp*60,col="Temp"))+geom_line(aes(x=date,y=CO2,col="CO2"))

ggplot(data=data_palas%>%filter(as.Date(date)=="2024-02-26"))

# CO2 test
# all available matching data
# Correlation against chamber and Co_vaisala
lookup <- lookup_table("ppb")
correlation(data_palas, 
reference = CO2_vaisala, 
parameter = "CO2", start = "2024-02-12", end = "2024-03-01", avg.time = "5 min", devicetype = "Palas", reference_device = "Vaisala_GMP251")

correlation(data_palas %>% 
mutate(abs_hum = threadr::absolute_humidity(air_temp = air_temp, rh = RH)), 
reference = data_chamber %>%
mutate(abs_hum = threadr::absolute_humidity(air_temp = air_temp, rh = RH)), 
parameter = "abs_hum", start = "2024-02-12", end = "2024-03-01", avg.time = "5 min", devicetype = "Palas", reference_device = "chamber LIM (calc)")

#Performance agaianst Palas
performance(data = data_palas, start = "2024-02-12", end = "2024-03-01", reference = CO2_vaisala, parameter = "CO2", avg.time = "5 min", devicetype = "Palas")

### Test correlation of RH and T
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_palas, reference = data_chamber, parameter = p, start = "2024-02-12", end = "2024-03-01", avg.time = "5 min", devicetype = "Palas", reference_device = "Chamber_LIM")
}

# Only data from 2024-02-26 as a check for any time
parameters <- match_parameters(
    data_chamber,
    data_palas
)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_palas, reference = data_chamber, parameter = p, 
    start = "2024-02-26", end = "2024-02-26", avg.time = "5 min", 
    devicetype = "Palas", reference_device = "Chamber_LIM")
}

#Summary perforamnce against chamber data
summary_performance(
    data_palas,
    df_ref = data_chamber,
    start = "2024-02-12",
    end = "2024-03-01",
    avg.time = "5 min",
    devicetype = "Palas",
    referencetype = "Chamber_LIM_5min"
)

#correlation with corrected PALAS against chamber
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_palas %>% 
    mutate(RH = (RH - 4.4) / 0.81, 
    air_temp = (air_temp - 2.09) / 0.952), 
    reference = data_chamber, 
    parameter = p, 
    start = "2024-02-12", 
    end = "2024-03-01", 
    avg.time = "1 min", 
    devicetype = "Palas", 
    reference_device = "Chamber_LIM_5min")
}

# 4 days of CO2 data
data_CO2 <- vaisalaCO2(start = "2024-02-12", end = "2024-02-16")
parameters <- match_parameters(data_CO2, data_palas)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_palas, 
    reference = data_CO2, 
    parameter = p, 
    start = "2024-02-12", 
    end = "2024-02-16", 
    avg.time = "5 min", 
    devicetype = "Palas", 
    reference_device = "Vaisala_GMP251")
}
#Correlation agaisnt Vaisala
parameters <- "CO2"
data_CO2 <- vaisalaCO2(start = "2024-02-12", end = "2024-03-01")
# parameters <- match_parameters(data_CO2,data_palas)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_palas, reference = data_CO2, 
    parameter = p, start = "2024-02-12", end = "2024-03-01", avg.time = "5 min", devicetype = "Palas", reference_device = "Vaisala_GMP251")
}

# 8 days of data
data_CO2 <- vaisalaCO2(start = "2024-02-08", end = "2024-02-16")
parameters <- match_parameters(data_CO2, data_palas)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation_with_RH(data_palas, reference = data_CO2, 
    parameter = p, start = "2024-02-12", end = "2024-02-16", avg.time = "5 min", devicetype = "Palas", reference_device = "Vaisala_GMP251_withRH")
}

#correaltion with correction factors from 8 day period
data_CO2 <- vaisalaCO2(start = "2024-02-08", end = "2024-02-16")
parameters <- match_parameters(data_CO2, data_palas)
for (p in parameters) {
    print(paste0("Parameter: ", p))
    correlation(data_palas %>% mutate(CO2 = (CO2 + 40.5) / 0.941), reference = data_CO2, parameter = p, start = "2024-02-12", end = "2024-02-16", avg.time = "5 min", devicetype = "Palas_Adj_CO2", reference_device = "Vaisala_GMP251")
}


### Palas test for ATD
results <- readr::read_delim("data\\result_room_ATD_2.65.csv", col_names = T, delim = ",") %>% 
spread(var,val) %>% 
rename(device_id="device")

palas <- getPalasData("data\\AQ_Guard\\",
        start = "2024-03-22",
        end = "2024-03-23",
        plot = FALSE
    )%>%dplyr::select(date,PM01,PM02.5,PM10) %>% 
    openair::timeAverage(avg.time="3 min",start="2024-03-22 00:00:00",statistic = "mean",data.thresh = 0) %>% 
    mutate(device_id="Palas_13265")%>%mutate(date=date-2*3600)

#plt.scatter <- full_join(palas%>%rename(palas="val"),results%>%rename(Tropos="val"))
parameters <- match_parameters(palas%>%dplyr::select(-device_id),results)
for(p in parameters){
   correlation(test_data = palas,reference=results, start="2024-03-22 12:00:00 UTC",
    end="2024-03-22 16:00:00 UTC",parameter=p,avg.time="3 min",devicetype="Palas_room_test",reference_device="MPSS_APSS_2.65")
 
}

### Palas test for AmSulf
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

#plt.scatter <- full_join(palas%>%rename(palas="val"),results%>%rename(Tropos="val"))
parameters <- match_parameters(palas%>%dplyr::select(-device_id),results_amsulf)
for(p in parameters){
   correlation(test_data = palas,reference=results_amsulf, start="2024-03-26 10:00:00 UTC",
    end="2024-03-26 16:00:00 UTC",parameter=p,avg.time="5 min",devicetype="Palas_room_test",reference_device="MPSS_APSS_1.77_NH42SO4")
 
}