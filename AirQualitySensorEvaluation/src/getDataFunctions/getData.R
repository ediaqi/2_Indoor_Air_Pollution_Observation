
getLSData <- function(
    path = "data\\LS_PID\\",
    datasource = "archive",
    start = "2024-02-13 00:00:00",
    end = "2024-02-16 00:00:00",
    hours_difference_to_local_time = -1,
    plot = FALSE) {
    packages <- c("tidyverse", "lubridate", "openair", "httr", "jsonlite", "rlang")
    # Install packages not yet installed
    installed_packages <- packages %in% rownames(installed.packages())
    # Packages loading
    invisible(lapply(packages, library, character.only = TRUE))
    if (any(installed_packages == FALSE)) {
        install.packages(packages[!installed_packages])
    }

    start_str <- strftime(start, format = "%Y%m%d_%H%M%S")
    end_str <- strftime(end, format = "%Y%m%d_%H%M%S")

    timeidentifier <- paste0(start_str, "_", end_str)

    # make a dataframe depending on the data source "archive" or "device"
    if (datasource == "archive") {
        sn.paths <- list.dirs(paste0(path, "\\Archive\\"), recursive = F)
        data_ls <- NULL
        for (s in sn.paths) {
            files_sn <- list.dirs(s, recursive = F) %>% .[str_detect(., "date")]

            sn <- substr(basename(s), 4, str_count(basename(s)))
            for (f in files_sn) {
                table <- read_csv(paste0(f, "\\log.csv")) %>% mutate(device_id = sn)
                data_ls <- data_ls %>% bind_rows(., table)
            }
        }
        data <- data_ls %>% dplyr::rename(date ="timestamp(UTC)") %>% dplyr::select(
        date, 
        device_id,
        `NetADC_NO2-NO2(ppb)`,
        `NetADC_CO-CO( ppb)`,
        `NetADC_O3-O3(ppb)`,
        `NetCO2-HUM(%)`,
        `NetCO2-CO2(ppm)`,
        `NetCO2-TEMP(°C)`,
        `NetPM-PM25(µg/m3)`,
        `NetPM-PM10(µg/m3)`,
        `NetPid_P1-PPB(ppm)`,
        `NetPid_P1-TEMP(°C)`,
        `NetBME280-PERC(%)`,
        `NetBME280-TEMP_EXT(°C)`,
        `NetBME280-HPA(hPa)`,
        contains("NetPid_P3-PPB(ppm)"))

        print(data %>% head())

         names(data) <- c("date", "device_id", "NO2", "CO", "O3", "RH_CO2", "CO2", "temp_CO2", "PM02.5", "PM10", "VOC", "temp_VOC", "RH", "air_temp", "air_pressure", "VOC_P3")

    } else if (datasource == "device") {
        storage_path <- paste0(path, "Download\\")
        print(storage_path)
        f.pid <- list.files(storage_path,
            pattern = ".csv", full.names = T, recursive = F
        )
        print(f.pid)
        t.pid <- data.frame()
        for (f in f.pid) {
            temp <- read_delim(f, delim = ",", col_names = T, id = "device_id") %>% mutate(device_id = substr(basename(f), 1, 12))
            t.pid <- bind_rows(t.pid, temp)
        }
        data <- t.pid %>%
            mutate(date = as.POSIXct(strptime(`timestamp(UTC)`, "%Y-%m-%d %H:%M:%S", tz = "UTC"), tz = "UTC") -
                lubridate::hours(hours_difference_to_local_time)) %>% # adjusts to local time
            arrange(date) %>%
            dplyr::select(-`timestamp(UTC)`) %>%
            dplyr::select(date, device_id, `NetADC_NO2-NO2`, `NetADC_CO-CO`, `NetADC_O3-O3`, `NetCO2-HUM`, `NetCO2-CO2`, `NetCO2-TEMP`, `NetPM-PM25`, `NetPM-PM10`, `NetPid_P1-PPM`, `NetPid_P1-TEMP`, `NetBME280-PERC`, `NetBME280-TEMP_EXT`, `NetBME280-HPA`, contains("NetPid_P3-PPM"))
        # considered parameters date, RH, ext, temp., PM2.5, PM10, VOC, and CO2

        names(data) <- c("date", "device_id", "NO2", "CO", "O3", "RH_CO2", "CO2", "temp_CO2", "PM02.5", "PM10", "VOC", "temp_VOC", "RH", "air_temp", "air_pressure", "VOC_P3")
    }
    data_return <- data %>%
        dplyr::filter(between(date, as.POSIXct(start, tz = "UTC"), as.POSIXct(end, tz = "UTC"))) %>%
        mutate(
            VOC = VOC * 1000,
            VOC_P3 = VOC_P3 * 1000) ### !!!! ppb

    if (plot == TRUE) {
        for (id in unique(data_return$device_id)) {
            plt <- ggplot(data_return %>% gather(var, val, -date, -device_id) %>% filter(device_id == id)) +
                geom_line(aes(x = date, y = val, col = var)) +
                    facet_wrap(. ~ var, scales = "free", nrow = 4)+
                    theme_bw(16)
            ggsave(plot = plt, filename = paste0(".\\plt\\LS\\", id, "_overview_", timeidentifier, ".png"),
             width = 20 * 1.5, height = 15 * 1.5, units = "cm", dpi = 450)
            print(plt)
        }
    }

    return(data_return)
}

# function that gets the data from the airWINGS cloud via API key (one must have one) and polts the data if needed (plt=TRUE)
getWingsData <- function(start="2023-12-01",end="2023-12-02",type="i",id="17",plt = TRUE, api_key = "dummy"){

  #### type can be "i" for indoor sensors or "o" for outdoor sensors
  #indoor id = 16
  #outdoor id = 113
  
  # Package names
  packages <- c("httr", "jsonlite")
  # Install packages not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  if (type=="i"){
    json <- GET(paste0("https://airwings-europe.wings-ict-solutions.eu/api/realvaluesindoor?raw=true&device_id=",id,"&start=",start,"T00:00:00&stop=",end,"T00:00:00&timezone=UTC"),
                add_headers("x-api-key" = api_key),accept_json())
  }else if (type=="o"){
    json <- GET(paste0("https://airwings-europe.wings-ict-solutions.eu/api/realvalues?raw=true&device_id=",id,"&start=",start,"T00:00:00&stop=",end,"T00:00:00&timezone=UTC"),
                add_headers("x-api-key" = api_key),accept_json())
                print(json)
  }else{
    print("Please define type indoor/outdoor")
    break()
  }
  lookup_wings <- c(date = "date",VOC="TVOC",PM01 = "PM1",PM02.5 = "PM25", air_temp = "temperature", RH = "humidity", pressure = "atmospheric_pressure")
  
  tmp <- jsonlite::fromJSON(httr::content(json, "text")) # jsonobject to table
  # conversion of timestamp to proper date POSIX.ct format
  data <- tmp %>%
    mutate(date=as.POSIXct(strptime(timestamp,"%Y-%m-%d %H:%M:%S",tz="UTC"),tz="UTC")) %>% dplyr::select(-timestamp) %>% dplyr::relocate(date,.before=timezone) %>% rename(any_of(lookup_wings))
  
  print(data %>% head()) #print(data)
  
  #if plot boolean is activated data is plotted as is facted by variables (all devices at once)
  if (plt == TRUE) {
    plot <- ggplot(data %>% tidyr::gather(var, val, -date)) +
      geom_line(aes(x = date, y = val, col = var)) +
      facet_wrap(. ~ var, scales = "free",nrow=3)+
      theme_bw(16)+
      theme(legend.pos="bottom")
    print(plot)
    ggsave(plot = plot, paste0(".\\plt\\Wings\\",start, "-", end, "_", id,"_wings_", type, ".png"), width = 45, height = 20, units = "cm", dpi = "screen")
  }
  return(data%>%dplyr::select(-timezone))
 }

## function to get (and return) the data from the chemical gas sensors at TROPOS from the ASCII file
getChemGases <- function(file){
    data <- read_delim(file, delim="\t") %>% spread(var, val)
    return(data)
}

#Functions that get one air pollution parameter (component) from a UBA station

#Component:
#   "1" ~ "PM10"
#   "2" ~ "CO"
#   "3" ~ "O3",
#   "4" ~ "SO2",
#   "5" ~ "NO2"

# Scope 2: mean hourly mean

getUBAdata <- function(
    start = "2016-01-01",
    end = "2023-12-10",
    scope = "2",
    component = "1",
    station = "DESN025" # station codes on https://www.umweltbundesamt.de/daten/luft/luftdaten/stationen/eJzrXpScv9BwUXEykEhJXGVkYGSia2Cka2C-qCRzkaHxorzUBYuKSxYsSUl0K4LKGuoaGwL5IfnIqpMTJyzKrWJblJvctDgnseS0g-eqea8a5Y4vzslLP-2gcs7F4ZPFbAALoSvB
    ) {
    # Load the required packages
    packages <- c("tidyverse", "httr", "jsonlite")
    # Install packages not yet installed
    installed_packages <- packages %in% rownames(installed.packages())
    # Packages loading
    invisible(lapply(packages, library, character.only = TRUE))
    if (any(installed_packages == FALSE)) {
        install.packages(packages[!installed_packages])
    }

    json <- GET(
        paste0(
            "https://www.umweltbundesamt.de/api/air_data/v3/measures/json?date_from=", # API address call to get data from UBA (Umwelt Bundesamt /German Agency for Environment)
            start,
            "&date_to=",
            end,
            "&time_from=1&time_to=24&station=", station, "&component=",
            component,
            "&scope=",
            scope
        ),
        accept_json()
    )
    data <- jsonlite::fromJSON(content(json, type = "text"), flatten = TRUE)
    if (!is_empty(data$data)) {
        station <- as.character(data$request$station)
        df <- as.data.frame(matrix(unlist(data$data[[station]]), ncol = 5, byrow = T))
        result <- df %>%
            rename(date = "V4", value = "V3") %>%
            mutate(date = as.POSIXct(strptime(date, "%Y-%m-%d %H:%M:%S", tz = "UTC"), tz = "UTC") - 3600) %>%
            mutate(pol = case_match(
                V1, "1" ~ "PM10",
                "2" ~ "CO",
                "3" ~ "O3",
                "4" ~ "SO2",
                "5" ~ "NO2"
            )) %>%
            dplyr::select(date, pol, value) %>%
            mutate(value = as.numeric(value))
        return(result)
    }
}

#### Function that collects compenent 1 to 5 from UBA station (STATIONCODE; Default DESN025 - LPZ-Mitte)
all_data_uba_station <- function(station = "DESN025",begin = "2024-03-14", ends="2024-03-17"){
  for (i in c("1","2","3","4","5")){
  tmp <- getUBAdata(component = i,station=station,start = begin, end = ends) 
  if (i == "1"){
    air_pol_data <- tmp
  }else{
    air_pol_data <- bind_rows(air_pol_data,tmp)
  }
  
  }
  air_pol_data <- air_pol_data%>%spread(pol,value)%>%mutate(station=station)
  return(air_pol_data)
} 


# gets (and returns) the data (in specific period) of the climate chamber at the Leipzig Institute for Meteorology (LIM) from a list of files in the given folder delimiter is ";"
#and plots the quick look of the measured RH and temperature plus their setpoints 
getChamberData <- function(
    path = "data\\chamber_lim\\data",
    start = "2024-02-12",
    end = "2024-02-16",
    devicetype = "chamber",
    hours_difference_to_local_time = 0) {
    packages <- c("tidyverse", "lubridate", "openair", "httr", "jsonlite", "rlang")
    # Install packages not yet installed
    installed_packages <- packages %in% rownames(installed.packages())
    # Packages loading
    invisible(lapply(packages, library, character.only = TRUE))
    if (any(installed_packages == FALSE)) {
        install.packages(packages[!installed_packages])
    }

    files_chamber <- list.files(path,full.names = T)
    data_chamber <- NULL
    data <- data.table::rbindlist(lapply(files_chamber, FUN = function(x) bind_rows(data_chamber,read_delim(x,col_names=T,delim=";")))) %>%
    mutate(date=as.POSIXct(strptime(Datum,"%d.%m.%Y %H:%M:%S",tz="UTC"),tz="UTC")+lubridate::hours(-1*hours_difference_to_local_time)+lubridate::minutes(2)) %>% dplyr::select(-Datum)%>%
    dplyr::filter(between(as.Date(date),as.Date(start),as.Date(end)))
    output <- data %>% dplyr::select(date, contains(c("IW", "SW")),`Y Lüfter`) %>% dplyr::select(date,contains(c("Feuchte","Temp")),`Y Lüfter`)
    names(output) <- c("date","RH","SP_RH","air_temp","SP_T", "Lüfter")
    
    plt <- ggplot(data %>% dplyr::select(date, contains(c("IW", "SW")),`Y Lüfter`) %>% dplyr::select(date,contains(c("Feuchte","Temp")),`Y Lüfter`) %>%
    gather(var, val, -date) %>%
    mutate(
        param = if_else(str_detect(var, "Temp"), "Temperature",if_else(str_detect(var,"Lüfter"),"Lüfter","RH")),
        type = if_else(str_detect(var, "SW"), "Sollwert", "Istwert")
    )) +
    geom_line(aes(x = date, y = val, col = param,lty=type))+
    facet_wrap(.~param,scales="free",nrow=2)+
    theme_bw(16)
    print(plt)
    ggsave(plot = plt,paste0("plt\\",devicetype,"_",start,"-",end,".png"),width=20,height=15,units="cm",dpi="screen")
    return(output)
    }


### get Palas Data within a certain period (**start**, **end**) from Downloadfile but with VOC in ppb
getPalasData <- function(path = "data",
                         pattern = ".txt",
                         start = "2024-02-12 00:00:00",
                         end = "2024-02-14 00:00:00",
                         device_identifier = "Palas_13265",
                         plot = FALSE) {
    packages <- c("tidyverse", "lubridate", "openair", "httr", "jsonlite", "rlang")
    # Install packages not yet installed
    installed_packages <- packages %in% rownames(installed.packages())
    # Packages loading
    invisible(lapply(packages, library, character.only = TRUE))
    if (any(installed_packages == FALSE)) {
        install.packages(packages[!installed_packages])
    }
    # header of data frame
    header_aq_guard_dl <- c(
        "date",
        "averaging[s]",
        "PM01",
        "PM02.5",
        "PM04",
        "PM10",
        "PMtot",
        "CO2",
        "VOC", # [ppb]",
        "VOC_mgm3",
        "Cn [1/cm³]",
        "M1,0 [µm]",
        "M2,0 [µm²]",
        "M3,0 [µm³]",
        "x10(dCn) [µm]",
        "x16(dCn) [µm]",
        "x50(dCn) [µm]",
        "x84(dCn) [µm]",
        "x90(dCn) [µm]",
        "Cn [1/m³]",
        "PIA [1/m³]",
        "air_temp",
        "air_pressure",
        "RH",
        "AQI - Palas Infection Risk Index",
        "volume flow [l/min]",
        "power (volume flow blower) [%]",
        "dN(µm) [P]",
        "dummy",
        "0.184462",
        "0.198224",
        "0.213013",
        "0.228905",
        "0.245984",
        "0.264336",
        "0.284057",
        "0.30525",
        "0.328024",
        "0.352497",
        "0.378797",
        "0.407058",
        "0.437427",
        "0.470063",
        "0.505133",
        "0.54282",
        "0.583319",
        "0.626839",
        "0.673606",
        "0.723862",
        "0.777868",
        "0.835903",
        "0.898268",
        "0.965286",
        "1.037304",
        "1.114695",
        "1.19786",
        "1.28723",
        "1.383267",
        "1.48647",
        "1.597372",
        "1.716548",
        "1.844616",
        "1.982239",
        "2.13013",
        "2.289054",
        "2.459835",
        "2.643358",
        "2.840573",
        "3.052502",
        "3.280243",
        "3.524975",
        "3.787966",
        "4.070578",
        "4.374274",
        "4.700629",
        "5.051333",
        "5.428202",
        "5.833189",
        "6.26839",
        "6.736061",
        "7.238624",
        "7.778682",
        "8.359033",
        "8.982682",
        "9.652861",
        "10.373039",
        "11.146949",
        "11.978599",
        "12.872296",
        "13.83267",
        "14.864696",
        "15.973718",
        "17.165483"
    )
    # file list
    f_palas_dl <- list.files(
        path, 
        pattern = pattern, 
        full.names = T)

    data_palas_dl <- NULL

    # creates data frame from file list
    for (f in f_palas_dl) {
        tmp <- readr::read_delim(
            f, 
            trim_ws = T, 
            col_names = header_aq_guard_dl, 
            delim = "\t",
            na = "NaN",
            skip_empty_rows = T,
            skip = 1) %>%
            mutate(date = as.POSIXct(strptime(date, "%d.%m.%Y %H:%M:%S", tz = "UTC"), tz = "UTC"))
        data_palas_dl <- data_palas_dl %>%
        bind_rows(., tmp)
    }
    #filters data for time frame 
    data_palas_dl <- data_palas_dl %>%
        dplyr::filter(between(date, as.POSIXct(start, tz = "UTC"), as.POSIXct(end, tz = "UTC")))%>% 
        mutate(device_id = device_identifier) %>% 
        mutate(VOC_P3 = VOC) # to compare with other VOC sensor on other device second column with the name VOC_P3 with same data as in VOC
    
    # selects necessary columns
    plt <- data_palas_dl %>%
        as_tibble(.) %>%
        dplyr::select(date, contains("PM"), CO2, air_temp, RH, VOC, air_pressure)

    #plots data if needed
    if(plot == TRUE){
        fig <- ggplot(plt %>% gather(var, val, -date)) +
                geom_line(aes(x = date, y = val, col = var)) +
                facet_wrap(. ~ var, scales = "free", ncol = 4) +
                theme_bw(16) +
                theme(legend.pos = "bottom")
        ggsave(paste0(".\\plt\\Palas\\", strftime(start,"%Y%m%d-%H%M%S"), "_", strftime(end,"%Y%m%d-%H%M%S"), "_Palas_.png"), plot = fig, width = 40, height = 20, units = "cm", dpi = "screen")
        print(fig)
    }
    
    return(plt)
}

### get Palas Data from Downloadfile but with VOC in mg/m (analog to getPalasData())
getPalasData_VOCmg <- function(path = "data", #C:\\Users\\duesing\\Documents\\Kampagnen\\EDIAQI\\data\\Palas_AQguard\\aqGuard_13265\\2nd_period_EDIAQI_202312-202402\\Download\\aqGuard_13265_20231213\\" 
                         pattern = ".txt",
                         start = "2024-02-12 00:00:00",
                         end = "2024-02-14 00:00:00",
                         device_identifier = "Palas_13265",
                         plot = FALSE) {
    packages <- c("tidyverse", "lubridate", "openair", "httr", "jsonlite", "rlang")
    # Install packages not yet installed
    installed_packages <- packages %in% rownames(installed.packages())
    # Packages loading
    invisible(lapply(packages, library, character.only = TRUE))
    if (any(installed_packages == FALSE)) {
        install.packages(packages[!installed_packages])
    }
    header_aq_guard_dl <- c(
        "date",
        "averaging[s]",
        "PM01",
        "PM02.5",
        "PM04",
        "PM10",
        "PMtot",
        "CO2",
        "VOC", # [ppb]",
        "VOC_mgm3",
        "Cn [1/cm³]",
        "M1,0 [µm]",
        "M2,0 [µm²]",
        "M3,0 [µm³]",
        "x10(dCn) [µm]",
        "x16(dCn) [µm]",
        "x50(dCn) [µm]",
        "x84(dCn) [µm]",
        "x90(dCn) [µm]",
        "Cn [1/m³]",
        "PIA [1/m³]",
        "air_temp",
        "air_pressure",
        "RH",
        "AQI - Palas Infection Risk Index",
        "volume flow [l/min]",
        "power (volume flow blower) [%]",
        "dN(µm) [P]",
        "dummy",
        "0.184462",
        "0.198224",
        "0.213013",
        "0.228905",
        "0.245984",
        "0.264336",
        "0.284057",
        "0.30525",
        "0.328024",
        "0.352497",
        "0.378797",
        "0.407058",
        "0.437427",
        "0.470063",
        "0.505133",
        "0.54282",
        "0.583319",
        "0.626839",
        "0.673606",
        "0.723862",
        "0.777868",
        "0.835903",
        "0.898268",
        "0.965286",
        "1.037304",
        "1.114695",
        "1.19786",
        "1.28723",
        "1.383267",
        "1.48647",
        "1.597372",
        "1.716548",
        "1.844616",
        "1.982239",
        "2.13013",
        "2.289054",
        "2.459835",
        "2.643358",
        "2.840573",
        "3.052502",
        "3.280243",
        "3.524975",
        "3.787966",
        "4.070578",
        "4.374274",
        "4.700629",
        "5.051333",
        "5.428202",
        "5.833189",
        "6.26839",
        "6.736061",
        "7.238624",
        "7.778682",
        "8.359033",
        "8.982682",
        "9.652861",
        "10.373039",
        "11.146949",
        "11.978599",
        "12.872296",
        "13.83267",
        "14.864696",
        "15.973718",
        "17.165483"
    )
    f_palas_dl <- list.files(
        path, 
        pattern = pattern, 
        full.names = T)

    data_palas_dl <- NULL

    for (f in f_palas_dl) {
        tmp <- readr::read_delim(
            f, 
            trim_ws = T, 
            col_names = header_aq_guard_dl, 
            delim = "\t",
            na = "NaN",
            skip_empty_rows = T,
            skip = 1) %>%
            mutate(date = as.POSIXct(strptime(date, "%d.%m.%Y %H:%M:%S", tz = "UTC"), tz = "UTC"))
        data_palas_dl <- data_palas_dl %>%
        bind_rows(., tmp)
    }
    data_palas_dl <- data_palas_dl %>%
        dplyr::filter(between(date, as.POSIXct(start, tz = "UTC"), as.POSIXct(end, tz = "UTC")))%>% 
        mutate(device_id = device_identifier) %>% 
        mutate(VOC_P3 = VOC)
    
    
    plt <- data_palas_dl %>%
        as_tibble(.) %>%
        dplyr::select(date, contains("PM"), CO2, air_temp, RH, VOC_mgm3, air_pressure) %>% rename(VOC = "VOC_mgm3")# %>%
        # t_palas_dl_prc <- data_palas_dl%>%as_tibble(.)%>%select(date,contains("PM"),CO2,air_temp,RH,`VOC..ppb.`)%>%
        #openair::timeAverage(., avg.time = "5 min", start.date = lubridate::floor_date(.$date[1], "1 hour"))
    
    # plot of downloaded data with factes for each variable
    if(plot == TRUE){
        fig <- ggplot(plt %>% gather(var, val, -date)) +
                geom_line(aes(x = date, y = val, col = var)) +
                facet_wrap(. ~ var, scales = "free", ncol = 4) +
                theme_bw(16) +
                theme(legend.pos = "bottom")
        ggsave(paste0(".\\plt\\Palas\\", strftime(start,"%Y%m%d-%H%M%S"), "_", strftime(end,"%Y%m%d-%H%M%S"), "_Palas_.png"), plot = fig, width = 40, height = 20, units = "cm", dpi = "screen")
        print(fig)
    }
    
    return(plt)
}


# function that converts RH and temp. data stored in ascii files recorded by a HYT939 and a M5Stack into a data frame
# only works for a given data set data is stored in data\\Vilnius\\Indoor\\RH_T_Indoor_Vilnius\\ with ending **.log
# date  seconds of day  number of satellites (99 is dummy)  time [GPS UTC]  seconds runtime RH_mean T_mean  RH_median   T_median    number of observations
# [Fri Mar 15 15:45:18.944 2024] 53101.312	99	2024-03-15 14:45:00	53101.3	24.79	28.19	24.79	28.19	10.000000
getRHTeraTermData <- function(start, end) {
    #converts ascii files of a custom data logger to a table (especially taylored to the project)
    files_rh <- list.files("data\\Vilnius\\Indoor\\RH_T_Indoor_Vilnius\\", ".log")
    df <- NULL
    for (f in files_rh) {
        table_rh <- read_delim(file_rh, col_names = F) %>%
            mutate(date_chr = str_replace(sapply(str_split(X1, "_"), "[[", 1), "\\[", "")) %>%
            mutate(date = as.POSIXct(strptime(date_chr, format = "%a %b %d %H:%M:%OS %Y", tz = "UTC"), tz = "UTC")) %>%
            select(date, X5, X6, X7, X8, X9)
        names(table_rh) <- c("date", "RH_mean", "T_mean", "RH_med", "T_med", "n")
        df <- bind_rows(df, table_rh)
    }

    df <- df %>% filter(between(date, start, end))
   
    # prints data from ascii files  and saves the plot
    plt <- ggplot(df %>% select(-n) %>% gather(var, val, -date) %>% mutate(type = ifelse(str_detect(var, "RH"), "RH", "T"), stat = ifelse(str_detect(var, "med"), "Median", "Mean"))) +
        geom_line(aes(x = date, y = val, col = type, lty = stat)) +
        facet_wrap(. ~ type, nrow = 2, scales = "free") +
        theme_bw(16)
    print(plt)

    #Plot saved stored at given path
    ggsave(plot = plt, paste0(".\\plt\\RH_T_Indoor\\HYT_939_indoor_Vilnius_", start, "-", end, ".png"), width = 25, height = 15, units = "cm", dpi = "screen")
    returns(df)
}

# function to convert ASCII data of the Thinnect sensors recordings (in Excel spreadhseets) to a dataframe averaged to a desired average time (*avg.time*) for a certain period (**start**, **end**). 
# Plot of period **plot**=TRUE/FALSE can be provided. 
getThinnectData <- function(path = "data\\Thinnect", avg.time = "5 min", start = "2024-02-08", end = "2024-03-15", plot = FALSE) {
    
    # Thinnect sensor data was recorded via cloud and was sent from sensor partner as EXCEL spreadsheet. There was standard format - newer files could not work with this code
    # This functions reads those files in, concatenates the data and converts the date into proper data format. returns all the data within the period (start, end)
    # a direct download from cloud is not implememented 

    files_thinnect <- list.files(path, full.names = T)
    data <- NULL
    for (f in files_thinnect) {
        sheets <- readxl::excel_sheets(f)
        look_up <- readxl::read_excel(path = f, sheets[1], skip = 1, col_names = T, trim_ws = T) %>% tidyr::gather(var, tag, -`device type`, -`device guid`)
        for (s in sheets[-1]) {
            data <- bind_rows(data, readxl::read_excel(f, s, skip = 0, col_names = T, trim_ws = T, col_types = c("text", "text", "numeric", "text", "numeric", "text", "text")))
        }
    }

    ### data processing, correcting, and filtering (pretty messy and taylored to the data from the project)
    data <- data %>%
            dplyr::full_join(., look_up) %>%
            mutate(date = as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%OS", tz = "UTC"), tz = "UTC")) %>%
            rename(device_id = `device guid`) %>%
            mutate(type = ifelse(str_detect(`device_id`, "1C34"), "1C34", "70B3")) %>%
            mutate(key = paste0(var, "-", device_id)) %>%
            dplyr::select(date, var, value, device_id) %>%
            distinct(date, var, value, device_id) %>%
            spread(var, value) %>%
            openair::timeAverage(., avg.time = avg.time, start.date = lubridate::floor_date(.$date[1], "1 day"), type = "device_id") %>%
            rename(CO2 = "CO2R", RH = "RHR", air_temp = "TAR") %>%
            mutate(RH = ifelse(RH > 40000, NA, RH)) # filter missreads
    
    #if plot boolean is activated data is plotted as is and facted by variables (all devices at once)
    if (plot == TRUE) {
            plt <- ggplot(data %>% filter(dplyr::between(as.Date(date), as.Date(start), as.Date(end))) %>% tidyr::gather(var, val, -date, -device_id)) +
                geom_point(aes(x = date, y = val, col = `device_id`, group = `device_id`)) +
                ggplot2::facet_wrap(~var, scales = "free", nrow = 5) +
                theme_bw(16)
            print(plt)
            #saves plot to given path            
            ggsave(plot = plt, filename = paste0(".\\plt\\Thinnect\\Thinnect_overview_", start, "-", end, ".png"),
             width = 20*1.5, height = 15*1.5, units = "cm", dpi = 450)
        }
    return(data)
}
