#Functions to evaluate performance of device with linear model, estimates breakpoints and summarises performance for all parameters

# functions calculates parameters of linear model for each unit: R², r, slope, intercept, RMSE, nRMSE for data averaged to averging time within a time frame of interest (start, end) and a parameter of interest (default CO2)
performance <- function(data = data,
                        start = "2023-02-08 00:00:00 UTC",
                        end = "2023-02-12 00:00:00 UTC",
                        reference, # dataframe containing datetime and reference with same time resolution as device under test
                        parameter = "CO2",
                        avg.time = "1 hour",
                        devicetype = "LS") {
    packages <- c("tidyverse", "lubridate", "openair", "httr", "jsonlite", "rlang", "cowplot", "broom")
    # Install packages not yet installed
    installed_packages <- packages %in% rownames(installed.packages())
    # Packages loading
    invisible(lapply(packages, library, character.only = TRUE))
    if (any(installed_packages == FALSE)) {
        install.packages(packages[!installed_packages])
    }
    # prepares test data
    data_tmp <- data %>%
        dplyr::select(date, device_id, !!sym(parameter)) %>%
        mutate(device_id = as.character(device_id)) %>%
        group_by(device_id) %>%
        openair::timeAverage(., avg.time = avg.time, start.date = round_date(.$date[1], "1 day") - days(1), type = "device_id") #averaging
    
    #prepares reference data
    reference_table <- reference %>%
        openair::timeAverage(., avg.time = avg.time, start.date = round_date(.$date[1], "1 day") - days(1)) #averaging
    
    #joins both adat sets along time stamp
    data_accuracy <- data_tmp %>%
        left_join(., reference_table, by = "date", suffix = c(".test", ".reference")) %>%
        dplyr::select(device_id, date, contains(".test"),contains(".reference"))%>%
        drop_na() %>%
        group_by(device_id) %>%
        dplyr::filter(between(date, as.POSIXct(start, tz = "UTC"), as.POSIXct(end, tz = "UTC")))
    
    print(data_accuracy%>%summary(.))
    
    # applies linear model per unit of device
    mod_params <- data_accuracy %>%
        nest(data = -device_id) %>%
        mutate(model = map(data, ~ lm(!!sym(paste0(parameter, ".test")) ~ !!sym(paste0(parameter, ".reference")), data = .)), tidied = map(model, tidy)) %>%
        unnest(tidied)
   
    #gets model stats
    mod_stats <- data_accuracy %>%
        nest(data = -device_id) %>%
        mutate(model = map(data, ~ lm(!!sym(paste0(parameter, ".test")) ~ !!sym(paste0(parameter, ".reference")), data = .)), tidied = map(model, glance)) %>%
        unnest(tidied)
   
    #prints model parameters
    print(mod_params)
    
    #extracts slope of linear models
    slopes <- mod_params %>%
        dplyr::filter(term == paste0(parameter, ".reference")) %>%
        dplyr::select(device_id, estimate) %>%
        rename(., slope = estimate)
    
    #extracts intercept of linear model
    intercepts <- mod_params %>%
        dplyr::filter(term == "(Intercept)") %>%
        dplyr::select(device_id, estimate) %>%
        rename(., intercept = estimate)
    
    #extracts R² of linear models
    r2 <- mod_stats %>%
        dplyr::select(device_id, r.squared) %>%
        rename(., r2 = r.squared)

    # calculates RMSE, nRMSE and pearosn correlation coefficent of each hunit
    RMSE <- data_accuracy %>%
        mutate(sq.residual = (!!sym(paste0(parameter, ".test")) - !!sym(paste0(parameter, ".reference")))^2) %>%
        group_by(device_id) %>%
        summarize(
            RMSE = sqrt(mean(sq.residual, na.rm = T)),
            NRMSE = sqrt(mean(sq.residual, na.rm = T)) / mean(!!sym(paste0(parameter, ".reference")), na.rm = T) * 100,
            pearson_r = cor(!!sym(paste0(parameter, ".test")), !!sym(paste0(parameter, ".reference")))
        )

    #combines all the statistical parameters of linear model
    performance <- left_join(r2, slopes, by = "device_id") %>%
        left_join(., intercepts, by = "device_id") %>%
        left_join(., RMSE, by = "device_id") %>%
        mutate(parameter = parameter)
    print(performance)
    return(performance)
}

### function that loops over the parameters whcih have test data and reference data in common

summary_performance <- function(df_test, df_ref, devicetype, referencetype, start, end, avg.time) {
    parameters <- match_parameters(df_ref, df_test %>% dplyr::select(-device_id)) # finds macthing IAQ parameters of both data sets (match_parameters() from "src\\getParameterListOfSensors.R")
    dataframe <- NULL
    for (p in parameters) {
        print(paste0("Parameter: ", p))
        dataframe <- bind_rows(dataframe, performance(df_test, reference = df_ref, parameter = p, start = start, end = end, avg.time = avg.time, devicetype = devicetype))
    }
    if (!dir.exists(file.path("output", devicetype, "performance"))) {
        dir.create(file.path("output", devicetype, "performance"), recursive = T)
    }
    # writes result in data frame and saves the table in a file 
    write.table(
        x = dataframe %>% mutate(reference_type = referencetype, avg_period = avg.time, start = start, end = end) %>% forestmangr::round_df(., 3, "signif"),
        file = paste0("output\\", devicetype, "\\performance\\", devicetype, "_", referencetype, "_", strftime(start, format = "%Y%m%d_%H%M%S"), "-", strftime(end, format = "%Y%m%d_%H%M%S"), "_", gsub(" ", "", avg.time), ".dat"), # path to store the dataframe of the result
        append = F, quote = F, na = "", sep = "\t", col.names = T, row.names = F,
    ) # nolint
    return(dataframe %>% mutate(reference_type = referencetype, avg_period = avg.time)) #returns and ads averaging period and reference device type to data frame
}

# function that creates a segmented linear model by infering the breakpoint of the linear model
performance_segmented <- function(data = data,
                                  start = "2023-02-08 00:00:00 UTC",
                                  end = "2023-02-12 00:00:00 UTC",
                                  reference, # dataframe containing datetime and reference with same time resolution as device under test
                                  parameter = "CO2",
                                  avg.time = "1 hour",
                                  devicetype = "LS") {
    packages <- c("tidyverse", "lubridate", "openair", "httr", "jsonlite", "rlang", "cowplot", "broom")
    # Install packages not yet installed
    installed_packages <- packages %in% rownames(installed.packages())
    # Packages loading
    invisible(lapply(packages, library, character.only = TRUE))
    if (any(installed_packages == FALSE)) {
        install.packages(packages[!installed_packages])
    }

    #prepares test data
    data_tmp <- data %>%
        dplyr::select(date, device_id, !!sym(parameter)) %>%
        mutate(device_id = as.character(device_id)) %>%
        group_by(device_id) %>%
        openair::timeAverage(., avg.time = avg.time, start.date = round_date(.$date[1], "1 day") - days(1), type = "device_id")
    
    #prepares reference data
    reference_table <- reference %>%
        openair::timeAverage(., avg.time = avg.time, start.date = round_date(.$date[1], "1 day") - days(1))

    #joins test data and reference data
    data_accuracy <- data_tmp %>%
        left_join(., reference_table, by = "date", suffix = c(".test", ".reference")) %>%
        drop_na() %>%
        dplyr::filter(between(date, as.POSIXct(start, tz = "UTC"), as.POSIXct(end, tz = "UTC")))

    # creates table containg test parameter and reference parameter measurements
    dat <- data_accuracy %>%
        dplyr::select(date, device_id, !!sym(paste0(parameter, ".reference")), !!sym(paste0(parameter, ".test"))) %>%
        spread(device_id, !!sym(paste0(parameter, ".test"))) %>%
        dplyr::select(-date) %>%
        rename(x = !!sym(paste0(parameter, ".reference")))

    model.results <- setNames(names(dat[, -1]), names(dat[, -1])) %>%
        map(~ lm(paste0(.x, " ~ x"), data = dat) %>%
            segmented::segmented(seg.Z = ~x) %>% #cals segmented function
            list(
                model = tidy(.),
                psi = data.frame(term = "breakpoint", estimate = .$psi[, 2]) # extracts breakboint
            )) %>%
        map_df(~ .[2:3] %>% bind_rows(), .id = "device_id")


    print(model.results) #prints model
    return(model.results) #returns model
}
