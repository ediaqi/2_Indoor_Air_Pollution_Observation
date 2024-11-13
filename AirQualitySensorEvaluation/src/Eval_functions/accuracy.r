# function that estimates the accuracy (normalised root mean square error in %) of a group of devices with respect to a given/known parameter

accuracy <- function(data = test_data, # which data to test
                     start = "2023-02-07 11:00:00", 
                     end = "2023-02-12 12:00:00", # start end of test period
                     reference = reference_data, # which test data
                     parameter = "CO2", # which parameter to test
                     avg.time = "5 min", # goven average time
                     devicetype = "device_identifier", # here one can easily change for different evaluations depending on the test data input since it is the foldername for the plot
                     reference_device = "reference_device", # what device is used with reference data (expands file name)
                     plot = TRUE) { # shall the plots be saved y/n (TRUE/FALSE; default TRUE)
    packages <- c("tidyverse", "lubridate", "openair", "httr", "jsonlite", "rlang", "viridis")
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

    id_list <- unique(data$device_id)
    # check if parameter is content of data
    if (!parameter %in% colnames(data)){
        print(paste0("Test data does not contain", parameter))
        break
    }
    # check if parameter is content of refrence data
    if (!parameter %in% colnames(reference)){
        print(paste0("Reference data does not contain" ,parameter))
        break
    }
    # check if date is content of test data
    if (!("date" %in% colnames(data))){
        print(paste0("Test data does not contain date"))
        break
    }
    # check if date is content of reference data
    if (!("date" %in% colnames(reference))){
        print(paste0("Reference data does not contain date"))
        break
    }

    data_tmp <- data %>%
        dplyr::select(date, device_id, !!sym(parameter)) %>%
        mutate(device_id = as.character(device_id)) %>% 
        openair::timeAverage(., 
        avg.time = avg.time, 
        start.date = lubridate::round_date(.$date[1], "1 day") - lubridate::days(1), 
        type = "device_id") %>% # averages to given average time starts at 00 O'clock of the previous day with respect to the first timestamp of the data
        group_by(date) %>% # group per observation
        drop_na() %>% # remove na readings
        mutate(n = n()) %>% # number per group
        filter(n >= 3) %>% # at least 3 devices per observation
        dplyr::select(-n) # remove number
    
    #print(data_tmp)
    print(data_tmp %>% summary()) # prints data summary

    reference_table <- reference %>%
        openair::timeAverage(., 
        avg.time = avg.time, 
        start.date = round_date(.$date[1], "1 day") - lubridate::days(1)) # averages analog to test data the data of reference to gieven average period

    #print(reference_table) # prints data
    print(reference_table %>% summary()) # prints a quick summary of the reference table
    
    #joins both data sets
    data_accuracy <- data_tmp %>%
        full_join(., reference_table, by = "date", suffix = c(".test", ".reference")) %>%
        dplyr::select(device_id, date, contains(".test"), contains(".reference"))%>%
        drop_na() %>%
        mutate(datum = as.Date(date)) %>%
        dplyr::filter(between(date, as.POSIXct(start, tz = "UTC"), as.POSIXct(end, tz = "UTC"))) %>%
        dplyr::select(-datum) %>%
        mutate(dev = (!!sym(paste0(parameter, ".test")) - !!sym(paste0(parameter, ".reference")))^2) %>% # calculates squared deviation per observation between reference and test device 
        group_by(device_id)
    
    print(data_accuracy)
    # print(data_accuracy)
    # data_accuracy %>%
    #     bind_rows(head(.), tail(.)) %>%
    #     print(.)

    result <- data_accuracy %>%
        mutate(n = n()) %>% # number of observations
        arrange(date) %>% # sort by date
        ungroup() %>%
        reframe(nrmse = sqrt(1 / (n * length(id_list)) * sum(dev)) / mean(!!sym(paste0(parameter, ".reference"))) * 100) %>% #calculation of RMSE
        slice(1) %>% # only first line needed
        as.numeric(.)
    
    # Plotting the result    
    if (plot == TRUE) {
        plt <- ggplot() +
        geom_line(data = reference_table %>%
            dplyr::filter(between(date, as.POSIXct(start, tz = "UTC"), as.POSIXct(end, tz = "UTC"))), aes(x = date, y = !!sym(paste0(parameter)), col = "Reference")) +
        geom_line(data = data_tmp %>%
            dplyr::filter(between(date, as.POSIXct(start, tz = "UTC"), as.POSIXct(end, tz = "UTC"))), aes(x = date, y = !!sym(paste0(parameter)), col = device_id)) +
        geom_ribbon(data = reference_table %>%
            dplyr::filter(between(date, as.POSIXct(start, tz = "UTC"), as.POSIXct(end, tz = "UTC"))), aes(x = date, ymin = !!sym(paste0(parameter)) * (1 - result / 100), ymax = !!sym(paste0(parameter)) * (1 + result / 100), col = "Reference"), fill = "gray", alpha = 0.5) +
        theme_bw(16) +
        scale_colour_manual(breaks = c(id_list, "Reference"), values = c(rainbow(length(id_list)), "black")) +
        xlim(range(data_accuracy$date)[1], range(data_accuracy$date)[2]) +
        ggtitle(paste0("Accuracy is: ", signif(result, 3), " % at ", avg.time, " average period")) +
        labs(col = "Device") +
        theme(legend.pos = "bottom") # displays data accuracy in a plot
        
        ggsave(
            plot = plt, paste0(
                ".\\plt\\", devicetype, "\\Accuracy\\", parameter, "_",
                timeidentifier, "_",
                reference_device, ".png"
            ),
            width = 30, height = 20, units = "cm", dpi = 450
        )
    }
    print(plt)
    print(paste0("Parameter ", parameter, " has a accuracy of:", signif(result, 3), "%."))
    return(result)
}
