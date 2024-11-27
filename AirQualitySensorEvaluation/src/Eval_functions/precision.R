# function to calculate the precison of a given sensor type based. 
#It's calculated based on the coeffcient of variation, the stndard deviation of all devices (at least 3), 
# normalised with the mean of the time series within the given period.
precision <- function(testdata = data,
                      start = "2023-02-07",
                      end = "2023-02-12",
                      parameter = "CO2",
                      avg.time = "10 min",
                      devicetype = "LS") {
    packages <- c("tidyverse", "lubridate", "openair", "httr", "jsonlite", "rlang", "RColorBrewer")
    # Install packages not yet installed
    installed_packages <- packages %in% rownames(installed.packages())
    # Packages loading
    invisible(lapply(packages, library, character.only = TRUE))
    if (any(installed_packages == FALSE)) {
        install.packages(packages[!installed_packages], chooseCRANmirror(ind = 37))
    }
    # preparation of test data (averaging and filter data points with a least three prallel observations)
    data_tmp <- testdata %>%
        # dplyr::filter(between(date, as.POSIXct(paste0(start, " 00:00:00 UTC"), tz = "UTC"), as.POSIXct(paste0(end, " 23:59:59 UTC"), tz = "UTC")))
        dplyr::filter(between(date, as.POSIXct(start, tz = "UTC"), as.POSIXct(end, tz = "UTC"))) %>%
        dplyr::select(date, device_id, !!sym(parameter)) %>%
        mutate(device_id = as.character(device_id)) %>%
        openair::timeAverage(., avg.time = avg.time, start.date = round_date(.$date[1], "days") - days(1), type = "device_id") %>%
        group_by(date) %>%
        drop_na() %>%
        mutate(n = sum(!is.na(!!sym(parameter)))) %>%
        dplyr::filter(n >= 3) %>%
        dplyr::select(-n)

    # list all the device_id of the data set
    id_list <- unique(data_tmp$device_id)
    # print(data_tmp)

    # process data (filtering, calculating mean the squared deviation agianst the mean)
    data_prc <- data_tmp %>%
        # dplyr::filter(between(date, as.POSIXct(paste0(start, " 00:00:00 UTC"), tz = "UTC"), as.POSIXct(paste0(end, " 23:59:59 UTC"), tz = "UTC")))
        dplyr::filter(between(date, as.POSIXct(start, tz = "UTC"), as.POSIXct(end, tz = "UTC"))) %>%
        full_join(
            data_tmp %>%
                group_by(date) %>%
                summarise(mean = mean(!!sym(parameter))),
            by = "date"
        ) %>%
        drop_na() %>%
        mutate(dev = (!!sym(parameter) - mean)^2) %>%
        group_by(device_id) %>%
        mutate(n = n()) %>%
        ungroup()
    #print(data_prc %>% arrange(date))
    
    # mean per data point (all devices)
    mean <- data_prc %>%
        group_by(date) %>%
        mutate(mean = mean(!!sym(parameter)))
    
    # coefficient of variation 
    result <- data_prc %>%
        reframe(cv = sqrt(1 / (n * length(id_list) - 1) * sum(dev)) / mean(!!sym(parameter)) * 100) %>%
        slice(1) %>%
        as.numeric(.)

    plt <- ggplot(data_prc) +
        geom_line(aes(x = date, y = !!sym(parameter), col = device_id)) +
        geom_point(data = mean, aes(x = date, y = mean, col = "mean")) +
        geom_line(data = mean, aes(x = date, y = mean, col = "mean")) +
        geom_ribbon(data = mean, aes(x = date, ymin = mean * (1 - result / 100), ymax = mean * (1 + result / 100), fill = "mean"), fill = "darkgray", alpha = .5) +
        ggtitle(paste0("Precision is: ", signif(result, 3), "% at an average time of ",avg.time)) +
        theme_bw(16) + 
        #theme(legend.pos = "bottom") +
        scale_color_manual("", values = c(rainbow(length(id_list)), "#444444"))
        

    print(plt)
    ggsave(plot = plt, paste0(".\\plt\\", devicetype, "\\Precision\\", parameter, "_", strftime(start, format = "%Y%m%d_%H%M%S"), "-", strftime(end, format = "%Y%m%d_%H%M%S"), "_", avg.time, ".png"), width = 25, height = 15, units = "cm", dpi = 450)
    print(paste0("Parameter ", parameter, " has a precision of: ", signif(result, 3), " %."))
    return(result)
}
