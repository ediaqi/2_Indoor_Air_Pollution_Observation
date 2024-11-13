bias_by_RH <- function(data = data,
                               start = "2023-02-08",
                               end = "2023-02-12",
                               reference = palas_reference, # dataframe containing datetime and reference with same time resolution as device under test
                               parameter = "CO2",
                               avg.time = "1 hour",
                               devicetype = "LS",
                               reference_device = "Palas") {
    packages <- c("tidyverse", "lubridate", "openair", "httr", "jsonlite", "rlang", "cowplot", "broom", "rlang")
    # Install packages not yet installed
    installed_packages <- packages %in% rownames(installed.packages())
    # Packages loading
    invisible(lapply(packages, library, character.only = TRUE))
    if (any(installed_packages == FALSE)) {
        install.packages(packages[!installed_packages])
    }
    #preperation of test data (filtering and mean)
    data_tmp <- data %>% # nolint: object_usage_linter.
        dplyr::select(date, device_id, !!sym(parameter),RH) %>%
        mutate(device_id = as.character(device_id)) %>%
        group_by(device_id) %>%
        openair::timeAverage(., avg.time = avg.time, start.date = round_date(.$date[1], "1 day") - days(1), type = "device_id")
    
    # prep of rerence data (average)
    reference_table <- reference %>%
        openair::timeAverage(., avg.time = avg.time, start.date = round_date(.$date[1], "1 day") - days(1))
    
    # #data filtering and joining
    data_accuracy <- data_tmp %>%
        full_join(., reference_table, by = "date", suffix = c(".test", ".reference")) %>%
        drop_na() %>%
        group_by(device_id) %>%
        dplyr::filter(between(date, as.POSIXct(paste0(start, " 00:00:00 UTC"), tz = "UTC"), as.POSIXct(paste0(end, " 23:59:59 UTC"), tz = "UTC"))) %>%
        drop_na() %>%
        ungroup() %>%
        droplevels(.$device_id)

    # selects needed columns and calulated difference between device and reference
    data_base <- data_accuracy %>% 
    dplyr::select(
        date, 
        device_id, 
        !!sym(paste0(parameter, ".reference")), 
        !!sym(paste0(parameter, ".test")), RH.test) %>% 
        mutate(bias = !!sym(paste0(parameter, ".test"))-!!sym(paste0(parameter, ".reference")))
    #plots RH of test device (y-axis) against bias (x-axis)
    plt_a <- ggplot(data_base) +
        geom_abline(slope = 0, intercept = 0, col = "#2e2e2e", lwd = 1.1, lty = 2) +
        geom_point(aes(y = bias, x = RH.test, col = device_id)) +
        geom_smooth(aes(y = bias, x = RH.test, col = device_id),method="loess")+
        theme_bw(16)  +
        ggtitle(lookup[parameter]) +
        theme(legend.pos = "bottom") +
        labs(x = "RH in %", y = "Bias (DUT - Reference)") + 
        scale_color_manual(values=c("red4","darkgreen","blue4")) +
        theme(aspect.ratio = 1) #+ scale_color_gradient2(midpoint = 40, low="#1742ff", high="#ff2222")

    print(plt_a)
    #saves plot in given path
    ggsave(paste0("plt\\", devicetype, "\\Bias_with_RH\\", parameter, "_", start, "-", end, "_", reference_device, ".png"), plot = plt_a, width = 40, height = 20, units = "cm", dpi = "screen")
    # print(head(data_accuracy))
    
}
