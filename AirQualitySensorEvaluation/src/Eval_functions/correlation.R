# function that plots the correlation 
# (linear model of test to reference with slope, intercept as scatter plot and time series)

correlation <- function(test_data = data,
                        start = "2023-02-08 00:00:00",
                        end = "2023-02-12 00:00:00",
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
    
    start_str <- strftime(start, format = "%Y%m%d_%H%M%S") #str of start time       
    end_str <- strftime(end, format = "%Y%m%d_%H%M%S") #string of end time

    timeidentifier <- paste0(start_str, "_", end_str)# pastes both
    
    id_list <- unique(test_data$device_id) # makes device id list
    
    #prepares test data
    data_tmp <- test_data %>% 
        dplyr::select(date, device_id, !!sym(parameter)) %>%
        mutate(device_id = as.character(device_id)) %>%
        group_by(device_id) %>%
        openair::timeAverage(.,
            avg.time = avg.time,
            start.date = round_date(.$date[1], "1 day") - days(1), type = "device_id"
        )
    
    #prepares reference data
    reference_table <- reference %>%
        openair::timeAverage(., avg.time = avg.time, start.date = round_date(.$date[1], "1 day") - days(1)) %>%
        dplyr::filter(between(date, as.POSIXct(start, tz = "UTC"), as.POSIXct(end, tz = "UTC")))
    
    # joins both data sets
    data_accuracy <- data_tmp %>%
        full_join(., reference_table, by = "date", suffix = c(".test", ".reference")) %>%
        dplyr::select(device_id, date, contains(".test"),contains(".reference")) %>%
        drop_na() %>%
        group_by(device_id) %>%
        dplyr::filter(between(date, as.POSIXct(start, tz = "UTC"), as.POSIXct(end, tz = "UTC")))

    # calculates performance (to get linear models of test and reference data)
    perf <- performance(
        data = test_data,
        reference = reference,
        parameter = parameter,
        start = start,
        end = end,
        avg.time = avg.time
    )

    #crates correlation plot (colored by device)
    plt_a <- ggplot(
        data_accuracy %>%
            dplyr::select(
                date,
                device_id,
                !!sym(paste0(parameter, ".reference")),
                !!sym(paste0(parameter, ".test"))
            ) %>%
            droplevels() %>%
            mutate(device_id = as.character(device_id))
    ) +
        geom_abline(
            data = perf %>%
                bind_rows(data.frame(slope = 1, intercept = 0, device_id = "1:1")) %>%
                droplevels(),
            aes(
                slope = slope,
                intercept = intercept,
                col = device_id
            ),
            lwd = 1.5
        ) +
        geom_point(
            aes(
                y = !!sym(paste0(parameter, ".test")),
                x = !!sym(paste0(parameter, ".reference")),
                col = device_id
            ),
            shape = 1
        ) +
        theme_bw(16) +
        ggtitle(lookup[parameter]) +
        labs(x = "Reference", y = "Device under test")  +
        scale_color_manual(
            values = c("black", "black", rainbow(length(id_list))),
            breaks = c("Reference", "1:1", id_list)
        ) +
        labs(
            col = element_blank(),
            lty = element_blank(),
            x = "Reference",
            y = "Device under test"
        ) + theme(legend.position = "bottom", legend.direction="vertical") # + theme(aspect.ratio=1)
    
    #plot for time series of refrence and test data (colored by ids)
    plt_b <- ggplot() +
        geom_line(
            data = data_accuracy %>%
                dplyr::select(
                    date,
                    device_id,
                    !!sym(paste0(parameter, ".test"))
                ) %>%
                gather(var, val, -device_id, -date) %>%
                drop_na(),
            aes(x = date, y = val, col = device_id)
        ) +
        geom_point(
            data = data_accuracy %>%
                dplyr::select(
                    date,
                    device_id,
                    !!sym(paste0(parameter, ".test"))
                ) %>%
                gather(var, val, -device_id, -date) %>%
                drop_na(),
            aes(x = date, y = val, col = device_id)
        )+
        geom_line(
            data = reference_table %>%
                dplyr::select(date, !!sym(parameter)) %>%
                rename(val = !!sym(parameter)) %>%
                mutate(device_id = "Reference") %>%
                drop_na(),
            aes(x = date, y = val, col = device_id)
        ) +
        geom_point(
            data = reference_table %>%
                dplyr::select(date, !!sym(parameter)) %>%
                rename(val = !!sym(parameter)) %>%
                mutate(device_id = "Reference") %>%
                drop_na(),
            aes(x = date, y = val, col = device_id)
        )+
        theme_bw(16) +
        scale_color_manual(values = c("black", "black", rainbow(length(id_list))), breaks = c("Reference", "1:1", id_list)) +
        labs(col = element_blank(), x = "datetime", y = element_blank()) + theme(legend.position = "bottom")+theme(legend.direction="vertical")

    #combines both blots
    plt_c <- cowplot::plot_grid(plt_a, plt_b, align = "h", rel_heights = c(1, 1))
    print(plt_c) # displays combined plots
    # saves combined plots 
    ggsave(
        paste0("plt\\", devicetype, "\\Correlation\\", parameter, "_", timeidentifier, "_", reference_device, "_", gsub(" ", "", avg.time), ".png"), # filenames were data is stored
    plot = plt_c, width = 40, height = 20, units = "cm", dpi = "screen")
    # print(head(data_accuracy))
    
    # calculates correaltion of both data sets 
    result <- data_accuracy %>%
        reframe(corr = cor(!!sym(paste0(parameter, ".reference")), !!sym(paste0(parameter, ".test")), method = "pearson")) # %>%
    # slice(3) %>%
    # as.numeric(.)
    print(result) # prints correlation per device unit
    return(result) #returns the correlation (pearson r)
}

# basically the same as correlation function but faceted by device_id and data points colored based on reference RH
correlation_with_RH <- function(data = data,
                                start = "2023-02-08 00:00:00",
                                end = "2023-02-12 00:00:00",
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
    
    start_str <- strftime(start, format = "%Y%m%d_%H%M%S")
    end_str <- strftime(end, format = "%Y%m%d_%H%M%S")

    timeidentifier <- paste0(start_str, "_", end_str)

    data_tmp <- data %>% # nolint: object_usage_linter.
        dplyr::select(date, device_id, !!sym(parameter)) %>%
        mutate(device_id = as.character(device_id)) %>%
        group_by(device_id) %>%
        openair::timeAverage(., avg.time = avg.time, start.date = round_date(.$date[1], "1 day") - days(1), type = "device_id")

    reference_table <- reference %>%
        openair::timeAverage(., avg.time = avg.time, start.date = round_date(.$date[1], "1 day") - days(1))

    data_accuracy <- data_tmp %>%
        full_join(., reference_table, by = "date", suffix = c(".test", ".reference")) %>%
        drop_na() %>%
        group_by(device_id) %>%
        dplyr::filter(between(date, as.POSIXct(start, tz = "UTC"), as.POSIXct(end, tz = "UTC"))) %>%
        drop_na() %>%
        ungroup() %>%
        droplevels(.$device_id)


    perf <- performance(data = data, reference = reference, parameter = parameter, start = start, end = end, avg.time = avg.time)

    plt_a <- ggplot(data_accuracy %>% dplyr::select(date, device_id, !!sym(paste0(parameter, ".reference")), !!sym(paste0(parameter, ".test")), RH)) +
        geom_abline(data = perf %>% droplevels(), aes(slope = slope, intercept = intercept), col = "#000000", lwd = 1.1) +
        geom_abline(slope = 1, intercept = 0, col = "#2e2e2e", lwd = 1.1, lty = 2) +
        geom_point(aes(y = !!sym(paste0(parameter, ".test")), x = !!sym(paste0(parameter, ".reference")), col = RH)) +
        theme_bw(16) +
        theme(panel.background = element_rect(fill = "gray")) +
        ggtitle(lookup[parameter]) +
        labs(x = "Reference", y = "Device under test") +
        theme(legend.pos = "bottom") +
        facet_wrap(. ~ device_id, ncol = 3) +
        scale_color_gradientn("RH [%]", limits = c(0, 100), colours = c("red", "white", "blue")) +
        theme(aspect.ratio = 1) #+ scale_color_gradient2(midpoint = 40, low="#1742ff", high="#ff2222")

    print(plt_a)
    ggsave(paste0("plt\\", devicetype, "\\Correlation\\", parameter, "_", timeidentifier, "_", reference_device, "_withRH.png"), plot = plt_a, width = 40, height = 20, units = "cm", dpi = "screen")
}

# basically the same as correlation function but faceted by device_id and data points colored based on reference temperature
correlation_with_T <- function(data = data,
                               start = "2023-02-08 00:00:00",
                               end = "2023-02-12 00:00:00",
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
    
    start_str <- strftime(start, format = "%Y%m%d_%H%M%S")
    end_str <- strftime(end, format = "%Y%m%d_%H%M%S")
    
    timeidentifier <- paste0(start_str, "_", end_str)
    
    data_tmp <- data %>% # nolint: object_usage_linter.
        dplyr::select(date, device_id, !!sym(parameter)) %>%
        mutate(device_id = as.character(device_id)) %>%
        group_by(device_id) %>%
        openair::timeAverage(., avg.time = avg.time, start.date = round_date(.$date[1], "1 day") - days(1), type = "device_id")

    reference_table <- reference %>%
        openair::timeAverage(., avg.time = avg.time, start.date = round_date(.$date[1], "1 day") - days(1))

    data_accuracy <- data_tmp %>%
        full_join(., reference_table, by = "date", suffix = c(".test", ".reference")) %>%
        drop_na() %>%
        group_by(device_id) %>%
        dplyr::filter(between(date, as.POSIXct(start, tz = "UTC"), as.POSIXct(end, tz = "UTC"))) %>%
        drop_na() %>%
        ungroup() %>%
        droplevels(.$device_id)


    perf <- performance(data = data, reference = reference, parameter = parameter, start = start, end = end, avg.time = avg.time)
    data_base <- data_accuracy %>% dplyr::select(date, device_id, !!sym(paste0(parameter, ".reference")), !!sym(paste0(parameter, ".test")), air_temp)
    plt_a <- ggplot(data_base) +
        geom_abline(data = perf %>% droplevels(), aes(slope = slope, intercept = intercept), col = "#000000", lwd = 1.1) +
        geom_abline(slope = 1, intercept = 0, col = "#2e2e2e", lwd = 1.1, lty = 2) +
        geom_point(aes(y = !!sym(paste0(parameter, ".test")), x = !!sym(paste0(parameter, ".reference")), col = air_temp)) +
        theme_bw(16) +
        theme(panel.background = element_rect(fill = "gray")) +
        ggtitle(lookup[parameter]) +
        theme(legend.pos = "bottom") +
        coord_equal() +
        facet_wrap(. ~ device_id, ncol = 3) +
        labs(x = "Reference", y = "Device under test") +
        scale_color_gradientn("T [°C]", limits = c(10, 45), colours = c("blue", "white", "red")) +
        theme(aspect.ratio = 1) #+ scale_color_gradient2(midpoint = 40, low="#1742ff", high="#ff2222")

    print(plt_a)
    ggsave(paste0("plt\\", devicetype, "\\Correlation\\", parameter, "_", timeidentifier, "_", reference_device, "_withT.png"), plot = plt_a, width = 40, height = 20, units = "cm", dpi = "screen")
    # print(head(data_accuracy))
    result <- data_accuracy %>%
        reframe(corr = cor(!!sym(paste0(parameter, ".reference")), !!sym(paste0(parameter, ".test")), method = "pearson")) # %>%
    # slice(3) %>%
    # as.numeric(.)
    print(result)
    return(result)
}

# basically the same as correlation function but faceted by device_id and data points colored based on abs. hum. (column abs_hum)
correlation_with_abshum <- function(data = data,
                                    start = "2023-02-08 00:00:00",
                                    end = "2023-02-12 00:00:00",
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

    start_str <- strftime(start, format = "%Y%m%d_%H%M%S")
    end_str <- strftime(end, format = "%Y%m%d_%H%M%S")

    timeidentifier <- paste0(start_str, "_", end_str)

    data_tmp <- data %>% # nolint: object_usage_linter.
        dplyr::select(date, device_id, !!sym(parameter)) %>%
        mutate(device_id = as.character(device_id)) %>%
        group_by(device_id) %>%
        openair::timeAverage(., avg.time = avg.time, start.date = round_date(.$date[1], "1 day") - days(1), type = "device_id")

    reference_table <- reference %>%
        openair::timeAverage(., avg.time = avg.time, start.date = round_date(.$date[1], "1 day") - days(1))

    data_accuracy <- data_tmp %>%
        full_join(., reference_table, by = "date", suffix = c(".test", ".reference")) %>%
        drop_na() %>%
        group_by(device_id) %>%
        dplyr::filter(between(date, as.POSIXct(start, tz = "UTC"), as.POSIXct(end, tz = "UTC"))) %>%
        drop_na() %>%
        ungroup() %>%
        droplevels(.$device_id)

    perf <- performance(data = data, reference = reference, parameter = parameter, start = start, end = end, avg.time = avg.time)

    plt_a <- ggplot(data_accuracy %>% dplyr::select(date, device_id, !!sym(paste0(parameter, ".reference")), !!sym(paste0(parameter, ".test")), abs_hum)) +
        geom_abline(data = perf %>% droplevels(), aes(slope = slope, intercept = intercept), col = "#000000", lwd = 1.1) +
        geom_abline(slope = 1, intercept = 0, col = "#2e2e2e", lwd = 1.1, lty = 2) +
        geom_point(aes(y = !!sym(paste0(parameter, ".test")), x = !!sym(paste0(parameter, ".reference")), col = abs_hum)) +
        theme_bw(16) +
        theme(panel.background = element_rect(fill = "gray")) +
        ggtitle(lookup[parameter]) +
        labs(x = "Reference", y = "Device under test") +
        theme(legend.pos = "bottom") +
        facet_wrap(. ~ device_id, ncol = 3) +
        scale_color_gradientn(name = "a in g m-3", limits = c(0, 50), colours = c("red", "white", "blue")) +
        theme(aspect.ratio = 1) #+ scale_color_gradient2(midpoint = 40, low="#1742ff", high="#ff2222")

    print(plt_a)
    ggsave(paste0("plt\\", devicetype, "\\Correlation\\", parameter, "_", timeidentifier, "_", reference_device, "_with_AbsHum.png"), plot = plt_a, width = 40, height = 20, units = "cm", dpi = "screen")
    
    result <- data_accuracy %>%
        reframe(corr = cor(!!sym(paste0(parameter, ".reference")), !!sym(paste0(parameter, ".test")), method = "pearson")) # %>%

    print(result)
    return(result)
}

# basically the same as correlation function but plotted against a chosen parameter (alongWhat) and facted by device_id
correlation_along_x <- function(data = data,
                                 start = "2023-02-08 00:00:00",
                                 end = "2023-02-12 00:00:00",
                                 reference = reference, # dataframe containing datetime and reference with same time resolution as device under test
                                 parameter = "CO2",
                                 avg.time = "1 hour",
                                 devicetype = "LS",
                                 reference_device = "Palas",
                                 alongWhat = "abs_hum") {
    packages <- c("tidyverse", "lubridate", "openair", "httr", "jsonlite", "rlang", "cowplot", "broom", "rlang")
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

    data_tmp <- data %>% # nolint: object_usage_linter.
        dplyr::select(date, device_id, !!sym(parameter)) %>%
        mutate(device_id = as.character(device_id)) %>%
        group_by(device_id) %>%
        openair::timeAverage(., avg.time = avg.time, start.date = round_date(.$date[1], "1 day") - days(1), type = "device_id")

    reference_table <- reference %>%
        openair::timeAverage(., avg.time = avg.time, start.date = round_date(.$date[1], "1 day") - days(1))

    data_accuracy <- data_tmp %>%
        full_join(., reference_table, by = "date", suffix = c(".test", ".reference")) %>%
        drop_na() %>%
        group_by(device_id) %>%
        dplyr::filter(between(date, as.POSIXct(start, tz = "UTC"), as.POSIXct(end, tz = "UTC"))) %>%
        drop_na() %>%
        ungroup() %>%
        droplevels(.$device_id)

    perf <- performance(data = data, reference = reference, parameter = parameter, start = start, end = end, avg.time = avg.time)

    plt_a <- ggplot(data_accuracy %>% dplyr::select(date, device_id, !!sym(paste0(parameter, ".reference")), !!sym(paste0(parameter, ".test")), !!sym(alongWhat))) +
        geom_smooth(aes(y = !!sym(paste0(parameter, ".test")), x = !!sym(alongWhat)),formula=y~x,method="lm",se = T)+
        ggpubr::stat_cor(aes(y = !!sym(paste0(parameter, ".test")), x = !!sym(alongWhat)),method = "pearson", label.x.npc = 0.3, label.y.npc = 0.1)+
        ggpmisc::stat_fit_tidy(method = "lm", method.args = list(formula = y~x),
                         aes(y = !!sym(paste0(parameter, ".test")), x = !!sym(alongWhat),label = paste("b =", signif(..x_estimate..,digits = 3),
                                           "\u00B1", signif(..x_se..,digits = 2)),col=device_id))+
        geom_point(aes(y = !!sym(paste0(parameter, ".test")), x = !!sym(alongWhat))) +
        theme_bw(16) +
        theme(panel.background = element_rect(fill = "gray")) +
        ggtitle(parameter) +
        theme(legend.pos = "bottom") +
        facet_wrap(. ~ device_id, ncol = 3) # + scale_color_gradientn(limits = c(0,100), colours=c("red","white","blue"))#+ scale_color_gradient2(midpoint = 40, low="#1742ff", high="#ff2222")

    print(plt_a)
    ggsave(paste0(
        "plt\\", devicetype, "\\Correlation\\", parameter, "_",
        timeidentifier, "_", reference_device, "_along",alongWhat,".png"
    ), plot = plt_a, width = 40, height = 20, units = "cm", dpi = "screen")
}