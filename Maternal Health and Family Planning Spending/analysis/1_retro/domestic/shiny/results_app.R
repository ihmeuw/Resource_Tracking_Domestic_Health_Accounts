
library(here)
library(viridis)
library(shiny)
library(data.table)
library(ggplot2)
library(plotly)

source(here::here("src", "r", "params.R"))

CONFIG <- list(
    dirs = list(
        stgpr_working = get_path("int", "data", c("domestic", "int", "stgpr")),
        covars = get_path("int", "data", c("covars"))
    )
)



#
# ==== HELPER FUNCTIONS & DATA ====
#
model_vars <- c(
    "public_dis2.1" = "public_mh"
    # "public_dis2.3" = "public_fp",
    # "oop_dis2.1" = "oop_mh",
    # "oop_dis2.3" = "oop_fp"
)


denominator_map <- c(
    "public_mh" = "ghes_totes"
)


gbd_locs <- fread(file.path(CONFIG$dirs$covars, "gbd_locations.csv"))[level == 3]
unfpa_locs <- fread(file.path(CONFIG$dirs$covars, "ihme_unfpa_locs.csv"))

gbd_locs <- merge(gbd_locs,
                  unfpa_locs[, .(ihme_loc_id, is_unfpa)],
                  by = "ihme_loc_id", all.x = TRUE)

get_var_path <- function(varname) {
    file.path(CONFIG$dirs$stgpr_working, varname, "final")
}

get_denoms <- function() {
    out <- list()
    for (varname in names(denominator_map)) {
        denom <- denominator_map[varname]
        dat <- fread(file.path(CONFIG$dirs$covars, paste0(denom, ".csv")))
        dat <- dat[, c("location_id", "year_id", denom), with = FALSE]
        dat$variable <- varname
        setnames(dat, c(denom), c("denominator"))
        out[[varname]] <- dat
    }
    return(rbindlist(out))
}


get_estimates <- function(denoms) {
    # load summary files
    data_ls <- list()
    for (varname in names(model_vars)) {
        path <- get_var_path(varname)
        data_ls[[varname]] <- arrow::read_feather(file.path(path, "summary.feather"))
        data_ls[[varname]]$variable <- model_vars[varname]
    }
    dat <- rbindlist(data_ls)
    dat <- merge(dat,
                 gbd_locs[, .(location_id,
                              region_name, super_region_name, is_unfpa)],
                 by = "location_id",
                 all.x = TRUE)
    dat <- merge(dat, denoms,
                 by = c("location_id", "year_id", "variable"),
                 all.x = TRUE)
    
    val_vars <- c("gpr_mean", "gpr_lower", "gpr_upper",
                  "stage1", "stage2")
    dat <- dat[, (val_vars) := lapply(.SD, function(x) x * denominator),
               .SDcols = val_vars]
    return(dat)
}

get_raw_data <- function(denoms) {
    dat <- fread(file.path(CONFIG$dirs$stgpr_working, "stgpr_all_data.csv"))
    dat[, variable := model_vars[value_id]]
    dat <- dat[!is.na(variable)] # drop variables not in model_vars vector
    dat[, is_outlier := as.logical(is_outlier)]
    
    dat <- merge(dat,
                 gbd_locs[, .(location_id, location_name,
                              region_name, super_region_name, is_unfpa)],
                 by = "location_id",
                 all.x = TRUE)
    dat <- merge(dat, denoms,
                 by = c("location_id", "year_id", "variable"),
                 all.x = TRUE)
    return(dat)
}


denoms <- get_denoms()

RAW <- get_raw_data(denoms)
EST <- get_estimates(denoms)

META <- within(list(), {
    variables <- unname(model_vars)
    countries <- sort(unique(EST$location_name))
    yr_range <- range(EST$year_id)
    denoms <- c("None", "Natural", "Thousand", "Million", "Billion")
    data_layers <- c("Country", "Region", "Super Region")
    est_layers <- c("Stage 1", "Stage 2", "GPR")
})


#
# ==== USER INTERFACE ====
#

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("UNFPA 3TR - Domestic Expenditure Results"),
    h2("Model Results:"),
    sidebarLayout(
        sidebarPanel(
            h3("Filter Results:"),
            selectInput("variable", "Modeled Variable",
                        META$variables, selected = META$variables[1]),
            selectInput("denom", "Denominator",
                        META$denoms, selected = META$denoms[1]),
            selectInput("country", "Country",
                        META$countries, selected = META$countries[1],
                        multiple = FALSE),
            sliderInput("years", "Years",
                        min = min(META$yr_range),
                        max = max(META$yr_range),
                        step = 1L,
                        sep = "",
                        value = c(min(META$yr_range), max(META$yr_range))
                        ),
            h3("Filter Input Data:"),
            selectInput("show_layer", "Show Level:",
                        META$data_layers, selected = META$data_layers[1],
                        multiple = TRUE),
            selectInput("outliers", "Show Outliers:",
                        c("Yes" = TRUE, "No" = FALSE),
                        selected = FALSE),
            p("Outliers will appear as an open circle on the plot."),
            h3("Filter Estimates:"),
            selectInput("show_est", "Show Stage:",
                        META$est_layers, selected = "GPR",
                        multiple = TRUE),
            p("Final estimates are the GPR mean. 95% uncertainty interval also shown.")
        ),
        mainPanel(
            plotlyOutput("timePlot"),
        ),
    )
)


#
# ==== SERVER ====
#


server <- function(input, output) {
    #
    # overall data coverage
    #
    output$timePlot <- renderPlotly({
        dat <- RAW[variable == input$variable &
                       between(year_id, input$years[1], input$years[2])]
        est <- EST[variable == input$variable &
                       location_name == input$country &
                       between(year_id, input$years[1], input$years[2])]
        if (nrow(est) == 0 || nrow(dat) == 0) {
            return(ggplot() + geom_blank() + labs(title = "No data"))
        }
        if (input$outliers == FALSE) {
            dat <- dat[is_outlier == FALSE]
        }
        scale_factor <- switch(input$denom,
                               "None" = 1,
                               "Thousand" = 1e3,
                               "Million" = 1e6,
                               "Billion" = 1e9,
                               "Natural" = -1)
        if (scale_factor == -1) {
            dat[, plot_var := value / denominator]
            est[, `:=` (
                gpr_mean = gpr_mean / denominator,
                gpr_lower = gpr_lower / denominator,
                gpr_upper = gpr_upper / denominator,
                stage1 = stage1 / denominator,
                stage2 = stage2 / denominator
            )]
        } else {
            dat[, plot_var := value / scale_factor]
            est[, `:=` (
                gpr_mean = gpr_mean / scale_factor,
                gpr_lower = gpr_lower / scale_factor,
                gpr_upper = gpr_upper / scale_factor,
                stage1 = stage1 / scale_factor,
                stage2 = stage2 / scale_factor
            )]
        }
        country <- EST[location_name == input$country, location_name[1]]
        region  <- EST[location_name == input$country, region_name[1]]
        supreg  <- EST[location_name == input$country, super_region_name[1]]
        is_unfpa <- EST[location_name == input$country, is_unfpa[1]]
        
        country_points <- if (nrow(dat[location_name == country]) > 0) {
            geom_point(
                data = dat[location_name == country],
                aes(x = year_id, y = plot_var, shape = is_outlier, color = "country"))
        } else {
            geom_blank()
        }
        region_points <- geom_point(
            data = dat[region_name == region],
            aes(x = year_id, y = plot_var, shape = is_outlier, color = "region"),
            alpha = 0.5
        )
        supreg_points <- geom_point(
            data = dat[super_region_name == supreg],
            aes(x = year_id, y = plot_var, shape = is_outlier, color = "super-region"),
            alpha = 0.5
        )
        stage1_line <- geom_line(
            data = est,
            aes(x = year_id, y = stage1, color = "stage1"),
            linetype = "dotdash"
        )
        stage2_line <- geom_line(
            data = est,
            aes(x = year_id, y = stage2, color = "st"),
            linetype = "dotdash"
        )
        gpr_line <- geom_line(
            data = est,
            aes(x = year_id, y = gpr_mean, color = "gpr")
        )
        gpr_ribbon <- geom_ribbon(
            data = est,
            aes(x = year_id, ymin = gpr_lower, ymax = gpr_upper),
            fill = "grey50", alpha = 0.2
        )
        p <- ggplot()
        # estimate lines
        if ("GPR" %in% input$show_est) {
            p <- p + gpr_ribbon
        }
        if ("Stage 1" %in% input$show_est) {
            p <- p + stage1_line
        }
        if ("Stage 2" %in% input$show_est) {
            p <- p + stage2_line
        }
        if ("GPR" %in% input$show_est) {
            p <- p + gpr_line
        }
        # data points
        if ("Region" %in% input$show_layer) {
            p <- p + region_points
        }
        if ("Super Region" %in% input$show_layer) {
            p <- p + supreg_points
        }
        if ("Country" %in% input$show_layer) {
            p <- p + country_points
        }
        p <- p +
            scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 1)) +
            scale_color_manual(values = c("gpr" = "green",
                                          "stage1" = "orange",
                                          "st" = "blue",
                                          "country" = "red",
                                          "region" = "#74c476",
                                          "super-region" = "#beaed4"),
                               breaks = c("stage1", "st", "gpr",
                                          "country", "region", "super-region")) +
            labs(title = paste0(country, " <br><sup>",
                                "(Region: ", region,
                                ", Super Region: ", supreg,
                                ", UNFPA: ", is_unfpa, ")",
                                "</sup>"),
                 color = "",
                 x = "", y = input$variable) +
            theme_minimal() +
            guides(shape = "none",
                   color = guide_legend(overide.aes = list(shape = 16)))
        (print(p))
    })
}
 


#
# ==== MAIN ====
#
shinyApp(ui = ui, server = server)
         # options = list(
         #     host = "0.0.0.0",
         #     port = 6144
         # ))

