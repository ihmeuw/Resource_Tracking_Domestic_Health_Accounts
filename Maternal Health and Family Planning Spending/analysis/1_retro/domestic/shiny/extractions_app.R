#
# app for exploring input data for reproductive health/unfpa-tr project
#
library(here)
library(viridis)
library(shiny)
library(data.table)
library(sf)
library(ggplot2)
library(plotly)
library(leaflet)
library(kableExtra)

source(here::here("src", "r", "params.R"))


CONFIG <- list(
    files = list(
        rh_data = get_path("int", "data", c("domestic", "int", "rh_extracted.csv")),
        locs = get_path("int", "data", c("covars", "ihme_unfpa_locs.csv")),
        world_shapefile = get_path("raw", "input",
                                    c("shapefiles", "lbd_standard_admin_0_simplified.shp"))
    )
)



#
# ==== HELPER FUNCTIONS & DATA ====
#

prep_data <- function() {
    d <- fread(CONFIG$files$rh_data)
    d <- d[value_code != "rh_HK"] # "capital formation" - for sake of this app, remove
    locs <- fread(CONFIG$files$locs)
    
    d <- merge(d,
               locs[, .(iso3 = ihme_loc_id, name = ihme_location_name,
                        is_unfpa, region_name, super_region_name, income_group)],
               by = "iso3", all.x = TRUE)
    d[, src_notes := paste0(src, " - ", src_notes)]
    
    # add flag for whether country-year contains DIS x SRC info:
    
    return(d)
}

prep_sf <- function(d) {
    world <- sf::read_sf(CONFIG$files$world_shapefile)
    # note: this sf doesn't have sao tome and principe
    world <- within(world, {
        name <- fcase(
            ADM0_NAME == "Bolivia", "Bolivia (Plurinational State of)",
            ADM0_NAME == "Cape Verde", "Cabo Verde",
            ADM0_NAME == "Republic of Congo", "Congo",
            ADM0_NAME == "Swaziland", "Eswatini",
            ADM0_NAME == "Laos", "Lao People's Democratic Republic",
            ADM0_NAME == "Tanzania", "United Republic of Tanzania",
            ADM0_NAME == "Vietnam", "Viet Nam",
            rep_len(TRUE, length(ADM0_NAME)), ADM0_NAME
        )
    })
    locs <- fread(CONFIG$files$locs)
    
    world <- merge(world,
                   unique(locs[, .(name = ihme_location_name, is_unfpa)]),
                   by = "name", all.x = TRUE)
    world[is.na(world$is_unfpa), "is_unfpa"] <- FALSE
    return(world)
}


get_country_tab <- function(d, cap) {
    v <- \(x) {
        ret <- d[value_code == x, value]
        if (length(ret) == 0)
            NA_real_
        else
            ret
    }
    
    tab <- data.table(
        `Disease x Source` = c("Dis2.1 (Maternal Health)",
                               "Dis2.2 (Perinatal Conditions)",
                               "Dis2.3 (Family Planning)",
                               "Dis2.Nec (Unspecified)",
                               "Total Reproductive Health"),
        `Public` = c(v("fs_domestic_public_dis2.1"),
                     v("fs_domestic_public_dis2.2"),
                     v("fs_domestic_public_dis2.3"),
                     v("fs_domestic_public_dis2.nec"),
                     v("fs_domestic_public_dis2")),
        `OOP` = c(v("fs_domestic_private_oop_dis2.1"),
                  v("fs_domestic_private_oop_dis2.2"),
                  v("fs_domestic_private_oop_dis2.3"),
                  v("fs_domestic_private_oop_dis2.nec"),
                  v("fs_domestic_private_oop_dis2")),
        `PPP` = c(v("fs_domestic_private_ppp_dis2.1"),
                  v("fs_domestic_private_ppp_dis2.2"),
                  v("fs_domestic_private_ppp_dis2.3"),
                  v("fs_domestic_private_ppp_dis2.nec"),
                  v("fs_domestic_private_ppp_dis2")),
        `DAH` = c(v("fs_dah_dis2.1"),
                  v("fs_dah_dis2.2"),
                  v("fs_dah_dis2.3"),
                  v("fs_dah_dis2.nec"),
                  v("fs_dah_dis2")),
        `NSK` = c(v("fs_nsk_dis2.1"),
                  v("fs_nsk_dis2.2"),
                  v("fs_nsk_dis2.3"),
                  v("fs_nsk_dis2.nec"),
                  v("fs_nsk_dis2")),
        `Total Source` = c(v("the_dis2.1"),
                           v("the_dis2.2"),
                           v("the_dis2.3"),
                           v("the_dis2.nec"),
                           v("the_dis2"))
    )
    tab[, names(tab) := lapply(.SD, function(x) {
        x <- if (is.numeric(x)) format(round(x), big.mark = ",") else as.character(x)
        return(cell_spec(x, color = ifelse(x == "NA" | is.na(x), "#ededed", "#000000")))
    }), .SDcols = names(tab)]
    fin <- kableExtra::kbl(tab, caption = cap, escape = FALSE) |>
        column_spec(1, bold = T) |>
        column_spec(7, background = "#c9c9c9") |>
        row_spec(5, background = "#c9c9c9") |>
        kable_styling()
    return(fin)
}


.transl <- c("the" = "Total Spending",
             "fs_domestic_public" = "Public Spending",
             "fs_domestic_private_oop" = "Out-of-Pocket Spending",
             "fs_domestic_private_ppp" = "Prepaid Private Spending",
             "fs_dah" = "Spending from External Sources",
             "fs_nsk" = "Unspecified Spending",
             "dis2" = "Reproductive Health",
             "dis2.1" = "Maternal Health",
             "dis2.2" = "Perinatal Conditions",
             "dis2.3" = "Family Planning",
             "dis2.nec" = "Unspecified Reproductive Health")
get_code_title <- function(code) {
    fs <- gsub("_dis.*", "", code)
    dis <- gsub(".*_", "", code)
    return(paste0(code, ": ", .transl[fs], " on ", .transl[dis]))
}


DATA <- prep_data()
WORLD <- prep_sf()
NSRC_COLOR_MAP <- c("red", viridis::viridis(length(unique(DATA$src))))
names(NSRC_COLOR_MAP) <- seq_along(NSRC_COLOR_MAP) - 1

META <- within(list(), {
    src <- unique(DATA$src)
    src_detailed <- unique(DATA$src_notes)
    yr_range <- seq(min(DATA$year_start), max(DATA$year_start), by = 1L)
    data_var_opts <- c("any", sort(unique(DATA$value_code)))
    country_opts <- sort(unique(DATA$name))
    region_opts <- sort(unique(DATA$region_name))
    super_region_opts <- sort(unique(DATA$super_region_name))
    dis_codes <- c("dis2", "dis2.1", "dis2.2", "dis2.3", "dis2.nec")
    src_codes <- c("fs_domestic_public", "fs_domestic_private_oop",
                   "fs_domestic_private_ppp", "fs_dah", "fs_nsk")
})



#
# ==== USER INTERFACE ====
#

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("UNFPA 3TR - Data Explorer"),
    #
    # overall data coverage
    #
    h2("Data coverage:"),
    sidebarLayout(
        sidebarPanel(
            selectInput("data_source", "Data source",
                        c("any", META$src), selected = "any"),
            selectInput("data_source_detailed", "Data source - detailed",
                        c("any", META$src_detailed), selected = "any"),
            # selectInput("map_var", "Map Variable",
            #             META$map_var_opts, selected = "nsrc"),
            sliderInput("map_years", "Map Years",
                        min = min(META$yr_range),
                        max = max(META$yr_range),
                        step = 1L,
                        sep = "",
                        value = c(min(META$yr_range), max(META$yr_range))
                        ),
            selectInput("data_var", "Select extracted variable",
                        META$data_var_opts, selected = "any",
                        multiple = TRUE),
            p("Use the above filter to explore which countries have multiple observations of the same variable."),
            p("See the tables below to decipher the variable names."),
            fluidRow(
                column(6, tableOutput("variableSummaryTable1")),
                column(6, tableOutput("variableSummaryTable2")),
            ),
            p("E.g., 'the_dis2' is total reproductive health spending and 'fs_domestic_public_dis2.3' is public spending on family planning.")
        ),
        mainPanel(
            plotlyOutput("timePlot"),
            h3("Coverage per country"),
            p("All UNFPA priority countries are highlighted"),
            leafletOutput("mapPlot")
        ),
    ),
    #
    # individual country data coverage
    #
    hr(style = "border-top: 1px solid #000000;"),
    h2("Data coverage by country:"),
    sidebarLayout(
        sidebarPanel(
            selectInput("country", "Country",
                        META$country_opts, selected = META$country_opts[1]),
            uiOutput("table_yr_select"),
            
        ),
        mainPanel(
            plotlyOutput("countryTimePlot"),
            h3("Extracted data:"),
            htmlOutput("countryTableNHA"),
            htmlOutput("countryTableFPSA"),
            htmlOutput("countryTableNIDI"),
            htmlOutput("countryTableRHSC"),
        )
    ),
    #
    # country comparisons
    #
    hr(style = "border-top: 1px solid #000000;"),
    h2("Country comparisons:"),
    sidebarLayout(
        sidebarPanel(
            h3("Filter countries:"),
            p("Add and delete regions to filter countries."),
            selectInput("super_region", "Super Region",
                        META$super_region_opts, multiple = TRUE,
                        selected = META$super_region_opts[1]),
            uiOutput("region_select"),
            uiOutput("country_select"),
            h3("Filter variables:"),
            p("Select variable to plot."),
            uiOutput("dis_select"),
            uiOutput("fs_select"),
            # uiOutput("cmp_var_select"),
            textOutput("list_selected_vars")
        ),
        mainPanel(
            plotlyOutput("cmpTimePlot")
        )
    ),
    hr(style = "border-top: 1px solid #000000;"),
    h2("About"),
    p("This tool aims to provide an overview of the input data we have extracted.")
)


#
# ==== SERVER ====
#


server <- function(input, output) {
    #
    # overall data coverage
    #
    output$timePlot <- renderPlotly({
        src_ <- input$data_source
        src_detailed_ <- input$data_source_detailed
        data_var_ <- input$data_var
        if (src_ == "any") src_ <- META$src
        if (src_detailed_ == "any") src_detailed_ <- META$src_detailed
        if ("any" %in% data_var_) data_var_ <- META$data_var_opts
        
        d <- DATA[src %in% src_ & src_notes %in% src_detailed_ &
                      !is.na(value) & value_code %in% data_var_,
                  .(N = length(unique(iso3)),
                    ISOs = paste(unique(iso3), collapse = ","),
                    Nsrc = length(unique(src)),
                    SRCs = paste(unique(src), collapse = ", ")),
                  by = year_start]
        d[, Nsrc := factor(Nsrc, levels = sort(unique(Nsrc)))]
        if (!nrow(d)) {
            return(ggplot() + labs(title = "No data"))
        }
        p <- ggplot(d, aes(x = year_start, y = N, color = Nsrc,
                           group = ISOs, linetype = SRCs)) + # add for tooltip
            geom_point(alpha = 0.7) +
            coord_cartesian(ylim = c(0, max(d$N) + 1)) +
            scale_color_manual(values = NSRC_COLOR_MAP) +
            labs(
                title = "Number of countries with data by year, from specified source, for specified variables",
                x = "Start year of data",
                y = "Countries with data",
                color = "Number of unique sources"
            ) +
            theme_minimal() +
            guides(linetype = "none")
        p
    })
    
    
    output$variableSummaryTable1 <- renderTable({
        codes = c(
            Dis2 = "total reproductive health",
            Dis2.1 = "maternal health",
            Dis2.2 = "perinatal conditions",
            Dis2.3 = "family planning",
            Dis2.Nec = "other reproductive health"
        )
        out <- data.table(
            `Disease Codes` = names(codes),
            Description = unname(codes)
        )
        return(out)
    })
    
    output$variableSummaryTable2 <- renderTable({
        codes = c(
            the = "total health expenditure",
            fs_domestic_public = "public spending",
            fs_domestic_private_oop = "OOP spending",
            fs_domestic_private_ppp = "PPP spending",
            fs_dah = "DAH/externally sourced/non-domestic spending",
            fs_nsk = "unidentified"
        )
        out <- data.table(
            `Financing Sources` = names(codes),
            Description = unname(codes)
        )
        return(out)
    })
    
    output$mapPlot <- renderLeaflet({
        src_ <- input$data_source
        src_detailed_ <- input$data_source_detailed
        if (src_ == "any") src_ <- META$src
        if (src_detailed_ == "any") src_detailed_ <- META$src_detailed
        
        d <- DATA[src %in% src_ & src_notes %in% src_detailed_ &
                      between(year_start, input$map_years[1], input$map_years[2])]
        dv <- input$data_var
        d <- if ("any" %in% dv) {
            d[, .(N = length(unique(src)),
                  SRCs = paste(unique(src), collapse = ",")),
              by = .(name)]
        } else {
            d[!is.na(value) & value_code %in% dv,
              .(N = length(unique(src)),
                SRCs = paste(unique(src), collapse = ",")),
              by = .(name)]
        }
        if (!nrow(d)) {
            return(leaflet() |> addControl(html = "<h1>No data</h1>"))
        }
        map_d <- merge(WORLD, d, by = "name", all.x = TRUE)
        map_d$N <- ifelse(map_d$is_unfpa == TRUE & is.na(map_d$N), 0, map_d$N)
        color_fn <- function(x) {
            out <- character(length(x))
            out[is.na(x)] <- "grey"
            out[which(x == 0 & !is.na(x))] <- NSRC_COLOR_MAP["0"]
            oth_ix <- which(x > 0 & !is.na(x))
            vals <- sort(unique(x[oth_ix]))
            pal <- NSRC_COLOR_MAP[seq(2, length(NSRC_COLOR_MAP))]
            out[oth_ix] <- pal[match(x[oth_ix], vals)] 
            return(out)
        }
        leg_pal_vals <- sort(unique(map_d$N))
        pal <- colorFactor(palette = color_fn,
                           levels = leg_pal_vals,
                           na.color = "grey")
        leaflet::leaflet(map_d) |>
            setView(lng = 0, lat = 0, zoom = 2) |>
            addProviderTiles("CartoDB.Positron") |>
            addPolygons(color = "black", weight = 1,
                        fillColor = ~color_fn(N),
                        fillOpacity = 0.5,
                        label = ~paste0(name, ": ", N, " (", SRCs, ")")) |>
            addLegend("bottomright",
                      pal = pal,
                      values = as.integer(leg_pal_vals),
                      title = "Count",
                      opacity = 0.5)
            
    })
    
    #
    # individual country data coverage
    #
    output$countryTimePlot <- renderPlotly({
        d <- DATA[name == input$country]
        if (!nrow(d)) {
            return(ggplot() + labs(title = "No data"))
        }
        d <- d[!is.na(value),
               .(Nval = length(unique(value_code)),
                 Nsrc = length(unique(src)),
                 SRCs = paste(sort(unique(src)), collapse = ", ")),
               by = year_start]
        d[, Nsrc := factor(Nsrc, levels = sort(unique(Nsrc)))]
        
        p <- ggplot(d, aes(x = year_start, y = Nval, fill = Nsrc,
                           group = SRCs)) + # for sake of plotly tooltip
            geom_bar(stat = "identity") +
            scale_fill_manual(values = NSRC_COLOR_MAP) +
            coord_cartesian(ylim = c(0, max(d$Nval) + 2)) +
            labs(
                title = paste0("Quantity of data for ", input$country),
                x = "Year",
                y = "Number of value codes",
                fill = "Number of sources",
            ) +
            theme_minimal()
        p
    })
    
    output$table_yr_select <- renderUI({
        yrs <- DATA[name == input$country, sort(unique(year_start))]
        selectInput("table_yr_select", "Year",
                    choices = yrs,
                    selected = max(yrs))
    })
    
    output$countryTableNHA <- renderText({
        yr <- if (is.null(input$table_yr_select)) 2021 else input$table_yr_select
        d <- DATA[name == input$country &
                      year_start == yr &
                      src == "nha"]
        return(get_country_tab(d,
                               paste0(input$country, " ", input$table_yr_select, ": NHA")))
    })
    
    output$countryTableFPSA <- renderText({
        yr <- if (is.null(input$table_yr_select)) 2021 else input$table_yr_select
        d <- DATA[name == input$country &
                      year_start == yr &
                      src == "fpsa"]
        return(get_country_tab(d,
                               paste0(input$country, " ", input$table_yr_select, ": FPSA")))
    })
    output$countryTableNIDI <- renderText({
        yr <- if (is.null(input$table_yr_select)) 2021 else input$table_yr_select
        d <- DATA[name == input$country &
                      year_start == yr &
                      src == "nidi"]
        return(get_country_tab(d,
                               paste0(input$country, " ", input$table_yr_select, ": NIDI")))
    })
    output$countryTableRHSC <- renderText({
        yr <- if (is.null(input$table_yr_select)) 2021 else input$table_yr_select
        d <- DATA[name == input$country &
                      year_start == yr &
                      src == "rhsc"]
        return(get_country_tab(d,
                               paste0(input$country, " ", input$table_yr_select, ": RHSC")))
    })
    
    #
    # country comparisons
    #
    output$region_select <- renderUI({
        srs <- input$super_region
        choices <- DATA[super_region_name %in% srs, sort(unique(region_name))]
        selectInput("region_select", "Region",
                    choices = choices,
                    selected = choices,
                    multiple = TRUE)
    })
    
    output$country_select <- renderUI({
        srs <- input$super_region
        rgn <- input$region_select
        choices <- DATA[super_region_name %in% srs & region_name %in% rgn,
                        sort(unique(name))]
        selectInput("country_select", "Country",
                    choices = choices,
                    selected = choices,
                    multiple = TRUE)
    })
    
    cache_dis_select <- \(x) NULL #reactive(input$dis_select)
    cache_fs_select <- \(x) NULL  #reactive(input$fs_select)
    
    output$dis_select <- renderUI({
        countries <- input$country_select
        if (length(countries) == 0) {
            return(selectInput("dis_select", "Select Disease Area",
                               META$dis_codes, multiple = TRUE))
        }
        choices <- DATA[name %in% countries, unique(value_code)]
        choices <- sort(unique(gsub(".*_", "", choices)))
        cache_dis <- cache_dis_select()
        if (!is.null(cache_dis)) {
            selected <- base::intersect(cache_dis, choices)
            selected <- if (!length(selected)) choices[1] else selected
        } else {
            selected <- choices[1]
        }
        selectInput("dis_select", "Select Disease Area",
                    choices = choices, multiple = TRUE, selected = selected)
    })
    output$fs_select <- renderUI({
        countries <- input$country_select
        if (length(countries) == 0 || is.null(input$dis_select)) {
            return(selectInput("fs_select", "Select Source",
                               META$src_codes, multiple = TRUE))
        }
        choices <- DATA[name %in% countries &
                            value_code %like% paste0(paste(input$dis_select, collapse = "$|"), "$"),
                        unique(value_code)]
        choices <- sort(unique(gsub("_dis.*", "", choices)), decreasing = TRUE)
        cache_fs <- cache_fs_select()
        if (!is.null(cache_fs)) {
            selected <- base::intersect(cache_fs, choices)
            selected <- if (!length(selected)) choices[1] else selected
        } else {
            selected <- choices[1]
        }
        selectInput("fs_select", "Select Source",
                    choices = choices, multiple = TRUE, selected = selected)
    })
    output$list_selected_vars <- renderText({
        codes <- META$data_var_opts[META$data_var_opts != "any"]
        codes <- codes[codes %like% paste(input$fs_select, collapse = "|")]
        codes <- codes[codes %like% paste0(paste(input$dis_select, collapse = "$|"), "$")]
        return(paste(sort(codes, decreasing = TRUE), collapse = ", "))
    })
    
    output$cmpTimePlot <- renderPlotly({
        ep <- ggplot() + labs(title = "No data")
        if (is.null(input$dis_select) || is.null(input$fs_select))
            return(ep)
        codes <- META$data_var_opts[META$data_var_opts != "any"]
        codes <- codes[codes %like% paste(input$fs_select, collapse = "|")]
        codes <- codes[codes %like% paste0(paste(input$dis_select, collapse = "$|"), "$")]
        d <- DATA[name %in% input$country_select & value_code %in% codes]
        if (!nrow(d))
            return(ep)
        p <- ggplot(d, aes(x = year_start, y = value,
                           color = name, shape = src, linetype = src,
                           group = paste0(name, "_", src))) +
            geom_line(alpha = 0.5) +
            geom_point(alpha = 0.7) +
            facet_wrap(~value_code, labeller = as_labeller(get_code_title)) +
            labs(
                x = "Year",
                y = "2022 USD",
                color = "Country",
                shape = "(Data Source)",
                linetype = "(Data Source)"
            ) +
            theme_light() +
            theme(
                panel.spacing.y = unit(2, "lines"),
                strip.background = element_rect(fill = "gray90"),
                strip.text = element_text(color = "black")
            )
        plotly::ggplotly(p, tooltip = c("x", "y", "color", "shape"))
    })

}


#
# ==== MAIN ====
#
shinyApp(ui = ui, server = server,
         options = list(
             host = "0.0.0.0",
             port = 6144
         ))
