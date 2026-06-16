# Download country ISO alpha-2 to alpha-3 mappings from:
#  https://gist.github.com/tadast/8827699
source(here::here("src", "r", "params.R"))

url <- "https://gist.githubusercontent.com/tadast/8827699/raw/61b2107766d6fd51e2bd02d9f78f6be081340efc/countries_codes_and_coordinates.csv"
isos <- read.csv(url)

names(isos) <- tolower(gsub("..", "_", names(isos), fixed = TRUE))
names(isos) <- gsub(".", "", names(isos), fixed = TRUE)
names(isos) <- gsub("code$", "", names(isos))

isos <- isos[, c("alpha2", "alpha3")]

isos <- within(isos, {
    alpha2 <- trimws(alpha2)
    alpha3 <- trimws(alpha3)
})

isos <- unique(isos)


dir.create(get_path("raw", "input", "misc"), showWarnings = FALSE)
write.csv(isos,
          file = get_path("raw", "input", c("misc", "country_iso2to3.csv")),
          row.names = FALSE)

