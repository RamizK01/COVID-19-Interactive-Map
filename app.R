# Packages
library(shiny)
library(dplyr)
library(wbstats)
library(flexdashboard)
library(leaflet)
library(dplyr)
library(rnaturalearth)
library(htmltools)

### Loading and Cleaning Data ###

# Loading the Covid 19 global data 
covid_19_data <- read.csv("WHO-COVID-19-global-data.csv")

# Case Counts
covid_19_data_cases <- covid_19_data %>%
  select(Date_reported, Country_code, Country, New_cases, Cumulative_cases)

# Keeping latest data regarding cumulative cases from each Country for the year 2022 (December.31.2022)
covid_19_data_latest_cases <- covid_19_data %>%
  select(Date_reported, Country_code, Country, Cumulative_cases)

covid_19_data_latest_cases <- covid_19_data_latest_cases %>%
  filter(Date_reported == "2022-12-31")

# Death Counts
covid_19_data_deaths <- covid_19_data %>%
  select(Date_reported, Country_code, Country, New_deaths, Cumulative_deaths)

# Keeping latest data regarding cumulative deaths from each Country for the year 2022 (December.31.2022)
covid_19_data_latest_deaths <- covid_19_data %>%
  select(Date_reported, Country_code, Country, Cumulative_deaths)

covid_19_data_latest_deaths <- covid_19_data_latest_deaths %>%
  filter(Date_reported == "2022-12-31")

# Viewing the GDP data for each Country 
df_gdp <- wb_data("NY.GDP.MKTP.CD")

filtered_df_gdp <- df_gdp %>%
  filter(date %in% c(2020, 2021, 2022))

# Viewing the unemployment levels per Country
df_unemployment <- wb_data("SL.UEM.TOTL.ZS")

filtered_df_unemployment <- df_unemployment %>%
  filter(date %in% c(2022))

# Viewing the population data for each Country 
df_population <- wb_data("SP.POP.TOTL")

filtered_df_population <- df_population %>%
  filter(date %in% c(2022))

### Calculations ###

# Calculating Prevalence (Confirmed cases by Population)
prevalence <- covid_19_data_latest_cases %>%
  left_join(filtered_df_population, 
            by = c("Country_code" = "iso2c")) %>%
  select(-iso3c:-date,-unit:-last_updated) %>%
  mutate(prevalence_covid = round(Cumulative_cases / SP.POP.TOTL,4))

# Case Fatality Rate (Deaths per Infected Population)
covid_19_data_cases_fatality <- covid_19_data %>%
  select(Date_reported, Country_code, Country, Cumulative_cases, Cumulative_deaths)

covid_19_data_cases_fatality <- covid_19_data_cases_fatality %>%
  filter(Date_reported == "2022-12-31")

covid_19_data_cases_fatality <- covid_19_data_cases_fatality %>%
  mutate(Case_Fatality_Rate = round(Cumulative_deaths / Cumulative_cases,4))

# Mortality Rate (Deaths per Total Population)
mortality_rate_per_country <- covid_19_data_latest_deaths %>%
  left_join(filtered_df_population, 
            by = c("Country_code" = "iso2c")) %>%
  select(-iso3c:-date,-unit:-last_updated) %>%
  mutate(Mortality_rate = round(Cumulative_deaths / SP.POP.TOTL,4))

### Actual Map ###

map_df <- cbind(mortality_rate_per_country$Country, 
                mortality_rate_per_country$Mortality_rate, 
                covid_19_data_cases_fatality$Case_Fatality_Rate, 
                prevalence$prevalence_covid
                )

# Get world shapefile
world_shape <- ne_countries(returnclass = "sf")

# Merge the shapefile with the data
merged_data <- merge(world_shape, map_df, by.x = "name", by.y = "V1")

# leaflet function to generate map
leaflet(merged_data) %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addPolygons(stroke = TRUE,
              fillColor = ~"blue",
              fillOpacity = 0.5,
              color = "white",
              weight = 1,
              popup = ~paste("Country: ", name, "<br>",
                             "Mortality Rate: ", V2, "<br>",
                             "Case Fatality Rate: ", V3, "<br>",
                             "Prevalence: ", V4)) %>%
  setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90) %>%
  setView(lng = 0, lat = 0, zoom = 2)
  

ui <- fluidPage(
  
  mainPanel(
    leafletOutput(outputId = 'map')
  )
  
)

server <- function(input, output, session) {
  
  # leaflet map
  output$map <- renderLeaflet({
    
    leaflet(merged_data) %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      addPolygons(stroke = TRUE,
                  fillColor = ~"blue",
                  fillOpacity = 0.5,
                  color = "white",
                  weight = 1,
                  popup = ~paste("Country: ", name, "<br>",
                                 "Mortality Rate: ", V2, "<br>",
                                 "Case Fatality Rate: ", V3, "<br>",
                                 "Prevalence: ", V4)) %>%
      setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90) %>%
      setView(lng = 0, lat = 0, zoom = 2)
    
  })
  
}

shinyApp(ui = ui, server = server)