#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
####                    Making maps for Solanaceae genera                   ####
#*            Using: expowo from  https://dboslab.github.io/expowo/           #
#*                         using the refrence page:                           #
#*              "Mapping global distribution for any taxonomic level"         #
#*                           2 May 2024                                       #
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_



library(plotly)
library(rjson)
library(RColorBrewer)
library(terra)
library(magrittr)
library(sf)
library(RColorBrewer)
library(geodata)
library(sp)
library(dplyr)
library(stringr)
library(tidyr)


# set wd
setwd("C:/solgen")

#---- Get solanaceae taxa info from expowo -----

#---- Load the library ----
# Instal developer version
#install.packages("devtools")
# library(devtools)
# install.packages("rnaturalearthdata")
# library(rnaturalearthdata)
# library(ggplot2)
# devtools::install_github("DBOSlab/expowo")
#
# library(expowo)




#---- Mine sp dist from Powo ------
# mapspdist <- powoSpecies(family = "Solanaceae",
#                          country = NULL,
#                          verbose = TRUE,
#                          save = FALSE,
#                          dir = "results_powoSpecies",
#                          filename = "Solanaceae_spp")
#
# mapspdist$genus
#
# head(mapspdist)
# head(POWOcodes)
# head(botregions)
#
# ## produces a cat of each genus and number of sp per gen
# ## takes a little while to load
#
# #---- Create global level scale map ----
# ?powoMap
#
# pm <- powoMap(inputdf = mapspdist,
#         botctrs = TRUE,
#         distcol = "native_to_botanical_countries",
#         taxclas = "genus",
#         verbose = FALSE,
#         save = FALSE,
#         vir_color = "viridis",
#         bre_color = NULL,
#         leg_title = "SR",
#         dpi = 600,
#         dir = "results_powoMap",
#         filename = "global_richness_botcountry_map",
#         format = "jpg")

# Check a genus
# pm$Cestrum
#

# load in Sol data --------------
# from expowo download
##create botanical regions map as used in expowo code
## from expowo function powoMap
# download.file(url = "https://github.com/tdwg/wgsrpd/archive/master.zip",
#               destfile = "wgsrpd-master.zip")
# unzip(zipfile = "wgsrpd-master.zip")
# unlink("wgsrpd-master.zip")
# # rename files:
# file.rename("wgsrpd-master", "WGSRPD")
# file.rename("WGSRPD/109-488-1-ED", "WGSRPD/book-1-ED")
# file.rename("WGSRPD/README.md", "WGSRPD/README.txt")
# # unlink files that are not needed:
# unlink("WGSRPD/level1", recursive = TRUE)
# unlink("WGSRPD/level2", recursive = TRUE)
# unlink("WGSRPD/level4", recursive = TRUE)
#
# #load the map
# world1 <- sf::st_read("WGSRPD/level3/level3.shp")
# world1 <- world1[!world1$LEVEL3_NAM %in% "Antarctica", ]
# world1 <- st_as_sf(world1)
#
# world1$LEVEL3_NAM
#make a dataframe with the country names from the map to compare names to expowo file
botcountry_names <- world1$LEVEL3_NAM


# you may need to edit the file location
sol_gen <- read.csv("C:/solgen/results_powoSpecies/02May2024/Solanaceae_gen.csv")
colnames(sol_gen)

# Select only taxon_name, bot region, genus, status = accepted
sol_genf <- sol_gen %>% filter(status == 'Accepted') %>%
  select(genus, status, taxon_name, native_to_botanical_countries)

sol_genf$native_to_botanical_countries <- as.character(sol_genf$native_to_botanical_countries)


#strip them into their own list
split_countries <- strsplit(sol_genf$native_to_botanical_countries, ",")
#remove leading whitespace
split_countries <- lapply(split_countries, function(x) trimws(x, "left"))

#make an empty list to load the looped data into
combined_data <- list()

# Loop through each row of sol_genf
for (i in 1:nrow(sol_genf)) {
  # Extract genus and species name from current row
  genus <- sol_genf$genus[i]
  species <- sol_genf$taxon_name[i]

  # Extract countries for current species from split_countries
  countries <- unlist(split_countries[i])

  # Loop through each country for the current species
  for (country in countries) {
    # Check if the country is present in world1
    #if (country %in% botcountr0y_names) {
      # Create a new entry with genus, species, and country
      entry <- c(genus = genus, species = species, country = country)
      # Add the entry to combined_data list
      combined_data <- c(combined_data, list(entry))
    }
  }
#}

# Convert the list of entries to a data frame
combined_data_df <- do.call(rbind, combined_data)


#aggregate the data

#for species (if a species map is desired)
#species_summary <- aggregate(species ~ country, data = combined_data_df, FUN = function(x) paste(unique(x), collapse = ", "))

#for genus
genus_summary <- aggregate(genus ~ country, data = combined_data_df, FUN = function(x) paste(unique(x), collapse = ", "))

#find the number of genera per country
# genera_split <- strsplit(genus_summary$genus, ", ")

genera_counts <- sapply(genera_split, function(x) length(unique(x)))
genus_summary$gen_per_country <- genera_counts

# Clean up names in solanaceae genus summary to match wgrspd level 3
genus_summary$country[genus_summary$country == 'Suriname'] <- 'Suriname'
genus_summary$country[genus_summary$country == 'Central African Repu'] <- 'Central African Republic'
genus_summary$country[genus_summary$country == 'Kirgizstan'] <- 'Kirgizistan'
genus_summary$country[genus_summary$country == 'Democratic Republic of the Congo'] <- 'Zaïre'
genus_summary$country[genus_summary$country == 'Galapagos'] <- 'Galápagos'
genus_summary$country[genus_summary$country == 'Quebec'] <- 'Québec'
genus_summary$country[genus_summary$country == 'Gambia'] <- 'Gambia, The'
genus_summary$country[genus_summary$country == 'Leeward Is.'] <- 'Leeward Is. AB Ant'
genus_summary$country[genus_summary$country == 'South European Russi'] <- 'South European Russia'
genus_summary$country[genus_summary$country == 'North European Russi'] <- 'North European Russia'
genus_summary$country[genus_summary$country == 'Central European Rus'] <- 'Central European Russia'
genus_summary$country[genus_summary$country == 'Northwest European R'] <- 'Northwest European Russia'

head(genus_summary)

##add data for northern canada provinces since there is not any
#create data frames for each region
Yukon <- data.frame('Yukon', 'NA', 0)
Nunavut <- data.frame('Nunavut', 'NA', 0)
NWT <- data.frame('Northwest Territories', 'NA', 0)

#name the data frame columns
names(Yukon) <- c("country", "genus", "gen_per_country")
names(Nunavut) <- c("country", "genus", "gen_per_country")
names(NWT) <- c("country", "genus", "gen_per_country")

#combine datasets
genus_summary <- rbind(genus_summary, Yukon)
genus_summary <- rbind(genus_summary, Nunavut)
genus_summary <- rbind(genus_summary, NWT)

# Make the map -----------------------
# code derived from:
# https://plotly.com/r/choropleth-maps/#customize-choropleth-chart

#Download tdwg level 3 botanical regions map
# download.file(url = "https://github.com/tdwg/wgsrpd/archive/master.zip",
#                             destfile = "wgsrpd-master.zip")
# unzip(zipfile = "wgsrpd-master.zip")
# unlink("wgsrpd-master.zip")

#import geojson file
geojson <- rjson::fromJSON(file="wgsrpd-master/geojson/level3.geojson")

g <- list(
  fitbounds = "locations",
  visible = FALSE
)

# Creating a map using plotly ------

# first format the genera with linebreaks for the hover tooltip
# Function to insert line breaks into long list of genera
insert_line_breaks <- function(text, width = 27) {
  sapply(strsplit(text, ""), function(x) {
    paste0(x, collapse = "")  # Combine the text into a single string
  }) %>%
    strwrap(width = width) %>%
    paste(collapse = "<br>")
}

# Apply the function to the genus data
genus_summary$genus_wrapped <- sapply(genus_summary$genus, insert_line_breaks)

#create appropriate color scale
colorscale <- list(
  list(NA, 'gray'),       # NA values will be gray
  list(0.0, 'blue'), # Adjust the starting color for the first valid data point
  list(0.5, 'green'),
  list(1, 'red')
)
# Create an empty plot
fig <- plot_ly()

# Begin to add features from genus_summ
fig <- fig %>% add_trace(type = "choropleth",
                         geojson = geojson,
                         locations = genus_summary$country,
                         z = genus_summary$gen_per_country,
                         customdata = genus_summary$genus_wrapped,
                         hovertemplate = paste("<b>%{location}</b><br>",
                                                "<b>Number of Genera:</b> %{z}<br>",
                                                "<b>Genera:</b> %{customdata}",
                                                "<extra></extra>"
                                                ),
                         colorscale='Viridis',
                         reversescale =T,
                         tickvals = c(0, 10, 20, 30, 40),  # Adjust based on your data range
                         ticktext = c('NA', '10', '20', '30'),
                         #colors = "Purples",

                         featureidkey = "properties.LEVEL3_NAM"  # Adjust this based on your actual geojson properties to match your df$country
)

# Format plot
fig <- fig %>% layout(
  geo = g
)

fig <- fig %>% colorbar(title = "Genera per Region")
fig <- fig %>% layout(title = list(text = paste0("Solanaceae Genus Summary Worldwide<br>",
                                                 "<sup>Native to Botanical Country</sup>")),
                      annotations = list(
                        x = 1,
                        y = -0.1,
                        align = 'right',
                        text = "Source: <https://powo.science.kew.org/> via expowo 2.0",
                        showarrow = FALSE,
                        xref = 'paper',
                        yref = 'paper',
                        xanchor = 'right',
                        font = list(size = 10))
)


#Or save a parital bundle to reduce size
save_widget <- function(p, f) {
  owd <- setwd(dirname("C:/solgen"))
  on.exit(setwd(owd))
  htmlwidgets::saveWidget(p, f)
  mb <- round(file.info(f)$size / 1e6, 3)
  message("File is: ", mb," MB")
}

# save the widget to .html -----
#use 'partial bundle' to reduce file size
save_widget(partial_bundle(fig, type = "auto", local = TRUE, minified = TRUE ), "C:/solgen/solgenmap_smaller.html")
zip("solmap_smaller.zip", c("solgenmap_smaller.html", "solgenmap_smaller_files"))

# Upload to github pages
# For instructions see https://towardsdatascience.com/how-to-create-a-plotly-visualization-and-embed-it-on-websites-517c1a78568b#:~:text=Host%20to%20GitHub%20Pages&text=Upload%20the%20index.,'%20select%20'master%20branch'%20.&text=And%20the%20one%20I%20just,publish%2Dplotly%2Dwebsite%2F.
# & https://docs.github.com/en/pages/getting-started-with-github-pages/creating-a-github-pages-site

#You can now embed the url into a website where there is an 'embed html' option.

