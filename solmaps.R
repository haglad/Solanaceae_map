#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
####                       Making maps for Solanaceae genera                ####
#*               Using: expowo from  https://dboslab.github.io/expowo/        #
#*                         using the refrence page:                           #
#*              "Mapping global distribution for any taxonomic level"         #
#*                           2 May 2024                                       #
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_



# set wd
setwd("C:/solgen")

#---- Get solanaceae taxa info from expowo -----

#---- Load the library ----
# Instal developer version
# install.packages("devtools")
# devtools::install_github("DBOSlab/expowo")
# library(expowo)
#
# # install.packages("rnaturalearthdata")
# library(rnaturalearthdata)
# library(ggplot2)


#---- Mine sp dist from Powo ------
# ?powoSpecies
#
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



#---- Load the datasets and format for ggplot -----------------------------------

# load the library
library(terra)
library(magrittr)
library(sf)
library(RColorBrewer)
library(geodata)
library(sp)
library(countrycode)
library(dplyr)
library(stringr)
library(tidyr)


##create botanical regions map as used in expowo code
## from expowo function powoMap
# download.file(url = "https://github.com/tdwg/wgsrpd/archive/master.zip",
#               destfile = "wgsrpd-master.zip")
# unzip(zipfile = "wgsrpd-master.zip")
# unlink("wgsrpd-master.zip")
## rename files:
# file.rename("wgsrpd-master", "WGSRPD")
# file.rename("WGSRPD/109-488-1-ED", "WGSRPD/book-1-ED")
# file.rename("WGSRPD/README.md", "WGSRPD/README.txt")
## unlink files that are not needed:
# unlink("WGSRPD/level1", recursive = TRUE)
# unlink("WGSRPD/level2", recursive = TRUE)
# unlink("WGSRPD/level4", recursive = TRUE)
# unlink("WGSRPD/geojson", recursive = TRUE)

#load the map
world1 <- sf::st_read("WGSRPD/level3/level3.shp")
world1 <- world1[!world1$LEVEL3_NAM %in% "Antarctica", ]
world1 <- st_as_sf(world1)

world1$LEVEL3_NAM
#change country names to match powo list, which uses the most accurate names
world1$LEVEL3_NAM[world1$LEVEL3_NAM == 'Surinam'] <- 'Suriname'
world1$LEVEL3_NAM[world1$LEVEL3_NAM == 'Kirgizistan'] <- 'Kirgizstan'
world1$LEVEL3_NAM[world1$LEVEL3_NAM == 'Zaïre'] <- 'Democratic Republic of the Congo'

#Names changed in the spreadsheet by hand:
#Central African Repu -> Central African Republic
#Can't change Quebec -> Québec in the spreadsheet due to encoding, so change it after loading it in
#there is an issue with names like 'south/north european russia (estonia, latvia, luthuania etc) -- do we use that language or do we change it to another regional name?

# load in Sol data from expowo download
# i've sent this to you, so you may need to edit the file location
sol_gen <- read.csv("C:/solgen/results_powoSpecies/02May2024/Solanaceae_gen.csv")
colnames(sol_gen)

# Select only taxon_name, bot region, genus, status = accepted
sol_genf <- sol_gen %>% filter(status == 'Accepted') %>%
                    select(genus, status, taxon_name, native_to_botanical_countries)

sol_genf$native_to_botanical_countries <- as.character(sol_genf$native_to_botanical_countries)

#make a dataframe with the country names from the map
botcountry_names <- world1$LEVEL3_NAM
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
    if (country %in% botcountry_names) {
      # Create a new entry with genus, species, and country
      entry <- c(genus = genus, species = species, country = country)
      # Add the entry to combined_data list
      combined_data <- c(combined_data, list(entry))
    }
  }
}

# Convert the list of entries to a data frame
combined_data_df <- do.call(rbind, combined_data)


#aggregate the data

#for species (if a species map is desired)
#species_summary <- aggregate(species ~ country, data = combined_data_df, FUN = function(x) paste(unique(x), collapse = ", "))

#for genus
genus_summary <- aggregate(genus ~ country, data = combined_data_df, FUN = function(x) paste(unique(x), collapse = ", "))

#find the number of genera per country
genera_split <- strsplit(genus_summary$genus, ", ")
genera_counts <- sapply(genera_split, function(x) length(unique(x)))
genus_summary$gen_per_country <- genera_counts

# Left join world data with species summary
world <- dplyr::left_join(world1, genus_summary, by = c("LEVEL3_NAM" = "country"))


# Convert world sf object to Simple Features (sf) format
world.spdf <- methods::as(world, 'Spatial')
world.spdf@data$id <- row.names(world.spdf@data)

#this creates a group ID that is needed to properly create the map
#feature from SpatilPolygonsDataframe was depreceated in broom 1.0.4
#that is why we upload an older version of broom.
# install.packages(
#   "https://cran.r-project.org/src/contrib/Archive/broom/broom_1.0.5.tar.gz",
#   repos = NULL, type = "source"
# )

#library(broom)
world_tidy <- broom::tidy(world.spdf)

# Join the tidy data with the original data
world_tidy <- dplyr::left_join(world_tidy, world.spdf@data, by = "id")
world_tidy_df <- as.data.frame(world_tidy)

#Add accents to country names where needed, as csv encoding clears it out
world_tidy_df$LEVEL3_NAM[world_tidy_df$LEVEL3_NAM == 'Quebec'] <- 'Québec'

#rename genera per country
world_tidy_df <- world_tidy_df %>% rename("Genera Count" = gen_per_country)

# create tooltips and onclick events
# this is the little floating highlight bit
# first for the label of the country
botreg <- sprintf("<b>%s</b> <br>", as.character(world_tidy_df$LEVEL3_NAM))

#lable of the table of genera
table_ <- sprintf("<b>Genera:</b> %s <br>",
                  as.character(world_tidy_df$genus))

#bind them all together
world_tidy_df$labs <- paste0(botreg, table_)



# Make map using ggplot and plotly  ----------
library(plotly)
library(tidyverse)
library(ggplot2)


# fix map aspect
map_aspect = function(x, y) {
  x.center <- sum(range(x)) / 2
  y.center <- sum(range(y)) / 2
  x.dist <- ggplot2:::dist_central_angle(x.center + c(-0.5, 0.5), rep(y.center, 2))
  y.dist <- ggplot2:::dist_central_angle(rep(x.center, 2), y.center + c(-0.5, 0.5))
  y.dist / x.dist
}

p <- ggplot()

## format map in ggplot ----
p <- ggplot(data = world_tidy_df,
           aes(x = long, y = lat,
               group = group,
               text = gsub('(.{1,30})(\\s|$)', '\\1\n',labs),                    #the code in quotations modifies the number of characters per line
               fill = `Genera Count`))+
  geom_polygon(color = "black",
               linewidth = 0.05) +
  scale_fill_gradient(name = "Genus Count",
                      low = "white", high = "blue",       #if i remove the above plus, the blue gradient is inverse and more navy than purple
                      limits = c(0, 40)) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    plot.caption = element_text(size = 10, face = "plain"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )+
  coord_fixed(with(world_tidy_df, map_aspect(long, lat)))

## convert to a ggplotly interactive map ------

# create empty plot
fig <- ggplotly()
# add style
fig <- fig %>% ggplotly(p, style(hoverlabel = "text")) %>% #calling the 'text' line from the ggplot code where the labs are saved
              layout(title = list(
                     text = paste0(
                    "Solanaceae Genus Summary Worldwide<br>",
                    "<sup>Native to Botanical Country</sup>"
                  ),
                  font = list(
                    size = 20
                  )
                ),
                annotations = list(
                  x = 1,
                  y = -0.1,
                  text = "Source: <https://powo.science.kew.org/> via expowo 2.0",
                  showarrow = FALSE,
                  xref = 'paper',
                  yref = 'paper',
                  xanchor = 'right',
                  yanchor = 'auto',
                  xshift = 0,
                  yshift = 0,
                  font = list(size = 10)
                ),
                margin = list(t = 50, b = 50)
              )
#printing it in viewer is too big for r, so view it in a html file in a browser window

#save to html file -----
library(htmlwidgets)

saveWidget(fig, "solanaceae_map2.html")

#Or save with files
saveWidget(fig, "solanaceae_map.html", selfcontained = F, libdir = "solmaplib")

#or save zip
zip("solmap.zip", c("solanaceae_map.html", "solmaplib"))

#Or save a parital bundle to reduce size
save_widget <- function(p, f) {
  owd <- setwd(dirname("C:/solgen"))
  on.exit(setwd(owd))
  htmlwidgets::saveWidget(p, f)
  mb <- round(file.info(f)$size / 1e6, 3)
  message("File is: ", mb," MB")
}

save_widget(partial_bundle(fig, type = "auto", local = TRUE, minified = TRUE ), "C:/solgen/solgenmap.html")

