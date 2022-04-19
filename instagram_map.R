# sp -- classes and methods for spatial data, 
# rgdal -- importing & exporting geospatial data formats,
# rgeos -- for handling operations on topologies,
# raster -- for raster data,
# tmap -- thematic maps,
# ggplot2 -- visualisation,
# ggmap -- base maps: Google Maps, Open Street MAps,
# leaflet -- intaractive maps,
# spatstat -- spatial point pattern analysis,
# gstat -- geostatistical modeling

token <- "***"
library(httr)
require(httr)
library(RJSONIO)
library(rjson)
library(RCurl)
library(readr)
limit <- as.numeric(Sys.time())

medias_data <- read_csv("~/summer_2017/dataset1.csv", 
                        col_types = cols(X1 = col_skip()))
medias_data <- medias_data[-c(1, 2),] 

# Loop for Instagram parsing by longitude and latitude numbers of geotags
for(i in 1:4694){
  try({
    l <- l + 1
    # Longitude of geotag
    lng <- df[l,]$x
    # Latitude of geotag
    lat <- df[l,]$y
    lng <- plyr::round_any(lng, accuracy=.000001, f=floor)
    lat <- plyr::round_any(lat, accuracy=.000001, f=floor)
    lng <- as.character(lng)
    lat <- as.character(lat)
    if (lat != "") {
      api <- paste("https://api.instagram.com/v1/media/search?lat=", lat, "&lng=", lng, "&distance=5&access_token=", token, "&max_timestamp=", limit, sep="")
    }
    print(api)
    
    #Getting API url
    raw_data <- getURL(api, ssl.verifypeer = FALSE)
    
    #Getting data from API url
    data <- fromJSON(raw_data)
    
    # Getting data frame from raw data
    if (length(data$data) != 0){
      for(media in 1:20){
        try({
          # Getting id of the post
          media_id <- data$data[[media]]$id
          
          # Getting latitude of geotag
          media_lat <- data$data[[media]]$location$latitude
          
          # Getting longitude of geotag
          media_lng <- data$data[[media]]$location$longitude
          
          # Getting location name of geotag
          media_location_name <- data$data[[media]]$location$name
          
          #Getting geotag id
          media_location_id <- data$data[[media]]$location$id
          
          # Getting user id
          media_user_id <- data$data[[media]]$user$id
          
          # Getting tags from the post
          media_tags <- paste(data$data[[media]]$tags, collapse=", ")
          
          # Getting number of likes
          media_likes <- data$data[[media]]$likes$count
          
          # Getting text under the post with geotag
          media_text <- data$data[[media]]$caption$text
          
          # Writing blank text if null data
          if(is.null(media_text)){media_text <- '' }
          
          # Getting time and date of the post with geotag
          media_created_time <- as.POSIXct(as.numeric(data$data[[media]]$created_time), origin="1970-01-01")   
          
          # Getting used Instagram filters
          media_filter <- data$data[[media]]$filter
          
          # Getting unique url of the post with geotag
          media_link <- data$data[[media]]$link
          
          # Getting number of the comments to the post
          media_comments_count <- data$data[[media]]$comments$count
          
          # Getting comments to the post
          media_comments_list <- c()
          
          # Getting texts of the comments
          if(length(data$data[[media]]$comments$data) > 0){
            for(comment in 1:length(data$data[[media]]$comments$data)){
              media_comments_list <- c(media_comments_list, data$data[[media]]$comments$data[[comment]]$text)
            }
          } else {
            media_comments_list <- ""
          }
          media_comments_list <- paste(media_comments_list, collapse="_comment_end; ")
          
          # Getting image of the post by url
          media_image <- data$data[[media]]$images$standard_resolution$url
          date <- Sys.time()
          
          # Creating mergeddata frame
          data1_medias <- data.frame(media_id, media_lat, media_lng, media_location_name, media_location_id, media_user_id, media_filter, media_tags, media_likes, media_text, media_created_time, media_link, media_comments_count, media_comments_list,  media_image)
          data1_medias$media_text <- gsub('\n', '', data1_medias$media_text)
          medias_data <<- rbind(medias_data, data1_medias)
          
        })}}
  })
}

# Reset loop
l = 0

# Deleting duplicated data
medias_data <- medias_data[!duplicated(medias_data$media_link),]

# Creating file with parsed data
write.csv(medias_data,"Saint_Petersburg.csv")

# Loading packages for mapping geotags
library(dplyr)
library(geosphere)

# Getting the map of Saint Petersburg
geocode("Russia, St Petersburg", output = "more")

# Cutting the range of latitude and longitude of the data within the city
df <- data.frame(
  x = rnorm(10000, 30.3351, .2),
  y = rnorm(10000,  59.93428, .2)
)

# Filtering the data by range of map of the city
library(dplyr)
df <- filter(df, x > 30.09033 & x < 30.55978 & y > 59.74522 & y < 60.08967)

# Layering geotags red points to the map of the city
ggmap(get_map("Russia, St Petersburg"), base_layer = ggplot(aes(x = x, y = y), data = df)) +
  geom_point(colour = "red")
