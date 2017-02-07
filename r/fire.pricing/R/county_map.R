library(sp)
library(maps)
library(maptools)
library(choroplethr)
library(choroplethrMaps)

#' This function draws a chorpleth map at either country or state level for the continental USA
#'
#' @param latitude Latitude of the location
#' @param longitude Longitude of the location
#' @param value The values that will be aggregated and displayed on the map
#' @param level The level at which values should be aggregated. Either 'state' or 'county'
#' @return A list of results:
#'  - plot is a ggplot object. If no plot could be drawn then this returns NULL
#'  - count_not_geocoded is a count of the locations that could not be geocoded
#'  - value_not_geocoded is the sum of the values that could not be geocoded
#' @details This function aggregates values to the selected level and returns a chorpleth ggplot object
#' displaying the results
#' @examples county_state_map(42, -99, 1e6, "county")
county_state_map <- function(latitude, longitude, value, level = "county"){

  ###http://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r/8751965#8751965
  tryCatch({

    # The single argument to this function, pointsDF, is a data.frame in which:
    #   - column 1 contains the longitude in degrees (negative in the US)
    #   - column 2 contains the latitude in degrees
    latlong2state_county <- function(pointsDF, level = "county") {
      # Prepare SpatialPolygons object with one SpatialPolygon
      # per state (plus DC, minus HI & AK)
      states <- map(tolower(level), fill=TRUE, col="transparent", plot=FALSE)
      IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
      states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                       proj4string=CRS("+proj=longlat +datum=WGS84"))

      # Convert pointsDF to a SpatialPoints object
      pointsSP <- SpatialPoints(pointsDF,
                                proj4string=CRS("+proj=longlat +datum=WGS84"))

      # Use 'over' to get _indices_ of the Polygons object containing each point
      indices <- over(pointsSP, states_sp)

      # Return the state names of the Polygons object containing each point
      stateNames <- sapply(states_sp@polygons, function(x) x@ID)
      retVal <- data.frame(name = stateNames[indices],
                           stringsAsFactors = F)

      if(level == "county"){
        retVal <- retVal %>%
          dplyr::left_join(maps::county.fips %>% mutate(polyname = as.character(polyname)),
                     by = c("name" = "polyname"))
      }

      return(retVal)
    }

    locs <- data.frame(x = longitude,
                       y = latitude,
                       value = value)

    locs <- cbind(locs, latlong2state_county(locs, level = level))

    plot_locs <- locs[!is.na(locs$name),]

    if(nrow(plot_locs) == 0){
      p = NULL
    } else {
      if(tolower(level) == "county"){
        p = .draw_county_map(plot_locs)
      } else {
        p = .draw_state_map(plot_locs)
      }
    }

    retVal = list(plot = p,
                  count_not_geocoded = nrow(locs[is.na(locs$name),]),
                  values_not_geocoded = sum(locs$value[is.na(locs$name)]))



    return(retVal)
  },
  error = function(e){
    stop("The map could not be drawn.")
  })
}

.draw_state_map <- function(locations){
  # highlight a county
  highlight_state = function(state, colour = "yellow")
  {
    library(choroplethrMaps)
    data(state.map, package="choroplethrMaps", envir=environment())
    df = state.map[state.map$region %in% state, ]
    geom_polygon(data=df, aes(long, lat, group = group), color = colour, fill = NA, size = 0.6)
  }


  p.df <- locations %>%
    dplyr::group_by(region = name) %>%
    dplyr::summarise(value = sum(value)/1e6)

  data(continental_us_states)

  p <- choroplethr::state_choropleth(p.df,
                                     zoom = continental_us_states,
                                     num_colors = 1,
                                     reference_map = T) +
    scale_fill_gradient(name = "Values (m)",
                        low = "green",
                        high = "red",
                        na.value = "transparent",
                        # breaks = round(quantile(p.df$value, seq(0,1,0.2)), -5),
                        label = scales::dollar_format(),
                        guide = "legend") +
    highlight_state(p.df$region, colour = "black") +
    theme(legend.position = "right",
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10))

  return(p)
}


.draw_county_map <- function(locations){
  # highlight a county
  highlight_county = function(county_fips, colour = "yellow")
  {
    library(choroplethrMaps)
    data(county.map, package="choroplethrMaps", envir=environment())
    df = county.map[county.map$region %in% county_fips, ]
    geom_polygon(data=df, aes(long, lat, group = group), color = colour, fill = NA, size = 0.6)
  }


  p.df <- locations %>%
    dplyr::group_by(region = fips) %>%
    dplyr::summarise(value = sum(value)/1e6)

  states <- unique(tolower(str_match(string = locations$name, pattern = "([\\s\\S]+),")[,2]))
  states <- states[!is.na(states)]

  counties <- unique(locations$name)
  counties <- counties[!is.na(counties)]

  p <- choroplethr::county_choropleth(p.df,
                                      state_zoom = states,
                                      num_colors = 1,
                                      reference_map = T) +
    scale_fill_gradient(name = "Values ($m)",
                        low = "green",
                        high = "red",
                        na.value = "transparent",
                        # breaks = round(quantile(p.df$value, seq(0,1,0.2)), -5),
                        label = scales::dollar_format(),
                        guide = "legend") +
    # scale_color_gradient(na.value = "green") +
    highlight_county(p.df$region, colour = "black") +
    theme(legend.position = "right",
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10))

  return(p)
}

# t0 <- proc.time()
# xx <- find_rms_account(sequel_id = "CG306H16A000", server = "GBLONTPD111") #, edms = "RMS_EDM_QTE_16_10")
# print(proc.time() - t0)
#
# t0 <- proc.time()
# xx <- find_rms_account(sequel_id = "CG299N16A000", server = "GBLONTPD111") #, edms = "RMS_EDM_QTE_16_10")
# print(proc.time() - t0)
#
# t0 <- proc.time()
# xx <- find_rms_account(sequel_id = "CB130V16A000", server = "GBLONTPD111") #, edms = "RMS_EDM_QTE_16_10")
# print(proc.time() - t0)
#
# yy <- get_rms_fire_engine_locations(edm = xx$edm[1],
#                                     server = "GBLONTPD111",
#                                     account = xx$ACCGRPNUM[1])
#
#
#
# county_state_map(longitude=  yy$LONGITUDE,
#            latitude = yy$LATITUDE,
#            value = yy$TIV,
#            level = "county")
#
# county_state_map(longitude = yy$LONGITUDE,
#                  latitude = yy$LATITUDE,
#                  value = yy$TIV,
#                  level = "state")
#
#
# longitude = yy$LONGITUDE
# latitude = yy$LATITUDE
# value = yy$TIV
