#### SETUP #####################################################################
# RAP API documentation page:
# https://rangelands.app/support/71-api-documentation

##### Required packages --------------------------------------------------------
# Uncomment and run these if you're missing the packages.
# You don't need to use library() if you're specifying the package a function is
# from, e.g. sf::st_read() or dplyr::filter() versus just st_read() or filter().

# install.packages("tidyverse")
# library(tidyverse)
# install.packages("sf")
# library(sf)
# install.packages("httr")
# library(httr)
# install.packages("stars")
# library(stars)
# remotes::install_github(repo = "landscape-data-commons/trex")
# library(trex)
# remotes::install_github(repo = "brownag/rapr")
# library(rapr)

##### Parameters ---------------------------------------------------------------
# This should be the full path to the folder containing your polygons.
# If your polygons are a feature class in a geodatabase, this should instead be
# the path to the geodatabase.
# I'd recommend always using a full path instead of a relative one,
# so something that includes :
# "C:/Users/Nelson/Documents/Projects/SRM_2025/data"
data_path <- "C:/Users/Nelson/Documents/Projects/other/SRM_2025/Unlocking-RAP-wkshp/data"

# The filename for the shapefile in the data folder.
# If this were a feature class in a geodatabase, it would be the name of the
# feature class (also called a layer).
polygon_filename <- "RAP_wkshp_ROI"

# The server has a mask stored that represents the extent of areas classified as
# cropland, developed, or water. This allows it to exclude those areas when
# returning data.
exclude_croplands_development_water <- TRUE

# Which years are relevant?
treatment_year <- 2010


#### THE "EASY" WAY ############################################################
# The package trex contains a function called fetch_rap() which will take a
# polygon sf object and return tabular data from RAP.

# The package rapr contains a function called get_rap() which will take a
# polygon sf object and return raster data within the bounding box.

##### Reading ------------------------------------------------------------------
# Read the polygons into the environment so we can work with them.
aoi_polygon <- sf::st_read(dsn = data_path,
                           layer = polygon_filename)

# And just to see that it looks like what we expect.
ggplot2::ggplot() +
  ggplot2::geom_sf(data = aoi_polygon)

##### Querying -----------------------------------------------------------------
# This will retrieve the cover data for the current area of interest across all
# available years.
cover_data_all <- trex::fetch_rap(polygons = aoi_polygon,
                                  data_type = "cover",
                                  mask = exclude_croplands_development_water)

# This will retrieve the cover data for the current area of interest only in the
# year following treatment.
cover_data_posttreatment <- trex::fetch_rap(polygons = aoi_polygon,
                                            data_type = "cover",
                                            mask = exclude_croplands_development_water,
                                            year = treatment_year + 1)

# You can also ask for cover with meteorological data, production, production in
# 16-day increments.
covermeteorology_data_all <- trex::fetch_rap(polygons = aoi_polygon,
                                             data_type = "covermeteorology",
                                             mask = exclude_croplands_development_water)

production_data_all <- trex::fetch_rap(polygons = aoi_polygon,
                                       data_type = "production",
                                       mask = exclude_croplands_development_water)

production16day_data_all <- trex::fetch_rap(polygons = aoi_polygon,
                                            data_type = "production16day",
                                            mask = exclude_croplands_development_water)


# And you can snag raster data with rapr::get_rap(), in this case the biomass
# data for the year after treatment.
rapr::get_rap(x = aoi_polygon,
              years = c(treatment_year + 1),
              product = "vegetation-biomass",
              filename = file.path(data_path,
                                   "test_biomass.tif"))

biomass_raster <- stars::read_stars(.x = file.path(data_path,
                                                   "test_biomass.tif"))

ggplot2::ggplot() +
  stars::geom_stars(data = biomass_raster)

#### THE INVOLVED WAY ##########################################################
# This is what the function fetch_rap() is doing internally. You may find that
# for some workflows you'd rather adapt this code than simply use the function.

fetch_rap <- function(polygons,
                      data_type = "cover",
                      mask = TRUE,
                      year = NULL) {
  #### SANITIZATION ############################################################
  # There are multiple URLs that serve as endpoints for the RAP API. Each one
  # serves a different kind of data. We'll use this to determine which one the
  # user needs to point the query at.
  endpoints <- c("cover" = "https://us-central1-rap-data-365417.cloudfunctions.net/coverV3",
                 "covermeteorology" = "https://us-central1-rap-data-365417.cloudfunctions.net/coverMeteorologyV3",
                 "production" = "https://us-central1-rap-data-365417.cloudfunctions.net/productionV3",
                 "production16day" = "https://us-central1-rap-data-365417.cloudfunctions.net/production16dayV3")
  
  # Stop and tell the user if they're asking for an unrecognized data type.
  # Otherwise, move on and we'll reference their selected endpoint when
  # submitting the query below.
  if (!(tolower(data_type) %in% names(endpoints))) {
    stop(paste0("'", data_type, "' is not a valid value for data_type. Valid values are: ",
                paste(names(endpoints),
                      collapse = ", ")))
  }
  
  # The RAP API can return a single year's data or all available years'.
  # If you want all years, then the year value in the query should be "null".
  # If you want only a specific year, it must be 1986 or later.
  last_available_year <- format(Sys.Date(), "%Y") |>
    as.numeric() - 1
  if (is.null(year)) {
    year <- "null"
  } else {
    if (length(year) > 1) {
      stop(paste0("year must be a single four-digit numeric year from 1986 to ", last_available_year, "."))
    } else if (!(year %in% 1986:last_available_year)) {
      stop(paste0("year must be a single four-digit numeric year from 1986 to ", last_available_year, "."))
    } else {
      year <- as.integer(year)
    }
  }
  
  
  #### CONVERSION ##############################################################
  # First up, we make sure that the mask and year variables have been added to
  # the polygons and that any other non-geometry variables have been removed
  # to avoid passing things to the API that it can't understand.
  polygons <- dplyr::mutate(.data = polygons,
                            # In R the logical values are represented by TRUE
                            # and FALSE but the geoJSON format expects true and
                            # false, so this makes sure that we won't get an
                            # error in conversion.
                            mask = tolower(as.character(mask)),
                            year = year) |>
    # This removes any variables besides mask, year, and the geometry of the
    # polygons. This is because other variables will not be understood by the
    # API but may be present in the polygons provided.
    dplyr::select(.data = _,
                  tidyselect::all_of(c("geometry",
                                       "mask",
                                       "year"))) 
  
  # Before we can convert these into a geoJSON, we need to be absolutely sure
  # that the coordinate reference system for the polygons is WGS84 because
  # that's what the API expects the coordinates to be in.
  polygons_geojson <- sf::st_transform(x = polygons,
                                       # This is the code for WGS84.
                                       crs = "EPSG:4326") |>
    # This creates the geoJSON from the polygons and their associated attributes
    # (which are just the mask and year variables at this point).
    geojsonsf::sf_geojson(sf = _,
                          atomise = TRUE) |>
    # Because the geoJSON is a character string, we need to make sure that it
    # hasn't been formatted to indicate the the mask and year values are
    # themselves character strings because they're supposed to be logical and
    # integer, respectively.
    # So this removes any quotation marks around those that would tell the API
    # that they're character strings.
    stringr::str_replace_all(string = _,
                             pattern = c("\"true\"" = "true",
                                         "\"false\"" = "false",
                                         "\"null\"" = "null"))
  
  #### QUERYING ################################################################
  # Finally time to submit the query to the API.
  # This will return a JSON with a lot of metadata about the query and data, but
  # most importantly contains the data itself, albeit in a raw format.
  rap_json <- httr::RETRY(verb = "POST",
                          url = endpoints[tolower(data_type)],
                          body = polygons_geojson,
                          httr::content_type_json())
  
  #### REFORMATTING ############################################################
  # This takes the data content from the API and converts it from a raw format
  # into a usable list.
  raw_data_list <- httr::content(x = rap_json,
                                 as = "parsed")
  
  # The data are in raw_data_list$properties, but include the mask and year
  # values that we supplied to the API, so we'll remove those to keep only the
  # values that we asked for.
  # This just asks for only the parts of raw_data_list that aren't named mask or
  # year.
  raw_data_list <- raw_data_list$properties[[setdiff(x = names(raw_data_list$properties),
                                                     y = c("mask", "year"))]]
  
  # raw_data_list has a bunch of vectors in it. The first one is the variable
  # names and the rest of them are the values for those variables.
  # We can go through all of the vectors of values, turn each into its own data
  # frame, and then bind them all together into a single output data frame.
  data <- lapply(X = raw_data_list[-1],
                 var_names = unlist(raw_data_list[[1]]),
                 FUN = function(X, var_names){
                   output <- as.data.frame(x = X)
                   names(output) <- var_names
                   output
                 }) |>
    dplyr::bind_rows()
  
  # Last step is to make sure that the variables are all the data type that we
  # expect because they're all character strings as a result of the steps we
  # took above to get them out of the original returned object.
  # If date is present, it'll be turned from string to date.
  # If year or doy (day of year) are present, they'll be turned into integers.
  # Whatever cover or meterological variables are present will be turned into
  # numeric.
  data <- dplyr::mutate(.data = data,
                        dplyr::across(.cols = tidyselect::any_of(c("date")),
                                      .fns = as.Date),
                        dplyr::across(.cols = tidyselect::any_of(c("year",
                                                                   "doy")),
                                      .fns = as.integer),
                        dplyr::across(.cols = tidyselect::any_of(c("AFG",
                                                                   "PFG",
                                                                   "HER",
                                                                   "SHR",
                                                                   "TRE",
                                                                   "LTR",
                                                                   "BGR",
                                                                   "annualTemp",
                                                                   "annualPrecip")),
                                      .fns = as.numeric))
  
  # Finally, we spit the data out for the user.
  data
}
