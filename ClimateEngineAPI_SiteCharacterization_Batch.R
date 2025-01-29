library(httr2)
library(tidyverse)
library(nhdplusTools)
library(sf)
library(spData)

# ------------------------- Define parameters for Climate Engine API -------------------------------------

# Define root url for Climate Engine API
root_url <- 'https://api-dev.climateengine.org/'

# Define Climate Engine API key
# Request a free API key from Climate Engine at https://support.climateengine.org/article/36-requesting-an-authorization-key-token
key = 'key_here'


# -------------------------- Generate report links for HUC10 watershed -----------------------------------

# Get HUC10s for Colorado
data(us_states)
aoi <- us_states |>
  filter(NAME == 'Colorado')
aoi_hucs <- get_huc(aoi, type = 'huc10')

# Get a list of HUC IDs
aoi_huc_ids <- aoi_hucs$huc10[1:10]

# Function to loop over HUCs and generate reports
generate_report <- function(huc_id) {
  
  # Subset a single HUC10 and visualize location
  huc <- aoi_hucs |> filter(huc10 == huc_id)
  print(paste("Making request for HUC:", huc$name))
  
  # Climate Engine endpoint for the reports
  endpoint <- 'reports/site_characterization/feature_collection' 
  
  # Generate list of parameters for API request
  params = list(
    user_email = 'eric.jensen@dri.edu', # Standard email address
    site_name = huc$name |> unlist() |> str_sub(1,35), # Pass HUC name as site_name; limit 35 characters
    site_type = 'HUC10 Watershed', # Limit 50 characters
    site_description = 'Report for Watershed Condition Assessment', # Limit 50 characters
    mask_ownership = 'BLM', # Select one from ['None', 'BIA', 'BLM', 'DOD', 'FWS', 'NPS', 'USFS']
    mask_landcover = 'True', # Boolean
    feature_collection_asset_id = "USGS/WBD/2017/HUC10",
    sub_choices = huc$huc10 |> unlist(),
    filter_by = 'huc10',
    batch = 'True' # Boolean
  )
  
  # Pass request to return HTML as a character string
  response <- request(base_url = paste0(root_url, endpoint)) |>
    req_url_query(query = !!!params) |>
    req_headers(Authorization = key) |>
    req_perform() |>
    resp_body_json()
  
  return(response)
}

response_list <- map(aoi_huc_ids, generate_report)


# -------------------------------- Write out and unzip the reports ----------------------------------------

# Convert nested list to dataframe
response_df <- response_list |>
  map_dfr(~ as_tibble_row(.))

# Define local directory to store downloaded files
download_dir <- "Reports_Out/"
dir.create(download_dir, showWarnings = FALSE)

# Pull down the reports zip files and mutate column for whether the report was successful or not
response_df <- response_df %>%
  rowwise() %>%
  mutate(
    local_path = {
      file_name <- basename(`Report link`)
      dest_path <- file.path(download_dir, file_name)
      
      # Try downloading file, store NA if it fails
      tryCatch(
        {
          download.file(`Report link`, destfile = dest_path, mode = "wb", quiet = TRUE)
          dest_path  # Return path if successful
        },
        error = function(e) {
          NA_character_  # Store NA if an error occurs
        }
      )
    },
    download_success = !is.na(local_path)  # TRUE if successful, FALSE if failed
  )

# Extract ZIP files
zip_files <- list.files(download_dir, full.names = TRUE)
for (zip_file in zip_files) {
  print(zip_file |> str_replace('.zip', ''))
  dir.create(zip_file |> str_replace('.zip', ''), showWarnings = FALSE)
  unzip(zip_file, exdir = zip_file |> str_replace('.zip', ''))
}
