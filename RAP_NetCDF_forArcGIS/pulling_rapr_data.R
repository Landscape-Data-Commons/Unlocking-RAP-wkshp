#### grabbing RAP data for SRM workshop
## goal is to grab the RAP rasters to work with in ArcGIS

library(terra)
library(sf)
library(rapr)
library(ncdf4)
library(tidyterra)


# read in the ROI
base_folder = ""
roi = st_read(paste0(base_folder, "RAP_wkshp_ROI.shp"))
# buffer the ROI 
roi_buffered = st_buffer(roi, dist = 1000)

# transform the coordinate reference system of the ROI to match RAP 
roi_tran = st_transform(roi_buffered, crs = "EPSG:4326")

# pull RAP data using rapr package
rap_cover = rapr::get_rap(
  vect(roi_tran),
  version = "v3",
  product = "vegetation-cover",
  year = c(2008:2018),
  progress = TRUE)

# Remove functional groups I don't want
rap_cover_cleaned <- rap_cover %>%
  select(-contains("tree_cover"), -contains("litter_cover"), -contains("annual_forb_and_grass_cover"))


## prepare raster stacks by functional group
# Subset rap_cover for the years 2008 to 2016 (use column names that start with "shrub_cover_", "bare_ground_cover_", and "perennial_forb_and_grass_cover_")
years_to_process <- 2008:2016
# Select the relevant columns for shrub, bare ground, and perennial forbs and grasses (subset only the desired years)
shr_cover <- rap_cover |> select(starts_with("shrub_cover_")) |> select(any_of(paste0("shrub_cover_", years_to_process, "_v3")))
# Define the time dimension (2008-2016 as Date objects)
years <- as.Date(paste0(years_to_process, "-01-01"))
# Add the time dimension to the stack
time(shr_cover) <- years
# Define output file path for NetCDF
output_folder <- "D:/SRM25/RAP_workshop/"
# Export raster stack as NetCDF with time dimension
writeCDF(shr_cover, filename = paste0(output_folder, "shrub_2008_2016.nc"),
         varname = "shrub_cover_RAPv3", zname = "time", 
         overwrite = TRUE)

# repeat for bare ground and PFG
# Select the relevant columns for shrub, bare ground, and perennial forbs and grasses (subset only the desired years)
bare_ground_cover <- rap_cover |> select(starts_with("bare_ground_cover_")) |> select(any_of(paste0("bare_ground_cover_", years_to_process, "_v3")))
# Define the time dimension (2008-2016 as Date objects)
# Add the time dimension to the stack
time(bare_ground_cover) <- years
# Export raster stack as NetCDF with time dimension
writeCDF(bare_ground_cover, filename = paste0(output_folder, "bareground_2008_2016.nc"),
         varname = "bareground_cover_RAPv3", zname = "time", 
         overwrite = TRUE)

# Select the relevant columns for shrub, bare ground, and perennial forbs and grasses (subset only the desired years)
perennial_forb_and_grass_cover <- rap_cover |> select(starts_with("perennial_forb_and_grass_cover_")) |> select(any_of(paste0("perennial_forb_and_grass_cover_", years_to_process, "_v3")))
# Add the time dimension to the stack
time(perennial_forb_and_grass_cover) <- years
# Export raster stack as NetCDF with time dimension
writeCDF(perennial_forb_and_grass_cover, filename = paste0(output_folder, "pfg_2008_2016.nc"),
         varname = "pfg_cover_RAPv3", zname = "time", 
         overwrite = TRUE)


# all cover
all_cover = c(bare_ground_cover, perennial_forb_and_grass_cover, shr_cover)
writeCDF(all_cover, filename = paste0(output_folder, "pfg_2008_2016.nc"),
         varname = "pfg_cover_RAPv3", zname = "time", 
         overwrite = TRUE)
