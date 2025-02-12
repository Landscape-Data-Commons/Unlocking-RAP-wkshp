# BEM Feb 2024

# Uses R version 4.4.1

# Processing libraries:
library(terra) # 1.8-15
library(httr) # 1.4.7

# Plotting libraries:
library(ggplot2) # 3.5.1

# Statistical libraries:
library(mblm) # 0.12.1
library(bsts) # 0.9.10

# Purpose: * Demonstrate a RAP analysis using R.
#          * This will focus on the down-stream analysis portion of a RAP workflow.
#          * It will utilize production data, rather than cover data.
#          * Because of current limitations to the RAP API, this will be a
#            time-series rather than spatial analysis.
#          * The 16-day data gives you special insights, and can be very useful
#            next to annual data analyses.

### Import the treatment shapefile

# The AOI is 240 acres (97 ha) prescribed fire in Utah.
# The date of the prescribed fire was September 2010.
unzip('RAP_wkshp_ROI.zip', exdir = tempdir())
aoi <- terra::vect(file.path(tempdir(), 'RAP_wkshp_ROI.shp'))
# The provided AOI is in EPSG:8827
# But, the RAP API requires EPSG:4326
aoi <- terra::project(aoi, 'EPSG:4326')
# The RAP API requires a text representation of the AOI coordinates.
# Here, we pull the coordinates from the shapefile, then format them.
c0 <- terra::crds(aoi)
c0 <- as.data.frame(c0)
c0 <- paste0('[', c0$x, ',', c0$y, ']')
c0 <- paste(c(c0, c0[1]), collapse = ', ')

### Pull production data from the RAP API

# Should cropland, developed land, and water be excluded from the RAP summary?
excl_CDW <- TRUE
# What years of data are we interested in? This code will pull ALL years of data
# for the AOI, but we will filter by year to make the data easier to visualize.
tx_year <- c(2009, 2011, 2015)

# The RAP API requires a very specific format in order to return data:
geojson <- paste('{
                  "type": "Feature",
                  "geometry": {
                    "geodesic": false,
                    "type": "Polygon",
                    "coordinates": [ [', c0, '] ] },
                  "properties": {
                    "mask": ", ', tolower(as.character(excl_CDW)), ', ",
                    "year": null}
                }')

# The `httr` R package provides an interface to the RAP API and allows it to
# be extracted in a mostly-readable form.
rap_json <- httr::RETRY("POST", "https://us-central1-rap-data-365417.cloudfunctions.net/production16dayV3", content_type_json(), body = geojson)
rap_obj <- httr::content(rap_json, "parsed")

# Manipulating the data into a data.frame suitable for analysis.
rap_df <- unlist(rap_obj$properties$production16day)
rap_df <- matrix(data = rap_df, ncol = 6, byrow = T)
rap_df <- as.data.frame(rap_df)
colnames(rap_df) <- rap_df[1, ]
rap_df <- rap_df[-1, ]
rap_df$date <- as.Date(rap_df$date)
rap_df$year <- as.numeric(rap_df$year)
rap_df$doy <- as.numeric(rap_df$doy) - 1
rap_df$AFG <- as.numeric(ifelse(rap_df$AFG == "-99", NA, rap_df$AFG))
rap_df$PFG <- as.numeric(ifelse(rap_df$PFG == "-99", NA, rap_df$PFG))
rap_df$HER <- as.numeric(ifelse(rap_df$HER == "-99", NA, rap_df$HER))
rap_df <- rap_df[rap_df$doy < 365, ]
# Pivot the data into a longer format to make it suitable for plotting.
rap_df <- tidyr::pivot_longer(rap_df, c(AFG, PFG, HER), names_to = 'Cover type', values_to = 'prod')
rap_df <- rap_df[complete.cases(rap_df), ]
rap_df_ALL <- rap_df
rap_df <- rap_df[rap_df$year %in% tx_year, ]

### Analyzing data

# What does the raw data look like?
ggplot(data = rap_df, aes(x = date, y = prod, color = `Cover type`)) +
  geom_line() +
  facet_wrap(~ year, scales = 'free_x') +

  theme_bw() +
  theme(
    axis.text.x = element_text(color = 'black', angle = 45, hjust = 1),
    axis.text.y = element_text(color = 'black')
  ) +
  labs(x = NULL, y = 'Yearly production (lbs/acre)')

# A line smoothing routine can be easier to visualize.
ggplot(data = rap_df, aes(x = date, y = prod, color = `Cover type`)) +
  geom_smooth() +
  facet_wrap(~ year, scales = 'free_x') +

  theme_bw() +
  theme(
    axis.text.x = element_text(color = 'black', angle = 45, hjust = 1),
    axis.text.y = element_text(color = 'black')
  ) +
  labs(x = NULL, y = 'Yearly production (lbs/acre)')

# Let's just look at production data. To help with this, we're going to use
# a Thiel-Sen regression to plot a trend line. This regression differs from
# ordinary least-squares regression in being insensitive to outliers, among
# other benefits.
rap_df$dateInt <- as.integer(as.factor(rap_df$date))
mblm_PFG <- coef(mblm::mblm(prod ~ dateInt, data = rap_df[which(rap_df$`Cover type` == 'PFG'), ], repeated = T))
mblm_PFG_ab <- data.frame(date = rap_df$date, prod_PFG_est = (mblm_PFG[2] * rap_df$dateInt) + mblm_PFG[1])
mblm_PFG_ab <- mblm_PFG_ab[!duplicated(mblm_PFG_ab), ]
rap_df <- dplyr::left_join(rap_df, mblm_PFG_ab, by = 'date')

ggplot(data = rap_df[which(rap_df$`Cover type` == 'PFG'), ]) +
  geom_line(aes(x = date, y = prod)) +
  geom_line(aes(x = date, y = prod_PFG_est), linetype = 'dashed') +
  facet_wrap(~ year, scales = 'free_x') +

  theme_bw() +
  theme(
    axis.text.x = element_text(color = 'black', angle = 45, hjust = 1),
    axis.text.y = element_text(color = 'black')
  ) +
  labs(x = NULL, y = 'Yearly perennial forb/grass production (lbs/acre)')

# It looks like there is an increasing trend through time, but the not-growing
# season appears to be affecting the pattern. We can reduce the data just to the
# months of April-October. We will also add Thiel-Sen predictions to this dataset.
rap_df_grow <- rap_df[as.POSIXlt(rap_df$date)$mon %in% c(3:9), ]
mblm_PFG_grow <- coef(mblm::mblm(prod ~ dateInt, data = rap_df_grow[which(rap_df_grow$`Cover type` == 'PFG'), ], repeated = T))
mblm_PFG_grow_ab <- data.frame(date = rap_df_grow$date, prod_PFG_grow_est = (mblm_PFG_grow[2] * rap_df_grow$dateInt) + mblm_PFG_grow[1])
mblm_PFG_grow_ab <- mblm_PFG_grow_ab[!duplicated(mblm_PFG_grow_ab), ]
rap_df_grow <- dplyr::left_join(rap_df_grow, mblm_PFG_grow_ab, by = 'date')

ggplot(data = rap_df_grow[which(rap_df_grow$`Cover type` == 'PFG'), ]) +
  geom_line(aes(x = date, y = prod)) +
  geom_line(aes(x = date, y = prod_PFG_grow_est), linetype = 'dashed') +
  facet_wrap(~ year, scales = 'free_x') +

  theme_bw() +
  theme(
    axis.text.x = element_text(color = 'black', angle = 45, hjust = 1),
    axis.text.y = element_text(color = 'black')
  ) +
  labs(x = NULL, y = 'Yearly perennial forb/grass production (lbs/acre)')

# What about with a bar chart?
rap_df_grow$yearFact <- as.factor(rap_df_grow$year)

PFG_bar <- ggplot(data = rap_df_grow[which(rap_df_grow$`Cover type` == 'PFG'), ], aes(x = yearFact, y = prod)) +
  geom_bar(stat = 'summary', fill = 'grey60') +
  geom_errorbar(stat = 'summary', width = 0.05) +

  theme_bw() +
  theme(
    axis.text.x = element_text(color = 'black', angle = 45, hjust = 1),
    axis.text.y = element_text(color = 'black')
  ) +
  labs(x = NULL, y = 'April-Oct annual forb/grass roduction (lbs/acre)')

PFG_bar

# what is this difference between PFG in 2015, and 2009?
PFG_mean_2009 <- mean(rap_df_grow$prod[which(rap_df_grow$`Cover type` == 'PFG' & rap_df_grow$year == 2009)])
PFG_mean_2009 <- round(PFG_mean_2009, 1)
PFG_mean_2011 <- mean(rap_df_grow$prod[which(rap_df_grow$`Cover type` == 'PFG' & rap_df_grow$year == 2011)])
PFG_mean_2011 <- round(PFG_mean_2011, 1)
PFG_mean_2015 <- mean(rap_df_grow$prod[which(rap_df_grow$`Cover type` == 'PFG' & rap_df_grow$year == 2015)])
PFG_mean_2015 <- round(PFG_mean_2015, 1)
# A difference of 21.3 lbs/acre (+59%) at fire + 5 years

PFG_bar +
  annotate('text', x = 1, y = 50, label = paste0('mean = ', as.character(PFG_mean_2009))) +
  annotate('text', x = 2, y = 55, label = paste0('mean = ', as.character(PFG_mean_2011))) +
  annotate('text', x = 3, y = 70, label = paste0('mean = ', as.character(PFG_mean_2015)))

# Now what about annual grass? There was an interesting pattern in the raw data.
ggplot(data = rap_df[which(rap_df$`Cover type` == 'AFG'), ]) +
  geom_line(aes(x = date, y = prod)) +
  facet_wrap(~ year, scales = 'free_x') +

  theme_bw() +
  theme(
    axis.text.x = element_text(color = 'black', angle = 45, hjust = 1),
    axis.text.y = element_text(color = 'black')
  ) +
  labs(x = NULL, y = 'Yearly annual forb/grass production (lbs/acre)')

# Now with just the growing season:
ggplot(data = rap_df_grow[which(rap_df_grow$`Cover type` == 'AFG'), ]) +
  geom_line(aes(x = date, y = prod)) +
  facet_wrap(~ year, scales = 'free_x') +

  theme_bw() +
  theme(
    axis.text.x = element_text(color = 'black', angle = 45, hjust = 1),
    axis.text.y = element_text(color = 'black')
  ) +
  labs(x = NULL, y = 'Yearly annual forb/grass production (lbs/acre)')

# And as a bar plot:
ggplot(data = rap_df_grow[which(rap_df_grow$`Cover type` == 'AFG'), ], aes(x = yearFact, y = prod)) +
  geom_bar(stat = 'summary', fill = 'grey60') +
  geom_errorbar(stat = 'summary', width = 0.05) +

  theme_bw() +
  theme(
    axis.text.x = element_text(color = 'black', angle = 45, hjust = 1),
    axis.text.y = element_text(color = 'black')
  ) +
  labs(x = NULL, y = 'April-October annual forb/grass roduction (lbs/acre)')

# Let's do a time-series analysis of all the data:
ggplot(data = rap_df_ALL[which(rap_df_ALL$`Cover type` == 'AFG'), ], aes(x = date, y = prod)) +
  geom_line() +

  theme_bw() +
  theme(
    axis.text.x = element_text(color = 'black', angle = 45, hjust = 1),
    axis.text.y = element_text(color = 'black')
  ) +
  labs(x = NULL, y = 'Aerennial production (lbs/acre)')

rap_ts_df <- rap_df_ALL
rap_ts_df <- rap_ts_df[which(rap_ts_df$`Cover type` == 'AFG'), ]
rap_ts_df$year <- as.POSIXlt(rap_ts_df$date)$year + 1900
rap_ts_df <- rap_ts_df[-which(rap_ts_df$year %in% 1986, 2025), ]
rap_ts <- rap_ts_df$prod

ss <- bsts::AddSeasonal(list(), rap_ts, nseasons = 39, season.duration = 63)
bsts_mod <- bsts::bsts(rap_ts, ss, niter = 500, ping = 10)

bsts_post <- bsts_mod$state.contributions
bsts_post <- bsts_post[, 1, ]
bsts_post <- colMeans(bsts_post)
rap_ts_df <- data.frame(rap_ts_df, post = bsts_post)
rap_ts_mean <- aggregate(rap_ts_df$post, by = list(rap_ts_df$year), FUN = mean)
colnames(rap_ts_mean) <- c('year', 'post')

# Plot of yearly trend in annual production cover:
ggplot(data = rap_ts_mean) +
  geom_line(aes(x = year, y = post)) +

  theme_bw() +
  theme(
    axis.text.x = element_text(color = 'black', angle = 45, hjust = 1),
    axis.text.y = element_text(color = 'black')
  ) +
  labs(x = NULL, y = 'Yearly trend, annual forage production (lbs/acre)')

# END
