'
Read in cflux data and create some plots
'

library(ncdf4)
library(data.table)
library(cffdrs)
library(tidyverse)
library(tidyr)
library(maps)
source("utils.R")

##### cFlux Data #####

## Open netCDF file with cflux data
cflux_fp = "../data/trace2/TraCE-21K-II.hv.CFLUXFIRE.nc"
cflux_file <- nc_open(cflux_fp) # Read in TraCE .netCDF file

## Get lon, lat, and time and cflux values
trace_lon <- ncvar_get(cflux_file, 'lon'); trace_lat <- ncvar_get(cflux_file, 'lat')
trace_time <- ncvar_get(cflux_file, 'time')

## Get values and close file
cflux_vals <- ncvar_get(cflux_file, 'CFLUXFIRE') 
nc_close(cflux_file) 

## Create 3-d array 
cflux_array = array(cflux_vals, dim = c(length(trace_lon), length(trace_lat), length(trace_time)))
# dimnames(cflux_array) <- list(lon = trace_lon, lat = trace_lat, time = trace_time)

## Create data.table
grid = expand.grid(lon = trace_lon, lat = trace_lat, time = trace_time) #All combos of these
cflux_vals_flat = as.vector(cflux_vals) # Add cflux
cflux_dt = data.table(grid, cflux = cflux_vals_flat) #Create dt

## Filter for specific time step from data.table
time_step = -22.00
time_step_ind = which(trace_time == time_step)
cflux_dt_fil = cflux_dt[time == time_step]
cflux_dt_fil$time = NULL

cflux_dt_fil$lon <- ifelse(cflux_dt_fil$lon > 180, cflux_dt_fil$lon - 360, cflux_dt_fil$lon)

## Reformat dataframe at specific time step into matrix for filled.contour
# cflux_matrix <- cflux_dt_fil |>
#   pivot_wider(names_from = lat, values_from = cflux)
# 
# cflux_matrix <- as.data.frame(cflux_matrix)
# rownames(cflux_matrix) <- cflux_matrix$lon
# cflux_matrix$lon = NULL
# 
# cflux_matrix = as.matrix(cflux_matrix)

###### MAKE PLOTS OF cflux AT SPECIFIC TIME ######

lake_lon = -122.87
lake_lat = 37.96
lake_coords = find_latlon(trace_lat, trace_lon, lake_lat, lake_lon)

## Set up mapping parameters ##
# World2Hires = maps::map("world2Hires",plot=F) ## This is not working
stat.proj = maps::map("state",plot=F)
stat.proj$x = 360+stat.proj$x
palette1 <- colorRampPalette(rev(RColorBrewer::brewer.pal(n = 9, name = "Spectral")))

## Make some maps using filled.contour function ##
# NA lims ## xlim=c(230,295),ylim=c(25,50),
cflux_timestep = cflux_array[,,time_step_ind]
cflux_timestep[cflux_timestep>100] = 100
cflux_timestep[is.na(cflux_timestep)] = 0

filled.contour(x=trace_lon, y=trace_lat, z=cflux_timestep, xlim=c(230,250),ylim=c(30,45), color.palette = palette1,
               levels = seq(0,100,5), xlab="Longitude",ylab="Latitude",
               main=paste("cflux at 22 ka"),
               plot.axes={
                 map.axes(las=1)
                 map(stat.proj,lwd=1,add=T,col="black",lty=1)
                 rect(
                   xleft = lake_coords$gridcell_lon - 3.75 / 2, xright = lake_coords$gridcell_lon + 3.75 / 2, 
                   ybottom = lake_coords$gridcell_lat - 3.75 / 2, ytop = lake_coords$gridcell_lat + 3.75 / 2, 
                   border = "blue", lwd = 2 # Set thickness
                 )
                 points(
                   x = lake_lon+360, y = lake_lat, col = "red", pch = 16, cex = 1
                 )
               }
               )

### Add point with highlighted grid cell for grid cell plot
state_map <- map_data("state")

## Grid cell plots (non-interpolated)
ggplot() +
  geom_tile(data = cflux_dt_fil, aes(x=lon, y=lat, fill=cflux)) +
  scale_x_continuous(limits = c(-130, -110)) +
  scale_y_continuous(limits = c(20, 50)) +
  scale_fill_gradient2(low = "#075AFF", mid = "#075AFF", high = "#FF0000") +
  labs(x = "Longitude", y = "Latitude", fill = "cflux") +
  theme_minimal() +
  geom_tile(
    data = data.frame(lon = lake_coords$gridcell_lon-360, lat = lake_coords$gridcell_lat),
    aes(x = lon, y = lat), width = 3.75, height = 3.75, fill = NA, color = "black", size = 1
  ) +
  geom_point(aes(x = lake_lon, y = lake_lat)) +
  geom_path(data = state_map, aes(x = long, y = lat, group = group), color = "black", inherit.aes = FALSE)

########## BURN DATA #########

## Open netCDF file with burn data
burn_fp = "../data/trace2/TraCE-21K-II.hv.BURN.nc"
burn_file <- nc_open(burn_fp) # Read in TraCE netCDF file

## Get lon, lat, and time and burn values
burn_vals <- ncvar_get(burn_file, 'BURN')

nc_close(burn_file) # Close nc file

## Create 3-d array 
burn_array = array(burn_vals, dim = c(length(trace_lon), length(trace_lat), length(trace_time)))

## Create data.table
grid = expand.grid(lon = trace_lon, lat = trace_lat, time = trace_time) #All combos of these
burn_vals_flat = as.vector(burn_vals) # Add burn
burn_dt = data.table(grid, burn = burn_vals_flat) #Create dt

## Filter for specific time step from data.table
time_step = -22.00
time_step_ind = which(trace_time == time_step)
burn_dt_fil = burn_dt[time == time_step]
burn_dt_fil$time = NULL

## Reformat dataframe at specific time step into matrix for filled.contour
burn_matrix <- burn_dt_fil |>
  pivot_wider(names_from = lat, values_from = burn)

burn_matrix <- as.data.frame(burn_matrix)
rownames(burn_matrix) <- burn_matrix$lon
burn_matrix$lon = NULL

burn_matrix = as.matrix(burn_matrix)

###### MAKE PLOTS OF burn AT SPECIFIC TIME ######

# Set up mapping parameters ##
# World2Hires = maps::map("world2Hires",plot=F) ## This is not working

# Make some maps using filled.contour function ##
# NA lims ## xlim=c(230,295),ylim=c(25,50),
filled.contour(x=trace_lon, y=trace_lat, z=burn_array[,,time_step_ind], xlim=c(230,295),ylim=c(25,50), color.palette = palette1,
               levels = seq(0,100,5), xlab="Longitude",ylab="Latitude",
               main=paste("burn at 22 ka"),
               plot.axes={
                 map.axes(las=1); map(stat.proj,lwd=1,add=T,col="black",lty=1)
               }
)

# Grid cell plots
ggplot() +
  geom_tile(data = burn_dt_fil, aes(x=lon, y=lat, fill=burn)) +
  scale_fill_gradient2(low = "#075AFF",
                       mid = "#075AFF",
                       high = "#FF0000") +
  scale_x_continuous(limits = c(220, 300)) +
  scale_y_continuous(limits = c(25, 50)) +
  labs(x = "Longitude", y = "Latitude", fill = "burn") +
  theme_minimal()

###### Create Anomaly Plots ######

## Define timesteps, anomaly will be calculated t2 - t1
t1 = -22
t2 = 0
# Get indices of these times
t1_ind = which(trace_time == t1)
t2_ind = which(trace_time == t2)

## Choose which dataset, comment others out and create anomaly

## cFlux
anom_matrix = cflux_array[,,t1_ind] - cflux_array[,,t2_ind]
anom_table = cflux_dt[time == t1]
anom_table$cflux_t2 = (cflux_dt[time == t2]$cflux)
anom_table$anomaly = anom_table$cflux_t2 - anom_table$cflux

## BURN
# anomaly = burn_array[,,t1] - burn_array[,,t2]
# anom_table = burn_dt[time == t1]
# anom_table$burn_t2 = (burn_dt[time == t2]$burn)
# anom_table$anomaly = anom_table$burn_t2 - anom_table$burn

## FWI
# anomaly = fwi_array[,,t1] - fwi_array[,,t2]
# anom_table = fwi_dt[time == t1]
# anom_table$fwi_t2 = (fwi_dt[time == t2]$fwi)
# anom_table$anomaly = anom_table$fwi_t2 - anom_table$fwi

#Plot filled.contour of anomaly
filled.contour(x=trace_lon, y=trace_lat, z=anom_matrix, xlim=c(230,295),ylim=c(25,50), color.palette = palette1,
               levels = seq(-150,150,5), xlab="Longitude",ylab="Latitude",
               main=paste("Anomaly"),
               plot.axes={
                 map.axes(las=1); map(stat.proj,lwd=1,add=T,col="black",lty=1)
               }
)

# Plot grid cells of anomaly
ggplot() +
  geom_tile(data = anom_table, aes(x=lon, y=lat, fill=anomaly)) +
  scale_fill_gradient2(low = "#075AFF",
                       high = "#FF0000") +
  scale_x_continuous(limits = c(230, 290)) +
  scale_y_continuous(limits = c(25, 50)) +
  labs(x = "Longitude", y = "Latitude", fill = "Anomaly") +
  theme_minimal()


####### Plot BURN, cflux, and fwi against each other #######

## Read in fwi values
## Open netCDF file with fwi data
fwi_fp = "../data/TraCE decadal avg. FWI.nc"
fwi_file <- nc_open(fwi_fp) # Read in TraCE .netCDF file

## Get lon, lat, and time and fwi values
fwi_vals <- ncvar_get(fwi_file, 'decavg fwi')

nc_close(fwi_file) # Close nc file

## Create 3-d array 
fwi_array = array(fwi_vals, dim = c(length(trace_lon), length(trace_lat), length(trace_time)))

lon_ind = 64
lat_ind = 35

## Slice for one grid cell to print
burn_vals_cal = burn_array[lake_lon,lake_lat,] #Get burn values for grid cell in california
cflux_vals_cal = cflux_array[lake_lon,lake_lat,] #Get cflux values for grid cell in california
fwi_vals_cal = fwi_array[lake_lon,lake_lat,] #Get fwi values for grid cell in california
fwi_vals_cal[2204] = 3 # this is not correct

ggplot()+
  geom_point(aes(x=burn_vals_cal, y = cflux_vals_cal)) +
  # geom_smooth(aes(x=burn_vals_cal, y = cflux_vals_cal), method = "loess") +
  labs(title = "BURN vs cFlux for California Grid Cell", x = "BURN", y = "cflux")

ggplot()+
  geom_point(aes(x=fwi_vals_cal, y = burn_vals_cal)) +
  # geom_smooth(aes(x=fwi_vals_cal, y = burn_vals_cal), method = "loess") +
  labs(title = "BURN vs FWI for California Grid Cell", x = "FWI", y = "BURN")

ggplot()+
  geom_point(aes(x=fwi_vals_cal, y = cflux_vals_cal)) +
  # geom_smooth(aes(x=fwi_vals_cal, y = cflux_vals_cal), method = "loess") +
  labs(title = "cFlux vs FWI for California Grid Cell", x = "FWI", y = "cFlux")


#### ADD VEGETATION DATA ####

dmi_fp = "../data/trace1/trace.01-36.22000BP.clm2.DMI.22000BP_decavg_400BCE.nc"
dmi_file <- nc_open(dmi_fp) # Read in TraCE .netCDF file
dmi_vals <- ncvar_get(dmi_file, 'DMI') #values
dmi_vals_cal = dmi_vals[lon_ind, lat_ind,]

ggplot(data = NULL, aes(x = fwi_vals_cal, y = burn_vals_cal, color = dmi_vals_cal)) +
  geom_point() +
  scale_color_continuous(low = "lightgreen", high = "darkgreen") +
  labs(title = "BURN vs FWI for California Grid Cell", x = "FWI", y = "BURN", color = "DMI")

ggplot(data = NULL, aes(x = fwi_vals_cal, y = cflux_vals_cal, color = dmi_vals_cal)) +
  geom_point() +
  scale_color_continuous(low = "lightgreen", high = "darkgreen") +
  labs(title = "cFlux vs FWI for California Grid Cell", x = "FWI", y = "cFlux", color = "DMI")


