'
Read in trace_var data and create some plots
'

library(RColorBrewer)
library(ncdf4)
library(data.table)
library(cffdrs)
library(tidyverse)
library(tidyr)
library(maps)
source("utils.R")

##### Read Lake Coords #####
lakes_fp = "../data/Paleofire Database.csv"
lakes_data = read.csv(lakes_fp)
lakes_data = lakes_data[c("Loc.Name", "Lon", "Lat")]

##### Trace Variable Data #####
## Open netCDF file with trace_var data
trace_var_fp = "../data/trace2/TraCE-21K-II.ann.TS.nc"
trace_var_file <- nc_open(trace_var_fp) # Read in TraCE .netCDF file

## Get values and close file
trace_var_vals <- ncvar_get(trace_var_file, 'TS') 

## Get lon, lat, and time and trace_var values
trace_lon <- ncvar_get(trace_var_file, 'lon')
trace_lat <- ncvar_get(trace_var_file, 'lat')
trace_time <- ncvar_get(trace_var_file, 'time')

nc_close(trace_var_file) 

## Create 3-d array 
trace_var_array = array(trace_var_vals, dim = c(length(trace_lon), length(trace_lat), length(trace_time)))
# dimnames(trace_var_array) <- list(lon = trace_lon, lat = trace_lat, time = trace_time)

## Create data.table
grid = expand.grid(lon = trace_lon, lat = trace_lat, time = trace_time) #All combos of these
trace_var_vals_flat = as.vector(trace_var_vals) # Add trace_var
trace_var_dt = data.table(grid, trace_var = trace_var_vals_flat) #Create dt

## Filter for specific time step from data.table
time_step = -22.00
time_step_ind = which(trace_time == time_step)
trace_var_dt_fil = trace_var_dt[time == time_step]
trace_var_dt_fil$time = NULL

trace_var_dt_fil$lon <- ifelse(trace_var_dt_fil$lon > 180, trace_var_dt_fil$lon - 360, trace_var_dt_fil$lon)

## Reformat dataframe at specific time step into matrix for filled.contour
# trace_var_matrix <- trace_var_dt_fil |>
#   pivot_wider(names_from = lat, values_from = trace_var)
# 
# trace_var_matrix <- as.data.frame(trace_var_matrix)
# rownames(trace_var_matrix) <- trace_var_matrix$lon
# trace_var_matrix$lon = NULL
# 
# trace_var_matrix = as.matrix(trace_var_matrix)

###### MAKE PLOTS OF trace_var AT SPECIFIC TIME ######

lake_name = "Crater Lake"
lake_lat = lakes_data[lakes_data$Loc.Name==lake_name,]$Lat
lake_lon = lakes_data[lakes_data$Loc.Name==lake_name,]$Lon

grid_coords = find_latlon(trace_lat, trace_lon, lat=lake_lat, lon=lake_lon)

all_lake_lons = lakes_data$Lon
all_lake_lats = lakes_data$Lat

## Set up mapping parameters ##
# World2Hires = maps::map("world2Hires",plot=F) ## This is not working
stat.proj = maps::map("state",plot=F)
stat.proj$x = 360+stat.proj$x
palette1 <- colorRampPalette(rev(RColorBrewer::brewer.pal(n = 9, name = "Spectral")))

## Make some maps using filled.contour function ##
# NA lims ## xlim=c(230,295),ylim=c(25,50),
trace_var_timestep = trace_var_array[,,time_step_ind]

### For visual scaling purposes, adjust extrema
# trace_var_timestep[trace_var_timestep<220] = 220
# trace_var_timestep[trace_var_timestep>290] = 290
# trace_var_timestep[is.na(trace_var_timestep)] = 0

### For global data, change trace_lon to adjusted_lon
### Change back to trace_lon for CONUS zoomed (colors/map will not show if wrong lons used)
adjusted_lon <- ifelse(trace_lon > 180, trace_lon - 360, trace_lon)
sorted_indices <- order(adjusted_lon)
adjusted_lon <- adjusted_lon[sorted_indices]
trace_var_timestep <- trace_var_timestep[sorted_indices, ]

filled.contour(x=trace_lon, y=trace_lat, z=trace_var_timestep, color.palette = palette1,
               xlim=c(233,248), ylim=c(31,45),  ## Comment out for global
               levels = seq(200,305,5), xlab="Longitude",ylab="Latitude",
               main=paste("Temp (K) at 22 ka"),
               plot.axes={
                 map.axes(las=1)
                 # map("world", add = TRUE, col = "black", lwd = 1) ## Country outlines
                 map(stat.proj,lwd=1,add=T,col="black",lty=1) ## State outlines
                 rect(
                   xleft = grid_coords$gridcell_lon - 3.75 / 2, xright = grid_coords$gridcell_lon + 3.75 / 2,
                   ybottom = grid_coords$gridcell_lat - 3.75 / 2, ytop = grid_coords$gridcell_lat + 3.75 / 2,
                   border = "blue", lwd = 2 # Set thickness
                 )
                 points(
                   x = all_lake_lons+360, y = all_lake_lats, col = "red", pch = 16, cex = 1
                 )
               }
               )

### Add point with highlighted grid cell for grid cell plot
state_map <- map_data("state")
world_map <- map_data("world")

## Grid cell plots (non-interpolated) 
## Command scales in and geom_path(state_map) in for CONUS
ggplot() +
  geom_tile(data = trace_var_dt_fil, aes(x=lon, y=lat, fill=trace_var)) +
  # scale_x_continuous(limits = c(-130, -110)) + ## For CONUS
  # scale_y_continuous(limits = c(20, 50)) + ## For CONUS
  scale_fill_distiller(palette="Spectral", direction=-1, limits = c(200, 300), oob = scales::squish) +
  labs(title = "Temperature (K) at 22 ka", x = "Longitude", y = "Latitude", fill = "Temp (K)") +
  theme_minimal() +
  geom_tile(
    data = data.frame(lon = lake_trace_coords$gridcell_lon-360, lat = lake_trace_coords$gridcell_lat),
    aes(x = lon, y = lat), width = 3.75, height = 3.75, fill = NA, color = "black", linewidth = 1
  ) +
  geom_point(aes(x = lake_lon, y = lake_lat)) +
  # geom_path(data = state_map, aes(x = long, y = lat, group = group), color = "black", inherit.aes = FALSE) +
  geom_path(data = world_map, aes(x = long, y = lat, group = group), color = "black", inherit.aes = FALSE)


###### Create Anomaly Plots ######

## Define timesteps, anomaly will be calculated t2 - t1
t1 = -22
t2 = 0
# Get indices of these times
t1_ind = which(trace_time == t1)
t2_ind = which(trace_time == t2)

## Choose which dataset, comment others out and create anomaly

## trace_var
anom_matrix = trace_var_array[,,t1_ind] - trace_var_array[,,t2_ind]
anom_table = trace_var_dt[time == t1]
anom_table$trace_var_t2 = (trace_var_dt[time == t2]$trace_var)
anom_table$anomaly = anom_table$trace_var_t2 - anom_table$trace_var

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


####### Plot BURN, trace_var, and fwi against each other #######

## Read in fwi values
## Open netCDF file with fwi data and get values
fwi_fp = "../data/trace2/TraCE II decadal avg. FWI and parameters.nc"
fwi_file <- nc_open(fwi_fp) 
fwi_vals <- ncvar_get(fwi_file, 'decavg fwi')
temp_vals <- ncvar_get(fwi_file, 'decavg surf. temp.')
prect_vals <- ncvar_get(fwi_file, 'decavg PRECT')

nc_close(fwi_file) # Close nc file

## Create 3-d array 
fwi_array = array(fwi_vals, dim = c(length(trace_lon), length(trace_lat), length(trace_time)))
temp_array = array(temp_vals, dim = c(length(trace_lon), length(trace_lat), length(trace_time)))
prect_array = array(prect_vals, dim = c(length(trace_lon), length(trace_lat), length(trace_time)))

lon_ind = 64
lat_ind = 35

## Slice for one grid cell to print
burn_vals_cal = burn_array[lake_lon,lake_lat,] #Get burn values for grid cell in california
trace_var_vals_cal = trace_var_array[lake_lon,lake_lat,] #Get trace_var values for grid cell in california
fwi_vals_cal = fwi_array[lake_lon,lake_lat,] #Get fwi values for grid cell in california
fwi_vals_cal[2204] = 3 # this is not correct

ggplot()+
  geom_point(aes(x=burn_vals_cal, y = trace_var_vals_cal)) +
  # geom_smooth(aes(x=burn_vals_cal, y = trace_var_vals_cal), method = "loess") +
  labs(title = "BURN vs trace_var for California Grid Cell", x = "BURN", y = "trace_var")

ggplot()+
  geom_point(aes(x=fwi_vals_cal, y = burn_vals_cal)) +
  # geom_smooth(aes(x=fwi_vals_cal, y = burn_vals_cal), method = "loess") +
  labs(title = "BURN vs FWI for California Grid Cell", x = "FWI", y = "BURN")

ggplot()+
  geom_point(aes(x=fwi_vals_cal, y = trace_var_vals_cal)) +
  # geom_smooth(aes(x=fwi_vals_cal, y = trace_var_vals_cal), method = "loess") +
  labs(title = "trace_var vs FWI for California Grid Cell", x = "FWI", y = "trace_var")


#### ADD VEGETATION DATA ####

dmi_fp = "../data/trace1/trace.01-36.22000BP.clm2.DMI.22000BP_decavg_400BCE.nc"
dmi_file <- nc_open(dmi_fp) # Read in TraCE .netCDF file
dmi_vals <- ncvar_get(dmi_file, 'DMI') #values
dmi_vals_cal = dmi_vals[lon_ind, lat_ind,]

ggplot(data = NULL, aes(x = fwi_vals_cal, y = burn_vals_cal, color = dmi_vals_cal)) +
  geom_point() +
  scale_color_continuous(low = "lightgreen", high = "darkgreen") +
  labs(title = "BURN vs FWI for California Grid Cell", x = "FWI", y = "BURN", color = "DMI")

ggplot(data = NULL, aes(x = fwi_vals_cal, y = trace_var_vals_cal, color = dmi_vals_cal)) +
  geom_point() +
  scale_color_continuous(low = "lightgreen", high = "darkgreen") +
  labs(title = "trace_var vs FWI for California Grid Cell", x = "FWI", y = "trace_var", color = "DMI")


