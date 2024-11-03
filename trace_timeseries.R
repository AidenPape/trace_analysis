### Create time series of cFlux for certain lakes from TraCE
### Oct 10

library(ncdf4)
library(data.table)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(devtools)

#Get lon, lat, and time and cflux values
cflux_fp = "../data/trace.01-36.22000BP.clm2.cfluxFIRE.22000BP_decavg_400BCE.nc"
cflux_file <- nc_open(cflux_fp) 
trace_lon <- ncvar_get(cflux_file, 'lon'); trace_lat <- ncvar_get(cflux_file, 'lat'); trace_time <- ncvar_get(cflux_file, 'time')
cflux_vals <- ncvar_get(cflux_file, 'CFLUXFIRE')

### Get burn data
burn_fp = "../data/trace.01-36.22000BP.clm2.BURN.22000BP_decavg_400BCE.nc"
burn_file <- nc_open(burn_fp)
burn_vals <- ncvar_get(burn_file, 'BURN')

### Get fwi data
fwi_fp = "../data/TraCE decadal avg. FWI.nc"
fwi_file <- nc_open(fwi_fp) 
fwi_vals <- ncvar_get(fwi_file, 'decavg fwi')


### Close nc files
nc_close(cflux_file) 
nc_close(burn_file)
nc_close(fwi_file)

### Create 3-d arrays 
cflux_array = array(cflux_vals, dim = c(length(trace_lon), length(trace_lat), length(trace_time)))
burn_array = array(burn_vals, dim = c(length(trace_lon), length(trace_lat), length(trace_time)))
fwi_array = array(fwi_vals, dim = c(length(trace_lon), length(trace_lat), length(trace_time)))

## Get increments for latitude and longitude
lat_increments = array(NA, dim = c(length(trace_lat)-1))
for(i in 1:47){
  lat_increments[i] = trace_lat[i+1] - trace_lat[i]
}

lat_incr = max(lat_increments)
lon_incr = trace_lon[2] - trace_lon[1]

### Get index for given lat and lon
'
LAKE           LON          LAT
Lily Lake	    -120.2      	 41.97
Mumbo	        -122.5         41.19
Wildcat Lake	-122.78	       37.96
East Lake	    -119.02	       37.17
'

# Entered lat/lon for sediment core
lakes_list = list(
  "Lily Lake" = list(lon = -120.2, lat = 41.97),
  "Mumbo" = list(lon = -122.5, lat = 41.19),
  "Wildcat Lake" = list(lon = -122.78, lat = 37.96),
  "East Lake" = list(lon = -119.02, lat = 37.17)
)

lake_name = "Mumbo"

lake_lon <- lakes_list[[lake_name]]$lon
lake_lat <- lakes_list[[lake_name]]$lat

#Convert lon if west of 0 lon
if(lake_lon < 0){
  lake_lon = 360 + lake_lon
}

# Get indices for closest trace grid cell to given lake lat/lon
lat_ind = which(trace_lat > lake_lat & trace_lat <= (lake_lat+lat_incr))
if(length(lat_ind)>1){
  print("GRABBBED TWO LAT INDS: bc lake was too close to grid line")
}
lon_ind = which(trace_lon < lake_lon & trace_lon >= (lake_lon-lon_incr))

# Select all cflux values at the selected grid cell
sel_grid_vals = cflux_array[lon_ind, lat_ind,]
# sel_grid_vals = burn_array[lon_ind, lat_ind,]
# sel_grid_vals = fwi_array[lon_ind, lat_ind,]

sel_time = trace_time
# Filter for a certain time range
lower_time = -4; upper_time = 0
time_indices = which(trace_time >= lower_time, trace_time <= upper_time)
sel_grid_vals = sel_grid_vals[time_indices]
sel_time = trace_time[time_indices]

### If using fwi with messed up final value
sel_grid_vals = sel_grid_vals[-length(sel_grid_vals)]
sel_time = sel_time[-length(sel_time)]

# Plot time series
ggplot() + 
  geom_line(aes(x=sel_time,y=sel_grid_vals), color = 'red', linewidth = 0.5) + 
  geom_smooth(aes(x=sel_time,y=sel_grid_vals), method = "loess") +
  theme(legend.position = 'none', legend.text = element_text(size = 5),
        legend.title = element_blank(), legend.key.size = unit(0.5,'cm'),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "gray"),
        panel.background = element_rect(fill = "white",colour = "black",
                                        size = 1, linetype = "solid")) +
  scale_x_continuous(breaks = seq(-22,0,1)
                     #                , limits = c(-9,-7)
  ) +
  xlab('Time (kyr BP)') + 
  ylab('Selected Grid cFlux') +
  ggtitle(lake_name) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))


  
