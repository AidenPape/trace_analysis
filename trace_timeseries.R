### Create time series of cFlux for certain lakes from TraCE
### Oct 10

library(ncdf4)
library(data.table)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(devtools)
source("utils.R")

#Get lon, lat, and time and cflux values
cflux_fp = "../data/trace2/TraCE-21K-II.decavg.CFLUXFIRE.nc"
cflux_file <- nc_open(cflux_fp) 
trace_lon <- ncvar_get(cflux_file, 'lon')
trace_lat <- ncvar_get(cflux_file, 'lat')
trace_time <- ncvar_get(cflux_file, 'time')
cflux_vals <- ncvar_get(cflux_file, 'CFLUXFIRE')

### Get burn data
burn_fp = "../data/trace2/TraCE-21K-II.decavg.BURN.nc"
burn_file <- nc_open(burn_fp)
burn_vals <- ncvar_get(burn_file, 'BURN')

### Get fwi data
fwi_fp = "../data/trace2/TraCE II decadal avg. FWI and parameters.nc"
fwi_file <- nc_open(fwi_fp) 
fwi_vals <- ncvar_get(fwi_file, 'decavg fwi')
temp_vals <- ncvar_get(fwi_file, 'decavg surf. temp.')
prect_vals <- ncvar_get(fwi_file, 'decavg PRECT')

### Close nc files
nc_close(cflux_file) 
nc_close(burn_file)
nc_close(fwi_file)

### Create 3-d arrays 
cflux_array = array(cflux_vals, dim = c(length(trace_lon), length(trace_lat), length(trace_time)))
burn_array = array(burn_vals, dim = c(length(trace_lon), length(trace_lat), length(trace_time)))
fwi_array = array(fwi_vals, dim = c(length(trace_lon), length(trace_lat), length(trace_time)))
temp_array = array(temp_vals, dim = c(length(trace_lon), length(trace_lat), length(trace_time)))
prect_array = array(prect_vals, dim = c(length(trace_lon), length(trace_lat), length(trace_time)))

##### Read Lake Coords #####
lakes_fp = "../data/Paleofire Database.csv"
lakes_data = read.csv(lakes_fp)
lakes_coords = lakes_data[c("Loc.Name", "Lon", "Lat")]

lake_name = "Crater Lake"
lake_lat = lakes_data[lakes_data$Loc.Name==lake_name,]$Lat
lake_lon = lakes_data[lakes_data$Loc.Name==lake_name,]$Lon

grid_coords = find_latlon(trace_lat, trace_lon, lat=lake_lat, lon=lake_lon)

lon_ind = grid_coords$lon_index
lat_ind = grid_coords$lat_index

# Select all cflux values at the selected grid cell
# sel_grid_vals = cflux_array[lon_ind, lat_ind,]; var_name = "CFlux"
# sel_grid_vals = burn_array[lon_ind, lat_ind,]; var_name = "BURN"
sel_grid_vals = fwi_array[lon_ind, lat_ind,]; var_name = "FWI"
# sel_grid_vals = temp_array[lon_ind, lat_ind,]; var_name = "Temperature (K)"
# sel_grid_vals = prect_array[lon_ind, lat_ind,]; var_name = "Precipation"


# Plot time series
ggplot() + 
  geom_line(aes(x=trace_time,y=sel_grid_vals), color = 'red', linewidth = 0.3) + 
  # geom_smooth(aes(x=sel_time,y=sel_grid_vals), method = "loess") +
  theme(legend.position = 'none', legend.text = element_text(size = 5),
        legend.title = element_blank(), legend.key.size = unit(0.5,'cm'),
        panel.grid.major = element_line(linewidth = 0.25, linetype = 'solid', colour = "gray"),
        panel.background = element_rect(fill = "white",colour = "black",
                                        size = 1, linetype = "solid")) +
  scale_x_continuous(breaks = seq(-22,0,1)
                                    # , limits = c(-15,0)
  ) +
  xlab('Time (kyr BP)') + 
  ylab(var_name) +
  ggtitle(paste0(lake_name, " TraCE Grid Cell")) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))


  
