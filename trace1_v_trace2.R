### Compare trace2 1 versus trace2 2

library(ncdf4)

### Open files and read data ###

## Pick variable code

var = "TS"

## Give filepaths
trace1_fp = paste0("../data/trace.01-36.22000BP.cam2.", var, ".22000BP_decavg_400BCE.nc") # Surface Temp

trace2_fp = paste0("../data/TraCE-21K-II.ann.", var, ".nc") # Surface Temp

trace1_file = nc_open(trace1_fp)
trace1_lon <- ncvar_get(trace1_file, 'lon'); trace1_lat <- ncvar_get(trace1_file, 'lat'); trace1_time <- ncvar_get(trace1_file, 'time')
trace1_t_vals <- ncvar_get(trace1_file, var) #values
nc_close(trace1_file) 

trace2_file = nc_open(trace2_fp)
trace2_lon <- ncvar_get(trace2_file, 'lon'); trace2_lat <- ncvar_get(trace2_file, 'lat'); trace2_time <- ncvar_get(trace2_file, 'time')
trace2_t_vals <- ncvar_get(trace2_file, var) #values
nc_close(trace2_file) 

## Create 3-D arrays
trace1_t_array = array(trace1_t_vals, dim = c(length(trace1_lon), length(trace1_lat), length(trace1_time)))
trace2_t_array = array(trace2_t_vals, dim = c(length(trace2_lon), length(trace2_lat), length(trace2_time)))

## Our known lat/lon indices for our wildcat california grid cell
lat_ind = 35
lon_ind = 64 

## Get our values and drop the last one for trace2 bc it goes further than trace1
trace1_t_sel_grid = trace1_t_array[lon_ind, lat_ind,]
trace2_t_sel_grid = trace2_t_array[lon_ind, lat_ind,]
trace2_t_sel_grid = trace2_t_sel_grid[-2205]
trace2_time_minus1 = trace2_time[-2205]

ggplot() + 
  geom_line(aes(x=trace1_time, y=trace1_t_sel_grid, color = "TraCE-I"), linewidth = 0.3) +
  geom_line(aes(x=trace2_time_minus1, y=trace2_t_sel_grid, color = 'TraCE-II'), linewidth = 0.3) +
  theme(legend.position = 'left', legend.text = element_text(size = 5),
        legend.title = element_blank(), legend.key.size = unit(0.5,'cm'),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "gray"),
        panel.background = element_rect(fill = "white",colour = "black",
                                        size = 1, linetype = "solid")) +
  scale_x_continuous(breaks = seq(-22,0,1)
                     #                , limits = c(-9,-7)
  ) +
  xlab('Time (kyr BP)') + 
  ylab(var) +
  ggtitle("Trace I v. Trace II") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))



