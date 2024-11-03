"
Get decadel average of annual data
"
library(ncdf4)

ts_fp = "../data/trace2/TraCE-21K-II.ann.TS.nc"
ts_file <- nc_open(ts_fp)

trace_lon <- ncvar_get(ts_file, 'lon')
trace_lat <- ncvar_get(ts_file, 'lat')
trace_time <- ncvar_get(ts_file, 'time')
trace_pft <- seq(1, 10)

### NPP data
var_fp = "../data/trace2/TraCE-21K-II.hv.BURN.nc"
var_file <- nc_open(var_fp) # Read in TraCE netCDF file

var_vals <- ncvar_get(var_file, 'BURN')
nc_close(var_file) # Close nc file

var_dims = dim(var_vals)
### For 4d variables
# decavg_var_vals = array(NA, dim = c(var_dims[1], var_dims[2], var_dims[3], var_dims[4]/10))
# 
# for(x in (1:var_dims[1])){
#   for(y in (1:var_dims[2])){
#     for(i in (1:var_dims[3])){
#       for(t in (1:var_dims[4]/10)){
#         sum_vals = 0
#         for(t_inc in (0:9)){
#           sum_vals = sum_vals + var_vals[x, y, i, t+t_inc]
#         }
#         decavg_var_vals[x, y, i, t] = sum_vals / 10
#       }
#     }
#   }
# }

## For 3d variables
decavg_var_vals = array(NA, dim = c(var_dims[1], var_dims[2], var_dims[3]/10))

for(x in (1:var_dims[1])){
  for(y in (1:var_dims[2])){
    for(t in (1:var_dims[3]/10)){
      sum_vals = 0
      for(t_inc in (0:9)){
        sum_vals = sum_vals + var_vals[x, y, t+t_inc]
      }
      decavg_var_vals[x, y, t] = sum_vals / 10
    }
  }
}

var_vals = NULL
### Create new netcdf file for decavg npp

lon <- ncdim_def("lon","degrees_east",trace_lon)
lat <- ncdim_def("lat","degrees_north",trace_lat)
time <- ncdim_def("time","decade",trace_time)
pft <- ncdim_def("pft","type",trace_pft)

### Add pft or fourth dimensions for 4d variables
TraCE.devavg_var<-ncvar_def("decavg BURN","fraction",list(lon,lat,time),longname="fraction of vegetated area burned")

name.hold <- nc_create('~/Desktop/ecsc_500/data/TraCE-II.decavg.BURN.nc',list(TraCE.devavg_var))

ncvar_put(name.hold,TraCE.devavg_var, decavg_var_vals)
