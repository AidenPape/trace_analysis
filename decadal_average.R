"
Get decadel average of annual data

This code works for 3-d (lon, lat, time) or 4-d (lon, lat, type, time) data, for
example NPP or PFT that have 10 types of pft for each lon, lat, time point.

You must specify the name of your 4 fourth dimension if you variable has 4 dimensions

This code reads data from ncdf file 1000 years at a time, this can be changed but
must be a multiple of 10
"
library(ncdf4)

### PARAMETERS ###
var_name = "FPCGRID"
var_units = "fraction of vegetated area"
var_longname = "decadal average plant functional type cover"
agg_func = mean
chunk_size = 1000

## Additional paremeter for 4-D data
type_name = "pft" ## ie "pft"
trace_type = seq(1:10) 

### FUNCTION ###

## File paths
# ts_fp = "../data/trace2/TraCE-21K-II.ann.TS.nc" #Just for decade time indices
var_fp = paste0("../data/trace2/TraCE-21K-II.hv.", var_name, ".nc")

## Open files
# ts_file <- nc_open(ts_fp)
var_file <- nc_open(var_fp)

## Get indices from ts file and define pft
trace_lon <- ncvar_get(var_file, 'lon')
trace_lat <- ncvar_get(var_file, 'lat')
trace_ann_time <- ncvar_get(var_file, 'time')
dec_inds <- seq(1, length(trace_ann_time), by = 10)
trace_dec_time <- trace_ann_time[dec_inds]

## Get dimensions of new variable
var_dims <- sapply(var_file$var[[var_name]]$dim, function(x) x$len)
num_dims = length(var_dims) ## This only makes sense for 4-d, ignore for 3-d

## Define our dimensions
lon <- ncdim_def("lon","degrees_east",trace_lon)
lat <- ncdim_def("lat","degrees_north",trace_lat)
time <- ncdim_def("time","decade",trace_dec_time)
type <- ncdim_def(type_name,"type",trace_type)

# Define variable and time length
time_length = NULL
if(num_dims==3){
  TraCE_devavg_var <- ncvar_def(var_name,var_units,list(lon,lat,time),longname=var_longname)
  time_length = var_dims[3]
} else if(num_dims==4){
  TraCE_devavg_var <- ncvar_def(var_name,var_units,list(lon,lat,type,time),longname=var_longname)
  time_length = var_dims[4]
}

## Create new nc file
outfile_fp = paste0("../data/trace2/TraCE-21K-II.decavg.", var_name, ".nc")
out_file <- nc_create(outfile_fp, list(TraCE_devavg_var))

## Begin looping through data time chunk by time chunk
start_time = Sys.time()
for (t in seq(1, time_length, by = chunk_size)) {
  if((t-1)%%1000 == 0){
    if(t==1){
      cat("Beginning processing at time index: 1\n")
    }
    else{
      cat("Processing ka starting at time index:", t, "\n")
    }
  }
  
  ## If there is less than chunksize data left
  if((t+chunk_size-1)>time_length){
    chunk_size = (time_length-t+1)
  }
  
  ## Get chunk size years of data at a time
  chunk_vals = NULL
  if(num_dims==3){
    chunk_vals <- ncvar_get(var_file, var_name, start = c(1, 1, t),
                            count = c(var_dims[1], var_dims[2], chunk_size))
  }else if(num_dims==4){
    chunk_vals <- ncvar_get(var_file, var_name, start = c(1, 1, 1, t),
                            count = c(var_dims[1], var_dims[2], var_dims[3], chunk_size))
  }
  
  ## For each chunksize, go through decade by decade and average, then store values
  for(decade in (1:(chunk_size/10))){
    
    ## Define starting and ending index
    start_ind = 1 + ((decade-1)*10)
    end_ind = start_ind+9
    
    ## Get 10 values, average them, and store the result
    if(num_dims==3){ # in the case we have only 3d variable
      
      decade_vals <- chunk_vals[,,start_ind:end_ind] ## slicing for each decade
      decavg_vals <- apply(decade_vals, c(1, 2), mean) ## averaging
      ncvar_put(out_file, TraCE_devavg_var, decavg_vals, start = c(1, 1, (t - 1) / 10 + decade), 
                count = c(var_dims[1], var_dims[2], 1))
      
    }else if(num_dims==4){ #in the case we have 4d variable
      
      decade_vals <- chunk_vals[,,,start_ind:end_ind] ## slicing for each decade
      decavg_vals <- apply(decade_vals, c(1, 2, 3), mean) ## averaging
      ncvar_put(out_file, TraCE_devavg_var, decavg_vals, start = c(1, 1, 1, (t - 1) / 10 + decade), 
                count = c(var_dims[1], var_dims[2], var_dims[3], 1))
    }
  }
}
nc_close(out_file)
nc_close(var_file)

end_time = Sys.time()
runtime = end_time - start_time
cat("Runtime: ", runtime, "minutes\n")
