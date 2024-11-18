"
This code takes netcdf data and combines them into one file

It can take variables with different dimensions, ie some 3-d variables
and some 4-d variables but I think all the variables need to have the same lon,
lat, and time dimensions
"

library(ncdf4)

### PARAMETERS ###

## Define were input data is and where output file will be stored
data_dir = "~/Desktop/ecsc_500/data/trace2/"

## Define files to be combined
files = list(
  list(var_name = "BURN", fp = "TraCE-21K-II.decavg.BURN.nc"),
  list(var_name = "CFLUXFIRE", fp = "TraCE-21K-II.decavg.CFLUXFIRE.nc"),
  list(var_name = "NPP", fp = "TraCE-21K-II.decavg.NPP.nc"),
  list(var_name = "FPCGRID", fp = "TraCE-21K-II.decavg.FPCGRID.nc")
)

### FUNCTION ###

new_nc_vars = vector("list", length(files))

## Create new nc variable definitions for all variables
for (i in seq_along(files)) {
  
  ## Get var name and open nc file
  var_name <- files[[i]]$var_name
  fp <- files[[i]]$f
  var_nc <- nc_open(paste0(data_dir, fp))
  
  ## Get name and units if there are defined
  var_longname <- ncatt_get(var_nc, var_name, "long_name")$value
  var_units <- ncatt_get(var_nc, var_name, "units")
  if(var_units$hasatt==0){
    var_units = ""
  }else{
    var_units = var_units$value
  }
  
  ## Get the dimensions of the variable, it should be 3-d or 4-d
  var_dims =  sapply(var_nc$var[[var_name]]$dim, function(x) x$len)
  dim_names = sapply(var_nc$var[[var_name]]$dim, function(x) x$name)
  num_dims = length(var_dims)
  
  ## Define the nc dimensions objects for the nc variable definition
  dims = vector("list", num_dims)
  for(j in (1:num_dims)){
    dim_atts = ncatt_get(var_nc, dim_names[j])
    dim_units = dim_atts$units
    dim_vals = ncvar_get(var_nc, dim_names[j])
    dims[[j]] = ncdim_def(dim_names[j],dim_units,dim_vals)
  }
  
  ## Define our new nc variable and store it to a list
  new_nc_var = ncvar_def(var_name,var_units,dims,longname=var_longname)
  new_nc_vars[[i]] = new_nc_var
  
  nc_close(var_nc)
}

## Create new nc file with all the new variable definitions
file_name = "TraCE-21K-II.decavg.combined_veg.nc"
outfile_fp = paste0(data_dir, file_name)
outfile <- nc_create(outfile_fp, new_nc_vars)

## Now we will transfer the data to our new nc file
for (i in seq_along(files)) {
  
  ## Get varname and open file
  var_name <- files[[i]]$var_name
  cat("On file ", var_name, "\n")
  fp <- files[[i]]$fp
  var_file <- nc_open(paste0(data_dir, fp))
  
  ## Get variable dimensions
  var_dims <- sapply(var_file$var[[var_name]]$dim, function(x) x$len)
  num_dims = length(var_dims)
  
  ## Define time length, we will use this to loop through data
  if(num_dims==3){
    time_length = var_dims[3]
  }else if(num_dims==4){
    time_length = var_dims[4]
  }else{
    stop("Not the correct amount of dims!")
  }
  
  ## Loop through data
  chunk_size = 1000 ## Reading by chunks to avoid memory issues
  for (t in seq(1, time_length, by = chunk_size)){
    
    cat("Looping through time index ", t, "\n")
    
    ## If there is less than chunksize data left
    if((t+chunk_size-1)>time_length){
      chunk_size = (time_length-t+1)
    }
    
    ## Read data and store in new nc
    chunk_vals = NULL
    if(num_dims==3){
      chunk_vals <- ncvar_get(var_file, var_name, start = c(1, 1, t),
                              count = c(var_dims[1], var_dims[2], chunk_size))
      ncvar_put(outfile, new_nc_vars[[i]], chunk_vals, start = c(1,1,t),
                count = c(var_dims[1], var_dims[2], chunk_size))
    }else if(num_dims==4){
      chunk_vals <- ncvar_get(var_file, var_name, start = c(1, 1, 1, t),
                              count = c(var_dims[1], var_dims[2], var_dims[3], chunk_size))
      ncvar_put(outfile, new_nc_vars[[i]], chunk_vals, start = c(1,1,1,t),
                count = c(var_dims[1], var_dims[2], var_dims[3], chunk_size))
    }
  }
  nc_close(var_file)
}
nc_close(outfile)

