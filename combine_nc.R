"
This code takes netcdf data and combines them into one file
"
library(ncdf4)

data_dir = "~/Desktop/ecsc_500/data/trace2/"

files = list(
  list(var_name = "BURN", fp = "TraCE-21K-II.decavg.BURN.nc"),
  list(var_name = "CFLUXFIRE", fp = "TraCE-21K-II.decavg.CFLUXFIRE.nc"),
  list(var_name = "NPP", fp = "TraCE-21K-II.decavg.NPP.nc")
)

new_nc_vars = vector("list", length(files))

for (i in seq_along(files)) {
  
  var_name <- files[[i]]$var_name
  fp <- files[[i]]$fp
  
  var_nc <- nc_open(paste0(data_dir, fp))
  
  var_longname <- ncatt_get(var_nc, var_name, "long_name")$value
  var_units <- ncatt_get(var_nc, var_name, "units")
  if(var_units$hasatt==0){
    var_units = ""
  }else{
    var_units = var_units$value
  }
  
  var_dims =  sapply(var_nc$var[[var_name]]$dim, function(x) x$len)
  dim_names = sapply(var_nc$var[[var_name]]$dim, function(x) x$name)
  num_dims = length(var_dims)
  
  dims = vector("list", num_dims)
  for(j in (1:num_dims)){
    dim_atts = ncatt_get(var_nc, dim_names[j])
    dim_units = dim_atts$units
    dim_vals = ncvar_get(var_nc, dim_names[j])
    dims[[j]] = ncdim_def(dim_names[j],dim_units,dim_vals)
  }
  
  new_nc_var = ncvar_def(var_name,var_units,dims,longname=var_longname)
  new_nc_vars[[i]] = new_nc_var
  
  nc_close(var_nc)
}

## Create new nc file
file_name = "TraCE-21K-II.decavg.combined_veg.nc"
outfile_fp = paste0(data_dir, file_name)
outfile <- nc_create(outfile_fp, new_nc_vars)


for (i in seq_along(files)) {
  
  var_name <- files[[i]]$var_name
  cat("On file ", var_name, "\n")
  fp <- files[[i]]$fp
  
  var_file <- nc_open(paste0(data_dir, fp))
  
  var_dims <- sapply(var_file$var[[var_name]]$dim, function(x) x$len)
  num_dims = length(var_dims)
  
  if(num_dims==3){
    time_length = var_dims[3]
  }else if(num_dims==4){
    time_length = var_dims[4]
  }else{
    stop("Unknown dims!")
  }
  
  ### Loop through data
  chunk_size = 1000
  for (t in seq(1, time_length, by = chunk_size)){
    
    print("starting looping through data of file")
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

