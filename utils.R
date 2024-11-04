"
Utility functions for analysis of TraCE climate simulation data
"

find_latlon <- function(trace_lats, trace_lons, lat, lon){
  
  ## Get increments for latitude and longitude
  lat_increments = array(NA, dim = c(length(trace_lats)-1))
  for(i in 1:47){
    lat_increments[i] = trace_lats[i+1] - trace_lats[i]
  }
  
  lat_incr = max(lat_increments)
  lon_incr = trace_lons[2] - trace_lons[1]
  
  #Convert lon if west of 0 lon
  if(lon < 0){
    lon = 360 + lon
    print("Shifted longitude to range (-180,180) from range (0,360)")
  }
  
  # Get indices for closest trace grid cell to given lake lat/lon
  lat_ind = which(trace_lat > lat & trace_lat <= (lat+lat_incr))
  
  if(length(lat_ind)>1){
    print("GRABBBED TWO LAT INDS: bc point was too close to grid line")
  }
  lon_ind = which(trace_lon < lon & trace_lon >= (lon-lon_incr))
  
  grid_lon = trace_lons[lon_ind]
  grid_lat = trace_lats[lat_ind]
  
  return(list(lon_index = lon_ind, lat_index = lat_ind, 
              gridcell_lon = grid_lon, gridcell_lat = grid_lat))
}

