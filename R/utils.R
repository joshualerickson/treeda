
#' Get Canopy Height Model stats
#'
#' @param sf A sf polygon or multipolygon object.
#' @param infile A file path to the Canopy Height Model (CHM).
#' @param outfile A file path where the .tifs will go.
#' @param FUN A function to use as lidR::lmf() function in lidR::find_trees().
#' @param unit Logical if CHM units are to be converted to feet.
#' @importFrom dplyr "%>%"
#'
#' @return A masked .tif per polygon within the outfile
#' path as well as a data.frame with zonal stats per id.
#' @export
#' @note Assumes that your infile CHM is in meters. Sometimes you will need to double quote your path if there are spaces or unique
#' characters that CLI needs quoted, e.g. '"C:/josh erickson/some file path/"'.
#'
get_tree_eda <- function(sf,
                       infile,
                       outfile,
                       FUN = function(x) { x * 0.17 + 3},
                       unit = TRUE){

  vals <- vapour::vapour_raster_info(infile)
  sf <- sf %>% sf::st_transform(vals$projection)
  sf_og <- sf %>% dplyr::mutate(id = dplyr::row_number())
  sf <- split(sf_og, sort(as.numeric(rownames(sf_og))))

  furrr::future_map(sf, ~get_raster_clip(., outfile = paste0(outfile,'objectid_',.$id, '.tif'),
                                       infile = infile))

  furrr::future_map(sf, purrr::safely(~get_raster_mask(., outfile, vals)))

  stats <- furrr::future_map(sf, purrr::safely(~get_raster_stats(., outfile, vals)))

  stats <- stats %>%
           purrr::keep(~length(.) != 0) %>%
           purrr::map(~.x[['result']]) %>%
           plyr::rbind.fill()

}


#' Get raster clip
#'
#' @param sf An sf object.
#' @param infile A file path to the simple feature to clip.
#' @param outfile A file path to write the clip to.
#' @note Sometimes you will need to double quote your path if there are spaces or unique
#' characters that CLI needs quoted, e.g. '"C:/josh erickson/some file path/"'.
#' @return
#'
get_raster_clip <- function(sf, infile, outfile){

  bb <- sf::st_bbox(sf)

  system(paste('gdalwarp -te ', paste(bb[[1]], bb[[2]], bb[[3]], bb[[4]], infile, outfile)))

}

#' Get rast mask
#'
#' @description This function uses terra::mask() to mask the input polygons to the Canopy Height Model (CHM) that
#' was clipped with the get_raster_clip(). This function is used within a furrr::future_map() call.
#' @param sf A sf object that has
#' @param outfile The path to put the masked .tif files.
#' @param vals A gdalinfo call via vapour package.
#' @param unit Logical if CHM units are to be converted to feet.
#'
#' @return Writes masked .tif files to the outfile folder.
get_raster_mask <- function(sf,
                            outfile,
                            vals,
                            unit = TRUE){

  r <- terra::rast(paste0(outfile,'objectid_',sf$id, '.tif'))
  terra::crs(r) <- vals$projstring
  r <- terra::mask(r, sf)
  if(unit){r <- r*3.28084}
  terra::writeRaster(r, paste0(outfile,'objectid_',sf$id, '.tif'), overwrite = TRUE)
}

#' Get raster stats
#'
#' @param sf A sf object (polygon).
#' @param outfile A file path to read-in the .tif files.
#' @param vals A gdalinfo call via vapour package.
#' @param FUN A function to use as lidR::lmf() function in lidR::find_trees().
#'
#' @return A data.frame with statistics per id.
#'
get_raster_stats <- function(sf,
                             outfile,
                             vals,
                             FUN = function(x) { x * 0.17 + 3}){


  raster_in <- terra::rast(paste0(outfile,'objectid_',sf$id, '.tif'))

  stats_new <- cbind.data.frame(sf, exactextractr::exact_extract(raster_in, sf,
                                                                 c('mean', 'min', 'max', 'median',
                                                                   'stdev', 'mode', 'coefficient_of_variation'))) %>%
    sf::st_as_sf()

  stats_new$quantiles <- exactextractr::exact_extract(raster_in, sf, function(values, coverage_fraction)
    list(quantile(values, na.rm = TRUE, probs = c(0.05, 0.1, 0.2, 0.25, 0.5,
                                                  0.75,0.80, 0.90, 0.95))))

  stats_new$skewness <- exactextractr::exact_extract(raster_in, sf, function(values, coverage_fraction)
    list(moments::skewness(values,na.rm = T)))

  stats_new$kurtosis <- exactextractr::exact_extract(raster_in,sf, function(values, coverage_fraction)
    list(moments::kurtosis(values,na.rm = T)))

  decTREE <- lidR::find_trees(raster_in, lidR::lmf(ws=FUN))

  crop_th <- sf::st_intersection(sf::st_as_sf(decTREE) %>%
                                   sf::st_transform(vals$projection),
                                 sf::st_geometry(sf))

  stats_new <- stats_new %>% dplyr::mutate(tree_count = nrow(crop_th),
                                           acre = as.numeric(units::set_units(sf::st_area(.), 'acres')),
                                           tree_per_acre = tree_count/acre)

}
