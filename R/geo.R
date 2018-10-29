#' Build a topographic rasters stack (elevation, slope, aspect)
#'
#' This function builds a topographic rasters stack for Belgium (elevation, slope, aspect). The rasters are projected in the
#' 3812 EPSG code. No input parameters are required.
#' @author Thomas Goossens - pokyah.github.io
#' @return A stack of topographic rasters
#' @export
build_lowRes_terrain_rasters.fun <- function() {
  # Get the Belgium DEM
  bel.ele.ras = raster::getData("alt", country = "BE", mask = TRUE)

  # The data are not projected but are in longlat so we need to project it to get the distance units
  bel.ele.ras <- raster::projectRaster(bel.ele.ras, crs = toString((dplyr::filter(rgdal::make_EPSG(), code=="3812"))$prj4))

  # compute the slope from the elevation
  bel.slope.ras <- raster::terrain(bel.ele.ras, opt="slope", unit="degrees")

  # compute the aspect from the elevation
  bel.aspect.ras <- raster::terrain(bel.ele.ras, opt="aspect", unit="degrees")

  # create the stack of rasters
  topo.stack.ras <- stack(bel.ele.ras, bel.slope.ras, bel.aspect.ras)

  # Return the stack of rasters
  return(topo.stack.ras)
}

#' Build a high resolution topographic rasters stack (elevation, slope, aspect)
#'
#' This function builds a topographic rasters stack for Belgium (elevation, slope, aspect). The rasters are projected in the
#' 3812 EPSG code.
#' @author Thomas Goossens - pokyah.github.io
#' @param country_code.chr a character specifying the ISO contrycode. Ex : BE for belgium
#' @param NAME_1.chr a character specifying the NAME_1 value for lower than country level information
#' @param aggregation_factor.num a numeric specifying the aggregation factor to get the desired spatial resolution
#' @param EPSG.chr a character specifying the EPSG code of the desired Coordiante Reference System (CRS)
#' @param path.chr a character specifying the path where to dowload the SRTM data
#' @return A stack of topographic rasters
#' @export
build.SRTM.terrain.90m.ras.fun <- function(country_code.chr, NAME_1.chr=NULL, aggregation_factor.num=NULL, EPSG.chr=NULL, path.chr) {

  # Path to downloaded SRTM Tiles refs
  srtm.tiles.ref <- raster::shapefile("./external-data/Digital_Elevation_Model/90m_resolution/srtm/tiles.shp")

  # Get country geometry first
  if(length(list.files(paste0(path.chr,"/Boundaries"), all.files = TRUE, include.dirs = TRUE, no.. = TRUE))>0){
    extent.sp <- readRDS(paste0(path.chr,"/Boundaries/", "GADM_2.8_BEL_adm1.rds"))
  }else{
    extent.sp <- raster::getData('GADM', country=country_code.chr, level=1)
    crs <- crs(extent.sp)
  }
  if(!is.null(NAME_1.chr)){
    extent.sp <- subset(extent.sp, NAME_1 == NAME_1.chr)
  }

  # to compute slope, aspect, etc, we need neighbourings pixels out of extent boundary .So we buffer it :
  # https://gis.stackexchange.com/questions/234135/enlarging-polygon-slightly-using-r
  extent.sf <- sf::st_transform(sf::st_as_sf(extent.sp), 3812)
  larger.extent.sp <- rgeos::gBuffer(as(extent.sf, "Spatial"), width = 5000)
  larger.extent.sp <- spTransform(larger.extent.sp, crs(extent.sp))

  # Intersect extent geometry and tile grid
  intersects <- rgeos::gIntersects(larger.extent.sp, srtm.tiles.ref, byid=T)
  tiles      <- srtm.tiles.ref[intersects[,1],]

  # Download tiles using getData

  # inspired from https://www.gis-blog.com/download-srtm-for-an-entire-country/
  srtm_list  <- list()
      for(i in 1:length(tiles)){
        lon <- raster::extent(tiles[i,])[1]  + (raster::extent(tiles[i,])[2] - raster::extent(tiles[i,])[1]) / 2
        lat <- raster::extent(tiles[i,])[3]  + (raster::extent(tiles[i,])[4] - raster::extent(tiles[i,])[3]) / 2


        tile <- raster::getData('SRTM', #data are downloaded from http://www.cgiar-csi.org/. See getData do of pokyah/raster repo on github
                                lon=lon,
                                lat=lat,
                                download = TRUE,
                                path = path.chr)
        srtm_list[[i]] <- tile
     }

  # Mosaic tiles
  srtm_list$fun <- mean
  devtools::use_data(srtm_list, overwrite = TRUE)
  srtm_mosaic.ras <- do.call(raster::mosaic, srtm_list)
  devtools::use_data(srtm_mosaic.ras, overwrite = TRUE)

  # Crop tiles to extent borders
  extent.elevation.ras <- raster::crop(srtm_mosaic.ras, larger.extent.sp)
  extent.elevation.ras <- raster::mask(extent.elevation.ras, larger.extent.sp)

  # transform to desired CRS
  if(!is.null(EPSG.chr)){
    raster::projectRaster(extent.elevation.ras, crs = toString((dplyr::filter(rgdal::make_EPSG(), code==EPSG.chr))$prj4))
  }

  # aggregate to lower resolution
  # inspired from https://stackoverflow.com/questions/32278825/how-to-change-the-resolution-of-a-raster-layer-in-r
  if(!is.null(aggregation_factor.num)){
    extent.elevation.ras <- raster::aggregate(extent.elevation.ras, fact=aggregation_factor.num)
  }

  # compute the slope from the elevation
  # inspired from https://rpubs.com/etiennebr/visualraster
  extent.slope.ras <- raster::terrain(extent.elevation.ras, opt="slope", unit="degrees")
  extent.aspect.ras <- raster::terrain(extent.elevation.ras, opt="aspect", unit="degrees")
  extent.roughness.ras <- raster::terrain(extent.elevation.ras, opt="roughness")

  # stack the rasters
  extent.terrain.ras = raster::stack(
    extent.elevation.ras,
    extent.slope.ras,
    extent.aspect.ras,
    extent.roughness.ras)

  # crop to non enlarged extent
  extent.terrain.ras <- raster::crop(extent.terrain.ras, extent.sp)
  devtools::use_data(extent.terrain.ras, overwrite = TRUE)
}

#' Build a sp/sf that contains the locations of the Pameseb automatic weather stations
#'
#' This function builds a spatial sp object that contains the he rasters are projected in the
#' 3812 EPSG code. No input parameters are required.
#' @author Thomas Goossens - pokyah.github.io
#' @param sf.bool A boolean specifying if we want as sp or sf (TRUE for sf)
#' @param EPSG.chr a character specifying the EPSG code of the desired Coordiante Reference System (CRS)
#' @return A sp spatial grid with the desired resolution clipped to the Wallonia Polygon
#' @export
build.ps.locations.points_sf.fun <- function(sf.bool, EPSG.chr){
  # proj4 of the Agromet API data
  proj4.chr <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  # Retrieving useless data from API
  demo.records.df <- prepare_agromet_API_data.fun(
    get_from_agromet_API.fun(
      user_token.chr = Sys.getenv("AGROMET_API_V1_KEY"),
      table_name.chr = "cleandata",
      stations_ids.chr = "all",
      sensors.chr = "tsa",
      dfrom.chr = as.character(Sys.Date()-60),
      dto.chr = as.character(Sys.Date()-59),
      api_v.chr = "v2"
    ), table_name.chr = "cleandata"
  )

  # Filtering records to keep only the useful ones (removing unecessary stations)
  demo.records.df <- dplyr::filter(demo.records.df, network_name == "pameseb")
  demo.records.df <- dplyr::filter(demo.records.df, type_name != "Sencrop")
  demo.records.df <- dplyr::filter(demo.records.df, !is.na(to))
  demo.records.df <- dplyr::filter(demo.records.df, state == "Ok")
  demo.records.df <- dplyr::filter(demo.records.df, !is.na(tsa))

  # Selecting only the useful features
  demo.records.df <- dplyr::select(demo.records.df, one_of(c("sid", "mtime", "longitude", "latitude", "altitude")))

  # defining the stations locations sp object
  stations.df <- dplyr::filter( demo.records.df, mtime == min(mtime, na.rm = TRUE))

  stations.sf <- sf::st_as_sf(
    x = stations.df,
    coords = c("longitude", "latitude"),
    crs = proj4.chr)

  # transform to desired CRS
  if(!is.null(EPSG.chr)){
    stations.sf <- sf::st_transform(x = stations.sf, crs = as.numeric(EPSG.chr) )
  }

  if(sf.bool == TRUE){
    stations.sf
  }
  # else transform to sf
  else
  {
    stations.sp <- as(stations.sf, "Spatial")
  }

}

#' Build a sp/sf interpolation grid with the desired spatial resolution for Wallonia
#'
#' Inspired from https://stackoverflow.com/questions/41787313/how-to-create-a-grid-of-spatial-points,
#' https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/OverviewCoordinateReferenceSystems.pdf, https://gis.stackexchange.com/questions/22843/converting-decimal-degrees-units-to-km-in-r,
#' https://stackoverflow.com/questions/48727511/r-grid-of-points-from-polygon-input
#' @author Thomas Goossens - pokyah.github.io
#' @param country_code.chr a character specifying the ISO contrycode. Ex : BE for belgium
#' @param NAME_1.chr a character specifying the NAME_1 value for lower than country level information
#' @param res.num A numeric representing the spatial resolution of the desired grid (in meters)
#' @param geom.chr A character specifying the geometry of the interpolation grid. Cant take a value among ("polygons", "centers" or "corners")
#' @param sf.bool A boolean specifying if we want as sp or sf (TRUE for sf)
#' @param EPSG.chr A character specifying the EPSG code of the desired Coordiante Reference System (CRS)
#' @return A sf spatial grid with the desired resolution clipped to the Wallonia Polygon
#' @export
build.vs.grid.fun <- function(country_code.chr, NAME_1.chr, res.num, geom.chr, sf.bool, EPSG.chr = NULL){ #build.SRTM_terrain.90m.ras.fun

  # Get country geometry first
  extent.sp <- raster::getData('GADM', country=country_code.chr, level=1, path =  "./external-data/Boundaries")

  if(!is.null(NAME_1.chr)){
    extent.sp <- subset(extent.sp, NAME_1 == NAME_1.chr)
  }

  # convert from geographic lat/lon to projected EPSG for Belgian Lambert 2008 (needed to make grid in meters)
  extent.sf <- sf::st_transform(sf::st_as_sf(extent.sp), 3812)

  # make the grid and clip it with the extent boundaries
  grid.sf <- sf::st_intersection(
    sf::st_sf(
      sf::st_make_grid(
        extent.sf, cellsize= res.num, n=c(500,500), what= geom.chr, crs = 3812)
    ),
    extent.sf)

  # Create column cell_area of each cell
  if(geom.chr=="polygons"){
    grid.sf <- grid.sf %>% dplyr::mutate(cell_area = sf::st_area(.))
  }

  # transform to desired CRS
  if(!is.null(EPSG.chr)){
    grid.sf <- sf::st_transform(x = grid.sf, crs = as.numeric(EPSG.chr) )
  }

  # append an id to each cell
  grid.sf$sid <- paste0(seq_along(1:nrow(data.frame(grid.sf))))

  # select useful features
  grid.sf <- grid.sf %>% dplyr::select(ISO, NAME_0, NAME_1, sid, sf..st_make_grid.extent.sf..cellsize...res.num..n...c.500..500...)

  # rename geometry column
  names(grid.sf)[names(grid.sf) == "sf..st_make_grid.extent.sf..cellsize...res.num..n...c.500..500..."] <- "geometry"
  sf::st_geometry(grid.sf) <- "geometry"

  if(sf.bool == TRUE){
    grid.sf
  }else{
    grid.sp <- as(grid.sf, "Spatial")
  }
}

#' Build a sf object containing reclassified CLC data
#'
#' @author Thomas Goossens - pokyah.github.io
#' @param country_code.chr a character specifying the ISO contrycode. Ex : BE for belgium
#' @param NAME_1.chr a character specifying the NAME_1 value for lower than country level information
#' @param EPSG.chr A character specifying the EPSG code of the desired Coordiante Reference System (CRS)
#' @param EPSG.corine.chr A character specifying the EPSG code of the downloaded Corine Data
#' @path.corine.shapefile.chr A character specifying the path where th corine shapefiles resides
#' @return a sf data frame containing reclasssified corine land cover data
build_cover.sf.fun <- function(
  country_code.chr,
  NAME_1.chr,
  EPSG.chr,
  path.corine.shapefile.chr,
  EPSG.corine.chr){

  # Get country geometry first
  extent.sp <- raster::getData('GADM', country=country_code.chr, level=1)
  file.remove(list.files(pattern = "GADM_"))
  crs <- crs(extent.sp)

  if(!is.null(NAME_1.chr)){
    extent.sp <- subset(extent.sp, NAME_1 == NAME_1.chr)
  }
  # reproject in the desired CRS
  extent.sp <- sp::spTransform(extent.sp, sp::CRS(projargs = dplyr::filter(rgdal::make_EPSG(), code == EPSG.chr)$prj4))

  # Download CORINE land cover for Belgium from http://inspire.ngi.be/download-free/atomfeeds/AtomFeed-en.xml
  corine.sp <- maptools::readShapePoly(path.corine.shapefile.chr)

  # Define the CRS of corine land cover data
  # We know the crs from the metadata provided on the website http://inspire.ngi.be/download-free/atomfeeds/AtomFeed-en.xml
  raster::crs(corine.sp) <- as.character(dplyr::filter(rgdal::make_EPSG(), code == "3812")$prj4)

  # Crop corine to extent
  corine.extent.sp <- raster::crop(corine.sp, extent.sp)

  # legend of corine
  download.file("http://www.eea.europa.eu/data-and-maps/data/corine-land-cover-2006-raster-1/corine-land-cover-classes-and/clc_legend.csv/at_download/file",
                destfile = "corine.legend.csv")
  legend <- read.csv(file = "corine.legend.csv", header = TRUE, sep = ",")
  file.remove("corine.legend.csv")

  # Legend codes present in extent
  legend.extent <- data.frame(unique(corine.extent.sp$code_12))

  # https://stackoverflow.com/questions/38850629/subset-a-column-in-data-frame-based-on-another-data-frame-list
  legend.extent <- subset(legend, CLC_CODE %in% legend.extent$unique.corine.extent.sp.code_12.)

  # CLC_CODE class from integer to numeric
  legend.extent$CLC_CODE <- as.numeric(legend.extent$CLC_CODE)

  # from sp to sf
  corine.extent.sf <- sf::st_as_sf(corine.extent.sp)
  corine.extent.sf$code_12 <- as.numeric(paste(corine.extent.sf$code_12))

  # Reclass Corine according to the following reclassification table
  cover.sf <-
    sf::st_as_sf(
      dplyr::mutate(
        corine.extent.sf,
        CLASS = dplyr::case_when(
          code_12 <= 142 ~ "Artificials surfaces",
          code_12 == 211 ~ "Agricultural areas",
          code_12 == 222 ~ "Agricultural areas",
          code_12 == 231 ~ "Herbaceous vegetation",
          code_12 == 242 ~ "Agricultural areas",
          code_12 == 243 ~ "Agricultural areas",
          code_12 == 311 ~ "Forest",
          code_12 == 312 ~ "Forest",
          code_12 == 313 ~ "Forest",
          code_12 == 321 ~ "Herbaceous vegetation",
          code_12 == 322 ~ "Herbaceous vegetation",
          code_12 == 324 ~ "Forest",
          code_12 > 400 ~ "Water"))
    )
}

#' Get cover classes percentages for buffered points with custom radius
#'
#' @author Thomas Goossens - pokyah.github.io
#' @param cover.sf a sf polygon of the land cover
#' @param points a sf points of the locations on which surrounding cover needs to be summarized
#' @param radius.num a numeric specifying the radius of the buffere th corine shapefiles resides
#' @return a sf containing the %
get.points.cover_pct.fun <- function(
  cover.sf,
  points.sf,
  radius.num){

  # transposing to dataframe for data spreading (impossible (?) to achieve with dplyr spread)
  cover_2mlr.fun <- function(data.sf) {

    # Delete geometry column
    data.df <- data.frame(data.sf)

    # Reshape data with CLASS labels as columns names
    # https://stackoverflow.com/questions/39053451/using-spread-with-duplicate-identifiers-for-rows
    data.df <- data.df %>%
      dplyr::select(sid, CLASS, cover_rate) %>%
      reshape2::dcast(sid ~ CLASS, fun = sum)

    # https://stackoverflow.com/questions/5620885/how-does-one-reorder-columns-in-a-data-frame
     return(data.df)
  }

  # reproject the cover in the same CRS as grid and physical stations
  sf::st_transform(cover.sf, sf::st_crs(points.sf))

  # Make a buffer around  points
  # https://gis.stackexchange.com/questions/229453/create-a-circle-of-defined-radius-around-a-point-and-then-find-the-overlapping-a
  # https://stackoverflow.com/questions/46704878/circle-around-a-geographic-point-with-st-buffer
  points.sf <- sf::st_buffer(x = points.sf, dist = radius.num)

  # extract cover information into the buffered points
  cover.points.sf <- sf::st_intersection(points.sf, cover.sf)
  cover.points.sf <- cover.points.sf %>%
    dplyr::mutate(
      bid = paste0(seq_along(1:nrow(cover.points.sf))))

  # create new column with area of each intersected cover polygon
  cover.area.points.sf <- cover.points.sf %>%
    dplyr:: group_by(bid) %>%
    dplyr::summarise() %>%
    mutate(shape.area = st_area(.))

  # Make a column with percentage of occupation of each land cover inside each grid point buffer
  # https://github.com/r-spatial/sf/issues/239
  cover_rate.points.sf <- sf::st_join(
    x = cover.points.sf,
    y = cover.area.points.sf,
    join = sf::st_covered_by
  ) %>%
    dplyr::select(sid, CLASS, shape.area) %>%
    dplyr::mutate(cover_rate = as.numeric(shape.area)/(pi*radius.num^2) * 100) #500 = buffer radius

  # transposing to dataframe for data spreading (impossible (?) to achieve with dplyr spread)
  cover_rate.points.df <- cover_2mlr.fun(cover_rate.points.sf)
  colnames(cover_rate.points.df) <- gsub(" ","_",colnames(cover_rate.points.df))

  # merge cover data with points.1000.pt.sf
  cover_rate.points.sf = merge(grid.1000.pt.sf, cover_rate.points.df, by = "sid")

  # only keep relevant columns
  cover_rate.points.sf <- cover_rate.points.sf %>%
    dplyr::select(1,15:19)
 }

#' Build a responsive leaflet map displaying agromet AWS network data
#' @author Thomas Goossens - pokyah.github.io
#' @param records.sf A sf containing the records to be displayed
#' @return a leaflet map object
#' @export
build_leaflet_template.fun <- function(records.sf){
  responsiveness.chr = "\'<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\'"

  template.map <- leaflet::leaflet() %>%
    addProviderTiles(group = "Stamen",
                     providers$Stamen.Toner,
                     options = providerTileOptions(opacity = 0.25)
    ) %>%
    addProviderTiles(group = "Satellite",
                     providers$Esri.WorldImagery,
                     options = providerTileOptions(opacity = 1)
    ) %>%
    fitBounds(sf::st_bbox(records.sf)[[1]],
              sf::st_bbox(records.sf)[[2]],
              sf::st_bbox(records.sf)[[3]],
              sf::st_bbox(records.sf)[[4]]
    ) %>%
    addLayersControl(baseGroups = c("Stamen", "Satellite"),
                     overlayGroups = c("KNMI rain radar", "stations", "MNT", "slope", "aspect"),
                     options = layersControlOptions(collapsed = TRUE)
    ) %>%
    addEasyButton(easyButton(
      icon="fa-crosshairs", title="Locate Me",
      onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
    htmlwidgets::onRender(paste0("
                                 function(el, x) {
                                 $('head').append(",responsiveness.chr,");
                                 }"))
  return(template.map)
}

#' Build a static map displaying predictions and their related error
#' @author Loïc Davadan <- ldavadan.github.io
#' @param gridded.data.df A data frame obtained from a SpatialGridDataFrame containing data
#' @param boundaries.sf A sf containing data from Wallonia boundaries
#' @param layer.error.bool A boolean specifying if you want to display the layer with error
#' @param legend.error.bool A boolean specifying if you want to display the legend of the error layer
#' @param pretty_breaks.bool A boolean specifying the type of legend you want. TRUE for pretty breaks, FALSE for quantile scale
#' @param title.chr A character specifying the title you want for your map
#' @param target.chr A character specifying the predicted parameter. One of "tsa", "hra" or "hct"
#' @return a ggplot map object
#' @export
static.ggmap <- function(
  gridded.data.df,
  boundaries.sf,
  layer.error.bool,
  legend.error.bool,
  pretty_breaks.bool,
  title.chr,
  target.chr
){
  
  library(ggplot2)
  library(grid)
  library(maps)
  library(maptools)
  library(ggsn)
  library(RColorBrewer)
  
  if(target.chr == "tsa"){ legend_title = "Temperature (°C)"}
  if(target.chr == "hra"){ legend_title = "Relative humidity (%)"}
  if(target.chr == "hct"){ legend_title = "Leaves wetness (%)"}
  
  if(pretty_breaks.bool == TRUE){
    # inspired by https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/
    # prepare legend with pretty breaks
    # compute quantiles from predictions values
    quantiles <- unique(stats::quantile(gridded.data.df$response,
                                 probs = seq(0, 1, length.out = 11), na.rm=T))
    labels <- c()
    breaks <- unique(round(c(-60,
                             min(gridded.data.df$response, na.rm = TRUE),
                             quantiles,
                             max(gridded.data.df$response, na.rm = TRUE)), 1))
    
    labels <- paste0(labels, paste0(format(round(breaks, 1), nsmall = 1)))
    labels <- labels[2:length(labels)]
    gridded.data.df$response_quantiles <- cut(gridded.data.df$response, 
                                              breaks = breaks, 
                                              labels = labels, 
                                              include.lowest = T)
    breaks_scale <- levels(gridded.data.df$response_quantiles)
    labels_scale <- rev(breaks_scale)
  }
  if(pretty_breaks.bool == FALSE){
    # inspired by https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/
    quantiles <- unique(stats::quantile(gridded.data.df$response,
                                 probs = seq(0, 1, length.out = 11), na.rm=T))
    labels <- c()
    labels <- paste0(labels, paste0(format(round(quantiles, 1), nsmall = 1),
                                    " – ",
                                    format(round(quantiles[2:length(quantiles)], 1), nsmall = 1)))
    labels <- labels[1:length(labels)-1]
    gridded.data.df$response_quantiles <- cut(gridded.data.df$response,
                                                       breaks = quantiles,
                                                       labels = labels,
                                                       include.lowest = T)
  }
  
  ggmap <- ggplot2::ggplot(gridded.data.df) +
    # choose data to display on the layer
    ggplot2::geom_raster(mapping = ggplot2::aes(coords.x1, coords.x2, fill = response_quantiles), na.rm = TRUE, interpolate = T)
  # choose color palette and create a legend with pretty breaks
  if(pretty_breaks.bool == TRUE){
    ggmap <- ggmap +
      ggplot2::scale_fill_manual(
        values = rev(RColorBrewer::brewer.pal(n = length(labels_scale), name = "RdYlBu")), # palette to use
        breaks = rev(breaks_scale), # legend breaks
        name = legend_title,
        drop = FALSE,
        labels = labels_scale, # legend labels
        # legend parameters
        guide = ggplot2::guide_legend(
          direction = "vertical",
          keyheight = grid::unit(7, units = "mm"),
          keywidth = grid::unit(3, units = "mm"),
          title.position = 'top',
          title.vjust = 0.5,
          label.vjust = 1,
          ncol = 1,
          bycol = T,
          reverse = F,
          label.position = "right"
        )
      )
  }
  # color palette with discrete classes with quantile scale
  if(pretty_breaks.bool == FALSE){
    ggmap <- ggmap +
      ggplot2::scale_fill_brewer(legend_title, palette = "RdYlBu", direction = -1)
  }
    
  if(layer.error.bool == TRUE){
    ggmap <- ggmap + 
      # display a layer with standard error
      ggplot2::geom_raster(ggplot2::aes(coords.x1, coords.x2, alpha = se), fill = "white", na.rm = TRUE, interpolate = TRUE) +
      # whitening it
      ggplot2::scale_alpha_continuous("Standard\nError",range = c(0.1,1), guide = legend.error.bool)
      # order the two legends if they both are displayed
      if(legend.error.bool == TRUE){
        ggmap <- ggmap + ggplot2::guides(fill = ggplot2::guide_legend(order = 1),
                                alpha = ggplot2::guide_legend(order = 0))
      }
      
  }
  ggmap <- ggmap +
    ggplot2::ggtitle(title.chr) +   # add title
    # add boundaries layer
    ggplot2::geom_sf(data = boundaries.sf, ggplot2::aes(fill = ISO), fill = NA, color = "black", size = 0.6) +
    # add north symbol
    ggsn::north(boundaries.sf, scale = 0.1, location = "bottomleft",
                anchor = c(x = 780000, y = 550000), symbol = 12) +
    # add scalebar
    ggsn::scalebar(boundaries.sf, dist = 50, dd2km = FALSE, model = "GRS80", 
                   st.dist = 0.03, st.size = 4, box.fill = c("black", "white"),
                   box.color = "black", anchor = c(x = 700000, y = 520000)) +
    # add copyright
    ggplot2::annotation_custom(grob = grid::textGrob("© CRA-W"),
                      xmin = 790000, xmax = 790000, ymin = 520000, ymax = 520000) +
    # display resolution of the map
    ggplot2::annotation_custom(grob = grid::textGrob("Resolution : 1 km²"),
                      xmin = 558000, xmax = 558000, ymin = 671000, ymax = 671000) +
    # parameters for visualization
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white"),
          axis.title = ggplot2::element_text(color = NA),
          panel.grid = ggplot2::element_line(color = NA),
          axis.ticks = ggplot2::element_line(color = NA),
          axis.text = ggplot2::element_text(colour = NA),
          legend.title = ggplot2::element_text(size = 12, face = "bold", vjust = 1),
          legend.text = ggplot2::element_text(size = 11, margin(b = 1)),
          legend.background = ggplot2::element_rect(fill = "transparent"),
          legend.position = c(0.12,0.38),
          legend.box = "horizontal")
  ggmap
}

