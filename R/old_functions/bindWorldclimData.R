bindWorldClimData <- function(df, var = "bio", bioclim_var_num = 1, new_var_colname = "tmax",
                              res = 10, path = 'D:/bioclim_data', month = NULL, cellsize_km = NULL) {

    # Download WorldClim data (SpatRaster)
    clim_data <- geodata::worldclim_global(var = var, res = res, path = path)

    # Pick layer
    selected_layer <- if (identical(var, "bio")) {
        clim_data[[bioclim_var_num]]
    } else if (var %in% c("srad", "vapr", "tavg", "tmin", "tmax", "wind")) {
        if (is.null(month)) {
            # mean over layers (SpatRaster) – keep it explicit
            terra::app(clim_data, fun = mean, na.rm = TRUE)
        } else {
            clim_data[[month]]
        }
    } else {
        stop("Unsupported var: use 'bio', 'srad', 'vapr', 'tavg', 'tmin', 'tmax', or 'wind'.")
    }

    # Guard column names
    if (!all(c("longitude", "latitude") %in% names(df))) {
        stop("df must have columns 'longitude' and 'latitude'")
    }

    # Build points in the raster's CRS
    pts_ll <- terra::vect(df, geom = c("longitude", "latitude"),
                          crs = terra::crs(selected_layer))

    # Extract (optionally with buffering)
    if (!is.null(cellsize_km)) {
        # project both to a metric CRS for meter-based buffering
        pts_m <- terra::project(pts_ll, "EPSG:3857")
        rad_m <- (cellsize_km * 1000) / 2
        buf_m <- terra::buffer(pts_m, width = rad_m)

        r_m <- terra::project(selected_layer, "EPSG:3857", method = "bilinear")
        extracted <- terra::extract(r_m, buf_m, fun = mean, na.rm = TRUE)
    } else {
        extracted <- terra::extract(selected_layer, pts_ll)
    }

    # Bind back and rename
    out <- cbind(df, extracted[, -1, drop = FALSE])
    names(out)[ncol(out)] <- new_var_colname
    return(out)
}
