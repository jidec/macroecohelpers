bindPrismData <- function(df,
                          prism_dir,
                          cell_width_km = 250,
                          variable = "tmean",
                          new_var_colname = "prism_tmean",
                          avg_of_years = NULL,
                          seasonal = TRUE) {
    library(dplyr); library(geosphere)
    library(raster); library(sf); library(rgdal)
    library(prism); library(colorEvoHelpers)

    # 1) set prism dir & filter to contiguous US + years window
    prism_set_dl_dir(prism_dir)
    df <- df %>%
        filter(cell_lat >= 24.52, cell_lat <= 49.38,
               cell_lon >= -125.0, cell_lon <= -66.94,
               year >= 2015, year < 2024)

    # 2) prep unique periods
    df2 <- df %>% select(year, season, cell, cell_lat, cell_lon)
    if (seasonal) {
        if (!is.null(avg_of_years)) {
            # one row per cell – season, average across all years in avg_of_years
            uniq <- df2 %>%
                filter(year %in% avg_of_years) %>%
                distinct(cell, season, .keep_all = TRUE)
            period_years <- rep(list(avg_of_years), nrow(uniq))
        } else {
            # one row per cell–season–year
            uniq <- df2 %>%
                distinct(cell, season, year, .keep_all = TRUE)
            period_years <- as.list(uniq$year)
        }
        # corresponding months for each season
        period_months <- lapply(uniq$season, function(ss) switch(
            ss, "1" = c(12,1,2), "2" = c(3,4,5),
            "3" = c(6,7,8), "4" = c(9,10,11)
        ))
    } else {
        if (!is.null(avg_of_years)) {
            # one row per cell, averaging all months & years in avg_of_years
            uniq <- df2 %>%
                filter(year %in% avg_of_years) %>%
                distinct(cell, .keep_all = TRUE)
            period_years  <- rep(list(avg_of_years), nrow(uniq))
        } else {
            # one row per cell–year, annual mean across all 12 months
            uniq <- df2 %>%
                distinct(cell, year, .keep_all = TRUE)
            period_years <- as.list(uniq$year)
        }
        # always all months
        period_months <- replicate(nrow(uniq), 1:12, simplify = FALSE)
    }

    # 3) build polygons for each grid-cell
    uniq <- uniq %>%
        rowwise() %>%
        mutate(cell_poly = list(
            cellPolyFromLatLonWidth(cell_lat, cell_lon, cell_width_km)
        )) %>%
        ungroup()

    # 4) helper to pull & average prism files for a given years+months+polygon
    getPrismValue <- function(years, months, cell_poly, variable) {
        pd <- prism_archive_subset(variable, "monthly",
                                   mon = months, years = years)
        files <- pd_to_file(pd)
        ras <- stack(lapply(files, raster))
        ras_mean <- calc(ras, fun = mean, na.rm = TRUE)
        crop_r <- crop(ras_mean, extent(cell_poly))
        mask_r <- mask(crop_r, cell_poly)
        cellStats(mask_r, stat = 'mean', na.rm = TRUE)
    }

    # 5) loop through each period
    uniq$value <- mapply(
        FUN = getPrismValue,
        years     = period_years,
        months    = period_months,
        cell_poly = uniq$cell_poly,
        MoreArgs  = list(variable = variable),
        SIMPLIFY  = TRUE
    )

    # 6) join back to original df
    if (seasonal) {
        if (!is.null(avg_of_years)) {
            join_cols <- c("cell", "season")
        } else {
            join_cols <- c("cell", "season", "year")
        }
    } else {
        if (!is.null(avg_of_years)) {
            join_cols <- "cell"
        } else {
            join_cols <- c("cell", "year")
        }
    }

    bound <- df %>%
        left_join(
            uniq %>% select(all_of(join_cols), value),
            by = join_cols
        ) %>%
        rename(!!new_var_colname := value)

    return(bound)
}
