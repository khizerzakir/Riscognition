library(ncdf4)
library(ggplot2)
library(dplyr)
library(stringr)

processFolders <- function(mainDir) {
  subfolders <- list.files(mainDir, pattern = "_2023$", full.names = TRUE)
  
  for (subfolder in subfolders) {
    output_dir <- file.path(subfolder, "Output")
    if (!dir.exists(output_dir)) {
      dir.create(output_dir)
    }
    
    raw_files <- list.files(path = subfolder, pattern = "ISS_LIS_.*\\.nc$", full.names = TRUE)
    
    orbit_start <- vector()
    orbit_end <- vector()
    flash_data <- data.frame(flash_lat = numeric(), flash_lon = numeric(), date = character())
    
    for (i in raw_files) {
      datafile <- nc_open(i)
      
      start_value <- ncvar_get(datafile, "orbit_summary_TAI93_start")
      end_value <- ncvar_get(datafile, "orbit_summary_TAI93_end")
      
      flash_lat <- ncvar_get(datafile, "lightning_flash_lat")
      flash_lon <- ncvar_get(datafile, "lightning_flash_lon")
      
      orbit_start <- c(orbit_start, start_value)
      orbit_end <- c(orbit_end, end_value)
      
      nc_close(datafile)
      
      # Extract the date from the subfolder title
      folder_date <- str_extract(subfolder, "\\d{4}-\\d{2}-\\d{2}")
      date <- as.character(format(start_value, "%Y-%m-%d"))
      
      flash_data <- bind_rows(flash_data, data.frame(flash_lat, flash_lon, date = rep(date, length(flash_lat))))
    }
    
    begin_date_value <- min(orbit_start)
    end_date_value <- max(orbit_end)
    
    begin_date <- format(begin_date_value, "%B %d, %Y")
    end_date <- format(end_date_value, "%B %d, %Y")
    begin_int <- format(begin_date_value, "%Y%m%d")
    end_int <- format(end_date_value, "%Y%m%d")
    
    csvfile <- file.path(output_dir, paste0("isslis_flashloc_", begin_int, "_", end_int, ".csv"))
    write.csv(flash_data, csvfile, row.names = FALSE)
    
    ggplot(flash_data, aes(x = flash_lon, y = flash_lat)) +
      geom_bin2d(bins = 300) +
      scale_fill_gradient(name = "Flash Count", guide = "colorbar") +
      coord_cartesian(xlim = c(-180, 180), ylim = c(-90, 90)) +
      labs(title = paste("ISS LIS Detected Lightning Flash Locations", begin_date, "-", end_date)) +
      theme_bw()
    
    output_plot <- file.path(output_dir, paste0("isslis_flashloc_", begin_int, "_", end_int, ".png"))
    ggsave(output_plot, width = 10, height = 6, dpi = 300)
  }
}

mainDir <- "E:/ERASMUS/Internship/Data"
processFolders(mainDir)
