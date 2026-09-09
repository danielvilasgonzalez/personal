library(dplyr)
library(ggplot2)
library(sf)
library(lubridate)
library(terra)

# list all monthly files
files <- list.files("/Users/daniel/Downloads/fleet-monthly-csvs-10-v3-2024/", pattern = "\\.csv$", full.names = TRUE)

df <- data.frame()  # start empty

bbox_wfs <- c(xmin = -6,
              ymin = 30,
              xmax = 36,
              ymax = 46)


for (fl in files) {
  cat("Reading", fl, "\n")
  idf <- read.csv(fl)
  
  # subset by bounding box
  idf1 <- subset(idf,
                 cell_ll_lon > bbox_wfs[[1]] & cell_ll_lon < bbox_wfs[[3]] &
                   cell_ll_lat > bbox_wfs[[2]] & cell_ll_lat < bbox_wfs[[4]])
  
  df <- bind_rows(df, idf1)
}

# aggregate daily values to monthly per vessel
df1 <- df %>%
  group_by(mmsi_present, cell_ll_lon, cell_ll_lat, date) %>%
  summarise(fishing_hours = sum(fishing_hours), .groups = "drop") %>%
  mutate(month = month(as.Date(date, format = "%m/%d/%y")))

# now average per cell per month
df2 <- df1 %>%
  group_by(cell_ll_lon, cell_ll_lat, month) %>%
  summarise(fishing_hours = mean(fishing_hours, na.rm = TRUE), .groups = "drop")

# convert to sf object
effort_sf <- st_as_sf(df2, coords = c("cell_ll_lon", "cell_ll_lat"), crs = 4326)

world <- ne_countries(scale="large", returnclass="sf")

library(dplyr)
library(sf)
library(terra)

# Your polygons
black_sea <- st_polygon(list(rbind(
  c(27,41.5), c(41.5,41.5), c(41.5,47),
  c(27,47), c(27,41.5)
))) %>% st_sfc(crs = 4326)

biscay <- st_polygon(list(rbind(
  c(-10,43), c(0,43), c(0,47),
  c(-10,47), c(-10,43)
))) %>% st_sfc(crs = 4326)

remove_mask <- st_union(black_sea, biscay)

# Convert tibble to sf
df2_sf <- st_as_sf(df2, coords = c("cell_ll_lon", "cell_ll_lat"), crs = 4326)

# Filter out points that intersect the mask
df2_masked <- df2_sf[!st_intersects(df2_sf, remove_mask, sparse = FALSE), ]

# Drop geometry to get a plain data frame for ggplot
df2_masked_df <- df2_masked %>%
  st_coordinates() %>% 
  as_tibble() %>% 
  bind_cols(df2_masked %>% st_drop_geometry() %>% select(fishing_hours, month)) %>%
  rename(cell_ll_lon = X, cell_ll_lat = Y)

# Dissolve all countries into one polygon
world_union <- world %>%
  st_make_valid() %>%
  st_union()  # mer

#p <- 
  ggplot(df2_masked_df) +
  geom_tile(aes(x = cell_ll_lon, y = cell_ll_lat, fill = fishing_hours + 1)) +
    geom_sf(data=world_union, fill="#d9d9d9", color="gray40", size=0.5) +  # only exterior boundary
  
    scale_fill_gradientn(
      colors = c("white", "yellow", "red"),
      values = scales::rescale(c(0.001, 1, max(df2$fishing_hours + 1, na.rm = TRUE))),
      trans = "log",
      labels = function(x) format(x, digits = 2, scientific = FALSE)
    ) +
    coord_sf(xlim=c(-6,36), ylim=c(30,46), expand=FALSE) +
    scale_x_continuous(expand=c(0,0), position="bottom", breaks=c(0,10,20,30)) +
    scale_y_continuous(expand=c(0,0), position="left", breaks=c(44,40,36,32)) +
    theme(
      axis.text.x = element_text(vjust = 3,
                                 margin = ggplot2::margin(t = -10, r = 0, b = 10, l = 0),
                                 color='black'),
      axis.text.y = element_text(hjust = 1.5,
                                 margin = ggplot2::margin(t = 0, r = -25, b = 0, l = 25),
                                 color='black'),
      axis.ticks.length = unit(-5, "pt"),
      panel.grid.major = element_line(color = rgb(0, 0, 0,20, maxColorValue = 285),
                                      linetype = 'dashed', linewidth = 0.5),
      panel.ontop = TRUE, text = element_text(size=10),
      legend.background = element_rect(colour = "transparent"),
      legend.key.height = unit(1.5, "cm"),
      legend.key.width = unit(0.5, "cm"),
      axis.title = element_blank(),
      legend.position = 'right',
      panel.border = element_rect(fill = NA, colour = 'black'),
      panel.background = element_rect(fill = NA),
      plot.margin = grid::unit(c(0,0,0,0), "mm")
    ) +
    guides(
      fill = guide_colorbar(
        order = 1,
        title.position = "top",
        title.hjust = 0.1,
        frame.colour = "black",
        ticks.colour = "black"
      )
    )




print(p)

