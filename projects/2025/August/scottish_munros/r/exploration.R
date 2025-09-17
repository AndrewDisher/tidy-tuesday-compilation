# ------------------------
# --- Packages Imports ---
# ------------------------

box::use(
  dplyr[filter, mutate, select],
  ggplot2[...],
  ggmapinset[...],
  ggtext[...],
  giscoR[gisco_get_nuts],
  glue[glue],
  scales[label_wrap],
  sf[...],
  showtext[showtext_opts],
  sysfonts[font_add_google],
  tidytuesdayR[tt_load],
  tmaptools[get_asp_ratio],
  units[drop_units]
)

# ---------------------
# --- Retrieve Data ---
# ---------------------

# giscoR map data (year 2024 does not include UK data, so used 2021 for year)
region_data <- gisco_get_nuts(
  year = 2021,
  epsg = 4326,
  resolution = "01",
  spatialtype = "RG",
  country = c("GBR")
) |>
  st_transform(crs = 27700)

# Tidy Tuesday Munro data
tuesdata <- tt_load("2025-08-19")
munros <- tuesdata$scottish_munros |>
  filter(!is.na(xcoord) | !(is.na(ycoord)))

# -------------------------------
# --- Examining Distributions ---
# -------------------------------

# Histogram of hill heights
ggplot(data = munros, mapping = aes(x = Height_m)) +
  geom_histogram(color = "black", fill = "blue") +
  theme_minimal()

# Estimated hill height density
ggplot(data = munros, mapping = aes(x = Height_m)) +
  geom_density() +
  theme_minimal()

# ------------
# --- Maps ---
# ------------

# Convert munros data to sf geometry data frame
munros_sf <- st_as_sf(
  munros,
  coords = c("xcoord", "ycoord"),
  crs = st_crs(27700)
)

# Regional boundary map + munro data
ggplot(data = region_data) +
  geom_sf() +
  geom_sf(data = munros_sf, mapping = aes(color = `2021`)) +
  coord_sf(
    xlim = c(min(munros$xcoord), max(munros$xcoord)),
    ylim = c(min(munros$ycoord), max(munros$ycoord))
  ) +
  theme_bw()

# -----------------------------------------------------------------------------------------
# --- Question: What is the average distance between a munro and the nearest munro top? ---
# -----------------------------------------------------------------------------------------

# Acquire data set of only the munros
munro_coords <- munros_sf |>
  filter(`2021` == "Munro")

# Acquire data set of only munro tops
munro_top_coords <- munros_sf |>
  filter(`2021` == "Munro Top")

# Find closest munro top to each munro
munro_coords <- munro_coords |>
  mutate(nearest_index = st_nearest_feature(geometry, munro_top_coords$geometry)) |>
  mutate(nearest_name = munro_top_coords$Name[nearest_index]) |>
  mutate(nearest_top_geom = munro_top_coords$geometry[nearest_index])

# Calculate distances
munro_coords <- munro_coords |>
  mutate(nearest_dist_m = st_distance(geometry, nearest_top_geom, by_element = TRUE)) |>
  mutate(nearest_dist_m = drop_units(nearest_dist_m))

# Find mean distance
munro_coords$nearest_dist_m |> mean() # 3139.511 meters

# Find median distance
munro_coords$nearest_dist_m |> median() # 1579.645 meters

# Plot the distribution
ggplot(data = munro_coords, mapping = aes(x = nearest_dist_m)) +
  # geom_density() +
  geom_histogram(color = "black", fill = "blue") +
  theme_minimal()

# Find most distant pair
most_distant_pair <- munro_coords |>
  filter(nearest_dist_m == max(nearest_dist_m)) |>
  select(Name, geometry, nearest_name, nearest_top_geom, nearest_dist_m)

# Create a line segment between the two, for mapping later
line_segment <- st_sfc(
  st_cast(
    st_union(most_distant_pair$geometry, most_distant_pair$nearest_top_geom),
    "LINESTRING"
  )
)

# Create a new data frame to contain the pair
point_labels <- data.frame(
  Name = c(most_distant_pair[[1]], most_distant_pair[[3]]),
  geometry = c(most_distant_pair[[2]], most_distant_pair[[4]])
) |> st_as_sf(crs = st_crs(27700), sf_column_name = "geometry")

# Draw most distant pair on map
ggplot(data = region_data) +
  geom_sf() +
  geom_sf(data = munros_sf, mapping = aes(color = `2021`)) +
  geom_sf(data = line_segment, color = "green") +
  coord_sf(
    xlim = c(min(munros$xcoord), max(munros$xcoord)),
    ylim = c(min(munros$ycoord), max(munros$ycoord))
  ) +
  theme_bw()

# ------------------
# --- Font Setup ---
# ------------------

font_add_google("Montserrat", "Montserrat")

showtext_opts(dpi = 300)

custom_font <- "Montserrat"

# --------------------------------------------------------------------------
# --- Map displaying most distant pair (Munro and its nearest Munro Top) ---
# --------------------------------------------------------------------------

# Turn off scientific notation
options(scipen = 999)

# Inset configuration
inset_config <- configure_inset(
  shape = shape_circle(
    centre = st_centroid(line_segment),
    radius = 40
  ),
  scale = 2,
  translation = c(-115, 135)
)

# Caption
title <- glue("<span style='font-size:20pt;font-family:{custom_font};'>How far apart is Scotland's most distant <span style='color:#F8766D;'><strong>Munro</strong></span> and <span style='color:#00BFC4'><strong>Munro Top</strong></span> pair?</span>")
caption_body <- glue("<br><br><span style='font-size:12pt;font-family:{custom_font};'>Scotland's Munros are distinct mountains that rise to at least 914 meters in elevation, while Munro Tops are subsidiary summits of Munros that are also at least 914 meters tall. Where there is a Munro, there are usually Monro Tops.</span>")
note_on_result <- glue("<br><br><span style='font-size:12pt;font-family:{custom_font};'>The mean distance between a Munro and its nearest Munro Top is 3.1 kilometers, whereas the <span style='color:#34ebc0;'><strong>most distant such pair</strong></span> is <span style='color:#34ebc0'><strong>53.7 kilometers</strong></span> apart.")

# Complete map
munro_plot <- ggplot(data = munros_sf) +
  # Base map
  geom_sf(data = region_data, fill = "#c28744") +
  geom_sf(mapping = aes(color = `2021`)) +
  # Circular map inset
  geom_inset_frame(
    source.aes = list(linewidth = .75, colour = "black"),
    target.aes = list(fill = "#3283a8", linewidth = .75, colour = "black"),
    lines.aes = list(linewidth = .75, linetype = 2, colour = "black")
  ) +
  geom_sf_inset(data = region_data, map_base = "none", fill = "#a87132") +
  geom_sf_inset(mapping = aes(color = `2021`), map_base = "none") +
  geom_sf_inset(data = line_segment, color = "#34ebc0", linetype = 1, linewidth = .7) +
  geom_inset_frame(
    target.aes = list(colour = "black", linewidth = .75),
    lines.aes = list(linewidth = .75, linetype = 2, colour = "black")
  ) +
  # Label for Ben More
  geom_sf_label_inset(
    data = point_labels[1,],
    mapping = aes(label = label_wrap(25)(Name)),
    vjust = -.5,
    label.padding = unit(0.4, "lines"),
    family = custom_font
  ) +
  # Label for Ben Cruachan - Stob Dearg
  geom_sf_label_inset(
    data = point_labels[2,],
    mapping = aes(label = label_wrap(25)(Name)),
    vjust = 1.3,
    hjust = .75,
    label.padding = unit(0.4, "lines"),
    family = custom_font
  ) +
  # Label for distance of Munro and Munro Top pair
  geom_sf_label_inset(
    data = st_centroid(line_segment),
    mapping = aes(
      label = formatC(
        most_distant_pair$nearest_dist_m / 1000,
        big.mark = ",",
        digits = 1,
        format = "f"
      ) |>
        paste("km")
    ),
    border.color = "#34ebc0",
    angle = -3,
    label.padding = unit(0.4, "lines"),
    family = custom_font,
    linewidth = 1
  ) +
  coord_sf_inset(
    inset = inset_config,
    xlim = c(min(munros$xcoord) - 130000, max(munros$xcoord) + 180000),
    ylim = c(min(munros$ycoord), max(munros$ycoord))
  ) +
  labs(tag = paste0(title, caption_body, note_on_result)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = fill_alpha("#3283a8", .5)),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid = element_blank(),
    plot.tag = element_textbox_simple(
      maxwidth = 0.35,
      valign = 1,
      halign = 1,
      vjust = 1,
      hjust = 1.1,
      lineheight = 1.5,
      family = custom_font,
      size = rel(1),
      padding = margin(6, 6, 6, 6),
      r = unit(5, "pt"),
      fill = fill_alpha("white", alpha = 1),
      box.colour = "black",
      linewidth = .5,
      linetype = 1
    ),
    plot.margin = margin(0, 0, 0, 0),
    plot.tag.position = c(1.02, 0.98),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

munro_plot

# --------------------------------
# --- Save the Map to PNG File ---
# --------------------------------

# Get aspect ratio to save the image properly
plot_ratio <- get_asp_ratio(munros_sf)

# Save the image
ggsave(
  filename = "munro_map.png",
  path = "projects/2025/August/scottish_munros/static/",
  plot = munro_plot,
  units = "in",
  width = 10, height = plot_ratio * 10,
  dpi = 300
)
