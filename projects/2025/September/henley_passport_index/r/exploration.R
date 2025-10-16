# ------------------------
# --- Packages Imports ---
# ------------------------

box::use(
  BBmisc[normalize],
  dplyr[arrange, bind_cols, desc, filter, first, group_by, join_by, left_join, mutate, n, pull, select, slice, summarize, ungroup],
  ggbump[geom_sigmoid],
  ggplot2[...],
  ggrepel[geom_text_repel],
  ggtext[...],
  glue[glue],
  giscoR[gisco_get_countries, gisco_countrycode],
  jsonlite[...],
  purrr[pluck],
  rnaturalearth[ne_countries],
  sf[...],
  showtext[showtext_opts],
  sysfonts[font_add_google],
  tidytuesdayR[tt_load]
)

# ---------------------
# --- Retrieve Data ---
# ---------------------

# Tidy Tuesday passport data
tuesdata <- tt_load("2025-09-09")
country_lists <- tuesdata$country_lists
rank_by_year <- tuesdata$rank_by_year

# Acquire list of countries in EUROPE, as defined by the Henley data. There are 49 such countries.
europe_henley <- rank_by_year |>
  filter(region == "EUROPE") |>
  select(code, country, region) |>
  unique()

# Table of country code available in giscoR, for needed countries (EUROPE)
code_table <- gisco_countrycode |>
  filter(iso2c %in% europe_henley$code)

# The above returns only 48 countries. Determine which was ommitted.
europe_henley |>
  filter(!code %in% code_table$iso2c) # XK: Kosovo

# Find what codes GISCOR uses for the above country
gisco_countrycode |>
  filter(iso.name.en == "Kosovo") # Nothing, we'll need to find the data somewhere else.

# Retrieve map data, using ISO-3 country codes (ISO-3 is more reliably available), aside from Kosovo.
europe_geo <- gisco_get_countries(
  year = 2024,
  epsg = 4326, # 3035
  resolution = "01",
  spatialtype = "RG",
  country = code_table$ISO3_CODE
) |>
  select(ISO3_CODE, NAME_ENGL, geometry)

# Change column names
colnames(europe_geo) <- c("code", "country", "geometry")

# Retrieve Kosovo geometry
kosovo <- ne_countries(
  country = "Kosovo"
)

# Set CRS of the above
kosovo_geo <- st_transform(
  kosovo,
  crs = st_crs(europe_geo)
) |>
  select(sov_a3, sovereignt, geometry)

# Change column names & redefine Kosovo code, to be consistent with the Henley data
colnames(kosovo_geo) <- c("code", "country", "geometry")

# Bind together geometries
map_geo <- rbind(europe_geo, kosovo_geo)

# ---------------------
# --- Data Cleaning ---
# ---------------------

# Create rankings df for 2025
europe_ranks_2025 <- rank_by_year |>
  mutate(year = as.integer(year)) |>
  filter(region == "EUROPE" & year == 2025)|>
  left_join(map_geo, by = join_by(country), keep = FALSE) |>
  st_as_sf(
    sf_column_name = "geometry"
  ) |>
  st_crop(xmin = -20, xmax = 60, ymin = 20, ymax = 70)


# Create rankings data frame with geometry
country_rankings <- st_geometry(europe_ranks_2025) |>
  st_point_on_surface() |>
  st_coordinates() |>
  as.data.frame() |>
  bind_cols(
    europe_ranks_2025 |>
      st_drop_geometry()
  ) |>
  arrange(visa_free_count, desc(country))

# Create helper variables for when plotting the data
country_rankings <- country_rankings |>
  mutate(
    xend = 60,
    x_axis_start = xend + 10,
    y_end = normalize(
      rank(1:nrow(country_rankings)),
      range = c(25, 70),
      method = "range"
    ),
    x_axis_end = normalize(
      country_rankings$visa_free_count,
      range = c(first(x_axis_start) + 10, 100),
      method = "range"
    ),
    text_lab = visa_free_count,
    text_lab2 = ifelse(country == "Denmark", paste(text_lab, "visa-free destinations"), text_lab)
  )

# ------------------------------------
# --- Caption Components for Graph ---
# ------------------------------------

font_add_google("Montserrat", "Montserrat")
custom_font <- "Montserrat"

font_color <- "#939394"

# Caption
title <- glue("<span style='font-size:30pt;color:{font_color};font-family:{custom_font};'>Which European passports have <span style='color:#440154FF;'><strong>the least</strong></span> and <span style='color:#FDE725FF'><strong>the most</strong></span> visa-free acces?</span>")
caption_body <- glue("<br><br><span style='font-size:18pt;color:{font_color};line-height:2;'>The Henley Passport Index is produced by Henley & Partners and captures the number of countries to which travelers in possession of each passport in the world may enter visa free. In September 2025, <span style='color:#440154FF;'>Azerbaijanis</span> can access only <span style='color:#440154FF;'>71 countries</span> without a visa, while <span style='color:#FDE725FF;'>those of Denmark, Finland, France, Germany, Ireland, Italy & Spain</span> all have access to <span style='color:#FDE725FF;'>189 countries.</span></span>")
footnote <- glue("<br><br><span style='font-size:14pt;font-family:{custom_font};color:{font_color};'><strong>Data:</strong> Henley Passport Index API & TidyTuesday | <strong>Visualization:</strong> Andrew Disher</span>")

# ----------------
# --- Graphing ---
# ----------------

ggplot() +
  geom_sf(
    data = europe_ranks_2025,
    size = .3,
    fill = "transparent",
    color = font_color
  ) +
  geom_sigmoid(
    data = country_rankings,
    mapping = aes(
      x = X,
      y = Y,
      xend = x_axis_start - .2,
      yend = y_end,
      group = country,
      color = visa_free_count
    ),
    alpha = .6,
    smooth = 10,
    size = 1.5
  ) +
  geom_segment(
    data = country_rankings,
    mapping = aes(
      x = x_axis_start,
      y = y_end,
      xend = x_axis_end,
      yend = y_end,
      color = visa_free_count
    ),
    alpha = .6, size = 2,
    lineend = "round"
  ) +
  geom_segment(
    data = country_rankings,
    mapping = aes(
      x = x_axis_start,
      y = 25,
      xend = x_axis_start,
      yend = 70
    ),
    alpha = .6,
    size = 1.3,
    color = "black"
  ) +
  geom_point(
    data = country_rankings,
    mapping = aes(
      x = X,
      y = Y,
      color = visa_free_count
    ),
    size = 2
  ) +
  # Country labels
  geom_text(
    data = country_rankings,
    mapping = aes(
      x = x_axis_start -.5,
      y = y_end,
      label = country,
      color = visa_free_count
     ),
     hjust = 1,
     size = 5.5,
     nudge_y = .5
  ) +
  # Value labels
  geom_text(
    data = country_rankings,
    mapping = aes(
      x = x_axis_end,
      y = y_end,
      label = text_lab,
      color = visa_free_count
    ),
    hjust = 0,
    size = 6,
    nudge_x = .4
  ) +
  # Europe label
  geom_text(
    data = data.frame(x_coord = -5, y_coord = 67, text_lab = "EUROPE"),
    mapping = aes(
      x = x_coord,
      y = y_coord,
      label = text_lab
    ),
    color = font_color,
    size = 10
  ) +
  coord_sf(clip = "off", expand = FALSE) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  labs(
    tag = paste0(title, caption_body, footnote)
  ) +
  theme_void() +
  theme(plot.margin = margin(.5, 1.5, .5, .5, "cm"),
        legend.position = "none",
        plot.background = element_rect(fill = "black"),
      plot.tag = element_textbox_simple(
      maxwidth = 0.48,
      valign = 0,
      halign = 0,
      vjust = .8,
      hjust = .8,
      lineheight = 1.5,
      # family = custom_font,
      size = rel(1),
      padding = margin(6, 6, 6, 6)
    ),
  plot.tag.position = c(.45, .15))
