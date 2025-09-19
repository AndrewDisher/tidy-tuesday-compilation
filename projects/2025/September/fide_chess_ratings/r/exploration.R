# -----------------------
# --- Package Imports ---
# -----------------------

box::use(
  dplyr[arrange, case_when, count, desc, filter, group_by, mutate, n, select, summarize],
  ggimage,
  ggplot2[...],
  ggtext[...],
  glue[glue],
  paletteer,
  showtext[showtext_opts],
  sysfonts[font_add_google],
  tidytuesdayR[tt_load]
)

# ---------------------
# --- Retrieve Data ---
# ---------------------

tuesdata <- tidytuesdayR::tt_load('2025-09-23')

fide_ratings_august <- tuesdata$fide_ratings_august
fide_ratings_september <- tuesdata$fide_ratings_september

# ------------------------
# --- Explore the Data ---
# ------------------------

# Question: Which federations have the most number of titled players? (September Data)
titles_by_fed <- fide_ratings_september |>
  group_by(fed) |>
  filter(!is.na(title)) |>
  summarize(total = n()) |>
  arrange(desc(total))

# Keep top 5 federations, but merge the rest into a group called other, for plotting
top_feds_vec <- c("GER", "ESP", "RUS", "IND", "FRA")

pie_data <- titles_by_fed |>
  mutate(group_var = case_when(
    fed %in% top_feds_vec ~ fed,
    TRUE ~ "OTHER"
  )) |>
  group_by(group_var) |>
  summarize(pie_totals = sum(total)) |>
  arrange(pie_totals) |>
  mutate(ordering = c(5:1, 6)) |>
  arrange(ordering) |>
  mutate(flag_code = c("DE", "ES", "RU", "IN", "FR", "Other"))

# Calculate positioning of country labels
pie_data <- pie_data |>
  mutate(y_prop = pie_totals / sum(pie_data$pie_totals) * 100) |>
  mutate(y_pos = cumsum(y_prop) - .5 * y_prop)

# ----------------------------------------
# --- Fonts and Such for Visualization ---
# ----------------------------------------

font_add_google("Montserrat", "Montserrat")

showtext_opts(dpi = 300)

custom_font <- "Montserrat"

# Title and caption
title <- "<span style='font-size: 14pt;'><strong>To Which Federations Do Most Professional Chess Title Holders Belong?</strong></span>"
caption_body <-  "<br><br><span>The International Chess Federation oversees the ratings of globally acclaimed chess players by compiling the results of various chess tournaments every month. The ratings assigned to players help determine if players recieve titles, such as the highly coveted Grandmaster title. While the many title-holders represent around 200 different countries, they most frequently hail from Germany, Spain, Russia, India, and France.</span>"

# --------------------------------------
# --- Create Pie Chart Visualization ---
# --------------------------------------

# Make a pie chart
pie_chart <- ggplot(data = pie_data, mapping = aes(x = "", y = y_prop, fill = reorder(group_var, desc(ordering)))) +
  geom_bar(stat = "identity", width = 1, color = "#cee6f0") +
  coord_polar(theta = "y", start = 0) +
  ggimage$geom_flag(mapping = aes(y = y_pos, x = 1.25, image = flag_code)) +
  geom_text(
    mapping = aes(y = pie_data$y_pos[6], label = "Other"),
    color = "white",
    size = 7,
    family = custom_font
  ) +
  paletteer$scale_fill_paletteer_d("MetBrewer::Hokusai2") +
  geom_label(
    mapping = aes(
      y = y_pos, x = 1.65,
      label = paste(
      formatC(y_prop, digits = 1, format = "f"),
      "%")
    ),
    text.color = "white",
    size = 4,
    family = custom_font) +
  labs(
    title = title,
    subtitle = caption_body
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = fill_alpha("#cee6f0", 1)),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    # plot.tag.position = c(.42, .98),
    plot.title = element_textbox_simple(
      lineheight = 1.5,
      fill = "#cee6f0",
      padding = margin(5, 5, 0, 5),
      margin = margin(0, 0, 0, 0),
      family = custom_font,
      color = "#134B73FF"
    ),
    plot.subtitle = element_textbox_simple(
      fill = "#cee6f0",
      padding = margin(0, 5, 0, 5),
      margin = margin(0, 0, 0, 0),
      family = custom_font,
      color = "#134B73FF"
    )
    # plot.tag = element_textbox_simple(
    #   maxwidth = 0.40,
    #   valign = 1,
    #   halign = 0,
    #   vjust = 1,
    #   hjust = 1,
    #   lineheight = 1.5,
    #   family = custom_font,
    #   size = rel(1),
    #   padding = margin(6, 6, 6, 6)
    # )
  )

pie_chart

# --------------------------------
# --- Save the Map to PNG File ---
# --------------------------------

ggsave(
  filename = "chess_pie_chart.png",
  path = "projects/2025/September/fide_chess_ratings/static/",
  plot = pie_chart,
  units = "in",
  width = 6, height = 7,
  dpi = 300
)
