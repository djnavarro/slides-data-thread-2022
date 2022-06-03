library(arrow)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggrepel)

# code shown in presentation ----------------------------------------------

nyc_taxi <- open_dataset("~/Datasets/nyc-taxi")

nyc_taxi_zones <- "data/taxi_zone_lookup.csv" |>
  read_csv_arrow() |>
  janitor::clean_names()

airport_zones <- nyc_taxi_zones |>
  filter(str_detect(zone, "Airport")) |>
  pull(location_id)

nyc_taxi_zones_2 <- nyc_taxi_zones |>
  transmute(
    dropoff_location_id = location_id,
    dropoff_borough = borough,
    dropoff_zone = zone) |>
  as_arrow_table(
    schema = schema(
      dropoff_location_id = int64(),
      dropoff_borough = utf8(),
      dropoff_zone = utf8()
    )
  )

nyc_taxi_zone_counts <- nyc_taxi |>
  filter(
    pickup_location_id %in% airport_zones
  ) |>
  select(
    matches("datetime"),
    matches("location_id")
  ) |>
  left_join(nyc_taxi_zones_2) |>
  count(dropoff_zone) |>
  arrange(desc(n)) |>
  collect()

dat <- "data/taxi_zones/taxi_zones.shp" |>
  sf::read_sf() |>
  janitor::clean_names() |>
  left_join(nyc_taxi_zone_counts,
            by = c("zone" = "dropoff_zone")) |>
  arrange(desc(n))

pic <- dat |>
  ggplot(aes(fill = n)) +
  geom_sf(size = .1, color = "#222222") +
  theme_void(base_size = 28)


# some nice extras --------------------------------------------------------

library(showtext)

showtext_auto()
font_add_google("Roboto")

shades <- list(
  primary_green = "#005050",
  accent_green = "#8cffaf",
  text_white = "#ffffff",
  text_black = "#000000",
  accent_dark = "#4f4f4f",
  accent_light = "#b0b0b0",
  bg_dark = "#090909",
  bg_light = "#f6f6f6",
  contrast_light = "#f97b64",
  contrast_dark = "#b92307"
)

pic2 <- pic +
  scale_fill_distiller(
    name = "Number of trips",
    labels = scales::label_comma(),
    palette = "Oranges",
    direction = 1
  ) +
  geom_label_repel(
    stat = "sf_coordinates",
    data = dat |>
      mutate(zone = case_when(
        str_detect(zone, "Airport") ~ zone,
        str_detect(zone, "Times") ~ zone,
        TRUE ~ "")
      ),
    mapping = aes(label = zone, geometry = geometry),
    max.overlaps = 50,
    box.padding = .5,
    label.padding = .5,
    label.size = .15,
    label.r = 0,
    force = 30,
    force_pull = 0,
    size = 10,
    fill = "white",
    family = "Roboto",
    min.segment.length = 0
  ) +
  scale_x_continuous(expand = expansion(mult = .1)) +
  scale_y_continuous(expand = expansion(mult = .1)) +
  theme(
    text = element_text(
      colour = shades$text_white
    ),
    legend.background = element_rect(
      colour = shades$text_white
    ),
    legend.margin = margin(10, 10, 10, 10)
  ) +
  NULL

ggsave("img/airport_map.png", pic2, width = 10, dpi = 300, bg = "#839496")


