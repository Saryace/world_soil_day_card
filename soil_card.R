
# Install packages --------------------------------------------------------

# install.packages("tidyverse")
# install.packages("munsell")
# install.packages("patchwork")
# install.packages("magick")

# install.packages("devtools") 
# devtools::install_github("kaizadp/soilpalettes")devtools::install_github("kaizadp/soilpalettes")

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(patchwork)
library(soilpalettes)
library(munsell)
library(grid)
library(magick)

# Data --------------------------------------------------------------------

soil_data <- data.frame(
  soil_id = c("A","A", "A","A"),
  limit_above = c(0,10,25,35),
  limit_below = c(10,25,35,55),
  horizon = c("Happy","Soil", "World","Day"),
  munsell = c('5YR 3/2','5YR 7/2', '2.5YR 6/2','10YR 7/2') 
)


# Data wrangling ----------------------------------------------------------

soil_data_plot <- soil_data %>% 
  mutate(depth = limit_below - limit_above) %>% 
  mutate(munsell_hex=mnsl(munsell))

# add background ----------------------------------------------------------

image_plot <- image_read("soil_keycup.jpeg")

image_plot <- image_plot %>%
  image_colorize(opacity = 50, color = "grey70")

# Plot --------------------------------------------------------------------

soil_card_plot <-
ggplot(
  data = soil_data_plot,
  aes(
    x=soil_id,
    y=-depth,
    fill=fct_reorder(horizon,
                     limit_below,
                     .desc=TRUE))
    ) +
  annotation_custom(grid::rasterGrob(image_plot
                                     , width = unit(1, "npc")
                                     , height = unit(1, "npc"))) +

  geom_col(
    width=0.3
  ) +
  geom_text(
    aes(y=-(limit_above + depth/2),label=horizon)
  ) +
  scale_fill_manual( 
    breaks=soil_data_plot$horizons,
    values=soil_data_plot$munsell_hex) +
  scale_y_continuous(labels = abs) +
  theme_void()


# Save it -----------------------------------------------------------------

soil_card <- 
soil_card_plot +  
  grid::textGrob('Some really important text about soil',
                 just = "bottom")
                 
ggsave("soil_card.png",soil_card, height=6, width=9)