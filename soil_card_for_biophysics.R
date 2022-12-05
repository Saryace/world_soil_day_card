
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

# Here you can add your own data

soil_data <- data.frame(
  soil_id = c("A","A", "A","A"),
  limit_above = c(0,10,25,35),
  limit_below = c(10,25,35,55),
  horizon = c("Best Lab\never","Soil", "Biophysics","Lab"),
  munsell = c('5YR 3/2','5YR 7/2', '2.5YR 6/2','10R 3/2') 
)

# Data wrangling ----------------------------------------------------------

# some data wrangling for adjusting soil depth

soil_data_plot <- soil_data %>% 
  mutate(depth = limit_below - limit_above) %>% 
  mutate(munsell_hex=mnsl(munsell))

# add background ----------------------------------------------------------

# here you can add a background image

image_plot <- image_read("soil_lab_photo.png")

# Here you can customise the opaciticy and color

image_plot <- image_plot %>%
  image_colorize(opacity = 50, color = "grey70")

# Plot --------------------------------------------------------------------

# Let´s plot it

soil_card_plot <-
ggplot(
  data = soil_data_plot,
  aes(
    x=soil_id,
    y=-depth,
    fill=fct_reorder(horizon, # soil horizons reordered
                     limit_below,
                     .desc=TRUE))
    ) +
  annotation_custom(grid::rasterGrob(image_plot # the background
                                     , width = unit(1, "npc")
                                     , height = unit(1, "npc"))) +

  geom_col(
    width=0.3 # width column
  ) +
  geom_text( #location of horizon text
    aes(y=-(limit_above + depth/2),label=horizon)
  ) +
  scale_fill_manual( 
    breaks=soil_data_plot$horizons,
    values=soil_data_plot$munsell_hex) +
  theme_void() # remove all plot components

# Save it -----------------------------------------------------------------

soil_card <- 
soil_card_plot +  # add some text
  grid::textGrob('Gracias por todo el apoyo <3\nFeliz Dia del suelo <3',
                 just = "bottom")

ggsave("soil_card_biophysics.png",soil_card, height=6, width=9)