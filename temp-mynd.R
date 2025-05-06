heildartala <- data |>
  dplyr::filter(grepl("Heildartala", profhluti))

kvardi_bil <- kvardi$kvardi_bil
kvardi_lysing <- kvardi$kvardi_lysing
kvardi_bil

fjardlaegd_punkts_fra_texta <- .5

heildartala |>

  ggplot(aes(1, einkunn)) +
  litud_maelistika(
    y_range = kvardi_bil,
    cutoffs = c(3,4,5,6,7,8),
    alpha = .75,
    litur = "#00FFFF",rev = T
  ) +
  geom_errorbar(aes(ymin = einkunn-1,ymax = einkunn+1),
                linewidth = .7,width = .01
                )+
  geom_point(
    size = 5,
    shape = 21,
    stroke = 1.5,
    color = "black",
    fill = "white",
    show.legend = F
  ) +
  scale_y_continuous(
    breaks = 1:20,
    limits = kvardi_bil
  ) +
  geom_label(
    data = kvardi_lysing,
    aes(
      x = 1.06,
      y = einkunn,
      label = stringr::str_wrap(umsogn , width = 75)
    ),
    #fill = "#c7fbd2",
    alpha = 0.1,
    label.size = NA,
    show.legend = F,
    hjust = 0,
    vjust = 0
  ) +
  scale_x_continuous(breaks = c(0.9:1.3), limits =  c(0.95,1.5)) +
  theme(
    line = element_line(linetype = 1, colour = "black"),
    axis.text = element_text(face = "bold", size = rel(1)),
    text = element_text(colour = "black"),
    rect = element_rect(
      fill = NULL,
      linetype = 0,
      colour = NA
    ),
    legend.background = element_rect(fill = NULL),
    # legend.position = "top",
    # legend.direction = "horizontal",
    # legend.box = "vertical",
    panel.grid = element_line(colour = NULL, linetype = 3),
    panel.grid.major = element_line(colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    # Removes the grey background
    plot.background = element_blank(),
    plot.title = element_text(hjust = 0, face = "bold"),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold",size = 11),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_line(
      linewidth = 1.5#,
      #arrow = grid::arrow(length = unit(0.3, "cm"), ends = "both")
    ),
    plot.margin = unit(c(1, 1, 2, 1), "cm")
  ) +
  facet_grid(~profhluti)
