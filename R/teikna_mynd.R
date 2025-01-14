




faerni_graf <- function(data, kvardi) {
  heildartala <- data |>
    dplyr::filter(grepl("Heildartala", profhluti))

  if (nrow(heildartala) != 1) {
    stop(
      "Gagnasettið inniheldur ekki prófhlutan heildartölu.
      Athugið hvort nafnið sé ekki rétt skrifað Heildartala."
    )
  }






  fjardlaegd_punkts_fra_texta <- .15


  heildartala |>
    ggplot(aes(1, einkunn)) +
    geom_point(
      size = 10,
      shape = 21,
      stroke = 2,
      color = "black",
      fill = "#ff681d",
      show.legend = F
    ) +
    geom_label(
      data = kvardi,
      aes(
        x = 1 - fjardlaegd_punkts_fra_texta,
        y = einkunn,
        label = str_wrap(kvardi_lysing, width = 40)
      ),
      fill = "#c7fbd2",
      label.size = NA,
      show.legend = F,
      hjust = .01
    ) +
    scale_y_continuous(
      breaks = c(2.5, 5.5, 9.5, 14.5, 19),
      labels = c("þrep 0\n1-3", "þrep 1\n3-6", "þrep 2\n6-10", "þrep 3\n10-13", ""),
      limits = c(0, 19)
    ) +
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
      strip.background = element_rect(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      axis.line.y = element_line(
        linewidth = 1.5,
        arrow = grid::arrow(length = unit(0.3, "cm"), ends = "both")
      ),
      plot.margin = unit(c(1, 1, 2, 1), "cm")
    ) +
    coord_cartesian(ylim = c(0, 21))


}

faerni_graf(fa_heildartolu(data, 5), kvardi)
faerni_graf(fa_heildartolu(data, 2), kvardi)
faerni_graf(fa_heildartolu(data, 19), kvardi)
faerni_graf(fa_heildartolu(data, 10), kvardi)
