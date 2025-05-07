#' Mynd af stadsetningu nemanda a kvarda asamt lysingu
#'
#' @param data einkunnir nemanda
#' @param kvardi tegund kvarda sem er notadur
#' @importFrom dplyr filter
#' @importFrom stringr str_wrap
#' @import ggplot2
#' @returns skilar mynd af stadsettningu nemanda
#' @export
#'
#' @examples lysa_stodu(fa_heildartolu(5), fa_kvarda())
#'
lysa_stodu <- function(data, kvardi) {
  heildartala <- data |>
    dplyr::filter(grepl("Heildartala", profhluti))

  kvardi_bil <- kvardi$kvardi_bil # kvardi bil nær frá 0 til 20
  kvardi_lysing <- kvardi$kvardi_lysing
  kvardi_texta_bil <- c(3.5, 9.5, 16)

  if (nrow(heildartala) != 1) {
    stop(
      "Gagnasettid inniheldur ekki profhlutan heildartolu.
      Athugid hvort nafnid se ekki rett skrifad Heildartala."
    )
  }


  fjardlaegd_punkts_fra_texta <- .5


  heildartala |>
    ggplot(aes(.1, einkunn)) +
    litud_maelistika(
      y_range = kvardi_bil,
      cutoffs = kvardi_lysing$einkunn,
      alpha = 1,
      litur = "#D8C1FF" # Breytti litnum úr #C7FBD2
    ) +
    geom_errorbar(
      aes(
        ymin = einkunn - sf,
        ymax = einkunn + sf
      ),
      width = 0.05,
      color = "#292A4B",
      linewidth = 0.6
    ) +
    geom_point(
      size = 3, # var 10
      shape = 21,
      stroke = 1,
      color = "#292A4B", # Breytt úr "black"
      fill = "#292A4B", # Breytt úr "black"
      show.legend = F
    ) +
    # geom_label(
    #   data = kvardi_lysing,
    #   aes(
    #     x = fjardlaegd_punkts_fra_texta,
    #     y = einkunn,
    #     label = stringr::str_wrap(umsogn , width = 40)
    #     #label = padded_umsogn # Bætti við padded_umsogn í reprt_tmpl þegar ég var að búa til kvardann
    #   ),
    #   #fill = "#c7fbd2",
    #   label.size = NA,
    #   fill = "white", # bætti við
    #   #color = "black",
    #   #show.legend = F,
    #   hjust = 0,
    #   vjust = 0
    # )
    geom_text(
      data = kvardi_lysing,
      aes(
        x = fjardlaegd_punkts_fra_texta,
        y = kvardi_texta_bil,  # use the adjusted y-coordinate
        label = stringr::str_wrap(umsogn, width = 40)
      ),
      hjust = 0,
      vjust = 0.5,  # center vertically
      color = "#292A4B",
      size = 3.5
    ) +

    # Teiknar lýsingartextann fyrir hvert bilc
    scale_x_continuous(limits = c(0, 1.5)) + # breytti limits úr c(0, 1)
    scale_y_continuous(
      limits = kvardi_bil,
      breaks = seq(from  = kvardi_bil[1], to = kvardi_bil[2], by = 1),
      name = "Mælitala"
    ) +
    #scale_y_continuous(
    # breaks = kvardi_lysing$einkunn,
    #labels = kvardi_lysing$lysing,
    #limits = kvardi_bil
    #) +
    theme(
      line = element_line(linetype = 1, colour = "#292A4B"), # Breytti litnum úr "black"
      axis.text = element_text(face = "bold", size = rel(1)),
      text = element_text(colour = "#292A4B"), # Breytti litnum úr "black"
      rect = element_rect(
        fill = NULL,
        linetype = 0,
        colour = NA
      ),
      legend.background = element_rect(fill = NULL),
      # legend.position = "top",
      # legend.direction = "horizontal",
      # legend.box = "vertical",
      panel.grid = element_line(color = NULL, linetype = 3),
      panel.grid.major = element_line(colour = "#292A4B"),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      # Removes the grey background
      plot.background = element_blank(),
      plot.title = element_text(hjust = 0, face = "bold"),
      strip.background = element_rect(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_blank(),
      #axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      axis.line.y = element_line(
        linewidth = 1#, # Breytti úr 1.5
        #arrow = grid::arrow(length = unit(0.3, "cm"), ends = "both")
      ),
      plot.margin = unit(c(1, 1, 2, 1), "cm")
    ) +
    coord_equal(ratio = 1 / 20)
}

#' Mynd af profil nemandans
#'
#' @param data einkunnir nemanda
#' @param kvardi tegund kvarda sem er notadur
#' @importFrom dplyr filter
#' @importFrom stringr str_wrap
#' @import ggplot2
#' @returns skilar mynd af stadsettningu nemanda
#' @export
#'
#' @examples kortleggja(fa_heildartolu(5), fa_kvarda())
#'
kortleggja <- function(data, kvardi) {
  heildartala <- data |>
    dplyr::filter(!grepl("Heildartala", profhluti))

  kvardi_bil <- kvardi$kvardi_bil
  kvardi_lysing <- kvardi$kvardi_lysing
  kvardi_bil

  if (nrow(heildartala) == 0) {
    stop(
      "Gagnasettid inniheldur ekki undirprof."
    )
  }


  fjardlaegd_punkts_fra_texta <- .3


  heildartala |>
    ggplot(aes(profhluti, einkunn)) +
    litud_maelistika(
      y_range = kvardi_bil,
      cutoffs = kvardi_lysing$einkunn,
      alpha = .8,
      litur = "#c7fbd2"
    ) +
    geom_segment(aes(xend = profhluti,y = 1, yend = einkunn),
                 color = "gray", linewidth =  0.8) +
    geom_point(
      size = 6,
      shape = 21,
      stroke = 2,
      color = "black",
      fill = "#ff681d",
      show.legend = F
    ) +
    scale_y_continuous(
      breaks = kvardi_lysing$einkunn,
      labels = kvardi_lysing$lysing,
      limits = kvardi_bil
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
      #axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      axis.line.y = element_line(
        linewidth = 1.5#,
        #arrow = grid::arrow(length = unit(0.3, "cm"), ends = "both")
      ),
      plot.margin = unit(c(1, 1, 2, 1), "cm")
    ) +
    coord_equal(ratio = 1 / 10)

}


#' Prenta ut myndir
#'
#' @param data einkunnir nemanda
#' @param kvardi tegund kvarda sem er notadur
#' @importFrom dplyr filter
#' @returns myndir fyrir nidurstodur
#' @export
#'
#' @examples
#' data <- fa_heildartolu()
#' kvardi <- fa_kvarda()
#' teikna_mynd(data,kvardi)


teikna_mynd <- function(data, kvardi) {
  heildartala <- data |>
    dplyr::filter(grepl("Heildartala", profhluti))

  undirthaettir <- data |>
    dplyr::filter(!grepl("Heildartala", profhluti))

  if (nrow(heildartala) == 1) {
    lysa_stodu(heildartala, kvardi) |> print()
  }

  if (nrow(undirthaettir) > 0) {
    kortleggja(undirthaettir, kvardi) |> print()
  }


}
