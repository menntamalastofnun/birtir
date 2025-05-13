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
  kvardi_texta_bil <- c(2, 5, 9.5, 15.5)

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
      y_range = c(kvardi_bil[1]+1, kvardi_bil[2]-1),
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
      breaks = seq(from  = kvardi_bil[1]+1, to = kvardi_bil[2]-1, by = 1),
      name = "Mælitala"
    ) +
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
      y_range = c(kvardi_bil[1]+1, kvardi_bil[2]-1),
      cutoffs = kvardi_lysing$einkunn,
      alpha = 1,
      litur = "#D8C1FF"
    ) +
    # Tek út línurnar fyrir hvern punkt sem tengja við titilinn á x-ás
    # geom_segment(aes(xend = profhluti,y = 1, yend = einkunn),
    #              color = "#292A4B", linewidth =  0.5) +
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
      size = 2,
      shape = 21,
      stroke = 2,
      color = "#292A4B",
      fill = "#292A4B",
      show.legend = F
    ) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
    scale_y_continuous(
      limits = c(1, 19), #kvardi_bil[1]+1, kvardi_bil[2]-1),
      breaks = seq(from  = kvardi_bil[1]+1, to = kvardi_bil[2]-1, by = 1),
      name = "Mælitala"
    ) +
    theme(
      line = element_line(linetype = 1, colour = "#292A4B"),
      axis.text = element_text(face = "bold", size = rel(1)),
      #axis.text.x = element_text(angle = 45, hjust = 1), # Bætti við til að snúa labels á x-ás þannig að þau blandist ekki inn í hvert annað
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
      # axis.title.y = element_blank(),
      #axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      axis.line.y = element_line(
        linewidth = 1#, # Breytti úr 1.5

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


#######################################################

#' Mynd af atriðum nemanda - rétt/röng svör per atriði
#'
#' @param df_items einkunnir nemanda á atriðalevel
#' @importFrom dplyr filter
#' @importFrom stringr str_wrap
#' @import ggplot2
#' @returns skilar mynd af svörum nemenda eftir atriði og þyngd atriða
#' @export
#'
#' @examples einstaklingar_atridi(df_items)
#' Áður en kallað er á fallið þarf að raða gögnunum með því að keyra þennan kóða þar sem df_atrdiði
#'
#' # Raða x-ásnum og y-ásnum þannig að punktarnir flokkist rétt saman
#' df_student <- df_atridi %>%
#'   dplyr::mutate(
#'     difficulty = factor(difficulty, levels = c("Mjög létt", "Létt", "Þungt", "Mjög þungt"))
#'   ) %>%
#'   dplyr::arrange(difficulty, item_id) %>%  # Order by difficulty, then item_id
#'   mutate(student_response = factor(student_response, levels = c(TRUE, FALSE)))
#'
#'
#' df_student <- subset(df_student, nafn == "María Birna Karlsdóttir")
#'

einstaklingar_atridi <- function(df_items) {

  # Sort the data by difficulty and item_id
  df_student <- df_items %>%
    dplyr::mutate(
      difficulty = factor(difficulty, levels = c("Mjög létt", "Létt", "Þungt", "Mjög þungt"))
    ) %>%
    dplyr::arrange(difficulty, item_id) %>%
    dplyr::mutate(
      item_id = factor(item_id, levels = unique(item_id))  # fix y-axis order
    )

  # Plot
  ggplot(df_student, aes(x = difficulty, y = item_id)) +
    geom_point(
      aes(fill = student_response),
      shape = 21,
      color = "#292A4B",
      size = 5,
      stroke = 1
    ) +
    scale_fill_manual(
      values = c("TRUE" = "#D8C1FF", "FALSE" = "white"),
      labels = c("TRUE" = "Rétt", "FALSE" = "Rangt"), # Label the legend
      name = ""
      #guide = FALSE
    ) +
    scale_y_discrete(limits = levels(df_student$item_id)) +
    labs(
      # title = paste0("Svör hjá ", df_student$nafn[1]),
      x = "",
      y = ""
    ) +
    theme_minimal() +
    theme(
      line = element_line(linetype = 1, colour = "#292A4B"),
      axis.text = element_text(face = "bold", size = rel(1)),
      #axis.text.x = element_text(angle = 45, hjust = 1), # Bætti við til að snúa labels á x-ás þannig að þau blandist ekki inn í hvert annað
      text = element_text(colour = "black"),
      rect = element_rect(
        fill = NULL,
        linetype = 0,
        colour = NA
      ),
      legend.position = "top",
      legend.background = element_rect(fill = NULL),
      # legend.position = "top",
      # legend.direction = "horizontal",
      # legend.box = "vertical",
      #panel.grid = element_line(color = NULL, linetype = 3),
      panel.grid.major.y = element_line(color = "#D8C1FF", linetype = "dotted", linewidth = 0.5),
      panel.grid.major = element_line(colour = "#292A4B"),
      panel.grid.major.x = element_blank(),
      #panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      # Removes the grey background
      plot.background = element_blank(),
      plot.title = element_text(hjust = 0, face = "bold"),
      strip.background = element_rect(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_blank(),
      # axis.title.y = element_blank(),
      #axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_line(
        linewidth = 1),
      axis.line.y = element_blank(),
      plot.margin = unit(c(1, 1, 2, 1), "cm")
    )
}



