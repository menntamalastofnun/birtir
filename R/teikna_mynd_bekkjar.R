#' Deprecated legacy plot for class-level score position
#'
#' @details
#' This function belongs to the legacy plotting workflow kept for reference
#' during the transition to `render_analysis_md()`.
#'
#' @param data einkunnir nemanda
#' @param kvardi tegund kvarda sem er notadur
#' @param fag les eda stf, akvardar fjolda lita i litud_maelistika()
#' @importFrom dplyr filter
#' @importFrom stringr str_wrap
#' @import ggplot2
#' @returns skilar mynd af stadsetningu bekkjar midad vid landsmedaltal a ollu profinu og undirthattum
#' @export
#'
#' @examplesIf FALSE
#' lysa_stodu_bekkjar(df, kvardi)
#'
lysa_stodu_bekkjar <- function(data, kvardi, fag = "les") {
  warn_legacy("lysa_stodu_bekkjar")

  # Compute stats per profhluti
  heildartolur <- data |>
    group_by(profhluti) |>
    summarise(
      medalmaelitala = mean(maelitala, na.rm = TRUE),
      sf = sqrt(sum(sfmt^2, na.rm = TRUE)) / n(),
      .groups = "drop"
    )

  kvardi_bil <- kvardi$kvardi_bil
  kvardi_lysing <- kvardi$kvardi_lysing

  # ggplot() +
  #   # Background scale (across y-axis only, not x)
  #   litud_maelistika(
  #     y_range = c(kvardi_bil[1], kvardi_bil[2]),
  #     cutoffs = kvardi_lysing$einkunn,
  #     alpha = 1,
  #     litur = "#D8C1FF",
  #     fag = fag
  #   ) +
  #   # Mean point per profhluti
  #   geom_point(
  #     data = heildartolur,
  #     aes(x = profhluti, y = medalmaelitala),
  #     size = 5,
  #     shape = 21,
  #     stroke = 1,
  #     color = "#292A4B",
  #     fill = "#292A4B"
  #   ) +
  #
  #   # Error bars per profhluti
  #   geom_errorbar(
  #     data = heildartolur,
  #     aes(
  #       x = profhluti,
  #       ymin = medalmaelitala - sf,
  #       ymax = medalmaelitala + sf
  #     ),
  #     width = 0.05,
  #     color = "#292A4B",
  #     linewidth = 0.6
  #   ) +
  #   geom_hline(
  #     yintercept = 10,
  #     linetype = "dotted",
  #     color = "#292A4B",
  #     linewidth = 1
  #   ) +
  #   geom_vline(
  #     xintercept = 1.5,
  #     color = "white",
  #     linewidth = 2
  #   ) +
  ggplot() +
    # Background scale
    litud_maelistika(
      y_range = c(kvardi_bil[1], kvardi_bil[2]),
      cutoffs = kvardi_lysing$einkunn,
      alpha = 1,
      litur = "#D8C1FF",
      fag = fag
    ) +
    geom_point(
      data = heildartolur,
      aes(x = profhluti, y = medalmaelitala),
      size = 5,
      shape = 21,
      stroke = 1,
      color = "#292A4B",
      fill = "#292A4B"
    ) +
    geom_errorbar(
      data = heildartolur,
      aes(
        x = profhluti,
        ymin = medalmaelitala - sf,
        ymax = medalmaelitala + sf
      ),
      width = 0.05,
      color = "#292A4B",
      linewidth = 0.6
    ) +
    geom_hline(
      yintercept = 10,
      linetype = "dotted",
      color = "#292A4B",
      linewidth = 1
    ) +
    if (fag == "les") geom_vline(
      xintercept = 1.5,
      color = "white",
      linewidth = 2
    ) else NULL +
    scale_y_continuous(
      limits = kvardi_bil,
      breaks = seq(kvardi_bil[1] + 1, kvardi_bil[2] - 1, by = 1),
      name = "Maelitala"
    ) +

    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) + # faerir textann a x-as i nyja linu ef hann er of langur
    theme(
      line = element_line(linetype = 1, colour = "#292A4B"),
      axis.text = element_text(face = "bold", size = rel(1)),
      text = element_text(colour = "black"),
      rect = element_rect(
        fill = NULL,
        linetype = 0,
        colour = NA
      ),
      legend.background = element_rect(fill = NULL),
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
      plot.margin = unit(c(0.2, 0.5, 0, 0.5), "cm"),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      axis.line.y = element_line(
        linewidth = 1#, # Breytti ur 1.5
      )
    )
}


#' Deprecated legacy legend for `lysa_stodu_bekkjar()`
#'
#' @details
#' This function belongs to the legacy plotting workflow kept for reference
#' during the transition to `render_analysis_md()`.
#'
#' @importFrom dplyr filter
#' @importFrom stringr str_wrap
#' @importFrom tidyr crossing
#' @import ggplot2
#' @returns skilar mynd af legend fyrir lysa_stodu
#' @export
#'
#' @examplesIf FALSE
#' lysa_stodu_bekkjar_legend()
#'
lysa_stodu_bekkjar_legend <- function() {
  warn_legacy("lysa_stodu_bekkjar_legend")

  legend_df <- tibble::tibble(
    group = c("Bekkur", "95% oryggisbil", "Landsmedaltal"),
    x = c(0.15, 0.6, 1.2),
    y = c(1, 1, 1),
    ymin = c(NA, 0.95, NA),
    ymax = c(NA, 1.05, NA)
  )

  ggplot() +
    # Dot (Nemandi)
    geom_point(
      data = dplyr::filter(legend_df, group == "Bekkur"),
      aes(x = x, y = y),
      size = 4,
      shape = 21,
      fill = "#292A4B",
      color = "#292A4B"
    ) +
    # Error bar (CI)
    geom_errorbar(
      data = dplyr::filter(legend_df, group == "95% oryggisbil"),
      aes(x = x, ymin = ymin, ymax = ymax),
      color = "#292A4B",
      width = 0.02,
      linewidth = 0.6
    ) +
    # Dotted line (Landsmedaltal)
    geom_segment(
      data = dplyr::filter(legend_df, group == "Landsmedaltal"),
      aes(x = x - 0.05, xend = x + 0.05, y = y, yend = y),
      linetype = "dotted",
      color = "#292A4B",
      linewidth = 1
    ) +
    # Labels next to symbols
    geom_text(
      data = legend_df,
      aes(x = x + 0.1, y = y, label = group),
      hjust = 0,
      size = 4,
      color = "#292A4B"
    ) +
    scale_x_continuous(limits = c(0, 2)) +
    coord_cartesian(ylim = c(0.8, 1.4)) +
    theme_void() +
    theme(
      plot.margin = margin(t = 0, r = 10, b = 0, l = 10)
    )
}

#############################################################################

# library(dplyr)
# library(ggplot2)
#
# plot_maelitala_donut <- function(df) {
#   # Categorize maelitala
#   df_summary <- df %>%
#     mutate(category = case_when(
#       maelitala < 3 ~ "Tharf inngrip",
#       maelitala >= 3 & maelitala <= 7 ~ "Tharf liklega inngrip",
#       maelitala >= 8 & maelitala <= 12 ~ "I medallagi",
#       maelitala > 12 ~ "Tharfnast floknara efnis"
#     )) %>%
#     count(category) %>%
#     mutate(
#       percent = n / sum(n),
#       label = paste0(category, "\n", scales::percent(percent, accuracy = 1))
#     )
#
#   # Make donut plot
#   ggplot(df_summary, aes(x = 2, y = percent, fill = category)) +
#     geom_col(width = 1, color = "white") +
#     coord_polar(theta = "y") +
#     geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "black", size = 4.5) +
#     scale_fill_manual(values = c("Tharf inngrip" = "#D8C1FF", "Tharf liklega inngrip" = "#E2D1FF", "I medallagi" = "#EBE0FF", "Tharfnast floknara efnis" = "#F5EFFF")) +
#     xlim(1, 2.5) +  # creates the hole
#     theme_void() +
#     theme(
#       legend.position = "none",
#       plot.title = element_text(hjust = 0.5, face = "bold", color = "#292A4B")
#     ) +
#     labs(title = "Dreifing nemenda eftir maelitolu")
# }
#


library(dplyr)
library(ggplot2)
library(scales)

plot_donut_subset <- function(df, label_name, filter_expr) {
  # Create logical column based on filter expression
  df_summary <- df %>%
    mutate(in_group = !!rlang::enquo(filter_expr)) %>%
    count(in_group) %>%
    mutate(
      percent = n / sum(n),
      label = ifelse(in_group, paste0( percent(percent, accuracy = 1), "\n", label_name), paste0("Adrir\n", percent(percent, accuracy = 1)))
    )

  # Dynamically create color mapping
  color_mapping <- stats::setNames(
    c("#D8C1FF", "#F5EFFF"),
    df_summary$label[order(df_summary$in_group, decreasing = TRUE)]
  )

  # Create donut plot
  ggplot(df_summary, aes(x = 2, y = percent, fill = label)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "black", size = 4.5) +
    xlim(1, 2.5) +
    scale_fill_manual(values = color_mapping) +
    theme_void() +
    labs(title = paste0("Hlutfall: ", label_name)) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", color = "#292A4B"),
      legend.position = "none"
    )
}


plot_donut_subset <- function(df, label_name, filter_expr) {
  df_summary <- df %>%
    mutate(in_group = !!rlang::enquo(filter_expr)) %>%
    count(in_group) %>%
    mutate(percent = n / sum(n))

  # Build the central label (just for the in_group = TRUE case)
  central_label <- df_summary %>%
    filter(in_group) %>%
    mutate(label = paste0(percent(percent, accuracy = 1), "\n", label_name)) %>%
    pull(label)

  # Define colors
  color_mapping <- stats::setNames(
    c("#D8C1FF", "#F5EFFF"),
    df_summary$in_group[order(df_summary$in_group, decreasing = TRUE)]
  )

  # Convert logical to character for fill
  df_summary$fill_label <- factor(df_summary$in_group, labels = c("Adrir", label_name))

  ggplot(df_summary, aes(x = 2, y = percent, fill = fill_label)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    # Center label manually added
    annotate("text", x = 0.2, y = 0, label = central_label, size = 6, fontface = "bold", color = "#292A4B", lineheight = 1.1) +
    xlim(0.1, 2.5) +
    scale_fill_manual(values = stats::setNames(c("#D8C1FF", "#F5EFFF"), c(label_name, "Adrir"))) +
    theme_void() +
    labs(title = NULL) +
    theme(
      legend.position = "none"
    )
}





#'
#' #' Mynd af profil nemandans
#' #'
#' #' @param data einkunnir nemanda
#' #' @param kvardi tegund kvarda sem er notadur
#' #' @importFrom dplyr filter
#' #' @importFrom stringr str_wrap
#' #' @import ggplot2
#' #' @returns skilar mynd af stadsettningu nemanda
#' #' @export
#' #'
#' #' @examples kortleggja(fa_heildartolu(5), fa_kvarda())
#' #'
#' kortleggja <- function(data, kvardi) {
#'   heildartala <- data |>
#'     dplyr::filter(!grepl("Heildartala", profhluti))
#'
#'   kvardi_bil <- kvardi$kvardi_bil
#'   kvardi_lysing <- kvardi$kvardi_lysing
#'   kvardi_bil
#'
#'   if (nrow(heildartala) == 0) {
#'     stop(
#'       "Gagnasettid inniheldur ekki undirprof."
#'     )
#'   }
#'
#'
#'   fjardlaegd_punkts_fra_texta <- .3
#'
#'
#'   heildartala |>
#'     ggplot(aes(profhluti, einkunn)) +
#'     litud_maelistika(
#'       y_range = c(kvardi_bil[1]+1, kvardi_bil[2]-1),
#'       cutoffs = kvardi_lysing$einkunn,
#'       alpha = 1,
#'       litur = "#D8C1FF"
#'     ) +
#'     # Tek ut linurnar fyrir hvern punkt sem tengja vid titilinn a x-as
#'     # geom_segment(aes(xend = profhluti,y = 1, yend = einkunn),
#'     #              color = "#292A4B", linewidth =  0.5) +
#'     geom_errorbar(
#'       aes(
#'         ymin = einkunn - sf,
#'         ymax = einkunn + sf
#'       ),
#'       width = 0.05,
#'       color = "#292A4B",
#'       linewidth = 0.6
#'     ) +
#'
#'     geom_point(
#'       size = 2,
#'       shape = 21,
#'       stroke = 2,
#'       color = "#292A4B",
#'       fill = "#292A4B",
#'       show.legend = F
#'     ) +
#'     scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
#'     # scale_y_continuous(
#'     #   breaks = kvardi_lysing$einkunn,
#'     #   labels = kvardi_lysing$lysing,
#'     #   limits = kvardi_bil
#'     # ) +
#'     scale_y_continuous(
#'       limits = c(1, 19), #kvardi_bil[1]+1, kvardi_bil[2]-1),
#'       breaks = seq(from  = kvardi_bil[1]+1, to = kvardi_bil[2]-1, by = 1),
#'       name = "Maelitala"
#'     ) +
#'     theme(
#'       line = element_line(linetype = 1, colour = "#292A4B"),
#'       axis.text = element_text(face = "bold", size = rel(1)),
#'       #axis.text.x = element_text(angle = 45, hjust = 1), # Baetti vid til ad snua labels a x-as thannig ad thau blandist ekki inn i hvert annad
#'       text = element_text(colour = "black"),
#'       rect = element_rect(
#'         fill = NULL,
#'         linetype = 0,
#'         colour = NA
#'       ),
#'       legend.background = element_rect(fill = NULL),
#'       # legend.position = "top",
#'       # legend.direction = "horizontal",
#'       # legend.box = "vertical",
#'       panel.grid = element_line(color = NULL, linetype = 3),
#'       panel.grid.major = element_line(colour = "#292A4B"),
#'       panel.grid.major.x = element_blank(),
#'       panel.grid.major.y = element_blank(),
#'       panel.grid.minor = element_blank(),
#'       panel.background = element_blank(),
#'       # Removes the grey background
#'       plot.background = element_blank(),
#'       plot.title = element_text(hjust = 0, face = "bold"),
#'       strip.background = element_rect(),
#'       axis.ticks.y = element_blank(),
#'       axis.title.x = element_blank(),
#'       # axis.title.y = element_blank(),
#'       #axis.text.x = element_blank(),
#'       axis.ticks.x = element_blank(),
#'       axis.line.x = element_blank(),
#'       axis.line.y = element_line(
#'         linewidth = 1#, # Breytti ur 1.5
#'
#'         #arrow = grid::arrow(length = unit(0.3, "cm"), ends = "both")
#'       ),
#'       plot.margin = unit(c(1, 1, 2, 1), "cm")
#'     ) +
#'     coord_equal(ratio = 1 / 10)
#'
#' }
#'
#' #' Prenta ut myndir
#' #'
#' #' @param data einkunnir nemanda
#' #' @param kvardi tegund kvarda sem er notadur
#' #' @importFrom dplyr filter
#' #' @returns myndir fyrir nidurstodur
#' #' @export
#' #'
#' #' @examples
#' #' data <- fa_heildartolu()
#' #' kvardi <- fa_kvarda()
#' #' teikna_mynd(data,kvardi)
#'
#'
#' teikna_mynd <- function(data, kvardi) {
#'   heildartala <- data |>
#'     dplyr::filter(grepl("Heildartala", profhluti))
#'
#'   undirthaettir <- data |>
#'     dplyr::filter(!grepl("Heildartala", profhluti))
#'
#'   if (nrow(heildartala) == 1) {
#'     lysa_stodu(heildartala, kvardi) |> print()
#'   }
#'
#'   if (nrow(undirthaettir) > 0) {
#'     kortleggja(undirthaettir, kvardi) |> print()
#'   }
#'
#'
#' }
#'
#'
#'

