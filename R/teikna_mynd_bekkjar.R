library(purrr)
library(rlang)
library(ggplot2)
library(readxl)


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
lysa_stodu_bekkjar <- function(data, kvardi) {
  heildartala <- data |>
    dplyr::summarise(
      medalmaelitala = mean(maelitala, na.rm = TRUE), # reikna meðaltal bekkjar
      sf = sqrt(sum(sfmt^2)) / nrow(data), # reikna staðalfrávik bekkjar (athuga hvaða upplýsingar við erum að birta hér og hvað við erum að taka inn!!!!!)
      .groups = "drop"
    )



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
    ggplot(aes(.1, medalmaelitala)) +
    litud_maelistika(
      y_range = c(kvardi_bil[1]+1, kvardi_bil[2]-1),
      cutoffs = kvardi_lysing$einkunn,
      alpha = 1,
      litur = "#D8C1FF" # Breytti litnum úr #C7FBD2
    ) +
    geom_errorbar(
      aes(
        ymin = medalmaelitala - sf,
        ymax = medalmaelitala + sf
      ),
      width = 0.05,
      color = "#292A4B",
      linewidth = 0.6
    ) +
    # Bæta við upplýsingum um stöðu einstaka nemenda - hálf gegnsæir punktar með mælitölu hvers nemanda
    geom_jitter(
      data = data,
      aes(x = 0.05, y = maelitala),
      width = 0.01,
      height = 0,
      shape = 21,
      color = "#292A4B",
      fill = "#292A4B",
      alpha = 0.3,
      size = 3,
      stroke = 1
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
      breaks = seq(from  = kvardi_bil[1]+1, to = kvardi_bil[2]-1, by = 1),
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
    coord_equal(ratio = 1 / 10) # Breytti úr 1/20 til að hækka grafið
}

lysa_stodu_bekkjar(df_bekkur, kvardi)

#############################################################################

# library(dplyr)
# library(ggplot2)
#
# plot_maelitala_donut <- function(df) {
#   # Categorize maelitala
#   df_summary <- df %>%
#     mutate(category = case_when(
#       maelitala < 3 ~ "Þarf inngrip",
#       maelitala >= 3 & maelitala <= 7 ~ "Þarf líklega inngrip",
#       maelitala >= 8 & maelitala <= 12 ~ "Í meðallagi",
#       maelitala > 12 ~ "Þarfnast flóknara efnis"
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
#     scale_fill_manual(values = c("Þarf inngrip" = "#D8C1FF", "Þarf líklega inngrip" = "#E2D1FF", "Í meðallagi" = "#EBE0FF", "Þarfnast flóknara efnis" = "#F5EFFF")) +
#     xlim(1, 2.5) +  # creates the hole
#     theme_void() +
#     theme(
#       legend.position = "none",
#       plot.title = element_text(hjust = 0.5, face = "bold", color = "#292A4B")
#     ) +
#     labs(title = "Dreifing nemenda eftir mælitölu")
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
      label = ifelse(in_group, paste0( percent(percent, accuracy = 1), "\n", label_name), paste0("Aðrir\n", percent(percent, accuracy = 1)))
    )

  # Dynamically create color mapping
  color_mapping <- setNames(
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
  color_mapping <- setNames(
    c("#D8C1FF", "#F5EFFF"),
    df_summary$in_group[order(df_summary$in_group, decreasing = TRUE)]
  )

  # Convert logical to character for fill
  df_summary$fill_label <- factor(df_summary$in_group, labels = c("Aðrir", label_name))

  ggplot(df_summary, aes(x = 2, y = percent, fill = fill_label)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    # Center label manually added
    annotate("text", x = 0.2, y = 0, label = central_label, size = 6, fontface = "bold", color = "#292A4B", lineheight = 1.1) +
    xlim(0.1, 2.5) +
    scale_fill_manual(values = setNames(c("#D8C1FF", "#F5EFFF"), c(label_name, "Aðrir"))) +
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
#'     # Tek út línurnar fyrir hvern punkt sem tengja við titilinn á x-ás
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
#'       name = "Mælitala"
#'     ) +
#'     theme(
#'       line = element_line(linetype = 1, colour = "#292A4B"),
#'       axis.text = element_text(face = "bold", size = rel(1)),
#'       #axis.text.x = element_text(angle = 45, hjust = 1), # Bætti við til að snúa labels á x-ás þannig að þau blandist ekki inn í hvert annað
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
#'         linewidth = 1#, # Breytti úr 1.5
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
