#' Mynd af stadsetningu nemanda a kvarda asamt lysingu
#'
#' @param data einkunnir nemanda
#' @param kvardi tegund kvarda sem er notadur
#' @param fag "les" eða "stf" - ræður fjölda bakgrunsslita
#' @importFrom dplyr filter
#' @importFrom stringr str_wrap
#' @import ggplot2
#' @returns skilar mynd af stadsettningu nemanda
#' @export
#'
#' @examples lysa_stodu(fa_heildartolu(5), fa_kvarda())
#'
lysa_stodu <- function(data, kvardi, fag) {
  heildartala <- data |>
    dplyr::filter(grepl("Heildartala", profhluti))

  kvardi_bil <- kvardi$kvardi_bil
  kvardi_lysing <- kvardi$kvardi_lysing
  # Ákveður staðsetningu texta eftir því hvort við erum með fjögur bil (les) eða þrjú (stf)
  kvardi_texta_bil <- if (fag == "les") {
    c(1.5, 5, 10, 16)
  } else if (fag == "stf") {
    c(3.5, 9.5, 16)
  } else {
    stop("Óþekkt gildi í 'fag'. Notaðu 'les' eða 'stf'.")
  }

  if (nrow(heildartala) != 1) {
    stop("Gagnasettid inniheldur ekki profhlutan heildartolu.")
  }

  fjardlaegd_punkts_fra_texta <- 0.45

  heildartala |>
    ggplot(aes(.15, einkunn)) +
    litud_maelistika(
      y_range = c(kvardi_bil[1], kvardi_bil[2]),
      cutoffs = kvardi_lysing$einkunn,
      alpha = 1,
      litur = "#D8C1FF",
      fag = fag
    ) +
    geom_errorbar(
      aes(ymin = einkunn - sf, ymax = einkunn + sf),
      width = 0.05,
      color = "#292A4B",
      linewidth = 0.6
    ) +
    geom_point(
      size = 4,
      shape = 21,
      stroke = 1,
      color = "#292A4B",
      fill = "#292A4B",
      show.legend = FALSE
    ) +
    geom_text(
      data = kvardi_lysing,
      aes(
        x = fjardlaegd_punkts_fra_texta,
        y = kvardi_texta_bil,
        label = stringr::str_wrap(umsogn, width = 70)
      ),
      hjust = 0,
      vjust = 0.5,
      color = "#292A4B",
      size = 4,
      inherit.aes = FALSE
    ) +
    geom_segment(
      aes(x = 0.05, xend = 0.25, y = 10, yend = 10),
      linetype = "dotted",
      color = scales::alpha("#292A4B", 1),
      linewidth = 1,
      inherit.aes = FALSE
    ) +
    # Draw the "Nemandi" point in legend
    scale_x_continuous(limits = c(0, 2)) +
    scale_y_continuous(
      #limits = kvardi_bil,
      limits = c(0, 20),
      breaks = seq(from = kvardi_bil[1] + 1, to = kvardi_bil[2] - 1, by = 1),
      name = "Mælitala"
    ) +

    guides(
      shape = guide_legend(order = 1, override.aes = list(fill = "#292A4B")),
      fill = "none",  # Hide original ribbon fill legend
      linetype = guide_legend(order = 2),
      color = "none"
    ) +
    theme(
      line = element_line(linetype = 1, colour = "#292A4B"),
      axis.text = element_text(face = "bold", size = rel(1)),
      text = element_text(colour = "#292A4B"),
      rect = element_rect(fill = NULL, linetype = 0, colour = NA),
      legend.background = element_rect(fill = NULL),
      panel.grid = element_line(color = NULL, linetype = 3),
      panel.grid.major = element_line(colour = "#292A4B"),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
      plot.title = element_text(hjust = 0, face = "bold"),
      strip.background = element_rect(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_text(face = "bold", size = rel(1), margin = margin(r = 10)),
      axis.text.x = element_blank(),
      axis.text.y = element_text(face = "bold", size = rel(1), margin = margin(r = 10)),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      axis.line.y = element_line(linewidth = 1),
      plot.margin = unit(c(0.2, 0.5, 0.5, 0.5), "cm"),
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      legend.key = element_blank(),
      legend.box.margin = margin(t = 10)
    )

}


#' Mynd af legend fyrir lysa_stodu
#'
#' @importFrom dplyr filter
#' @importFrom stringr str_wrap
#' @importFrom tidyr crossing
#' @import ggplot2
#' @returns skilar mynd af legend fyrir lysa_stodu
#' @export
#'
#' @examples lysa_stodu_legend()
#'
lysa_stodu_legend <- function() {
  legend_df <- tibble::tibble(
    group = c("Nemandi", "95% öryggisbil", "Landsmeðaltal"),
    x = c(0.15, 0.6, 1.2),
    y = c(1, 1, 1),
    ymin = c(NA, 0.95, NA),
    ymax = c(NA, 1.05, NA)
  )

  ggplot() +
    # Dot (Nemandi)
    geom_point(
      data = dplyr::filter(legend_df, group == "Nemandi"),
      aes(x = x, y = y),
      size = 4,
      shape = 21,
      fill = "#292A4B",
      color = "#292A4B"
    ) +
    # Error bar (CI)
    geom_errorbar(
      data = dplyr::filter(legend_df, group == "95% öryggisbil"),
      aes(x = x, ymin = ymin, ymax = ymax),
      color = "#292A4B",
      width = 0.02,
      linewidth = 0.6
    ) +
    # Dotted line (Landsmeðaltal)
    geom_segment(
      data = dplyr::filter(legend_df, group == "Landsmeðaltal"),
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



#' Uppfærð mynd sem sýnir rétt/röng svör nemanda eftir atriðalýsingu og þyngd atriðis
#'
#' @param df einkunnir nemanda á atriðalevel
#' @import dplyr
#' @import stringr
#' @import ggplot2
#' @returns skilar mynd af svörum nemenda eftir atriði og þyngd atriða
#' @export
#'
#' @examples atridagreining_einstaklinga(df)
#'
atridagreining_einstaklinga <- function(df) {
  df <- df %>%
    mutate(
      item_statement_wrapped = stringr::str_wrap(item_statement, width = 45),
      rett = factor(rett, levels = c(TRUE, FALSE)),
      tyngd = factor(tyngd, levels = c("Létt", "Miðlungs", "Þungt")),
      tyngd_num = as.numeric(tyngd)
    ) %>%
    group_by(tyngd, item_statement_wrapped) %>%
    mutate(
      n = n(),
      point_index = row_number()
    ) %>%
    ungroup() %>%
    group_by(tyngd, item_statement_wrapped) %>%
    mutate(
      x_jitter = make_jitter(unique(n))[point_index]
    ) %>%
    ungroup()

  ggplot(df, aes(x = tyngd_num + x_jitter, y = item_statement_wrapped)) +
    geom_point(
      aes(fill = rett, color = rett),
      shape = 21,
      size = 5,
      alpha = 0.9
    ) +
    # scale_x_continuous(
    #   breaks = 1:3,
    #   labels = c("Létt", "Miðlungs", "Þungt"),
    #   expand = expansion(add = 0.3)
    # ) +
    scale_x_continuous(
      breaks = 1:3,
      labels = c("Létt", "Miðlungs", "Þungt"),
      limits = c(0.7, 3.3),  # Pad slightly around 1–3 to leave space for jitter
      expand = expansion(add = 0)
    ) +
    scale_fill_manual(
      values = c("TRUE" = "#D8C1FF", "FALSE" = "white"),
      labels = c("Rétt", "Rangt"),
      guide = guide_legend(order = 1)
    ) +
    scale_color_manual(
      values = c("TRUE" = "#292A4B", "FALSE" = "#292A4B"),
      labels = c("Rétt", "Rangt"),
      guide = guide_legend(order = 1)
    ) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text.y = element_text(size = 12, hjust = 0),
      axis.text.x = element_text(size = 12),
      legend.text = element_text(size = 12),
      legend.position = "bottom",
      legend.title = element_blank()
    ) +
    labs(
      fill = NULL,
      color = NULL
    )
}

#' Mynd sem sýnir hlutfall nemenda sem er undir ákveðnu viðmiði
#'
#' @param df einkunnir nemanda á atriðalevel með þyngd atriðanna flokkað í mjög létt, létt, þungt og mjög þungt
#' @param leidbeiningar umsögn um stöðu nemenda á viðmiðinu
#' @param filter_expr mælitölubilið sem verið er að skoða
#' @importFrom dplyr filter
#' @importFrom stringr str_wrap
#' @import ggplot2
#' @import scales
#' @returns skilar mynd af hlutfalli nemenda á ákveðnu mælitölubili
#' @export
#'
#' @examples skifurit(df_bekkur, "þurfa mikinn stuðning", filter_expr < 3)
#'
skifurit <- function(df, leidbeiningar, filter_expr) {
  filter_expr <- rlang::enquo(filter_expr)

  df_summary <- df %>%
    mutate(in_group = !!filter_expr) %>%
    count(in_group) %>%
    mutate(percent = n / sum(n))

  # Build the central label (just for the in_group = TRUE case)
  central_label <- df_summary %>%
    filter(in_group) %>%
    mutate(label = paste0(scales::percent(percent, accuracy = 1), " nemenda\n", leidbeiningar)) %>%
    pull(label)

  if(length(central_label) == 0) {
    central_label <- paste0("0% nemenda\n", leidbeiningar)
  }

  df_summary$fill_label <- factor(df_summary$in_group, labels = c("Aðrir", leidbeiningar))

  ggplot(df_summary, aes(x = 2, y = percent, fill = fill_label)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    annotate("text", x = 0.2, y = 0, label = central_label, size = 12, fontface = "bold", color = "#292A4B", lineheight = 1.1) +
    xlim(0.1, 2.5) +
    scale_fill_manual(values = setNames(c("#D8C1FF", "#F5EFFF"), c(leidbeiningar, "Aðrir"))) +
    theme_void() +
    labs(title = NULL) +
    theme(
      legend.position = "none",
      plot.margin = margin(0.5, 0.5, 0.5, 0.5),
      clip = "off"
    )
}


