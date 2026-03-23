#' Deprecated legacy helper that builds example data
#'
#' @details
#' This function belongs to the legacy plotting workflow kept for reference
#' during the transition to `render_analysis_md()`.
#'
#' @param maelitala nemandans
#' @param profhlutar listi yfir profhluta sem profid samanstendur ur
#' @param breytileiki staerd notud til ad akvarda breytileika i undirprofum
#' @returns prufugogn
#' @importFrom stats rnorm
#' @export
#'
#' @examplesIf FALSE
#' fa_heildartolu(10)
fa_heildartolu <- function(maelitala = 10,
                           profhlutar = c(
                             "Heildartala",
                             "Ordskilningur",
                             "Djupur skilningur",
                             "Alyktun",
                             "Bokstaflegur skilningur"
                           ),
                           breytileiki = 1.5)
{
  warn_legacy("fa_heildartolu")

  maelitolur <- round(stats::rnorm(length(profhlutar) - 1,
                            maelitala, breytileiki))

  tibble::tibble(
    kennitala = "310200-3257",
    nafn_nemanda = "Grettir Asmundsson",
    prof_numer = "les07",
    dagsetnings_profs = "2025-03-17",
    profhluti = profhlutar,
    einkunn = c(maelitala, maelitolur)
  )

}

#' Deprecated legacy helper that returns example scale metadata
#'
#' @details
#' This function belongs to the legacy plotting workflow kept for reference
#' during the transition to `render_analysis_md()`.
#'
#' @returns upplysingum um kvardan
#' @export
#'
#' @examplesIf FALSE
#' fa_kvarda()
fa_kvarda <- function() {
  warn_legacy("fa_kvarda")

  umsogn <- c(
    "Nemandi skilur illa textann. \n",
    "Nemandi skilur textann ad hluta, og getur fundid einfaldar upplysingar  i texta. \n\n",
    "Nemandinn skilur innihald textans, getur audveldlega fundid upplysingar i textanum og getur dregid alyktanir. \n\n",
    "Nemandi synir godan skilning a theim texta sem hann les, getur lesid a milli linanna og dregid floknar alyktanir. \n"

  )

  list(
    kvardi_bil = c(0, 20),
    kvardi_lysing = tibble::tibble(
      einkunn = c(3, 6, 10, 15),
      lysing = c(
        "Tharfnast mikillar thjalfunar",
        "Tharfnast thjalfunar",
        "A godri leid",
        "Framurskarandi"
      ),
      umsogn = factor(umsogn, levels = umsogn)
    )
  )

}


#' Deprecated legacy helper for plot color palettes
#'
#' @details
#' This function belongs to the legacy plotting workflow kept for reference
#' during the transition to `render_analysis_md()`.
#'
#' @param litur grunnlitur sem er sidan lystur upp med fj_punkta
#' @param fj_punkta fjoldi throskuldsgilda sem skipta kvardanum
#'
#' @returns skilar kvarda med N litum
#' @importFrom grDevices colorRampPalette
#' @export
#'
#' @examplesIf FALSE
#' utbua_litapalletu("#C6D8CD", 4)
utbua_litapalletu <- function(litur, fj_punkta) {
  warn_legacy("utbua_litapalletu")

  grDevices::colorRampPalette(c("white", litur))(fj_punkta+1)
}



#' Deprecated legacy helper for colored score scales
#'
#' @details
#' This function belongs to the legacy plotting workflow kept for reference
#' during the transition to `render_analysis_md()`.
#'
#' @param y_range haesta og laegsta gildi a kvardanum
#' @param cutoffs thar sem skilin a milli flokka eru
#' @param litur litur kvardans
#' @param alpha gegnsaei lita
#' @param fag fagid, "les" eda "stf" - raedur fjolda lita
#'
#' @returns litadri maelistiku fyrir ggplot
#' @export
litud_maelistika <- function(y_range,
                             cutoffs,
                             litur = "#D8C1FF",  # base purple
                             alpha = 1,
                             fag = "les") {
  warn_legacy("litud_maelistika")

  # Ensure cutoffs are sorted and within range
  cutoffs <- sort(unique(cutoffs))
  cutoffs <- cutoffs[cutoffs > y_range[1] & cutoffs < y_range[2]]

  # Define segment boundaries
  segments <- c(y_range[1], cutoffs, y_range[2])

  # Determine number of segments (based on length of cutoffs + 1)
  n_segments <- length(segments) - 1

  # Adjust number of colors depending on `fag`
  n_colors <- if (fag == "les") 4 else if (fag == "stf") 3 else n_segments

  # Generate palette without white
  base_colors <- colorRampPalette(c(litur, "#F0E6FF"))(n_colors)

  # Create annotation rectangles
  purrr::map(seq_len(n_segments), function(i) {
    annotate(
      "rect",
      ymin = segments[i],
      ymax = segments[i + 1],
      xmin = -Inf,
      xmax = Inf,
      fill = base_colors[i],
      alpha = alpha
    )
  })
}



#' Deprecated legacy helper for item-plot jitter
#'
#' @details
#' This function belongs to the legacy plotting workflow kept for reference
#' during the transition to `render_analysis_md()`.
#'
#' @param n Fjoldi gagnapunkta i hverri linu
#' @returns Laretta upprodun a gagnapunktum i atridagreining_einstaklinga()
#' @export
#'
#' @examplesIf FALSE
#' make_jitter(unique(n))[point_index]
make_jitter <- function(n) {
  warn_legacy("make_jitter")

  if (n == 1) {
    return(0)
  } else if (n %% 2 == 1) {
    half <- (n - 1) / 2
    return(seq(-half, half, by = 1) * 0.15)
  } else {
    half <- n / 2
    return(seq(-half + 0.5, half - 0.5, by = 1) * 0.15)
  }
}

