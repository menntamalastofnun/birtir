#' litid hjalparfall sem byr til gogn
#'
#' @param maelitala nemandans
#' @param profhlutar listi yfir profhluta sem profid samanstendur ur
#' @param breytileiki staerd notud til ad akvarda breytileika i undirprofum
#' @returns prufugogn
#' @importFrom stats rnorm
#' @export
#'
#' @examples fa_heildartolu(10)
fa_heildartolu <- function(maelitala = 10,
                           profhlutar = c(
                             "Heildartala",
                             "Orðskilningur",
                             "Djúpur skilningur",
                             "Ályktun",
                             "Bókstaflegur skilningur"
                           ),
                           breytileiki = 1.5)
{
  maelitolur <- round(stats::rnorm(length(profhlutar) - 1,
                            maelitala, breytileiki))

  tibble::tibble(
    kennitala = "310200-3257",
    nafn_nemanda = "Grettir Ásmundsson",
    prof_numer = "les07",
    dagsetnings_profs = "2025-03-17",
    profhluti = profhlutar,
    einkunn = c(maelitala, maelitolur)
  )

}

#' Daemi um kvarda
#'
#' @returns upplysingum um kvardan
#' @export
#'
#' @examples fa_kvarda()
fa_kvarda <- function() {
  umsogn <- c(
    "Nemandi skilur illa textann. \n",
    "Nemandi skilur textann að hluta, og getur fundið einfaldar upplýsingar  í texta. \n\n",
    "Nemandinn skilur innihald textans, getur auðveldlega fundið upplýsingar í textanum og getur dregið ályktanir. \n\n",
    "Nemandi sýnir góðan skilning á þeim texta sem hann les, getur lesið á milli línanna og dregið flóknar ályktanir. \n"

  )

  list(
    kvardi_bil = c(0, 20),
    kvardi_lysing = tibble::tibble(
      einkunn = c(3, 6, 10, 15),
      lysing = c(
        "Þarfnast mikillar þjálfunar",
        "Þarfnast þjálfunar",
        "Á góðri leið",
        "Framúrskarandi"
      ),
      umsogn = factor(umsogn, levels = umsogn)
    )
  )

}


#' Litapalleta
#'
#' @param litur grunnlitur sem er sidan lystur upp med fj_punkta
#' @param fj_punkta fjoldi throskuldsgilda sem skipta kvardanum
#'
#' @returns skilar kvarda med N litum
#' @importFrom grDevices colorRampPalette
#' @export
#'
#' @examples utbua_litapalletu("#C6D8CD", 4)
utbua_litapalletu <- function(litur, fj_punkta) {
  grDevices::colorRampPalette(c("white", litur))(fj_punkta+1)
}



#' Byr til litada maelistiku
#'
#' @param y_range hæsta og lægsta gildi á kvarðanum
#' @param cutoffs þar sem skilin á milli flokka eru
#' @param litur litur kvarðans
#' @param alpha gegnsæi lita
#' @param fag fagið, "les" eða "stf" — ræður fjölda lita
#'
#' @returns litadri mælistiku fyrir ggplot
#' @export
litud_maelistika <- function(y_range,
                             cutoffs,
                             litur = "#D8C1FF",  # base purple
                             alpha = 1,
                             fag = "les") {

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



#' Fall sem hjálpar að raða atriðunum í atridagreining_einstaklinga() fallinu
#'
#' @param n Fjöldi gagnapunkta í hverri línu
#' @returns Lárétta uppröðun á gagnapunktum í atridagreining_einstaklinga()
#' @export
#'
#' @examples make_jitter(unique(n))[point_index]
make_jitter <- function(n) {
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
