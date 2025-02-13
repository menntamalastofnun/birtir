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
  grDevices::colorRampPalette(c("white", litur))(fj_punkta)
}


#' Byr til litada maelistiku
#'
#' @param y_range haesta og laegsta gildi a kvardanum
#' @param cutoffs thar sem skilin a milli flokka er
#' @param litur a kvardanum
#' @param alpha gildi sem lysir gegnsaei litarins
#'
#' @returns litadri maelistiku fyrir ggplot
#' @export

litud_maelistika <-  function(y_range,
                              cutoffs,
                              litur = "#C6D8CD",
                              alpha = .8) {
  # Ensure cutoffs are sorted and within range
  cutoffs <- sort(unique(cutoffs))
  cutoffs <- cutoffs[cutoffs > y_range[1] & cutoffs < y_range[2]]

  # Define segment boundaries
  segments <- c(y_range[1], cutoffs, y_range[2])


  colors <- utbua_litapalletu(litur, length(cutoffs) + 1)


  map(c(1, seq_along(cutoffs) + 1), function(i) {
    annotate(
      "rect",
      ymin = segments[i],
      ymax = segments[i + 1],
      xmin = -Inf,
      xmax = Inf,
      fill = colors[i],
      alpha = alpha
    )
  })

}
