


#' Title
#'
#' @param litur grunnlitur sem er sidan lystur upp med fj_punkta
#' @param fj_punkta fjoldi throskuldsgilda sem skipta kvardanum
#'
#' @returns
#' @export
#'
#' @examples utbua_litapalletu("#C6D8CD", 4)
utbua_litapalletu <- function(litur, fj_punkta) {
  colorRampPalette(c("white", litur))(fj_punkta)
}


litud_maelistika <-  function(
                              y_range,
                              cutoffs,
                              litur = "#C6D8CD",
                              alpha = .8) {
  # Ensure cutoffs are sorted and within range
  cutoffs <- sort(unique(cutoffs))
  cutoffs <- cutoffs[cutoffs > y_range[1] & cutoffs < y_range[2]]

  # Define segment boundaries
  segments <- c(y_range[1], cutoffs, y_range[2])


  colors <- utbua_litapalletu(litur, length(cutoffs)+1)


    map(c(1,seq_along(cutoffs)+1), function(i) {
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
