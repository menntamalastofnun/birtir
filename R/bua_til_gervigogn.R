library(tidyverse)


# Dæmi um nemendur --------------------------------------------------------


data <- tibble::tibble(
  kennitala = "310200-3257",
  nafn_nemanda = "Grettir Ásmundsson",
  prof_numer = "les07",
  dagsetnings_profs = "2025-03-17",
  profhluti = c(
    "Heildartala", "Orðskilningur",
    "Djúpur skilningur", "Ályktun",
    "Bókstaflegur skilningur"
  ),
  einkunn = c(8, 7, 7, 9, 8)
)

fa_heildartolu <- function(data, talal) {
  tibble::tibble(
    kennitala = "310200-3257",
    nafn_nemanda = "Grettir Ásmundsson",
    prof_numer = "les07",
    dagsetnings_profs = "2025-03-17",
    profhluti = c(
      "Heildartala", "Orðskilningur",
      "Djúpur skilningur", "Ályktun",
      "Bókstaflegur skilningur"
    ),
    einkunn = c(talal, 7, 7, 9, 8)
  )

}


# Dæmi um kvarða ----------------------------------------------------------

kvardi_lysing <- c(
  "Nemandi skilur illa textann. \n",
  "Nemandi skilur textann að hluta, og getur fundið einfaldar upplýsingar  í texta. \n\n",
  "Nemandinn skilur innihald textans, getur auðveldlega fundið upplýsingar í textanum og getur dregið ályktanir. \n\n",
  "Nemandi sýnir góðan skilning á þeim texta sem hann les, getur lesið á milli línanna og dregið flóknar ályktanir. \n"

)

kvardi <-
  tibble(
    einkunn = c(1, 6, 10, 15),
    kvardi_lysing = factor(kvardi_lysing, levels = kvardi_lysing)
  )

