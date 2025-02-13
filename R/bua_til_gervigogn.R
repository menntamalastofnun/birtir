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

data <- fa_heildartolu(2)


# Dæmi um kvarða ----------------------------------------------------------

umsogn <- c(
  "Nemandi skilur illa textann. \n",
  "Nemandi skilur textann að hluta, og getur fundið einfaldar upplýsingar  í texta. \n\n",
  "Nemandinn skilur innihald textans, getur auðveldlega fundið upplýsingar í textanum og getur dregið ályktanir. \n\n",
  "Nemandi sýnir góðan skilning á þeim texta sem hann les, getur lesið á milli línanna og dregið flóknar ályktanir. \n"

)

kvardi <- list(
  kvardi_bil = c(0,20),
  kvardi_lysing = tibble(
    einkunn = c(3, 6, 10, 15),
    lysing = c("Þarfnast mikillar þjálfunar",
               "Þarfnast þjálfunar",
               "Á góðri leið",
               "Framúrskarandi"),
    umsogn = factor(umsogn, levels = umsogn)
  )
)



