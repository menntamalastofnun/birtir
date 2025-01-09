library(tidyverse)

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
