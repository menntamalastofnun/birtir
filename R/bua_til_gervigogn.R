# Dæmi um nemendur --------------------------------------------------------

df <- tibble::tibble(
  kennitala = "310200-3257",
  nafn_nemanda = "Grettir Ásmundsson",
  prof_numer = "les07",
  dagsetnings_profs = "2025-03-17",
  profhluti = c(
    "Heildartala",
    "Leita og finna upplýsingar",
    "Túlkun og ígrundun upplýsinga"
  ),
  einkunn = c(8, 7, 9)
)


# Dæmi um kvarða ----------------------------------------------------------

umsogn <- c(
  "Nemandi skilur illa textann. \n",
  "Nemandi skilur textann að hluta, og getur fundið einfaldar upplýsingar  í texta. \n\n",
  "Nemandinn skilur innihald textans, getur auðveldlega fundið upplýsingar í textanum og getur dregið ályktanir. \n\n",
  "Nemandi sýnir góðan skilning á þeim texta sem hann les, getur lesið á milli línanna og dregið flóknar ályktanir. \n"

)

kvardi <- list(
  kvardi_bil = c(0,20),
  kvardi_lysing = tibble::tibble(
    einkunn = c(3, 7, 12, 20),
    lysing = c("Þarfnast mikillar þjálfunar",
               "Þarfnast þjálfunar",
               "Á góðri leið",
               "Framúrskarandi"),
    umsogn = factor(umsogn, levels = umsogn)
  )
)


# Dæmi um bekkjargögn ----------------------------------------------------------
df_bekkur <- tibble::tibble(
  #kennitala = "310200-3257",
  nafn_nemenda = c("Grettir Ásmundsson", "Helena Birgisdóttir", "María Birna Karlsdóttir", "Patrekur Sóleyjarson", "Pétur Vilhjálmsson"),
  prof = "les07",
  dagsetnings_profs = "2025-03-17",
  maelitala = c(2, 17, 5, 18, 9),
  sfmt = c(0.8, 1.2, 1, 0.9, 1.1)
  # profhluti = c(
  #   "Heildartala",
  #   "Leita og finna upplýsingar",
  #   "Túlkun og ígrundun upplýsinga"
  # )
  # #einkunn = c(8, 7, 9)
)



# Dæmi um gögn á atriðaleveli ----------------------------------------------------------
library(tibble)
library(dplyr)

set.seed(42)  # for reproducibility

# Define sample values
nafn <- c("Grettir Ásmundsson", "Helena Birgisdóttir", "María Birna Karlsdóttir", "Patrekur Sóleyjarson", "Pétur Vilhjálmsson")
kennitala <- c("310200-3257", "250201-4489", "180300-8765", "041199-1123", "120200-5567")
item_ids <- paste0("Spurning_", 1:10)
difficulty_levels <- c("Mjög létt", "Létt", "Þungt", "Mjög þungt")

# Create the dummy dataframe
df_atridi <- expand.grid(
  nafn = nafn,
  item_id = item_ids
) %>%
  mutate(
    kennitala = rep(kennitala, each = length(item_ids)),
    difficulty = sample(difficulty_levels, size = n(), replace = TRUE),
    student_response = sample(c(TRUE, FALSE), size = n(), replace = TRUE, prob = c(0.7, 0.3))
  )

df_atridi <- as_tibble(df_atridi)




