# ============================================================
# project_settings.R
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggtext)
  library(pals)
})

# ----------------------------
# Default paths
# ----------------------------
if (!exists("data_dir", inherits = FALSE)) data_dir <- "../data"
if (!exists("fig_dir",  inherits = FALSE)) fig_dir  <- "../figs"

# ----------------------------
# Species taxonomy (23 members)
# ----------------------------
raw_lines <- c(
  "Pseudomonas putida HAMBI 6",
  "Acinetobacter johnsonii HAMBI 97",
  "Agrobacterium tumefaciens HAMBI 105",
  "Brevundimonas bullata HAMBI 262",
  "Comamonas testosteroni HAMBI 403",
  "Hafnia alvei HAMBI 1279",
  "Citrobacter koseri HAMBI 1287",
  "Morganella morganii HAMBI 1292",
  "Kluyvera intermedia HAMBI 1299",
  "Sphingobium yanoikuyae HAMBI 1842",
  "Sphingobacterium spiritivorum HAMBI 1896",
  "Aeromonas caviae HAMBI 1972",
  "Pseudomonas chlororaphis HAMBI 1977",
  "Chitinophaga sancti HAMBI 1988",
  "Trinickia caryophylli HAMBI 2159",
  "Bordetella avium HAMBI 2160",
  "Cupriavidus oxalaticus HAMBI 2164",
  "Paracoccus denitrificans HAMBI 2443",
  "Paraburkholderia kururiensis HAMBI 2494",
  "Stenotrophomonas maltophilia HAMBI 2659",
  "Moraxella canis HAMBI 2792",
  "Niabella yanshanensis HAMBI 3031",
  "Microvirga lotononidis HAMBI 3237"
)

species_map <- tibble(raw = raw_lines) %>%
  tidyr::separate(raw, into = c("genus", "species", "HAMBI", "hambi_num"),
                  sep = " ", remove = TRUE) %>%
  mutate(
    hambi_num_int = as.integer(hambi_num),
    strainID      = sprintf("HAMBI_%04d", hambi_num_int),
    strainID_dash = sprintf("HAMBI-%04d", hambi_num_int),
    full_name     = paste(genus, species),
    label_html    = sprintf("<i>%s</i> HAMBI %d", full_name, hambi_num_int)
  ) %>%
  select(strainID, strainID_dash, genus, species, hambi_num = hambi_num_int, full_name, label_html)

tax <- species_map %>%
  transmute(
    strainID = strainID_dash,
    genus    = genus,
    species  = species
  )

# ----------------------------
# Species colors + labels
# ----------------------------
palette_vec <- pals::alphabet(26)[1:nrow(species_map)]
species_colors <- setNames(palette_vec, species_map$strainID)
species_labels_html <- setNames(species_map$label_html, species_map$strainID)

scale_color_species <- function(...) {
  scale_color_manual(
    values = species_colors,
    breaks = names(species_labels_html),
    labels = species_labels_html,
    ...
  )
}

scale_fill_species <- function(...) {
  scale_fill_manual(
    values = species_colors,
    breaks = names(species_labels_html),
    labels = species_labels_html,
    ...
  )
}

ampicillin_col      <- "#D55E00"
growth_capacity_col <- "#009E73"

# ----------------------------
# Utilities
# ----------------------------
`%nin%` <- Negate(`%in%`)

logit <- function(x) {
  log(x / (1 - x))
}

minnz <- function(V) {
  C <- NULL
  k <- length(V)
  for (i in 1:k) {
    if ((V[i] == 0) == FALSE) (C[i] <- V[i]) else (C[i] <- 9999919)
  }
  m <- min(C)
  if (max(V) == 1) (m <- 1)
  if (m == 9999919) (warning("Error: Minimum calculation failed."))
  return(m)
}

quibble95 <- function(x, q = c(0.025, 0.5, 0.975)) {
  tibble(x = quantile(x, q), quantile = c("q2.5", "q50", "q97.5"))
}

# ----------------------------
# Themes
# ----------------------------
mybartheme <- function(...) {
  theme_bw() + theme(
    panel.spacing.x = unit(0, "line"),
    strip.placement = "outside",
    strip.background.x = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    ...
  )
}

mytheme <- function() {
  theme_bw() + theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank()
  )
}