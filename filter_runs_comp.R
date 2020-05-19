# Function to filter runs
require(tidyverse)

# Inputs: run sample, warming categories, region, sustainability limits, models to exclude if region selected
filter_runs_comp <- function(runs, temp_cats, limits, ms_exclude) {
  
  # Select runs that meet temp criteria
  runs <- runs %>% filter(category %in% temp_cats)

  # --- BECCS & Bioenergy - global
  bio_lim <- limits["bio"]
  beccs_lim <- limits["beccs"]
  af_lim <- limits["af"]
  
  # Which models meet these criteria at a global level in 2050?
  keep_ms <- runs$mod_scen[runs$Year==2050 & runs$Region %in% "World" & runs$CarbonSequestration.CCS.Biomass<=beccs_lim & runs$PrimaryEnergy.Biomass<=bio_lim]
  
  # --- FILTER runs
  runs <- filter(runs, mod_scen %in% keep_ms)
  
  return(runs)
  
}