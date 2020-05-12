# Function to filter runs
require(tidyverse)

# Inputs: run sample, warming categories, region, sustainability limits, models to exclude if region selected
filter_runs <- function(runs, temp_cats, reg="World", limits, ms_exclude) {
  
  # Select runs that meet temp criteria
  runs <- runs %>% filter(category %in% temp_cats)

  # calculate 2040-2060 average
  runs_avg <- runs %>% 
    filter(Year>=2040, 
           Year<=2060, 
           Region %in% "World") %>%
    select(mod_scen, 
           CarbonSequestration.CCS.Biomass,
           CarbonSequestration.LandUse) %>%
    #replace(is.na(.),0) %>%
    group_by(mod_scen) %>%
    summarise(avg_beccs = mean(CarbonSequestration.CCS.Biomass, na.rm=T),
              avg_af = mean(CarbonSequestration.LandUse, na.rm=T)) %>%
    replace(is.na(.), 0)
  
  # --- BECCS & Bioenergy - global
  bio_lim <- limits["bio"]
  beccs_lim <- limits["beccs"]
  af_lim <- limits["af"]
  # Which models meet these criteria at a global level in 2050?
  #keep_ms <- runs$mod_scen[runs$Year==2050 & runs$Region %in% "World" & runs$CarbonSequestration.CCS.Biomass<=beccs_lim & runs$PrimaryEnergy.Biomass<=bio_lim]
  #keep_ms <- runs$mod_scen[runs$Year==2050 & runs$Region %in% "World" & runs$CarbonSequestration.CCS.Biomass<=beccs_lim & runs$CarbonSequestration.LandUse <= af_lim]
  keep_ms <- runs_avg$mod_scen[runs_avg$avg_beccs <= beccs_lim & runs_avg$avg_af <= af_lim]
  
  # --- FILTER runs
  runs <- filter(runs, mod_scen %in% keep_ms, Region %in% reg)
  
  # --- REMOVE runs that don't include regional breakdowns
  # TEST shows which runs don't have Electricity in 2050 (All do in world, after filters)
  #table(runs$mod_scen[runs$Year==2050], !is.na(runs$SecondaryEnergy.Electricity[runs$Year==2050]))
  if (!reg  %in% "World") runs <- filter(runs, !mod_scen %in% ms_exclude)
  
  return(runs)
  
}