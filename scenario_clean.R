library(tidyverse)
library(readxl)
library(viridis)
library(cowplot)

# Variables explained here https://data.ene.iiasa.ac.at/iamc-1.5c-explorer/#/docs

# ==== READ data ====

# Read model data
models <- read.csv("data/iamc15_scenario_data_all_regions_r2.0.csv", header=T) %>%
  mutate(mod_scen = str_c(Model, Scenario, sep=" | "))

# Re-structure model data
runs <- models %>%
  pivot_longer(X2000:X2100, names_to="Year", values_to="Value") %>%
  pivot_wider(id_cols=c("Model", "Scenario", "Region", "Variable", "Year", "Value"), names_from=Variable, values_from=Value, names_repair="unique") %>%
  mutate(mod_scen = str_c(Model, Scenario, sep=" | "),
         Year = as.numeric(str_sub(Year,2,5)))
colnames(runs) <- gsub(" ", "", colnames(runs))
colnames(runs) <- gsub("\\|", ".", colnames(runs))
colnames(runs) <- gsub("\\/", "", colnames(runs))

# ==== Clean data ====

# Exclude reference scenarios, and scenarios that don't account for carbon sequestration (Shell World Energy) or bioenergy (C-ROADS)
mod_exclude <- c("Reference", "C-ROADS-5.005", "Shell World Energy Model 2018")
runs <- filter(runs, !Model %in% mod_exclude)

# For all remaining runs...
ms <- unique(runs$mod_scen)
n_ms <- length(ms)

# Replace variables of interest with zero, in the years they report data
for (i in seq(1,n_ms)) {
  # Use Final Energy variable to pick out which years are present in the model
  yrs <- runs$Year[runs$mod_scen %in% ms[i] & !is.na(runs$FinalEnergy)]
  # replace variables of interest in these years with zero, if they are NA
  runs$SecondaryEnergy.Electricity.Ocean[is.na(runs$SecondaryEnergy.Electricity.Ocean) & runs$mod_scen %in% ms[i] & runs$Year %in% yrs] <- 0
  runs$SecondaryEnergy.Electricity.Geothermal[is.na(runs$SecondaryEnergy.Electricity.Geothermal) & runs$mod_scen %in% ms[i] & runs$Year %in% yrs] <- 0
  runs$SecondaryEnergy.Electricity.Gas.wCCS[is.na(runs$SecondaryEnergy.Electricity.Gas.wCCS) & runs$mod_scen %in% ms[i] & runs$Year %in% yrs] <- 0
  runs$SecondaryEnergy.Electricity.Gas.woCCS[is.na(runs$SecondaryEnergy.Electricity.Gas.woCCS) & runs$mod_scen %in% ms[i] & runs$Year %in% yrs] <- 0
  runs$SecondaryEnergy.Electricity.Coal.wCCS[is.na(runs$SecondaryEnergy.Electricity.Coal.wCCS) & runs$mod_scen %in% ms[i] & runs$Year %in% yrs] <- 0
  runs$SecondaryEnergy.Electricity.Coal.woCCS[is.na(runs$SecondaryEnergy.Electricity.Coal.woCCS) & runs$mod_scen %in% ms[i] & runs$Year %in% yrs] <- 0
  runs$SecondaryEnergy.Electricity.Oil.wCCS[is.na(runs$SecondaryEnergy.Electricity.Oil.wCCS) & runs$mod_scen %in% ms[i] & runs$Year %in% yrs] <- 0
  runs$SecondaryEnergy.Electricity.Oil.woCCS[is.na(runs$SecondaryEnergy.Electricity.Oil.woCCS) & runs$mod_scen %in% ms[i] & runs$Year %in% yrs] <- 0
  runs$SecondaryEnergy.Electricity.Biomass.wCCS[is.na(runs$SecondaryEnergy.Electricity.Biomass.wCCS) & runs$mod_scen %in% ms[i] & runs$Year %in% yrs] <- 0
  runs$SecondaryEnergy.Electricity.Biomass.woCCS[is.na(runs$SecondaryEnergy.Electricity.Biomass.woCCS) & runs$mod_scen %in% ms[i] & runs$Year %in% yrs] <- 0
  runs$CarbonSequestration.CCS.Biomass[is.na(runs$CarbonSequestration.CCS.Biomass) & runs$mod_scen %in% ms[i] & runs$Year %in% yrs] <- 0
} 

# Create Renewables variable = Solar + Wind + Hydro
runs <- mutate(runs, SecondaryEnergy.Electricity.Renewables = SecondaryEnergy.Electricity.Hydro + SecondaryEnergy.Electricity.Solar + SecondaryEnergy.Electricity.Wind + SecondaryEnergy.Electricity.Ocean + SecondaryEnergy.Electricity.Geothermal)
# Create Fossil, Fossil CCS, and Fossil woCCS variables
runs <- mutate(runs, SecondaryEnergy.Electricity.Fossil.wCCS = SecondaryEnergy.Electricity.Coal.wCCS + SecondaryEnergy.Electricity.Gas.wCCS + SecondaryEnergy.Electricity.Oil.wCCS)
runs <- mutate(runs, SecondaryEnergy.Electricity.Fossil.woCCS = SecondaryEnergy.Electricity.Coal.woCCS + SecondaryEnergy.Electricity.Gas.woCCS + SecondaryEnergy.Electricity.Oil.woCCS)
runs <- mutate(runs, SecondaryEnergy.Electricity.Fossil2 = SecondaryEnergy.Electricity.Coal + SecondaryEnergy.Electricity.Gas + SecondaryEnergy.Electricity.Oil)


# ==== Write clean data ====

write.csv(runs, file="data/runs_clean.csv", row.names=F)


