# script to lead ipcc 1.5C scenarios and summarise power info
library(tidyverse)
library(readxl)
library(viridis)
library(cowplot)

# ggplot theme
theme_set(
  theme_minimal() +
    theme(legend.position = "right")
)

# Variables explained here https://data.ene.iiasa.ac.at/iamc-1.5c-explorer/#/docs

# ---- READ data ----
# Read metadata
meta <- read_excel("data/sr15_metadata_indicators_r2.0.xlsx", sheet="meta") %>%
  mutate(mod_scen = str_c(model, scenario, sep=" | "))

# Read model data
models <- read.csv("data/iamc15_scenario_data_all_regions_r2.0.csv", header=T) %>%
  mutate(mod_scen = str_c(Model, Scenario, sep=" | "))

runs <- models %>% filter(Region %in% "World") %>%
  pivot_longer(X2000:X2100, names_to="Year", values_to="Value") %>%
  pivot_wider(id_cols=c("Model", "Scenario", "Region", "Variable", "Year", "Value"), names_from=Variable, values_from=Value, names_repair="unique") %>%
  mutate(mod_scen = str_c(Model, Scenario, sep=" | "),
         Year = as.numeric(str_sub(Year,2,5)))
colnames(runs) <- gsub(" ", "", colnames(runs))
colnames(runs) <- gsub("\\|", ".", colnames(runs))
colnames(runs) <- gsub("\\/", "", colnames(runs))

# Read actual generation data from GER
ger <- read.csv("data/global_electricity_review_2020_v2.csv", header = T) %>%
  filter(Country %in% "World") %>%
  mutate(Type2 = ifelse(Type %in% "Coal", "Coal", 
                        ifelse(Type %in% "Gas", "Gas", 
                               ifelse(Type %in% c("Solar", "Wind", "Hydro"), "Renewables", "Other"))))

# ---- First approach: Filter scenarios ----

# Warming and world
#paris_runs <- models %>% filter(Region %in% "World",
#                                mod_scen %in% paris_scenarios)

# Bioenergy <100EJ in 2050. Variable = "Primary Energy|Biomass"
# Which runs?
# Summarise bioenergy in 2050
#bioenergy_2050 <- filter(paris_runs, Variable == "Primary Energy|Biomass")
#hist(bioenergy_2050$X2050,5)
#sum(is.na(bioenergy_2050$X2050)) # 0 means all paris runs have valid value
  
#bio_lim_s <- models$mod_scen[models$Variable %in% "Primary Energy|Biomass" & models$X2050 < 100]

# BECCS <5Gt in 2050. Variable = "Carbon Sequestration|CCS|Biomass - Mt CO2/yr"
# Which runs?
# Summarise BECCS in 2050
#beccs_2050 <- filter(paris_runs, Variable == "Carbon Sequestration|CCS|Biomass")
#hist(beccs_2050$X2050,12)
#sum(is.na(beccs_2050$X2050)) # 0 means all paris runs have valid value

#beccs_lim_s <- models$mod_scen[models$Variable %in% "Carbon Sequestration|CCS|Biomass" & models$X2050 < 5000]

# Apply Bioenergy & BECCS limits
#runs <- filter(paris_runs, (mod_scen %in% bio_lim_s) & (mod_scen %in% beccs_lim_s))

# ---- New approach: Filter scenarios ----

# Get names of model - scenario combos that satisfy different temp criteria
paris_scenarios <- meta$mod_scen[meta$category %in% c("1.5C low overshoot", "Below 1.5C")]
two_deg_scenarios <- meta$mod_scen[meta$category %in% c("1.5C low overshoot", "Below 1.5C", "1.5C high overshoot", "Lower 2C", "Higher 2C")]

# Data completeness tables
#comp_tab <- models[models$Region %in% "World",c(1,2,4)]
#comp_tab$Nval <- NA
#comp_tab$Nval <- rowSums(!is.na(models[models$Region %in% "World", 6:106]))
#write.csv(comp_tab, file="data/comp_tab.csv", row.names=F)

#ms_vec <- unique(models$mod_scen)
#n_mod <- length(ms_vec)
#var_vec <- unique(as.character(models$Variable))
#n_var <- length(var_vec)
#
#tab_list <- list()
#
#for (i in 1:n_mod) {
#  tab <- tibble(mod_scen = rep(ms_vec[i],n_var),
#                var = NA,
#                included = NA)
#  for (j in 1:n_var) {
#    tab$var[j] <- var_vec[j]
#    tab$included[j] <- sum(models$mod_scen %in% ms_vec[i] & models$Variable %in% var_vec[j])
#  }  
#  tab_list[[i]] <- tab
#}
#
#vars_tab <- bind_rows(tab_list)

#var_tab <- as_tibble(table(model_scenarios=models$mod_scen[models$Region %in% "World"], variable=models$Variable[models$Region %in% "World"]))# pivot_longer()
#write.csv(var_tab, file="data/var_tab.csv", row.names=F)

# What years are available?
#runs$Year[runs$mod_scen=="AIM/CGE 2.1 | TERL_Baseline_LowCarbonTransportPolicy" & !is.na(runs$SecondaryEnergy.Electricity)]

# count SFCM scenarios
#sum(grepl("SFCM_SSP2", unique(models$mod_scen)), na.rm=T)

# runs of AIM/CGE 2.0 to include in BECCS correction. 
# They are all <2C and do not include the BECCS variable
AIM_CGE_20 <- c("AIM/CGE 2.0 | ADVANCE_NoPolicy", 
                "AIM/CGE 2.0 | ADVANCE_Reference",
                "AIM/CGE 2.0 | SFCM_SSP2_Bio_1p5Degree",
                "AIM/CGE 2.0 | SFCM_SSP2_Bio_2Degree",
                "AIM/CGE 2.0 | SFCM_SSP2_Bio_Baseline",
                "AIM/CGE 2.0 | SFCM_SSP2_EEEI_1p5Degree",
                "AIM/CGE 2.0 | SFCM_SSP2_EEEI_2Degree",
                "AIM/CGE 2.0 | SFCM_SSP2_EEEI_Baseline",
                "AIM/CGE 2.0 | SFCM_SSP2_LifeStyle_1p5Degree",
                "AIM/CGE 2.0 | SFCM_SSP2_LifeStyle_2Degree",
                "AIM/CGE 2.0 | SFCM_SSP2_LifeStyle_Baseline",
                "AIM/CGE 2.0 | SFCM_SSP2_Ref_1p5Degree",
                "AIM/CGE 2.0 | SFCM_SSP2_Ref_2Degree",
                "AIM/CGE 2.0 | SFCM_SSP2_Ref_Baseline",
                "AIM/CGE 2.0 | SFCM_SSP2_ST_CCS_1p5Degree",
                "AIM/CGE 2.0 | SFCM_SSP2_ST_CCS_2Degree",
                "AIM/CGE 2.0 | SFCM_SSP2_ST_CCS_Baseline",
                "AIM/CGE 2.0 | SFCM_SSP2_ST_bio_1p5Degree",
                "AIM/CGE 2.0 | SFCM_SSP2_ST_bio_2Degree",
                "AIM/CGE 2.0 | SFCM_SSP2_ST_bio_Baseline",
                "AIM/CGE 2.0 | SFCM_SSP2_ST_nuclear_1p5Degree",
                "AIM/CGE 2.0 | SFCM_SSP2_ST_nuclear_2Degree",
                "AIM/CGE 2.0 | SFCM_SSP2_ST_nuclear_Baseline",
                "AIM/CGE 2.0 | SFCM_SSP2_ST_solar_1p5Degree",
                "AIM/CGE 2.0 | SFCM_SSP2_ST_solar_2Degree",
                "AIM/CGE 2.0 | SFCM_SSP2_ST_solar_Baseline",
                "AIM/CGE 2.0 | SFCM_SSP2_ST_wind_1p5Degree",
                "AIM/CGE 2.0 | SFCM_SSP2_ST_wind_2Degree",
                "AIM/CGE 2.0 | SFCM_SSP2_ST_wind_Baseline",
                "AIM/CGE 2.0 | SFCM_SSP2_SupTech_1p5Degree",
                "AIM/CGE 2.0 | SFCM_SSP2_SupTech_2Degree",
                "AIM/CGE 2.0 | SFCM_SSP2_SupTech_Baseline",
                "AIM/CGE 2.0 | SFCM_SSP2_combined_1p5Degree",
                "AIM/CGE 2.0 | SFCM_SSP2_combined_2Degree",
                "AIM/CGE 2.0 | SFCM_SSP2_combined_Baseline",
                "AIM/CGE 2.0 | SSP1-45",
                "AIM/CGE 2.0 | SSP1-Baseline",
                "AIM/CGE 2.0 | SSP2-60",
                "AIM/CGE 2.0 | SSP2-Baseline",
                "AIM/CGE 2.0 | SSP3-Baseline",
                "AIM/CGE 2.0 | SSP4-Baseline",
                "AIM/CGE 2.0 | SSP5-Baseline"
)

AIM_CGE_21 <- c("AIM/CGE 2.1 | CD-LINKS_NPi",
                "AIM/CGE 2.1 | CD-LINKS_NoPolicy",
                "AIM/CGE 2.1 | EMF33_Baseline",
                "AIM/CGE 2.1 | EMF33_Med2C_nobeccs",
                "AIM/CGE 2.1 | EMF33_Med2C_nofuel",
                "AIM/CGE 2.1 | EMF33_Med2C_none",
                "AIM/CGE 2.1 | EMF33_tax_hi_none",
                "AIM/CGE 2.1 | EMF33_tax_lo_none",
                "AIM/CGE 2.1 | TERL_Baseline_LowCarbonTransportPolicy",
                "AIM/CGE 2.1 | TERL_Baseline_NoTransportPolicy"
)

# LowEnergyDemand Scenario uses no negative emissions technology. 
# It reports other energy figures for years 2000, 2005, 2010, then every 10 years to 2100. Replace these years with 0 rather than NA, so they aren't thrown out by filtering
runs$CarbonSequestration.CCS.Biomass[runs$mod_scen %in% "MESSAGEix-GLOBIOM 1.0 | LowEnergyDemand" & runs$Year %in% c(2000, 2005, 2010, 2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)] <- 0
runs$CarbonSequestration.CCS.Biomass[runs$mod_scen %in% "GENeSYS-MOD 1.0 | 1.0" & runs$Year %in% c(2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050)] <- 0
#runs$CarbonSequestration.CCS.Biomass[grepl("AIM/CGE 2.0 | SFCM_SSP2",runs$mod_scen) & runs$Year %in% c(2005,2010,2015,2020,2025,2030,2035,2040,2045,2050,2055,2060,2065,2070,2075,2080,2085,2090,2095,2100)] <- 0
runs$CarbonSequestration.CCS.Biomass[runs$mod_scen %in% AIM_CGE_20 & runs$Year %in% c(2005,2010,2015,2020,2025,2030,2035,2040,2045,2050,2055,2060,2065,2070,2075,2080,2085,2090,2095,2100)] <- 0
runs$CarbonSequestration.CCS.Biomass[runs$mod_scen %in% AIM_CGE_21 & runs$Year %in% c(2005,2010,2015,2020,2025,2030,2035,2040,2045,2050,2055,2060,2065,2070,2075,2080,2085,2090,2095,2100)] <- 0
# NEED TO INCLUDE ALL THE ONES I'VE FLAGGED IN MY NOTES.

paris_runs <- runs %>% filter(mod_scen %in% paris_scenarios)
two_deg_runs <- runs %>% filter(mod_scen %in% two_deg_scenarios)


# Bioenergy use
hist(paris_runs$PrimaryEnergy.Biomass[paris_runs$Year==2050])
hist(two_deg_runs$PrimaryEnergy.Biomass[paris_runs$Year==2050])

# BECCS use
hist(paris_runs$CarbonSequestration.CCS.Biomass[paris_runs$Year==2050], 12)
hist(two_deg_runs$CarbonSequestration.CCS.Biomass[paris_runs$Year==2050], 12)

# Bioenergy use by year
ggplot(paris_runs[!is.na(paris_runs$PrimaryEnergy.Biomass),], aes(x=Year, y=PrimaryEnergy.Biomass, group=mod_scen)) +
  #geom_point()
  #geom_line(linetype="solid")
  geom_line(aes(color=mod_scen)) +
  geom_point(aes(color=mod_scen)) +
  scale_y_continuous(limits=c(0,200)) +
  theme(legend.position = "none")
  #scale_x_continuous(limits=2040, 2060)

# BECCS use by year
ggplot(paris_runs[!is.na(paris_runs$CarbonSequestration.CCS.Biomass),], aes(x=Year, y=CarbonSequestration.CCS.Biomass, group=mod_scen)) +
  #geom_point()
  #geom_line(linetype="solid")
  geom_line(aes(color=mod_scen)) +
  geom_point(aes(color=mod_scen)) +
  scale_y_continuous(limits=c(0,10000)) +
  theme(legend.position = "none")


# TEST for absence of 2050 data
# Do any runs not have 2050 data, and are therefore being excluded unfairly?
bioen_2050_na_runs <- runs$mod_scen[runs$Year==2050 & is.na(runs$PrimaryEnergy.Biomass)]
bioen_2050_na <- filter(runs, mod_scen %in% bioen_2050_na_runs)
table(bioen_2050_na$PrimaryEnergy.Biomass, useNA="always")
beccs_2050_na_runs <- runs$mod_scen[runs$Year==2050 & is.na(runs$CarbonSequestration.CCS.Biomass)]
beccs_2050_na <- filter(runs, mod_scen %in% beccs_2050_na_runs)
table(beccs_2050_na$CarbonSequestration.CCS.Biomass, useNA="always")
table(beccs_2050_na$CarbonSequestration.CCS ,useNA="always")
# Yes. The BECCS run should have been included - the line 68 accounts for this 

# Apply limits on paris runs
keep_runs <- paris_runs$mod_scen[paris_runs$Year==2050 & paris_runs$CarbonSequestration.CCS.Biomass<5000 & paris_runs$PrimaryEnergy.Biomass<100]
keep2_runs <- two_deg_runs$mod_scen[two_deg_runs$Year==2050 & two_deg_runs$CarbonSequestration.CCS.Biomass<5000 & two_deg_runs$PrimaryEnergy.Biomass<100]
p_runs <- filter(paris_runs, mod_scen %in% keep_runs)
p2_runs <- filter(two_deg_runs, mod_scen %in% keep2_runs)
 
# Everything now on should be reproducible for whichever sub-set of runs is selected
# runs variable = run_samp

run_samp <- p_runs

# Create alternative version with NA in certain variables & certain years replaced with zero
run_samp_fill <- run_samp

ms <- unique(run_samp$mod_scen)
n_ms <- length(ms)
vars_fill <- c()

for (i in seq(1,n_ms)) {
  # Use Final Energy variable to pick out which years are present
  #   what years?
  yrs <- run_samp_fill$Year[run_samp_fill$mod_scen %in% ms[i] & !is.na(run_samp_fill$FinalEnergy)]
  # replace variables of interest in these years with zero, if they are NA
  #run_samp_fill[run_samp_fill$mod_scen %in% ms[i] & run_samp_fill$Year %in% yrs, colnames(run_samp_fill) %in% vars_fill] <- 0
  run_samp_fill$SecondaryEnergy.Electricity.Ocean[is.na(run_samp_fill$SecondaryEnergy.Electricity.Ocean) & run_samp_fill$mod_scen %in% ms[i] & run_samp_fill$Year %in% yrs] <- 0
  run_samp_fill$SecondaryEnergy.Electricity.Geothermal[is.na(run_samp_fill$SecondaryEnergy.Electricity.Geothermal) & run_samp_fill$mod_scen %in% ms[i] & run_samp_fill$Year %in% yrs] <- 0
  run_samp_fill$SecondaryEnergy.Electricity.Gas.wCCS[is.na(run_samp_fill$SecondaryEnergy.Electricity.Gas.wCCS) & run_samp_fill$mod_scen %in% ms[i] & run_samp_fill$Year %in% yrs] <- 0
  run_samp_fill$SecondaryEnergy.Electricity.Coal.wCCS[is.na(run_samp_fill$SecondaryEnergy.Electricity.Coal.wCCS) & run_samp_fill$mod_scen %in% ms[i] & run_samp_fill$Year %in% yrs] <- 0
  run_samp_fill$SecondaryEnergy.Electricity.Oil.wCCS[is.na(run_samp_fill$SecondaryEnergy.Electricity.Oil.wCCS) & run_samp_fill$mod_scen %in% ms[i] & run_samp_fill$Year %in% yrs] <- 0
  run_samp_fill$SecondaryEnergy.Electricity.Biomass.wCCS[is.na(run_samp_fill$SecondaryEnergy.Electricity.Biomass.wCCS) & run_samp_fill$mod_scen %in% ms[i] & run_samp_fill$Year %in% yrs] <- 0
  #run_samp_fill$SecondaryEnergy.Electricity.[run_samp_fill$mod_scen %in% ms[i] & run_samp_fill$Year %in% yrs] <- 0
  #run_samp_fill$SecondaryEnergy.Electricity.[run_samp_fill$mod_scen %in% ms[i] & run_samp_fill$Year %in% yrs] <- 0
  #run_samp_fill$SecondaryEnergy.Electricity.[run_samp_fill$mod_scen %in% ms[i] & run_samp_fill$Year %in% yrs] <- 0
  #run_samp_fill$SecondaryEnergy.Electricity.[run_samp_fill$mod_scen %in% ms[i] & run_samp_fill$Year %in% yrs] <- 0
  
} 

# Renewables variable = Solar + Wind + Hydro
run_samp_fill <- mutate(run_samp_fill, SecondaryEnergy.Electricity.Renewables = SecondaryEnergy.Electricity.Hydro + SecondaryEnergy.Electricity.Solar + SecondaryEnergy.Electricity.Wind)
run_samp <- mutate(run_samp, SecondaryEnergy.Electricity.Renewables = SecondaryEnergy.Electricity.Hydro + SecondaryEnergy.Electricity.Solar + SecondaryEnergy.Electricity.Wind)

######### PLOTS ############

year_range <- c(2000, 2100)

# ---- Electricity Variables ----
#SecondaryEnergy.Electricity.#SecondaryEnergy.Electricity.Biomass
#SecondaryEnergy.Electricity.Biomass.wCCS
#SecondaryEnergy.Electricity.Biomass.woCCS
#SecondaryEnergy.Electricity.Coal
#SecondaryEnergy.Electricity.Coal.wCCS
#SecondaryEnergy.Electricity.Coal|woCCS
#SecondaryEnergy.Electricity.Fossil
#SecondaryEnergy.Electricity.Gas
#SecondaryEnergy.Electricity.Gas.wCCS
#SecondaryEnergy.Electricity.Gas.woCCS
#SecondaryEnergy.Electricity.Geothermal
#SecondaryEnergy.Electricity.Hydro
#SecondaryEnergy.Electricity.Non-Biomass.Renewables
#SecondaryEnergy.Electricity.Nuclear
#SecondaryEnergy.Electricity.Ocean
#SecondaryEnergy.Electricity.Oil
#SecondaryEnergy.Electricity.Oil.wCCS
#SecondaryEnergy.Electricity.Oil.woCCS
#SecondaryEnergy.Electricity.Other
#SecondaryEnergy.Electricity.Solar
#SecondaryEnergy.Electricity.Solar.CSP
#SecondaryEnergy.Electricity.Solar.PV
#SecondaryEnergy.Electricity.Wind
#SecondaryEnergy.Electricity.Wind.Offshore
#SecondaryEnergy.Electricity.Wind.Onshore


# ---- Electricity demand ----

# Overall demand
table(run_samp$mod_scen, useNA="always", is.na(run_samp$SecondaryEnergy.Electricity))

#SecondaryEnergy|Electricity
png(file="plots/elec_demand_1p5.png",width=600,height=400,res=150,type='cairo')
ggplot(run_samp[!is.na(run_samp$SecondaryEnergy.Electricity),], 
       aes(x=Year, y=SecondaryEnergy.Electricity, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Demand (EJ)", title="Electricity demand (<1.5C)") +
  theme(legend.position = "none") +
  scale_color_viridis(discrete=T)
  #colorRampPalette()
dev.off()

# Electrification
# Electricity as a % of final energy
png(file="plots/electrification_1p5.png",width=600,height=400,res=150,type='cairo')
ggplot(run_samp[!is.na(run_samp$SecondaryEnergy.Electricity) & !is.na(run_samp$FinalEnergy),], 
       aes(x=Year, y=100*SecondaryEnergy.Electricity/FinalEnergy, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Electricity % of final energy", title="Electrification (<1.5C)") +
  theme(legend.position = "none") +
  lims(y=c(0,100), x=year_range) +
  scale_color_viridis(discrete=T)
dev.off()

# ---- Fossil demand ----
png(file="plots/fossil_demand_1p5.png",width=600,height=400,res=150,type='cairo')
ggplot(run_samp[!is.na(run_samp$PrimaryEnergy.Fossil) & !is.na(run_samp$PrimaryEnergy),], 
       aes(x=Year, y=100*PrimaryEnergy.Fossil/PrimaryEnergy, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="% of primary energy", title="Fossil fuel use (<1.5C)") +
  theme(legend.position = "none") +
  lims(y=c(0,100), x=year_range) +
  scale_color_viridis(discrete=T)
dev.off()

# ---- Electricity mix ----
#run_samp_comp <-
#ggplot(run_samp[!is.na(run_samp$SecondaryEnergy.Electricity.Coal) & !is.na(run_samp$SecondaryEnergy.Electricity.Gas) & !is.na(run_samp$SecondaryEnergy.Electricity.Nuclear) & !is.na(run_samp$SecondaryEnergy.Electricity.Biomass) & !is.na(run_samp$SecondaryEnergy.Electricity.Hydro) & !is.na(run_samp$SecondaryEnergy.Electricity.Solar) & !is.na(run_samp$SecondaryEnergy.Electricity.Wind) & !is.na(run_samp$SecondaryEnergy.Electricity.Geothermal), ], 
coal_p <- ggplot(run_samp[!is.na(run_samp$SecondaryEnergy.Electricity.Coal), ],
                 aes(x=Year, y=SecondaryEnergy.Electricity.Coal, group=mod_scen)) + 
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Coal (EJ)", title="") +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)

gas_p <- ggplot(run_samp[!is.na(run_samp$SecondaryEnergy.Electricity.Gas), ],
                 aes(x=Year, y=SecondaryEnergy.Electricity.Gas, group=mod_scen)) + 
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Gas (EJ)", title="") +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)

# Make 4 other plots, then use plot_grid() to arrange them. :)
nuc_p <- ggplot(run_samp[!is.na(run_samp$SecondaryEnergy.Electricity.Nuclear), ],
                aes(x=Year, y=SecondaryEnergy.Electricity.Nuclear, group=mod_scen)) + 
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Nuclear (EJ)", title="") +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)

bio_p <- ggplot(run_samp_fill[!is.na(run_samp_fill$SecondaryEnergy.Electricity.Biomass), ],
                aes(x=Year, y=SecondaryEnergy.Electricity.Biomass, group=mod_scen)) + 
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Biomass (EJ)", title="") +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)

# Wind + Solar + Hydro 
renew_p <- ggplot(run_samp_fill[!is.na(run_samp_fill$SecondaryEnergy.Electricity.Hydro) & !is.na(run_samp_fill$SecondaryEnergy.Electricity.Solar) & !is.na(run_samp_fill$SecondaryEnergy.Electricity.Wind), ],# & !is.na(run_samp$SecondaryEnergy.Electricity.Geothermal), ],# & !is.na(run_samp$SecondaryEnergy.Electricity.Ocean), ],
                aes(x=Year, 
                    y=SecondaryEnergy.Electricity.Hydro + SecondaryEnergy.Electricity.Wind + SecondaryEnergy.Electricity.Solar,# + SecondaryEnergy.Electricity.Geothermal,# + SecondaryEnergy.Electricity.Ocean, 
                    group=mod_scen)) + 
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="renewables (EJ)", title="") +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)

# All CCS = Coal, Gas, Oil, Biomass
ccs_p <- ggplot(run_samp_fill[!is.na(run_samp_fill$SecondaryEnergy.Electricity.Coal.wCCS) & !is.na(run_samp_fill$SecondaryEnergy.Electricity.Gas.wCCS) & !is.na(run_samp_fill$SecondaryEnergy.Electricity.Biomass.wCCS) & !is.na(run_samp_fill$SecondaryEnergy.Electricity.Oil.wCCS), ],
                aes(x=Year, 
                    y=(SecondaryEnergy.Electricity.Coal.wCCS+SecondaryEnergy.Electricity.Gas.wCCS+SecondaryEnergy.Electricity.Biomass.wCCS+SecondaryEnergy.Electricity.Oil.wCCS), 
                    group=mod_scen)) + 
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="All CCS (EJ)", title="") +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)

png(file="plots/elec_mix_1p5.png",width=600,height=800,res=150,type='cairo')
plot_grid(coal_p, gas_p, renew_p, bio_p, nuc_p, ccs_p, nrow=3, labels=c("<1.5C"))
dev.off()

# ---- Electricity price ----

# Data availability
table(run_samp$mod_scen, useNA="always", is.na(run_samp$Price.SecondaryEnergy.Electricity))
# Only 1/6 1.5C scenarios has electricity price

#ggplot(run_samp[!is.na(run_samp$Price.SecondaryEnergy.Electricity),], 
#       aes(x=Year, y=Price.SecondaryEnergy.Electricity, group=mod_scen)) +
#  geom_line(aes(color=mod_scen), size=1) +
#  labs(x="", y="Price (USD)", title="Electricity price") +
#  theme(legend.position = "none") +
#  lims(x=year_range) +
#  scale_color_viridis(discrete=T)

# ---- Carbon Price ----
#Price|Carbon
table(run_samp$mod_scen, useNA="always", is.na(run_samp$Price.Carbon))
# All 1.5C scenarios has carbon price

png(file="plots/carbon_price_1p5.png",width=600,height=400,res=150,type='cairo')
ggplot(run_samp[!is.na(run_samp$Price.Carbon),], 
       aes(x=Year, y=Price.Carbon, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Price (USD/tCO2)", title="Carbon price (<1.5C)") +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)
dev.off()

# ---- Energy Investments ----

table(run_samp$mod_scen, useNA="always", is.na(run_samp$Investment.EnergySupply.Electricity))
# Only 3/6 1.5C scenarios has electricity investmants

# Not that enlightening for 1.5C
png(file="plots/elec_investment_1p5.png",width=600,height=400,res=150,type='cairo')
ggplot(run_samp[!is.na(run_samp$Investment.EnergySupply.Electricity),], 
       aes(x=Year, y=Investment.EnergySupply.Electricity, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Investment (USD/yr)", title="Total electricity Investment (<1.5C)") +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)
dev.off()


# ---- CCS breakdown ----
table(run_samp$mod_scen, useNA="always", is.na(run_samp$CarbonSequestration.CCS.Biomass))
# All 1.5 scenarios have it
table(run_samp$mod_scen, useNA="always", is.na(run_samp$CarbonSequestration.CCS.Fossil))
# 4/6 1.5 scenarios have it
table(run_samp$mod_scen, useNA="always", is.na(run_samp$CarbonSequestration.LandUse))
# 5/6 1.5 scenarios have it
table(run_samp$mod_scen, useNA="always", is.na(run_samp$SecondaryEnergy.Electricity.Coal.wCCS))
# All 1.5
table(run_samp$mod_scen, useNA="always", is.na(run_samp$SecondaryEnergy.Electricity.Gas.wCCS))
# All 1.5
table(run_samp$mod_scen, useNA="always", is.na(run_samp$SecondaryEnergy.Electricity.Biomass.wCCS))
# 5/6 1.5

coal_no_ccs_p <- ggplot(run_samp[!is.na(run_samp$SecondaryEnergy.Electricity.Coal.woCCS),], 
       aes(x=Year, y=SecondaryEnergy.Electricity.Coal.woCCS, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Coal without CCS (EJ)", title="") +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)

coal_ccs_p <- ggplot(run_samp[!is.na(run_samp$SecondaryEnergy.Electricity.Coal.wCCS),], 
                     aes(x=Year, y=SecondaryEnergy.Electricity.Coal.wCCS, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Coal with CCS (EJ)", title="") +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)

gas_no_ccs_p <- ggplot(run_samp[!is.na(run_samp$SecondaryEnergy.Electricity.Gas.woCCS),], 
                       aes(x=Year, y=SecondaryEnergy.Electricity.Gas.woCCS, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Gas without CCS (EJ)", title="") +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)

gas_ccs_p <- ggplot(run_samp[!is.na(run_samp$SecondaryEnergy.Electricity.Gas.wCCS),], 
                       aes(x=Year, y=SecondaryEnergy.Electricity.Gas.wCCS, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Gas with CCS (EJ)", title="") +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)

bio_no_ccs_p <- ggplot(run_samp[!is.na(run_samp$SecondaryEnergy.Electricity.Biomass.woCCS),], 
                       aes(x=Year, y=SecondaryEnergy.Electricity.Biomass.woCCS, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Biomass without CCS (EJ)", title="") +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)

bio_ccs_p <- ggplot(run_samp[!is.na(run_samp$SecondaryEnergy.Electricity.Biomass.wCCS),], 
                       aes(x=Year, y=SecondaryEnergy.Electricity.Biomass.wCCS, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Biomass with CCS (EJ)", title="") +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)

png(file="plots/ccs_breakdown_1p5.png",width=600,height=800,res=150,type='cairo')
plot_grid(coal_no_ccs_p, coal_ccs_p, gas_no_ccs_p, gas_ccs_p, bio_no_ccs_p, bio_ccs_p, nrow=3, labels=c("<1.5C"))
dev.off()


# ==== Summarising scenarios ====
test_yr <- 2030
table(run_samp_fill$mod_scen[run_samp_fill$Year==test_yr], useNA="always", is.na(run_samp_fill$FinalEnergy[run_samp_fill$Year==test_yr]))
# all 1.5C scenarios have 2020, 2030, and 2050!

coal_sum <- run_samp_fill %>%
  filter(Year %in% c(2020, 2030, 2050)) %>%
  group_by(Year) %>%
  summarise(med = median(SecondaryEnergy.Electricity.Coal),
            q25 = quantile(SecondaryEnergy.Electricity.Coal, 0.25),
            q75 = quantile(SecondaryEnergy.Electricity.Coal, 0.75)) %>%
  ungroup() %>%
  mutate(Type="Coal_scen")

coal_woCCS_sum <- run_samp_fill %>%
  filter(Year %in% c(2020, 2030, 2050)) %>%
  group_by(Year) %>%
  summarise(med = median(SecondaryEnergy.Electricity.Coal.woCCS),
            q25 = quantile(SecondaryEnergy.Electricity.Coal.woCCS, 0.25),
            q75 = quantile(SecondaryEnergy.Electricity.Coal.woCCS, 0.75)) %>%
  ungroup() %>%
  mutate(Type="Coal_woCCS_scen")

gas_woCCS_sum <- run_samp_fill %>%
  filter(Year %in% c(2020, 2030, 2050)) %>%
  group_by(Year) %>%
  summarise(med = median(SecondaryEnergy.Electricity.Gas.woCCS),
            q25 = quantile(SecondaryEnergy.Electricity.Gas.woCCS, 0.25),
            q75 = quantile(SecondaryEnergy.Electricity.Gas.woCCS, 0.75)
            ) %>%
  ungroup() %>%
  mutate(Type="Gas_woCCS_scen")

renew_sum <- run_samp_fill %>%
  filter(Year %in% c(2020, 2030, 2050)) %>%
  group_by(Year) %>%
  summarise(med = median(SecondaryEnergy.Electricity.Renewables),
            q25 = quantile(SecondaryEnergy.Electricity.Renewables, 0.25),
            q75 = quantile(SecondaryEnergy.Electricity.Renewables, 0.75)) %>%
  ungroup() %>%
  mutate(Type="Renew_scen")

runs_summary <- bind_rows(coal_sum, coal_woCCS_sum, gas_woCCS_sum, renew_sum)

ger_sum <- ger %>%
  group_by(Type2, Year) %>%
  summarise(Value_TWh = sum(Value_TWh)) %>%
  ungroup()

#%>%
  #pivot_longer(cols=c("all_coal_med", "coal_no_ccs_med", "gas_no_ccs_med"), names_to=c("median")) 

# Here's where actual values could be appended, as further columns
# Actually I think it'd be easier to use a separate data set. From GER! 
#runs_summary$coal_actual <- NA
#runs_summary <- bind_rows(runs_summary, c("Year"=2019, "coal_med"=NA, "coal_q25"=NA, "coal_q75"=NA, "coal_actual"=32.7))

#pd <- position_dodge(0.1)

## All coal
#png(file="plots/all_coal_sum_1p5.png",width=900,height=600,res=150,type='cairo')
#ggplot(filter(runs_summary, Type %in% "Coal_scen"), aes(x=Year, y=med, group=Type)) +
#  geom_errorbar(aes(ymin=q25, ymax=q75), width=2, size=1, colour="grey20") +
#  geom_line(size=1, aes(colour=Type)) +
#  geom_point(size=4, colour="grey10") +
#  # individual scenarios?
#  geom_line(data=run_samp[!is.na(run_samp$SecondaryEnergy.Electricity.Coal) & run_samp$Year<=2050, ], 
#            aes(x=Year, y=SecondaryEnergy.Electricity.Coal, group=mod_scen), 
#            size=0.5, alpha=0.3, colour="grey20") +
#  geom_line(data=filter(ger, Country %in% "World", Type %in% "Coal"), 
#            aes(x=Year, y=Value_TWh/278, colour=Type), size=2) +
#  labs(x="",
#       y="Electricity production (EJ)",
#       title="Total electricity from coal",
#       colour="") +
#  scale_x_continuous(breaks=c(2000, 2010, 2020, 2030, 2040, 2050), limits = c(2000, 2055)) +
#  scale_color_manual(values=c("Coal_scen"="grey20", "Coal"="red"), labels=c("Historic", "1.5C scenarios"))
#dev.off()
#
#
## unabated coal
#png(file="plots/coal_woCCS_sum_1p5.png",width=900,height=600,res=150,type='cairo')
#ggplot(filter(runs_summary, Type %in% "Coal_woCCS_scen"), aes(x=Year, y=med, group=Type)) +
#  geom_errorbar(aes(ymin=q25, ymax=q75), width=2, size=1, colour="grey20") +
#  geom_line(size=1, aes(colour=Type)) +
#  geom_point(size=4, colour="grey10") +
#  # individual scenarios?
#  geom_line(data=run_samp[!is.na(run_samp$SecondaryEnergy.Electricity.Coal.woCCS) & run_samp$Year<=2050, ], 
#            aes(x=Year, y=SecondaryEnergy.Electricity.Coal.woCCS, group=mod_scen), 
#            size=0.5, alpha=0.3, colour="grey20") +
#  geom_line(data=filter(ger, Country %in% "World", Type %in% "Coal"), 
#            aes(x=Year, y=Value_TWh/278, colour=Type), size=2) +
#  labs(x="",
#       y="Electricity production (EJ)",
#       title="Total electricity from unabated coal",
#       colour="") +
#  scale_x_continuous(breaks=c(2000, 2010, 2020, 2030, 2040, 2050), limits = c(2000, 2055)) +
#  scale_color_manual(values=c("Coal_woCCS_scen"="grey20", "Coal"="red"), labels=c("Historic", "1.5C scenarios"))
#dev.off()
#
## unabated gas
#png(file="plots/gas_woCCS_sum_1p5.png",width=900,height=600,res=150,type='cairo')
#ggplot(filter(runs_summary, Type %in% "Gas_woCCS_scen"), aes(x=Year, y=med, group=Type)) +
#  geom_errorbar(aes(ymin=q25, ymax=q75), width=2, size=1, colour="grey20") +
#  geom_line(size=1, aes(colour=Type)) +
#  geom_point(size=4, colour="grey10") +
#  # individual scenarios?
#  geom_line(data=run_samp[!is.na(run_samp$SecondaryEnergy.Electricity.Gas.woCCS) & run_samp$Year<=2050, ], 
#            aes(x=Year, y=SecondaryEnergy.Electricity.Gas.woCCS, group=mod_scen), 
#            size=0.5, alpha=0.3, colour="grey20") +
#  geom_line(data=filter(ger, Country %in% "World", Type %in% "Gas"), 
#            aes(x=Year, y=Value_TWh/278, colour=Type), size=2) +
#  labs(x="",
#       y="Electricity production (EJ)",
#       title="Total electricity from unabated gas",
#       colour="") +
#  scale_x_continuous(breaks=c(2000, 2010, 2020, 2030, 2040, 2050), limits = c(2000, 2055)) +
#  scale_color_manual(values=c("Gas"="blue", "Gas_woCCS_scen"="grey20"), labels=c("Historic", "1.5C scenarios"))
#dev.off()


#med_fuel_cols <- values=c("Gas"="blue", 
#                          "Coal"="red", 
#                          "Gas_woCCS_scen"="grey20",
#                          "Coal_woCCS_scen"="grey20",
#                          "Coal_scen"="grey20")

med_plot <- function(fuel_type, yr_max, ttl) {
  # scenario match
  if (fuel_type %in% "Coal") scen_dat <- filter(run_samp_fill, !is.na(SecondaryEnergy.Electricity.Coal), Year<=yr_max) %>% rename(Value = SecondaryEnergy.Electricity.Coal)
  if (fuel_type %in% "Coal_woCCS") scen_dat <- filter(run_samp_fill, !is.na(SecondaryEnergy.Electricity.Coal.woCCS), Year<=yr_max) %>% rename(Value = SecondaryEnergy.Electricity.Coal.woCCS)
  if (fuel_type %in% "Gas_woCCS") scen_dat <- filter(run_samp_fill, !is.na(SecondaryEnergy.Electricity.Gas.woCCS), Year<=yr_max) %>% rename(Value = SecondaryEnergy.Electricity.Gas.woCCS)
  if (fuel_type %in% "Renew") scen_dat <- filter(run_samp_fill, !is.na(SecondaryEnergy.Electricity.Renewables), Year<=yr_max) %>% rename(Value = SecondaryEnergy.Electricity.Renewables)
  
  # GER match
  if (fuel_type %in% c("Coal", "Coal_woCCS")) ger_ft <- "Coal"
  if (fuel_type %in% c("Gas", "Gas_woCCS")) ger_ft <- "Gas"
  if (fuel_type %in% c("Renew")) ger_ft <- "Renewables"
  
  med_fuel_cols <- c("Gas"="blue", 
                     "Coal"="red", 
                     "Gas_woCCS_scen"="grey20",
                     "Coal_woCCS_scen"="grey20",
                     "Coal_scen"="grey20",
                     "Renewables"="purple",
                     "Renew_scen"="grey20")
  med_fuel_labs <- c("Renewables"="Historic", 
                     "Renew_scen"="1.5C scenarios",
                     "Coal"="Historic",
                     "Gas"="Historic",
                     "Coal_scen"="1.5C scenarios",
                     "Coal_woCCS_scen"="1.5C scenarios",
                     "Gas_woCCS_scen"="1.5C scenarios",
                     "Gas_scen"="1.5C scenarios")
  
#png(file=paste0("plots/",fuel_type,"_sum_1p5.png"),width=900,height=600,res=150,type='cairo')
ggplot(filter(runs_summary, Type %in% paste0(fuel_type,"_scen")), aes(x=Year, y=med, colour=Type)) +#, group=Type, colour=Type)) +
  geom_errorbar(aes(ymin=q25, ymax=q75), width=2, size=1, colour="grey20") +
  geom_line(size=1) +
  geom_point(size=4, colour="grey10") +
  # individual scenarios?
  geom_line(data=scen_dat, 
            aes(x=Year, y=Value, group=mod_scen), 
            size=0.5, alpha=0.3, colour="grey20") +
  geom_line(data=filter(ger_sum, Type2 %in% ger_ft), 
            aes(x=Year, y=Value_TWh/278, colour=Type2), size=2) +
  labs(x="",
       y="Electricity production (EJ)",
       title=paste("Total electricity from", ttl),
       colour="") +
  scale_x_continuous(breaks=c(2000, 2010, 2020, 2030, 2040, 2050), limits = c(2000, yr_max+5)) +
  scale_color_manual(values=med_fuel_cols, labels=med_fuel_labs)
#dev.off()

}

png(file="plots/all_coal_sum_1p5.png",width=900,height=600,res=150,type='cairo')
med_plot("Coal", 2050, "all Coal")
dev.off()

png(file="plots/coal_woCCS_sum_1p5.png",width=900,height=600,res=150,type='cairo')
med_plot("Coal_woCCS", 2050, "unabated Coal")
dev.off()

png(file="plots/gas_woCCS_sum_1p5.png",width=900,height=600,res=150,type='cairo')
med_plot("Gas_woCCS", 2050, "unabated Gas")
dev.off()

png(file="plots/renew_sum_1p5.png",width=900,height=600,res=150,type='cairo')
med_plot("Renew", 2050, "Solar + Wind + Hydro")
dev.off()

