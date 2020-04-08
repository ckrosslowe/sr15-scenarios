# script to read different samples of ipcc scenarios and summarise power info
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

# ==== READ data ====
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

# ==== Data wrangling ====

# Get names of model - scenario combos that satisfy given different temp criteria
# <1.5C
temp_scenarios <- meta$mod_scen[meta$category %in% c("1.5C low overshoot", "Below 1.5C")]
t_lab <- "<1.5C"
t_folder <- "temp_1p5C"
# <2C
#temp_scenarios <- meta$mod_scen[meta$category %in% c("1.5C low overshoot", "Below 1.5C", "1.5C high overshoot", "Lower 2C", "Higher 2C")]
#t_lab <- "<2C"
#t_folder <- "temp_2C"


# Select runs that meet temp criteria
runs <- runs %>% filter(mod_scen %in% temp_scenarios)

# BECCS correction
# Some runs have NA BECCS that should be zero, for the purposes of filtering 
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

# runs of AIM/CGE 2.1 to include in BECCS correction.
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
# It reports other energy figures for years 2000, 2005, 2010, then every 10 years to 2100. 
runs$CarbonSequestration.CCS.Biomass[runs$mod_scen %in% "MESSAGEix-GLOBIOM 1.0 | LowEnergyDemand" & runs$Year %in% c(2000, 2005, 2010, 2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)] <- 0
runs$CarbonSequestration.CCS.Biomass[runs$mod_scen %in% "GENeSYS-MOD 1.0 | 1.0" & runs$Year %in% c(2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050)] <- 0
runs$CarbonSequestration.CCS.Biomass[runs$mod_scen %in% AIM_CGE_20 & runs$Year %in% c(2005,2010,2015,2020,2025,2030,2035,2040,2045,2050,2055,2060,2065,2070,2075,2080,2085,2090,2095,2100)] <- 0
runs$CarbonSequestration.CCS.Biomass[runs$mod_scen %in% AIM_CGE_21 & runs$Year %in% c(2005,2010,2015,2020,2025,2030,2035,2040,2045,2050,2055,2060,2065,2070,2075,2080,2085,2090,2095,2100)] <- 0


# Bioenergy use in 2050
hist(runs$PrimaryEnergy.Biomass[runs$Year==2050])

# BECCS use in 2050
hist(runs$CarbonSequestration.CCS.Biomass[runs$Year==2050], 12)

# Apply Bioenergy and BECCS limits on runs
bio_lim <- 5000
beccs_lim <- 100
keep_runs <- runs$mod_scen[runs$Year==2050 & runs$CarbonSequestration.CCS.Biomass<=bio_lim & runs$PrimaryEnergy.Biomass<=beccs_lim]
runs <- filter(runs, mod_scen %in% keep_runs)

run_samp <- runs

ms <- unique(run_samp$mod_scen)
n_ms <- length(ms)

# Replace variables of interest with zero, in the years they report data
for (i in seq(1,n_ms)) {
  # Use Final Energy variable to pick out which years are present in the model
  yrs <- run_samp$Year[run_samp$mod_scen %in% ms[i] & !is.na(run_samp$FinalEnergy)]
  # replace variables of interest in these years with zero, if they are NA
  run_samp$SecondaryEnergy.Electricity.Ocean[is.na(run_samp$SecondaryEnergy.Electricity.Ocean) & run_samp$mod_scen %in% ms[i] & run_samp$Year %in% yrs] <- 0
  run_samp$SecondaryEnergy.Electricity.Geothermal[is.na(run_samp$SecondaryEnergy.Electricity.Geothermal) & run_samp$mod_scen %in% ms[i] & run_samp$Year %in% yrs] <- 0
  run_samp$SecondaryEnergy.Electricity.Gas.wCCS[is.na(run_samp$SecondaryEnergy.Electricity.Gas.wCCS) & run_samp$mod_scen %in% ms[i] & run_samp$Year %in% yrs] <- 0
  run_samp$SecondaryEnergy.Electricity.Gas.woCCS[is.na(run_samp$SecondaryEnergy.Electricity.Gas.woCCS) & run_samp$mod_scen %in% ms[i] & run_samp$Year %in% yrs] <- 0
  run_samp$SecondaryEnergy.Electricity.Coal.wCCS[is.na(run_samp$SecondaryEnergy.Electricity.Coal.wCCS) & run_samp$mod_scen %in% ms[i] & run_samp$Year %in% yrs] <- 0
  run_samp$SecondaryEnergy.Electricity.Coal.woCCS[is.na(run_samp$SecondaryEnergy.Electricity.Coal.woCCS) & run_samp$mod_scen %in% ms[i] & run_samp$Year %in% yrs] <- 0
  run_samp$SecondaryEnergy.Electricity.Oil.wCCS[is.na(run_samp$SecondaryEnergy.Electricity.Oil.wCCS) & run_samp$mod_scen %in% ms[i] & run_samp$Year %in% yrs] <- 0
  run_samp$SecondaryEnergy.Electricity.Oil.woCCS[is.na(run_samp$SecondaryEnergy.Electricity.Oil.woCCS) & run_samp$mod_scen %in% ms[i] & run_samp$Year %in% yrs] <- 0
  run_samp$SecondaryEnergy.Electricity.Biomass.wCCS[is.na(run_samp$SecondaryEnergy.Electricity.Biomass.wCCS) & run_samp$mod_scen %in% ms[i] & run_samp$Year %in% yrs] <- 0
  run_samp$SecondaryEnergy.Electricity.Biomass.woCCS[is.na(run_samp$SecondaryEnergy.Electricity.Biomass.woCCS) & run_samp$mod_scen %in% ms[i] & run_samp$Year %in% yrs] <- 0
  #run_samp$SecondaryEnergy.Electricity.[run_samp$mod_scen %in% ms[i] & run_samp$Year %in% yrs] <- 0
} 

# Create Renewables variable = Solar + Wind + Hydro
run_samp <- mutate(run_samp, SecondaryEnergy.Electricity.Renewables = SecondaryEnergy.Electricity.Hydro + SecondaryEnergy.Electricity.Solar + SecondaryEnergy.Electricity.Wind)
# Create Fossil CCS and Fossil woCCS variables
run_samp <- mutate(run_samp, SecondaryEnergy.Electricity.Fossil.wCCS = SecondaryEnergy.Electricity.Coal.wCCS + SecondaryEnergy.Electricity.Gas.wCCS + SecondaryEnergy.Electricity.Oil.wCCS)
run_samp <- mutate(run_samp, SecondaryEnergy.Electricity.Fossil.woCCS = SecondaryEnergy.Electricity.Coal.woCCS + SecondaryEnergy.Electricity.Gas.woCCS + SecondaryEnergy.Electricity.Oil.woCCS)
run_samp <- mutate(run_samp, SecondaryEnergy.Electricity.Fossil2 = SecondaryEnergy.Electricity.Coal + SecondaryEnergy.Electricity.Gas + SecondaryEnergy.Electricity.Oil)


######### PLOTS ############

year_range <- c(2000, 2100)

# ---- Electricity Variables Reference:
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
png(file=paste0(t_folder,"/elec_demand.png"),width=600,height=400,res=150,type='cairo')
ggplot(run_samp[!is.na(run_samp$SecondaryEnergy.Electricity),], 
       aes(x=Year, y=SecondaryEnergy.Electricity, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Demand (EJ)", title=paste("Electricity demand", t_lab)) +
  theme(legend.position = "none") +
  scale_color_viridis(discrete=T)
#colorRampPalette()
dev.off()

# Electrification
# Electricity as a % of final energy
png(file=paste0(t_folder,"/electrification.png"),width=600,height=400,res=150,type='cairo')
ggplot(run_samp[!is.na(run_samp$SecondaryEnergy.Electricity) & !is.na(run_samp$FinalEnergy),], 
       aes(x=Year, y=100*SecondaryEnergy.Electricity/FinalEnergy, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Electricity % of final energy", title=paste("Electrification", t_lab)) +
  theme(legend.position = "none") +
  lims(y=c(0,100), x=year_range) +
  scale_color_viridis(discrete=T)
dev.off()

# ---- Fossil demand ----
png(file=paste0(t_folder,"/fossil_demand.png"),width=600,height=400,res=150,type='cairo')
ggplot(run_samp[!is.na(run_samp$PrimaryEnergy.Fossil) & !is.na(run_samp$PrimaryEnergy),], 
       aes(x=Year, y=100*PrimaryEnergy.Fossil/PrimaryEnergy, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="% of primary energy", title=paste("Fossil fuel use", t_lab)) +
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

bio_p <- ggplot(run_samp[!is.na(run_samp$SecondaryEnergy.Electricity.Biomass), ],
                aes(x=Year, y=SecondaryEnergy.Electricity.Biomass, group=mod_scen)) + 
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Biomass (EJ)", title="") +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)

# Wind + Solar + Hydro 
renew_p <- ggplot(run_samp[!is.na(run_samp$SecondaryEnergy.Electricity.Hydro) & !is.na(run_samp$SecondaryEnergy.Electricity.Solar) & !is.na(run_samp$SecondaryEnergy.Electricity.Wind), ],# & !is.na(run_samp$SecondaryEnergy.Electricity.Geothermal), ],# & !is.na(run_samp$SecondaryEnergy.Electricity.Ocean), ],
                  aes(x=Year, 
                      y=SecondaryEnergy.Electricity.Hydro + SecondaryEnergy.Electricity.Wind + SecondaryEnergy.Electricity.Solar,# + SecondaryEnergy.Electricity.Geothermal,# + SecondaryEnergy.Electricity.Ocean, 
                      group=mod_scen)) + 
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="renewables (EJ)", title="") +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)

# All CCS = Coal, Gas, Oil, Biomass
ccs_p <- ggplot(run_samp[!is.na(run_samp$SecondaryEnergy.Electricity.Coal.wCCS) & !is.na(run_samp$SecondaryEnergy.Electricity.Gas.wCCS) & !is.na(run_samp$SecondaryEnergy.Electricity.Biomass.wCCS) & !is.na(run_samp$SecondaryEnergy.Electricity.Oil.wCCS), ],
                aes(x=Year, 
                    y=(SecondaryEnergy.Electricity.Coal.wCCS+SecondaryEnergy.Electricity.Gas.wCCS+SecondaryEnergy.Electricity.Biomass.wCCS+SecondaryEnergy.Electricity.Oil.wCCS), 
                    group=mod_scen)) + 
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="All CCS (EJ)", title="") +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)

png(file=paste0(t_folder,"/elec_mix.png"),width=600,height=800,res=150,type='cairo')
plot_grid(coal_p, gas_p, renew_p, bio_p, nuc_p, ccs_p, nrow=3, labels=c(t_lab))
dev.off()

# ---- Electricity price ----

# Data availability
table(run_samp$mod_scen, useNA="always", is.na(run_samp$Price.SecondaryEnergy.Electricity))
# Only 1/6 1.5C scenarios has electricity price

png(file=paste0(t_folder,"/electricity_price.png"),width=600,height=400,res=150,type='cairo')
ggplot(run_samp[!is.na(run_samp$Price.SecondaryEnergy.Electricity),], 
       aes(x=Year, y=Price.SecondaryEnergy.Electricity, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Price (USD/GJ)", title="Electricity price", subtitle=t_lab) +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)
dev.off()

# ---- Carbon Price ----
#Price|Carbon
table(run_samp$mod_scen, useNA="always", is.na(run_samp$Price.Carbon))
# All 1.5C scenarios has carbon price

png(file=paste0(t_folder,"/carbon_price.png"),width=600,height=400,res=150,type='cairo')
ggplot(run_samp[!is.na(run_samp$Price.Carbon),], 
       aes(x=Year, y=Price.Carbon, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Price (USD/tCO2)", title="Carbon price", subtitle=t_lab) +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)
dev.off()

# ---- Energy Investments ----

table(run_samp$mod_scen, useNA="always", is.na(run_samp$Investment.EnergySupply.Electricity))
# Only 3/6 1.5C scenarios has electricity investmants

# Not that enlightening for 1.5C
png(file=paste0(t_folder,"/elec_investment.png"),width=600,height=400,res=150,type='cairo')
ggplot(run_samp[!is.na(run_samp$Investment.EnergySupply.Electricity),], 
       aes(x=Year, y=Investment.EnergySupply.Electricity, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Investment (USD/yr)", title=paste("Total electricity Investment", t_lab)) +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)
dev.off()


# ---- Proportion of fossil that's abated ----

table(run_samp$mod_scen, useNA="always", is.na(run_samp$SecondaryEnergy.Electricity.Fossil.wCCS))
table(run_samp$mod_scen, useNA="always", is.na(run_samp$SecondaryEnergy.Electricity.Fossil.woCCS))
table(run_samp$mod_scen, useNA="always", is.na(run_samp$SecondaryEnergy.Electricity.Fossil2))

png(file=paste0(t_folder,"/fraction_fossil_abated.png"),width=600,height=400,res=150,type='cairo')
ggplot(run_samp[!is.na(run_samp$SecondaryEnergy.Electricity.Fossil.wCCS) & !is.na(run_samp$SecondaryEnergy.Electricity.Fossil2),], 
       aes(x=Year, y=100*SecondaryEnergy.Electricity.Fossil.wCCS/SecondaryEnergy.Electricity.Fossil2, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="% of production", title="Proportion of fossil production with CCS", subtitle= t_lab) +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)
dev.off()

# ---- Proportion of biomass that's abated ----

png(file=paste0(t_folder,"/fraction_biomass_abated.png"),width=600,height=400,res=150,type='cairo')
ggplot(run_samp[!is.na(run_samp$SecondaryEnergy.Electricity.Biomass.wCCS) & !is.na(run_samp$SecondaryEnergy.Electricity.Biomass),], 
       aes(x=Year, y=100*SecondaryEnergy.Electricity.Biomass.wCCS/SecondaryEnergy.Electricity.Biomass, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="% of production", title="Proportion of biomass production with CCS", subtitle= t_lab) +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)
dev.off()


# ---- Capacity ----

table(run_samp$mod_scen, useNA="always", is.na(run_samp$Capacity.Electricity.Coal))
table(run_samp$mod_scen, useNA="always", is.na(run_samp$CumulativeCapacity.Electricity.Coal.woCCS))
table(run_samp$mod_scen, useNA="always", is.na(run_samp$CumulativeCapacity.Electricity.Coal.wCCS))
table(run_samp$mod_scen, useNA="always", is.na(run_samp$CumulativeCapacity.Electricity.Coal))
table(run_samp$mod_scen, useNA="always", is.na(run_samp$CumulativeCapacity.Electricity.Gas))
table(run_samp$mod_scen, useNA="always", is.na(run_samp$CumulativeCapacity.Electricity.Biomass))


# Coal -
png(file=paste0(t_folder,"/coal_capacity.png"),width=600,height=400,res=150,type='cairo')
ggplot(run_samp[!is.na(run_samp$CumulativeCapacity.Electricity.Coal) & !is.na(run_samp$CumulativeCapacity.Electricity.Coal.wCCS),], 
       aes(x=Year, group=mod_scen)) +
  geom_line(aes(color=mod_scen, y=CumulativeCapacity.Electricity.Coal.wCCS), size=1, linetype="dashed") +
  geom_line(aes(y=CumulativeCapacity.Electricity.Coal, color=mod_scen), size=1, linetype="solid") +
  labs(x="", y="Cumulative capacity (GW)", title="Abated vs unabated coal", subtitle= t_lab) +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)
dev.off()

# Gas -
png(file=paste0(t_folder,"/gas_capacity.png"),width=600,height=500,res=150,type='cairo')
ggplot(run_samp[!is.na(run_samp$CumulativeCapacity.Electricity.Gas) & !is.na(run_samp$CumulativeCapacity.Electricity.Gas.wCCS),], 
       aes(x=Year, group=mod_scen)) +
  geom_line(aes(color=mod_scen, y=CumulativeCapacity.Electricity.Gas.wCCS), size=1, linetype="dashed") +
  geom_line(aes(y=CumulativeCapacity.Electricity.Gas, color=mod_scen), size=1, linetype="solid") +
  labs(x="", y="Cumulative capacity (GW)", title="Abated vs unabated gas", subtitle= t_lab, color="") +
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        text=element_text(size=8)) +
  lims(x=year_range) +
  scale_color_viridis(discrete=T, guide = guide_legend(override.aes = list(linetype="solid"))) 
  #scale_color_manual(guide = guide_legend(override.aes = list(linetype="solid")))
dev.off()

# Biomass -
png(file=paste0(t_folder,"/biomass_capacity.png"),width=600,height=500,res=150,type='cairo')
ggplot(run_samp[!is.na(run_samp$CumulativeCapacity.Electricity.Biomass) & !is.na(run_samp$CumulativeCapacity.Electricity.Biomass.wCCS),], 
       aes(x=Year, group=mod_scen)) +
  geom_line(aes(color=mod_scen, y=CumulativeCapacity.Electricity.Biomass.wCCS), size=1, linetype="dashed") +
  geom_line(aes(y=CumulativeCapacity.Electricity.Biomass, color=mod_scen), size=1, linetype="solid") +
  labs(x="", y="Cumulative capacity (GW)", title="Abated vs unabated Biomass", subtitle= t_lab, color="") +
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        text=element_text(size=8)) +
  #theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T, guide = guide_legend(override.aes = list(linetype="solid"))) 
#scale_color_manual(guide = guide_legend(override.aes = list(linetype="solid")))
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

coal_lims <- c(0, 1.1*max(run_samp$SecondaryEnergy.Electricity.Coal, na.rm=T))
gas_lims <- c(0, 1.1*max(run_samp$SecondaryEnergy.Electricity.Gas, na.rm=T))
bio_lims <- c(0, 1.1*max(run_samp$SecondaryEnergy.Electricity.Biomass, na.rm=T))

coal_all_p <- ggplot(run_samp[!is.na(run_samp$SecondaryEnergy.Electricity.Coal),], 
                        aes(x=Year, y=SecondaryEnergy.Electricity.Coal, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="all Coal (EJ)", title="") +
  theme(legend.position = "none", 
        text = element_text(size=8)) +
  lims(x=year_range, y=coal_lims) +
  scale_color_viridis(discrete=T)

coal_no_ccs_p <- ggplot(run_samp[!is.na(run_samp$SecondaryEnergy.Electricity.Coal.woCCS),], 
                        aes(x=Year, y=SecondaryEnergy.Electricity.Coal.woCCS, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Coal without CCS (EJ)", title="") +
  theme(legend.position = "none", 
        text = element_text(size=8)) +
  lims(x=year_range, y=coal_lims) +
  scale_color_viridis(discrete=T)

coal_ccs_p <- ggplot(run_samp[!is.na(run_samp$SecondaryEnergy.Electricity.Coal.wCCS),], 
                     aes(x=Year, y=SecondaryEnergy.Electricity.Coal.wCCS, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Coal with CCS (EJ)", title="") +
  theme(legend.position = "none", 
        text = element_text(size=8)) +
  lims(x=year_range, y=coal_lims) +
  scale_color_viridis(discrete=T)

gas_all_p <- ggplot(run_samp[!is.na(run_samp$SecondaryEnergy.Electricity.Gas),], 
                       aes(x=Year, y=SecondaryEnergy.Electricity.Gas, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="All gas (EJ)", title="") +
  theme(legend.position = "none",
        text = element_text(size=8)) +
  lims(x=year_range, y=gas_lims) +
  scale_color_viridis(discrete=T)

gas_no_ccs_p <- ggplot(run_samp[!is.na(run_samp$SecondaryEnergy.Electricity.Gas.woCCS),], 
                       aes(x=Year, y=SecondaryEnergy.Electricity.Gas.woCCS, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Gas without CCS (EJ)", title="") +
  theme(legend.position = "none",
        text = element_text(size=8)) +
  lims(x=year_range, y=gas_lims) +
  scale_color_viridis(discrete=T)

gas_ccs_p <- ggplot(run_samp[!is.na(run_samp$SecondaryEnergy.Electricity.Gas.wCCS),], 
                    aes(x=Year, y=SecondaryEnergy.Electricity.Gas.wCCS, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Gas with CCS (EJ)", title="") +
  theme(legend.position = "none",
        text = element_text(size=8)) +
  lims(x=year_range, y=gas_lims) +
  scale_color_viridis(discrete=T)

bio_all_p <- ggplot(run_samp[!is.na(run_samp$SecondaryEnergy.Electricity.Biomass),], 
                       aes(x=Year, y=SecondaryEnergy.Electricity.Biomass, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="All biomass (EJ)", title="") +
  theme(legend.position = "none",
        text = element_text(size=8)) +
  lims(x=year_range, y=bio_lims) +
  scale_color_viridis(discrete=T)

bio_no_ccs_p <- ggplot(run_samp[!is.na(run_samp$SecondaryEnergy.Electricity.Biomass.woCCS),], 
                       aes(x=Year, y=SecondaryEnergy.Electricity.Biomass.woCCS, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Biomass without CCS (EJ)", title="") +
  theme(legend.position = "none",
        text = element_text(size=8)) +
  lims(x=year_range, y=bio_lims) +
  scale_color_viridis(discrete=T)

bio_ccs_p <- ggplot(run_samp[!is.na(run_samp$SecondaryEnergy.Electricity.Biomass.wCCS),], 
                    aes(x=Year, y=SecondaryEnergy.Electricity.Biomass.wCCS, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Biomass with CCS (EJ)", title="") +
  theme(legend.position = "none",
        text = element_text(size=8)) +
  lims(x=year_range, y=bio_lims) +
  scale_color_viridis(discrete=T)

png(file=paste0(t_folder,"/ccs_breakdown.png"),width=800,height=800,res=150,type='cairo')
plot_grid(coal_all_p, coal_no_ccs_p, coal_ccs_p, gas_all_p, gas_no_ccs_p, gas_ccs_p, bio_all_p, bio_no_ccs_p, bio_ccs_p, nrow=3, labels=c(t_lab))
dev.off()


# ==== Summarising scenarios ====
test_yr <- 2050
table(run_samp$mod_scen[run_samp$Year==test_yr], useNA="always", is.na(run_samp$FinalEnergy[run_samp$Year==test_yr]))
# all 1.5C scenarios have 2020, 2030, and 2050!

coal_sum <- run_samp %>%
  filter(Year %in% c(2020, 2030, 2040, 2050)) %>%
  group_by(Year) %>%
  summarise(med = median(SecondaryEnergy.Electricity.Coal),
            q25 = quantile(SecondaryEnergy.Electricity.Coal, 0.25),
            q75 = quantile(SecondaryEnergy.Electricity.Coal, 0.75)) %>%
  ungroup() %>%
  mutate(Type="Coal_scen")

coal_woCCS_sum <- run_samp %>%
  filter(Year %in% c(2020, 2030, 2040, 2050)) %>%
  group_by(Year) %>%
  summarise(med = median(SecondaryEnergy.Electricity.Coal.woCCS),
            q25 = quantile(SecondaryEnergy.Electricity.Coal.woCCS, 0.25),
            q75 = quantile(SecondaryEnergy.Electricity.Coal.woCCS, 0.75)) %>%
  ungroup() %>%
  mutate(Type="Coal_woCCS_scen")

gas_woCCS_sum <- run_samp %>%
  filter(Year %in% c(2020, 2030, 2040, 2050)) %>%
  group_by(Year) %>%
  summarise(med = median(SecondaryEnergy.Electricity.Gas.woCCS),
            q25 = quantile(SecondaryEnergy.Electricity.Gas.woCCS, 0.25),
            q75 = quantile(SecondaryEnergy.Electricity.Gas.woCCS, 0.75)
  ) %>%
  ungroup() %>%
  mutate(Type="Gas_woCCS_scen")

renew_sum <- run_samp %>%
  filter(Year %in% c(2020, 2030, 2040, 2050)) %>%
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

# Function to plot different summarised technologies
# fuel_type
med_plot <- function(fuel_type, yr_max, ttl) {
  # scenario match
  if (fuel_type %in% "Coal") {
    scen_dat <- filter(run_samp, !is.na(SecondaryEnergy.Electricity.Coal), Year<=yr_max) %>% rename(Value = SecondaryEnergy.Electricity.Coal)
  }
  else if (fuel_type %in% "Coal_woCCS") {
    scen_dat <- filter(run_samp, !is.na(SecondaryEnergy.Electricity.Coal.woCCS), Year<=yr_max) %>% rename(Value = SecondaryEnergy.Electricity.Coal.woCCS)
  }
  else if (fuel_type %in% "Gas_woCCS") {
    scen_dat <- filter(run_samp, !is.na(SecondaryEnergy.Electricity.Gas.woCCS), Year<=yr_max) %>% rename(Value = SecondaryEnergy.Electricity.Gas.woCCS)
  }
  else if (fuel_type %in% "Renew") {
    scen_dat <- filter(run_samp, !is.na(SecondaryEnergy.Electricity.Renewables), Year<=yr_max) %>% rename(Value = SecondaryEnergy.Electricity.Renewables)
  }
  else {
    stop("Invalid input to 'fuel_type' variable")
  }
    
  # GER match
  if (fuel_type %in% c("Coal", "Coal_woCCS")) {
    ger_ft <- "Coal"
  }
  else if (fuel_type %in% c("Gas", "Gas_woCCS")) {
    ger_ft <- "Gas"
  }
  else if (fuel_type %in% c("Renew")) {
    ger_ft <- "Renewables"
  }
  else {
    stop("Invalid input to 'fuel_type' variable")
  }
  
  med_fuel_cols <- c("Gas"="blue", 
                     "Coal"="red", 
                     "Gas_woCCS_scen"="grey20",
                     "Coal_woCCS_scen"="grey20",
                     "Coal_scen"="grey20",
                     "Renewables"="purple",
                     "Renew_scen"="grey20")
  med_fuel_labs <- c("Renewables"="Historic", 
                     "Renew_scen"="Scenarios",
                     "Coal"="Historic",
                     "Gas"="Historic",
                     "Coal_scen"="Scenarios",
                     "Coal_woCCS_scen"="Scenarios",
                     "Gas_woCCS_scen"="Scenarios",
                     "Gas_scen"="Scenarios")
  
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
         subtitle=t_lab,
         colour="") +
    scale_x_continuous(breaks=c(2000, 2010, 2020, 2030, 2040, 2050), limits = c(2000, yr_max+5)) +
    scale_color_manual(values=med_fuel_cols, labels=med_fuel_labs)
}

png(file=paste0(t_folder,"/all_coal_sum.png"),width=900,height=600,res=150,type='cairo')
med_plot("Coal", 2050, "all Coal")
dev.off()

png(file=paste0(t_folder,"/coal_woCCS_sum.png"),width=900,height=600,res=150,type='cairo')
med_plot("Coal_woCCS", 2050, "unabated Coal")
dev.off()

png(file=paste0(t_folder,"/gas_woCCS_sum.png"),width=900,height=600,res=150,type='cairo')
med_plot("Gas_woCCS", 2050, "unabated Gas")
dev.off()

png(file=paste0(t_folder,"/renew_sum.png"),width=900,height=600,res=150,type='cairo')
med_plot("Renew", 2050, "Solar + Wind + Hydro")
dev.off()

# ==== 1.5C vs 2C samples ====

# I ran the above twice, to get 1.5C and 2C sets of runs + summary info, then saved these to csv 
#runs_summary_2 <- runs_summary
#run_samp_2 <- run_samp
#runs_summary_15 <- runs_summary
#run_samp_15 <- run_samp

write.csv(runs_summary, file="data/runs_summary_2C.csv", row.names=F)
write.csv(runs_summary, file="data/runs_summary_1p5C.csv", row.names=F)
write.csv(run_samp, file="data/run_samp_2C.csv", row.names=F)
write.csv(run_samp, file="data/run_samp_1p5C.csv", row.names=F)

