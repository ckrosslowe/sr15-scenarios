# script to read different samples of ipcc scenarios and summarise power info
library(tidyverse)
library(readxl)
library(viridis)
library(cowplot)
library(plotly)
library(gganimate)

# ggplot theme
theme_set(
  theme_minimal() +
    theme(legend.position = "right")
)

# temp category colour scheme
temp_cols <- c("Below 1.5C"="steelblue1",
               "1.5C low overshoot"="seagreen1",
               "1.5C high overshoot"="gold",
               "Lower 2C"="darkorange",
               "Higher 2C"="indianred2")

# Variables explained here https://data.ene.iiasa.ac.at/iamc-1.5c-explorer/#/docs

# ==== READ data ====
# Read metadata
meta <- read_excel("data/sr15_metadata_indicators_r2.0.xlsx", sheet="meta") %>%
  mutate(mod_scen = str_c(model, scenario, sep=" | "))

# Read model data
runs_clean <- read.csv("data/runs_clean.csv", header=T, stringsAsFactors = F)

# Read actual generation data from GER (with IPCC region variable)
ger <- read.csv("data/ger_ipcc.csv", header = T, stringsAsFactors = F) %>%
  select(-Source)

# ==== Filter scenarios ====

# --- Temperature
temp_cats <- c("1.5C low overshoot", "Below 1.5C")
#temp_cats <- c("1.5C low overshoot", "Below 1.5C", "1.5C high overshoot", "Lower 2C", "Higher 2C")

temp_ms <- meta$mod_scen[meta$category %in% temp_cats]
# Select runs that meet temp criteria
runs <- runs_clean %>% filter(mod_scen %in% temp_ms)

#t_lab <- "<1.5C"
#t_folder <- "temp_1p5C"
t_lab <- "<2C"
t_folder <- "temp_2C"

# --- BECCS & Bioenergy - global
# Global bioenergy use in 2050
#hist(runs$PrimaryEnergy.Biomass[runs$Year==2050 & runs$Region %in% "World"])
bio_lim <- 100

# Global BECCS use in 2050
#hist(runs$CarbonSequestration.CCS.Biomass[runs$Year==2050 & runs$Region %in% "World"], 12)
beccs_lim <- 5000

# Which models meet these criteria at a global level in 2050?
keep_ms <- runs$mod_scen[runs$Year==2050 & runs$Region %in% "World" & runs$CarbonSequestration.CCS.Biomass<=beccs_lim & runs$PrimaryEnergy.Biomass<=bio_lim]

# --- Region
reg <- "R5OECD90+EU"    # IPCC name
reg_lab <- "OECD90_EU"  # GER name and chart label 

#reg <- "R5MAF"
#reg_lab <- "MAF"

#reg <- "R5ASIA"
#reg_lab <- "ASIA"

#reg <- "R5LAM"
#reg_lab <- "LAM"

#reg <- "R5REF"
#reg_lab <- "REF"

#reg <- "R5ROWO"
#reg_lab <- "ROW"

#reg <- "World"
#reg_lab <- "World"

# --- FILTER runs
runs <- filter(runs, mod_scen %in% keep_ms, Region %in% reg)

# --- REMOVE runs that don't include selected breakdowns
if (!reg  %in% "World") runs <- filter(runs, !mod_scen %in% "MESSAGEix-GLOBIOM 1.0 | LowEnergyDemand")

######### PLOTS ############

year_range <- c(2005, 2100)

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
table(runs$mod_scen, useNA="always", is.na(runs$SecondaryEnergy.Electricity))

#SecondaryEnergy|Electricity
png(file=paste0("plots/elec_demand_",str_replace(t_lab,"<",""),"_",reg_lab,".png"),width=600,height=400,res=150,type='cairo')
ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity),], 
       aes(x=Year, y=SecondaryEnergy.Electricity, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Demand (EJ)", title="Electricity demand", subtitle=paste0(t_lab,", ",reg_lab)) +
  theme(legend.position = "none") +
  scale_color_viridis(discrete=T)
#colorRampPalette()
dev.off()

# Electrification
# Electricity as a % of final energy
png(file=paste0("plots/electrification_",str_replace(t_lab,"<",""),"_",reg_lab,".png"),width=600,height=400,res=150,type='cairo')
ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity) & !is.na(runs$FinalEnergy),], 
       aes(x=Year, y=100*SecondaryEnergy.Electricity/FinalEnergy, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Electricity % of final energy", title="Electrification", subtitle=paste0(t_lab, ", ", reg_lab)) +
  theme(legend.position = "none") +
  lims(y=c(0,100), x=year_range) +
  scale_color_viridis(discrete=T)
dev.off()

# ---- Fossil demand ----
# Completeness test
table(runs$mod_scen, useNA="ifany", is.na(runs$PrimaryEnergy.Fossil))

png(file=paste0("plots/fossil_demand_",str_replace(t_lab,"<",""),"_",reg_lab,".png"),width=600,height=400,res=150,type='cairo')
ggplot(runs[!is.na(runs$PrimaryEnergy.Fossil) & !is.na(runs$PrimaryEnergy),], 
       aes(x=Year, y=100*PrimaryEnergy.Fossil/PrimaryEnergy, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="% of primary energy", title="Fossil fuel use", subtitle=paste0(t_lab, ", ", reg_lab)) +
  theme(legend.position = "none") +
  lims(y=c(0,100), x=year_range) +
  scale_color_viridis(discrete=T)
dev.off()

# ---- Electricity mix ----
#runs_comp <-
#ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Coal) & !is.na(runs$SecondaryEnergy.Electricity.Gas) & !is.na(runs$SecondaryEnergy.Electricity.Nuclear) & !is.na(runs$SecondaryEnergy.Electricity.Biomass) & !is.na(runs$SecondaryEnergy.Electricity.Hydro) & !is.na(runs$SecondaryEnergy.Electricity.Solar) & !is.na(runs$SecondaryEnergy.Electricity.Wind) & !is.na(runs$SecondaryEnergy.Electricity.Geothermal), ], 
coal_p <- ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Coal), ],
                 aes(x=Year, y=SecondaryEnergy.Electricity.Coal, group=mod_scen)) + 
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Coal (EJ)", title="") +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)

gas_p <- ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Gas), ],
                aes(x=Year, y=SecondaryEnergy.Electricity.Gas, group=mod_scen)) + 
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Gas (EJ)", title="") +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)

# Make 4 other plots, then use plot_grid() to arrange them. :)
nuc_p <- ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Nuclear), ],
                aes(x=Year, y=SecondaryEnergy.Electricity.Nuclear, group=mod_scen)) + 
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Nuclear (EJ)", title="") +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)

bio_p <- ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Biomass), ],
                aes(x=Year, y=SecondaryEnergy.Electricity.Biomass, group=mod_scen)) + 
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Biomass (EJ)", title="") +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)

# Wind + Solar + Hydro 
renew_p <- ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Hydro) & !is.na(runs$SecondaryEnergy.Electricity.Solar) & !is.na(runs$SecondaryEnergy.Electricity.Wind), ],# & !is.na(runs$SecondaryEnergy.Electricity.Geothermal), ],# & !is.na(runs$SecondaryEnergy.Electricity.Ocean), ],
                  aes(x=Year, 
                      y=SecondaryEnergy.Electricity.Hydro + SecondaryEnergy.Electricity.Wind + SecondaryEnergy.Electricity.Solar,# + SecondaryEnergy.Electricity.Geothermal,# + SecondaryEnergy.Electricity.Ocean, 
                      group=mod_scen)) + 
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="renewables (EJ)", title="") +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)

# All CCS = Coal, Gas, Oil, Biomass
ccs_p <- ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Coal.wCCS) & !is.na(runs$SecondaryEnergy.Electricity.Gas.wCCS) & !is.na(runs$SecondaryEnergy.Electricity.Biomass.wCCS) & !is.na(runs$SecondaryEnergy.Electricity.Oil.wCCS), ],
                aes(x=Year, 
                    y=(SecondaryEnergy.Electricity.Coal.wCCS+SecondaryEnergy.Electricity.Gas.wCCS+SecondaryEnergy.Electricity.Biomass.wCCS+SecondaryEnergy.Electricity.Oil.wCCS), 
                    group=mod_scen)) + 
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="All CCS (EJ)", title="") +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)

png(file=paste0("plots/elec_mix_",str_replace(t_lab,"<",""),"_",reg_lab,".png"),width=600,height=800,res=150,type='cairo')
plot_grid(coal_p, gas_p, renew_p, bio_p, nuc_p, ccs_p, nrow=3, labels=c(paste0(t_lab, ", ", reg_lab)))
dev.off()

# ---- Electricity price ----

# Data availability
table(runs$mod_scen, useNA="always", is.na(runs$Price.SecondaryEnergy.Electricity))
# Only 1/6 1.5C scenarios has electricity price

png(file=paste0("plots/electricity_price_",str_replace(t_lab,"<",""),"_",reg_lab,".png"),width=600,height=400,res=150,type='cairo')
ggplot(runs[!is.na(runs$Price.SecondaryEnergy.Electricity),], 
       aes(x=Year, y=Price.SecondaryEnergy.Electricity, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Price (USD/GJ)", title="Electricity price", subtitle=paste0(t_lab,", ",reg_lab)) +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)
dev.off()

# ---- Carbon Price ----
#Price|Carbon
table(runs$mod_scen, useNA="always", is.na(runs$Price.Carbon))
# All 1.5C scenarios has carbon price

png(file=paste0("plots/carbon_price_",str_replace(t_lab,"<",""),"_",reg_lab,".png"),width=600,height=400,res=150,type='cairo')
ggplot(runs[!is.na(runs$Price.Carbon),], 
       aes(x=Year, y=Price.Carbon, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Price (USD/tCO2)", title="Carbon price", subtitle=paste0(t_lab,", ",reg_lab)) +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)
dev.off()

# ---- Energy Investments ----

table(runs$mod_scen, useNA="always", is.na(runs$Investment.EnergySupply.Electricity))
# Only 3/6 1.5C scenarios has electricity investmants

# Not that enlightening for 1.5C
png(file=paste0("plots/elec_investment_",str_replace(t_lab,"<",""),"_",reg_lab,".png"),width=600,height=400,res=150,type='cairo')
ggplot(runs[!is.na(runs$Investment.EnergySupply.Electricity),], 
       aes(x=Year, y=Investment.EnergySupply.Electricity, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Investment (USD/yr)", title="Total electricity Investment", subtitle=paste0(t_lab,", ",reg_lab)) +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)
dev.off()


# ---- Proportion of fossil that's abated ----

#table(runs$mod_scen, useNA="always", is.na(runs$SecondaryEnergy.Electricity.Fossil.wCCS))
#table(runs$mod_scen, useNA="always", is.na(runs$SecondaryEnergy.Electricity.Fossil.woCCS))
#table(runs$mod_scen, useNA="always", is.na(runs$SecondaryEnergy.Electricity.Fossil2))
#
#png(file=paste0(t_folder,"/fraction_fossil_abated.png"),width=600,height=400,res=150,type='cairo')
#ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Fossil.wCCS) & !is.na(runs$SecondaryEnergy.Electricity.Fossil2),], 
#       aes(x=Year, y=100*SecondaryEnergy.Electricity.Fossil.wCCS/SecondaryEnergy.Electricity.Fossil2, group=mod_scen)) +
#  geom_line(aes(color=mod_scen), size=1) +
#  labs(x="", y="% of production", title="Proportion of fossil production with CCS", subtitle=paste0(t_lab,", ",reg_lab)) +
#  theme(legend.position = "none") +
#  lims(x=year_range) +
#  scale_color_viridis(discrete=T)
#dev.off()

# ---- Proportion of biomass that's abated ----

#png(file=paste0(t_folder,"/fraction_biomass_abated.png"),width=600,height=400,res=150,type='cairo')
#ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Biomass.wCCS) & !is.na(runs$SecondaryEnergy.Electricity.Biomass),], 
#       aes(x=Year, y=100*SecondaryEnergy.Electricity.Biomass.wCCS/SecondaryEnergy.Electricity.Biomass, group=mod_scen)) +
#  geom_line(aes(color=mod_scen), size=1) +
#  labs(x="", y="% of production", title="Proportion of biomass production with CCS", subtitle=paste0(t_lab,", ",reg_lab)) +
#  theme(legend.position = "none") +
#  lims(x=year_range) +
#  scale_color_viridis(discrete=T)
#dev.off()


# ---- Capacity ----
# NOT VERY USEFUL

#table(runs$mod_scen, useNA="always", is.na(runs$Capacity.Electricity.Coal))
#table(runs$mod_scen, useNA="always", is.na(runs$CumulativeCapacity.Electricity.Coal.woCCS))
#table(runs$mod_scen, useNA="always", is.na(runs$CumulativeCapacity.Electricity.Coal.wCCS))
#table(runs$mod_scen, useNA="always", is.na(runs$CumulativeCapacity.Electricity.Coal))
#table(runs$mod_scen, useNA="always", is.na(runs$CumulativeCapacity.Electricity.Gas))
#table(runs$mod_scen, useNA="always", is.na(runs$CumulativeCapacity.Electricity.Biomass))
#
#
## Coal -
#png(file=paste0(t_folder,"/coal_capacity.png"),width=600,height=400,res=150,type='cairo')
#ggplot(runs[!is.na(runs$CumulativeCapacity.Electricity.Coal) & !is.na(runs$CumulativeCapacity.Electricity.Coal.wCCS),], 
#       aes(x=Year, group=mod_scen)) +
#  geom_line(aes(color=mod_scen, y=CumulativeCapacity.Electricity.Coal.wCCS), size=1, linetype="dashed") +
#  geom_line(aes(y=CumulativeCapacity.Electricity.Coal, color=mod_scen), size=1, linetype="solid") +
#  labs(x="", y="Cumulative capacity (GW)", title="Abated vs unabated coal", subtitle=paste0(t_lab,", ",reg_lab)) +
#  theme(legend.position = "none") +
#  lims(x=year_range) +
#  scale_color_viridis(discrete=T)
#dev.off()
#
## Gas -
#png(file=paste0(t_folder,"/gas_capacity.png"),width=600,height=500,res=150,type='cairo')
#ggplot(runs[!is.na(runs$CumulativeCapacity.Electricity.Gas) & !is.na(runs$CumulativeCapacity.Electricity.Gas.wCCS),], 
#       aes(x=Year, group=mod_scen)) +
#  geom_line(aes(color=mod_scen, y=CumulativeCapacity.Electricity.Gas.wCCS), size=1, linetype="dashed") +
#  geom_line(aes(y=CumulativeCapacity.Electricity.Gas, color=mod_scen), size=1, linetype="solid") +
#  labs(x="", y="Cumulative capacity (GW)", title="Abated vs unabated gas", subtitle=paste0(t_lab,", ",reg_lab), color="") +
#  theme(legend.position = "bottom",
#        legend.direction = "vertical",
#        text=element_text(size=8)) +
#  lims(x=year_range) +
#  scale_color_viridis(discrete=T, guide = guide_legend(override.aes = list(linetype="solid"))) 
#  #scale_color_manual(guide = guide_legend(override.aes = list(linetype="solid")))
#dev.off()
#
## Biomass -
#png(file=paste0(t_folder,"/biomass_capacity.png"),width=600,height=500,res=150,type='cairo')
#ggplot(runs[!is.na(runs$CumulativeCapacity.Electricity.Biomass) & !is.na(runs$CumulativeCapacity.Electricity.Biomass.wCCS),], 
#       aes(x=Year, group=mod_scen)) +
#  geom_line(aes(color=mod_scen, y=CumulativeCapacity.Electricity.Biomass.wCCS), size=1, linetype="dashed") +
#  geom_line(aes(y=CumulativeCapacity.Electricity.Biomass, color=mod_scen), size=1, linetype="solid") +
#  labs(x="", y="Cumulative capacity (GW)", title="Abated vs unabated Biomass", subtitle=paste0(t_lab,", ",reg_lab), color="") +
#  theme(legend.position = "bottom",
#        legend.direction = "vertical",
#        text=element_text(size=8)) +
#  #theme(legend.position = "none") +
#  lims(x=year_range) +
#  scale_color_viridis(discrete=T, guide = guide_legend(override.aes = list(linetype="solid"))) 
##scale_color_manual(guide = guide_legend(override.aes = list(linetype="solid")))
#dev.off()


# ---- CCS breakdown ----
#table(runs$mod_scen, useNA="always", is.na(runs$CarbonSequestration.CCS.Biomass))
# All 1.5 scenarios have it
#table(runs$mod_scen, useNA="always", is.na(runs$CarbonSequestration.CCS.Fossil))
# 4/6 1.5 scenarios have it
#table(runs$mod_scen, useNA="always", is.na(runs$CarbonSequestration.LandUse))
# 5/6 1.5 scenarios have it
#table(runs$mod_scen, useNA="always", is.na(runs$SecondaryEnergy.Electricity.Coal.wCCS))
# All 1.5
#table(runs$mod_scen, useNA="always", is.na(runs$SecondaryEnergy.Electricity.Gas.wCCS))
# All 1.5
#table(runs$mod_scen, useNA="always", is.na(runs$SecondaryEnergy.Electricity.Biomass.wCCS))
# All 1.5

coal_lims <- c(0, 1.1*max(runs$SecondaryEnergy.Electricity.Coal, na.rm=T))
gas_lims <- c(0, 1.1*max(runs$SecondaryEnergy.Electricity.Gas, na.rm=T))
bio_lims <- c(0, 1.1*max(runs$SecondaryEnergy.Electricity.Biomass, na.rm=T))

coal_all_p <- ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Coal),], 
                        aes(x=Year, y=SecondaryEnergy.Electricity.Coal, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="all Coal (EJ)", title="") +
  theme(legend.position = "none", 
        text = element_text(size=8)) +
  lims(x=year_range, y=coal_lims) +
  scale_color_viridis(discrete=T)

coal_no_ccs_p <- ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Coal.woCCS),], 
                        aes(x=Year, y=SecondaryEnergy.Electricity.Coal.woCCS, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Coal without CCS (EJ)", title="") +
  theme(legend.position = "none", 
        text = element_text(size=8)) +
  lims(x=year_range, y=coal_lims) +
  scale_color_viridis(discrete=T)

coal_ccs_p <- ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Coal.wCCS),], 
                     aes(x=Year, y=SecondaryEnergy.Electricity.Coal.wCCS, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Coal with CCS (EJ)", title="") +
  theme(legend.position = "none", 
        text = element_text(size=8)) +
  lims(x=year_range, y=coal_lims) +
  scale_color_viridis(discrete=T)

gas_all_p <- ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Gas),], 
                       aes(x=Year, y=SecondaryEnergy.Electricity.Gas, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="All gas (EJ)", title="") +
  theme(legend.position = "none",
        text = element_text(size=8)) +
  lims(x=year_range, y=gas_lims) +
  scale_color_viridis(discrete=T)

gas_no_ccs_p <- ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Gas.woCCS),], 
                       aes(x=Year, y=SecondaryEnergy.Electricity.Gas.woCCS, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Gas without CCS (EJ)", title="") +
  theme(legend.position = "none",
        text = element_text(size=8)) +
  lims(x=year_range, y=gas_lims) +
  scale_color_viridis(discrete=T)

gas_ccs_p <- ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Gas.wCCS),], 
                    aes(x=Year, y=SecondaryEnergy.Electricity.Gas.wCCS, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Gas with CCS (EJ)", title="") +
  theme(legend.position = "none",
        text = element_text(size=8)) +
  lims(x=year_range, y=gas_lims) +
  scale_color_viridis(discrete=T)

bio_all_p <- ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Biomass),], 
                       aes(x=Year, y=SecondaryEnergy.Electricity.Biomass, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="All biomass (EJ)", title="") +
  theme(legend.position = "none",
        text = element_text(size=8)) +
  lims(x=year_range, y=bio_lims) +
  scale_color_viridis(discrete=T)

bio_no_ccs_p <- ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Biomass.woCCS),], 
                       aes(x=Year, y=SecondaryEnergy.Electricity.Biomass.woCCS, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Biomass without CCS (EJ)", title="") +
  theme(legend.position = "none",
        text = element_text(size=8)) +
  lims(x=year_range, y=bio_lims) +
  scale_color_viridis(discrete=T)

bio_ccs_p <- ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Biomass.wCCS),], 
                    aes(x=Year, y=SecondaryEnergy.Electricity.Biomass.wCCS, group=mod_scen)) +
  geom_line(aes(color=mod_scen), size=1) +
  labs(x="", y="Biomass with CCS (EJ)", title="") +
  theme(legend.position = "none",
        text = element_text(size=8)) +
  lims(x=year_range, y=bio_lims) +
  scale_color_viridis(discrete=T)

png(file=paste0("plots/ccs_breakdown_",str_replace(t_lab,"<",""),"_",reg_lab,".png"),width=800,height=800,res=150,type='cairo')
plot_grid(coal_all_p, coal_no_ccs_p, coal_ccs_p, gas_all_p, gas_no_ccs_p, gas_ccs_p, bio_all_p, bio_no_ccs_p, bio_ccs_p, nrow=3, labels=c(paste0(t_lab,", ",reg_lab)))
dev.off()


# ==== Summarising scenarios ====
test_yr <- 2050
table(runs$mod_scen[runs$Year==test_yr], useNA="always", is.na(runs$FinalEnergy[runs$Year==test_yr]))
# all 1.5C scenarios at World level have 2020, 2030, and 2050!

coal_sum <- runs %>%
  filter(Year %in% c(2020, 2030, 2040, 2050)) %>%
  group_by(Year) %>%
  summarise(med = median(SecondaryEnergy.Electricity.Coal),
            q25 = quantile(SecondaryEnergy.Electricity.Coal, 0.25),
            q75 = quantile(SecondaryEnergy.Electricity.Coal, 0.75)) %>%
  ungroup() %>%
  mutate(Type="Coal_scen")

coal_woCCS_sum <- runs %>%
  filter(Year %in% c(2020, 2030, 2040, 2050)) %>%
  group_by(Year) %>%
  summarise(med = median(SecondaryEnergy.Electricity.Coal.woCCS),
            q25 = quantile(SecondaryEnergy.Electricity.Coal.woCCS, 0.25),
            q75 = quantile(SecondaryEnergy.Electricity.Coal.woCCS, 0.75)) %>%
  ungroup() %>%
  mutate(Type="Coal_woCCS_scen")

gas_woCCS_sum <- runs %>%
  filter(Year %in% c(2020, 2030, 2040, 2050)) %>%
  group_by(Year) %>%
  summarise(med = median(SecondaryEnergy.Electricity.Gas.woCCS),
            q25 = quantile(SecondaryEnergy.Electricity.Gas.woCCS, 0.25),
            q75 = quantile(SecondaryEnergy.Electricity.Gas.woCCS, 0.75)
  ) %>%
  ungroup() %>%
  mutate(Type="Gas_woCCS_scen")

renew_sum <- runs %>%
  filter(Year %in% c(2020, 2030, 2040, 2050)) %>%
  group_by(Year) %>%
  summarise(med = median(SecondaryEnergy.Electricity.Renewables),
            q25 = quantile(SecondaryEnergy.Electricity.Renewables, 0.25),
            q75 = quantile(SecondaryEnergy.Electricity.Renewables, 0.75)) %>%
  ungroup() %>%
  mutate(Type="Renew_scen")

nuc_sum <- runs %>%
  filter(Year %in% c(2020, 2030, 2040, 2050)) %>%
  group_by(Year) %>%
  summarise(med = median(SecondaryEnergy.Electricity.Nuclear),
            q25 = quantile(SecondaryEnergy.Electricity.Nuclear, 0.25),
            q75 = quantile(SecondaryEnergy.Electricity.Nuclear, 0.75)) %>%
  ungroup() %>%
  mutate(Type="Nuclear_scen")

runs_summary <- bind_rows(coal_sum, coal_woCCS_sum, gas_woCCS_sum, renew_sum, nuc_sum)

if (reg_lab == "World") {
  ger_sum <- ger %>%
    group_by(Type2, Year) %>%
    summarise(Value_TWh = sum(Value_TWh)) %>%
    ungroup()
} else {
  ger_sum <- ger %>%
    filter(Region_ipcc %in% reg_lab) %>%
    group_by(Type2, Year) %>%
    summarise(Value_TWh = sum(Value_TWh)) %>%
    ungroup()
}

# Function to plot different summarised technologies
# fuel_type
med_plot <- function(fuel_type, yr_max, ttl) {
  # scenario match
  if (fuel_type %in% "Coal") {
    scen_dat <- filter(runs, !is.na(SecondaryEnergy.Electricity.Coal), Year<=yr_max) %>% rename(Value = SecondaryEnergy.Electricity.Coal)
  }
  else if (fuel_type %in% "Coal_woCCS") {
    scen_dat <- filter(runs, !is.na(SecondaryEnergy.Electricity.Coal.woCCS), Year<=yr_max) %>% rename(Value = SecondaryEnergy.Electricity.Coal.woCCS)
  }
  else if (fuel_type %in% "Gas_woCCS") {
    scen_dat <- filter(runs, !is.na(SecondaryEnergy.Electricity.Gas.woCCS), Year<=yr_max) %>% rename(Value = SecondaryEnergy.Electricity.Gas.woCCS)
  }
  else if (fuel_type %in% "Renew") {
    scen_dat <- filter(runs, !is.na(SecondaryEnergy.Electricity.Renewables), Year<=yr_max) %>% rename(Value = SecondaryEnergy.Electricity.Renewables)
  }
  else if (fuel_type %in% "Nuclear") {
    scen_dat <- filter(runs, !is.na(SecondaryEnergy.Electricity.Nuclear), Year<=yr_max) %>% rename(Value = SecondaryEnergy.Electricity.Nuclear)
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
  else if (fuel_type %in% c("Nuclear")) {
    ger_ft <- "Nuclear"
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
                     "Renew_scen"="grey20",
                     "Nuclear_scen"="grey20",
                     "Nuclear"="darkorange")
  med_fuel_labs <- c("Renewables"="Historic", 
                     "Renew_scen"="Scenarios",
                     "Coal"="Historic",
                     "Gas"="Historic",
                     "Nuclear"="Historic",
                     "Coal_scen"="Scenarios",
                     "Coal_woCCS_scen"="Scenarios",
                     "Gas_woCCS_scen"="Scenarios",
                     "Gas_scen"="Scenarios",
                     "Nuclear_scen"="Scenarios")
  
  ggplot(filter(runs_summary, Type %in% paste0(fuel_type,"_scen")), aes(x=Year, y=med, colour=Type)) +#, group=Type, colour=Type)) +
    geom_errorbar(aes(ymin=q25, ymax=q75), width=1, size=1, colour="grey20") +
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
         subtitle=paste0(t_lab,", ",reg_lab),
         colour="") +
    scale_x_continuous(breaks=c(2010, 2020, 2030, 2040, 2050), limits = c(2010, yr_max+2)) +
    scale_color_manual(values=med_fuel_cols, labels=med_fuel_labs)
}

png(file=paste0("plots/all_coal_sum_",str_replace(t_lab,"<",""),"_",reg_lab,".png"),width=900,height=600,res=150,type='cairo')
med_plot("Coal", 2050, "all Coal")
dev.off()

png(file=paste0("plots/coal_woCCS_sum_",str_replace(t_lab,"<",""),"_",reg_lab,".png"),width=900,height=600,res=150,type='cairo')
med_plot("Coal_woCCS", 2050, "unabated Coal")
dev.off()

png(file=paste0("plots/gas_woCCS_sum_",str_replace(t_lab,"<",""),"_",reg_lab,".png"),width=900,height=600,res=150,type='cairo')
med_plot("Gas_woCCS", 2050, "unabated Gas")
dev.off()

png(file=paste0("plots/renew_sum_",str_replace(t_lab,"<",""),"_",reg_lab,".png"),width=900,height=600,res=150,type='cairo')
med_plot("Renew", 2050, "Solar + Wind + Hydro")
dev.off()

png(file=paste0("plots/nuclear_sum_",str_replace(t_lab,"<",""),"_",reg_lab,".png"),width=900,height=600,res=150,type='cairo')
med_plot("Nuclear", 2050, "Nuclear")
dev.off()

# ---- Evolution charts ----

runs_anim <- filter(runs, Year %in% c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)) %>%
  left_join(meta[c("mod_scen", "category")], by="mod_scen")

# Line plot of nuclear vs renewables
runs_anim %>% select(SecondaryEnergy.Electricity.Nuclear, SecondaryEnergy.Electricity.Renewables, Year, SecondaryEnergy.Electricity, mod_scen, category) %>%
  ggplot(aes(x=100*SecondaryEnergy.Electricity.Nuclear/SecondaryEnergy.Electricity, y=100*SecondaryEnergy.Electricity.Renewables/SecondaryEnergy.Electricity, group=mod_scen)) +
  #geom_line(size=1, colour="grey20") +
  geom_point(aes(colour=category), size=3, alpha=0.7) +
  labs(title = 'Year: {frame_time}', x = '% Nuclear', y = '% Renewables', colour='Warming category') +
  scale_colour_manual(values=temp_cols) +
  transition_time(Year) +
  ease_aes('linear')
anim_save("plots/evolution_nuc-vs-renew.gif")


# Line plot of gas vs renewables
runs_anim %>% select(SecondaryEnergy.Electricity.Gas, SecondaryEnergy.Electricity.Renewables, Year, SecondaryEnergy.Electricity, mod_scen, category) %>%
  ggplot(aes(x=100*SecondaryEnergy.Electricity.Gas/SecondaryEnergy.Electricity, y=100*SecondaryEnergy.Electricity.Renewables/SecondaryEnergy.Electricity, group=mod_scen)) +
  #geom_line(size=1, colour="grey20") +
  geom_point(aes(colour=category), size=3, alpha=0.7) +
  labs(title = 'Year: {frame_time}', x = '% Gas', y = '% Renewables', colour='Warming category') +
  scale_colour_manual(values=temp_cols) +
  transition_time(Year) +
  ease_aes('linear')
anim_save("plots/evolution_gas-vs-renew.gif")
# ==== WRITE RESULTS ====

write.csv(runs_summary, file=paste0("data/runs_summary_",reg_lab,"_",str_replace(t_lab,"<",""),".csv"), row.names=F)

