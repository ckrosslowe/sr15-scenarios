# script to read different samples of ipcc scenarios and summarise power info
library(tidyverse)
library(readxl)
library(viridis)
library(cowplot)
#library(plotly)
library(gganimate)

theme_set(
  theme_minimal() +
    theme(legend.position = "right",
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(), 
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))
    )
)
# New comment
# temp category colour scheme
temp_cols <- c("Below 1.5C"="steelblue1",
               "1.5C low overshoot"="seagreen1",
               "1.5C high overshoot"="gold",
               "Lower 2C"="darkorange",
               "Higher 2C"="indianred2")

fuel_cols <- c("Wind"="#FF624E", "#E41A1C", # Red - Wind
               "Gas"="#3A91D6", #377EB8", # Blue - coal
               "Biomass"="#60BC5D", #4CAB49  Green - Biomass & waste
               "Nuclear"="#AA47B9", #984EA3 # Purple - Nuclear
               #""="#FF7C00", # Orange
               "Solar"="#FFC300", "#FFD700", # Gold - Solar
               "Other fossil"="#B76022", #8B4513 # Brown - Other fossil
               "Other renewable"="#F98BC5", # Pink - Other renewables
               "Hydro"="#6CE3E5", #00CED1  # Dark turquoise - Hydro
               "Coal"="#2F4F4F",
               "Other"="#77bbd1")

# Variables explained here https://data.ene.iiasa.ac.at/iamc-1.5c-explorer/#/docs

# --- Region - key to set reg_lab value
regions <- tibble(region=c("R5OECD90+EU","R5MAF","R5ASIA","R5LAM","R5REF","R5ROWO","World"),
                  reg_lab=c("OECD90_EU","MAF","ASIA","LAM","REF","ROW","World"))

ms_exclude <- c("MESSAGEix-GLOBIOM 1.0 | LowEnergyDemand",
  "POLES EMF33 | EMF33_1.5C_limbio",
  "POLES EMF33 | EMF33_tax_hi_none")

# Create run filtering function
source("filter_runs.R")
source("filter_runs_avg.R")
source("filter_runs_comp.R")

# ==== READ data ====
# Read metadata
meta <- read_excel("data/sr15_metadata_indicators_r2.0.xlsx", sheet="meta") %>%
  mutate(mod_scen = str_c(model, scenario, sep=" | "))

# Read model data
runs_clean <- read.csv("data/runs_clean.csv", header=T, stringsAsFactors = F) %>%
  #left_join(meta[c("mod_scen", "category")], by="mod_scen") %>%
  mutate(cat2 = ifelse(category %in% c("1.5C low overshoot", "Below 1.5C"), "<1.5C",
                       ifelse(category %in% c("1.5C high overshoot", "Lower 2C"), "1.5-2C", category)),
         category = factor(category, ordered=T, levels=c("Below 1.5C", "1.5C low overshoot", "1.5C high overshoot", "Lower 2C", "Higher 2C", "Above 2C", "no-climate-assessment")))


# Read actual generation data from GER (with IPCC region variable)
ger <- read.csv("data/ger_ipcc.csv", header = T, stringsAsFactors = F) %>%
  select(-Source)

ger_sum_regions <- ger %>%
    group_by(Region_ipcc, Type2, Year) %>%
    summarise(Value_TWh = sum(Value_TWh)) %>%
    ungroup()
ger_sum <- ger %>%
  group_by(Type2, Year) %>%
  summarise(Value_TWh = sum(Value_TWh)) %>%
  mutate(Region_ipcc="World") %>%
  ungroup() %>%
  bind_rows(ger_sum_regions)
  #mutate(Type_ipcc = ifelse(Type2))

ger_pc_regions <- ger %>% filter(Year > 2010) %>%
  group_by(Region_ipcc, Type2, Year) %>%
  summarise(Production = sum(Value_TWh, na.rm=T)) %>%
  #ungroup() %>%
  pivot_wider(id_cols=c("Year", "Production", "Region_ipcc", "Type2"), names_from=Type2, values_from=Production, names_repair="unique") %>%
  mutate(Total=Coal+Gas+Nuclear+Solar+Wind+Hydro+Other) %>%
  mutate(pc.Coal=100*Coal/Total,
         pc.Gas=100*Gas/Total,
         pc.Solar=100*Solar/Total,
         pc.Wind=100*Wind/Total,
         pc.Hydro=100*Hydro/Total,
         pc.Nuclear=100*Nuclear/Total)

ger_pc_sum <- ger %>% filter(Year > 2010) %>%
  group_by(Type2, Year) %>%
  summarise(Production = sum(Value_TWh, na.rm=T)) %>%
  #ungroup() %>%
  pivot_wider(id_cols=c("Year", "Production", "Type2"), names_from=Type2, values_from=Production, names_repair="unique") %>%
  mutate(Total=Coal+Gas+Nuclear+Solar+Wind+Hydro+Other,
         Region_ipcc = "World") %>%
  mutate(pc.Coal=100*Coal/Total,
         pc.Gas=100*Gas/Total,
         pc.Solar=100*Solar/Total,
         pc.Wind=100*Wind/Total,
         pc.Hydro=100*Hydro/Total,
         pc.Nuclear=100*Nuclear/Total) %>%
         bind_rows(ger_pc_regions)

# ==== Test variable assumptions ====

runs_test <- runs_clean %>%
  filter(Region %in% "World",
         Year %in% 2030) %>%
  select(Emissions.CO2, 
         Emissions.CO2.AFOLU, 
         Emissions.CO2.Energy, 
         Emissions.CO2.IndustrialProcesses, 
         Emissions.CO2.Other,
         Emissions.CO2.Energy.Supply.Electricity, 
         Emissions.CO2.Energy.Supply.Heat, 
         Emissions.CO2.Energy.Supply.Liquids, 
         Emissions.CO2.Energy.Supply.Gases, 
         Emissions.CO2.Energy.Supply.Solids, 
         Emissions.CO2.Energy.Supply.OtherSector,
         Emissions.CO2.Energy.Supply) %>%
  replace(is.na(.), 0) %>%
  mutate(Total_CO2 = Emissions.CO2.AFOLU + Emissions.CO2.Energy + Emissions.CO2.IndustrialProcesses + Emissions.CO2.Other,
         Supply_CO2 = Emissions.CO2.Energy.Supply.Electricity + Emissions.CO2.Energy.Supply.Heat + Emissions.CO2.Energy.Supply.Liquids + Emissions.CO2.Energy.Supply.Gases + Emissions.CO2.Energy.Supply.Solids + Emissions.CO2.Energy.Supply.OtherSector) %>%
  mutate(CO2_frac = 100*Total_CO2/Emissions.CO2,
         CO2_diff = Emissions.CO2 - Total_CO2,
         CO2_esup_frac = 100*Supply_CO2/Emissions.CO2.Energy.Supply,
         CO2_esup_diff = Emissions.CO2.Energy.Supply - Supply_CO2)
hist(runs_test$CO2_diff, 30)
hist(runs_test$CO2_frac[runs_test$CO2_frac < 200 & runs_test$CO2_frac > 0], breaks=seq(0,200,5))
hist(runs_test$CO2_esup_diff, 30, main="Difference between Em.CO2.En.Supply and sum of sub-variables")
hist(runs_test$CO2_esup_frac[runs_test$CO2_esup_frac < 200 & runs_test$CO2_esup_frac > 0], breaks=seq(0,200,5), main="% difference between Em.CO2.En.Supply and sum of sub-vars", xlab="")
hist(100*runs_test$Emissions.CO2.Energy.Supply.Electricity[runs_test$Emissions.CO2.Energy.Supply.Electricity>0]/runs_test$Emissions.CO2[runs_test$Emissions.CO2.Energy.Supply.Electricity>0],
     main="Electricity emissions as % of total emissions")

sum(abs(runs_test$CO2_diff)<50, na.rm=T)
sum(runs_test$CO2_frac < 0, na.rm=T)
sum(runs_test$CO2_frac > 200, na.rm=T)
sum(runs_test$CO2_frac > 95 & runs_test$CO2_frac < 105, na.rm=T)

# ==== Filter scenarios ====

# --- Temperature
#t_lab <- "<1.5C"
t_lab <- "<2C"
#t_lab <- "1.5-2C"

if (t_lab %in% "<1.5C")  temp_cats <- c("1.5C low overshoot", "Below 1.5C")
if (t_lab %in% "<2C")    temp_cats <- c("1.5C low overshoot", "Below 1.5C", "1.5C high overshoot", "Lower 2C")
#if (t_lab %in% "1.5-2C") temp_cats <- c("1.5C high overshoot", "Lower 2C", "Higher 2C")
if (t_lab %in% "1.5-2C") temp_cats <- c("1.5C high overshoot", "Lower 2C")

#reg <- "World"
#reg <- "R5OECD90+EU"
#reg <- "R5MAF"
#reg <- "R5ASIA"
#reg <- "R5LAM"
reg <- "R5REF"
#reg <- "R5ROWO"

reg_lab <- regions$reg_lab[regions$region %in% reg]

limits <- c("bio"=100,
            "beccs"=5000,
            "af"=3600)

runs <- runs_clean %>% filter_runs(temp_cats=temp_cats, reg=reg, limits=limits, ms_exclude=ms_exclude)
#runs <- runs_clean %>% filter_runs_avg(temp_cats=temp_cats, reg=reg, limits=limits, ms_exclude=ms_exclude)
n_distinct(runs$mod_scen)
unique(runs$mod_scen)

# Alternative dataset with all regions included ('comp' = complete)
runs_comp <- runs_clean %>% filter_runs_comp(temp_cats=temp_cats, limits=limits, ms_exclude=ms_exclude)
n_distinct(runs_comp$mod_scen)
unique(runs_comp$mod_scen)

######### STATS ############

# ---- Summarise runs by technology ----
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
            #q95 = quantile(SecondaryEnergy.Electricity.Coal.woCCS, 0.95)) %>%
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

# ---- Median elec shares by year ----
med_share <- runs %>% select(Year, pc.Renewables, pc.Fossil) %>%
  filter(Year %in% c(2020, 2030, 2050, 2100)) %>%
  group_by(Year) %>%
  summarise(Share_renew = median(pc.Renewables, na.rm=T),
            q25_renew = quantile(pc.Renewables, 0.25),
            q75_renew = quantile(pc.Renewables, 0.75),
            Share_fossil = median(pc.Fossil, na.rm=T),
            q25_fossil = quantile(pc.Fossil, 0.25),
            q75_fossil = quantile(pc.Fossil, 0.75)
            )
med_share

# ---- Time granularity of selected runs ----
table(runs$mod_scen[!is.na(runs$FinalEnergy)], runs$Year[!is.na(runs$FinalEnergy)])
table(runs$Year[!is.na(runs$FinalEnergy)])

# ---- Regional breakdown of fuel use in 2019 ----
ger_pc_regions[ger_pc_regions$Year==2019, c("Region_ipcc", "Biomass", "Coal", "Gas", "Renewables")]

# ---- Median gas generation by region by year ----
reg_med_gas_woccs <- runs_comp %>% filter(Year %in% c(2020, 2030, 2040, 2050)) %>% 
  group_by(Region, Year) %>%
  summarise(med = median(SecondaryEnergy.Electricity.Gas.woCCS))


######### ONE SAMPLE PLOTS ############

year_range <- c(2020, 2100)

# ---- Electricity Variables REFERENCE: ----
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
png(file=paste0("plots/elec_demand_",str_replace(t_lab,"<",""),"_",reg_lab,".png"),width=1200,height=800,res=220,type='cairo')
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
png(file=paste0("plots/electrification_2020-2030_",str_replace(t_lab,"<",""),"_",reg_lab,".png"),width=1200,height=800,res=220,type='cairo')
ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity) & !is.na(runs$FinalEnergy),], 
       aes(x=Year, y=100*SecondaryEnergy.Electricity/FinalEnergy, group=mod_scen)) +
  geom_line(aes(color=cat2), size=1) +
  labs(x="", y="Electricity % of final energy", title="Electrification", subtitle=paste0(t_lab, ", ", reg_lab)) +
  theme(legend.position = "none") +
  lims(y=c(0,40), x=year_range) +
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

# Set some limits here
coal_lims <- c(0, max(runs$SecondaryEnergy.Electricity.Coal, na.rm=T))
gas_lims <- c(0, max(runs$SecondaryEnergy.Electricity.Gas, na.rm=T))
bio_lims <- c(0, max(runs$SecondaryEnergy.Electricity.Biomass, na.rm=T))
sz <- 0.5

#ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Coal) & !is.na(runs$SecondaryEnergy.Electricity.Gas) & !is.na(runs$SecondaryEnergy.Electricity.Nuclear) & !is.na(runs$SecondaryEnergy.Electricity.Biomass) & !is.na(runs$SecondaryEnergy.Electricity.Hydro) & !is.na(runs$SecondaryEnergy.Electricity.Solar) & !is.na(runs$SecondaryEnergy.Electricity.Wind) & !is.na(runs$SecondaryEnergy.Electricity.Geothermal), ], 
coal_woCCS_p <- ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Coal.woCCS), ],
                 aes(x=Year, y=SecondaryEnergy.Electricity.Coal.woCCS, group=mod_scen)) + 
  geom_line(aes(color=mod_scen), size=sz, alpha=0.7) +
  labs(x="", y="Unabated coal (EJ)", title="") +
  theme(legend.position = "none") +
  lims(x=year_range, y=coal_lims) +
  scale_color_viridis(discrete=T)

coal_wCCS_p <- ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Coal.wCCS), ],
                       aes(x=Year, y=SecondaryEnergy.Electricity.Coal.wCCS, group=mod_scen)) + 
  geom_line(aes(color=mod_scen), size=sz, alpha=0.7) +
  labs(x="", y="Coal + CCS (EJ)", title="") +
  theme(legend.position = "none") +
  lims(x=year_range, y=coal_lims) +
  scale_color_viridis(discrete=T)

gas_woCCS_p <- ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Gas.woCCS), ],
                aes(x=Year, y=SecondaryEnergy.Electricity.Gas.woCCS, group=mod_scen)) + 
  geom_line(aes(color=mod_scen), size=sz, alpha=0.7) +
  labs(x="", y="Unabated gas (EJ)", title="") +
  theme(legend.position = "none") +
  lims(x=year_range, y=gas_lims) +
  scale_color_viridis(discrete=T)

gas_wCCS_p <- ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Gas.wCCS), ],
                aes(x=Year, y=SecondaryEnergy.Electricity.Gas.wCCS, group=mod_scen)) + 
  geom_line(aes(color=mod_scen), size=sz, alpha=0.7) +
  labs(x="", y="Gas + CCS (EJ)", title="") +
  theme(legend.position = "none") +
  lims(x=year_range, y=gas_lims) +
  scale_color_viridis(discrete=T)

nuc_p <- ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Nuclear), ],
                aes(x=Year, y=SecondaryEnergy.Electricity.Nuclear, group=mod_scen)) + 
  geom_line(aes(color=mod_scen), size=sz, alpha=0.7) +
  labs(x="", y="Nuclear (EJ)", title="") +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)

bio_woCCS_p <- ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Biomass.woCCS), ],
                aes(x=Year, y=SecondaryEnergy.Electricity.Biomass.woCCS, group=mod_scen)) + 
  geom_line(aes(color=mod_scen), size=sz, alpha=0.7) +
  labs(x="", y="Unabated Biomass (EJ)", title="") +
  theme(legend.position = "none") +
  lims(x=year_range, y=bio_lims) +
  scale_color_viridis(discrete=T)

bio_wCCS_p <- ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Biomass.wCCS), ],
                      aes(x=Year, y=SecondaryEnergy.Electricity.Biomass.wCCS, group=mod_scen)) + 
  geom_line(aes(color=mod_scen), size=sz, alpha=0.7) +
  labs(x="", y="BECCS (EJ)", title="") +
  theme(legend.position = "none") +
  lims(x=year_range, y=bio_lims) +
  scale_color_viridis(discrete=T)

renew_p <- ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Renewables), ],
                  aes(x=Year, 
                      y=SecondaryEnergy.Electricity.Renewables, 
                      group=mod_scen)) + 
  geom_line(aes(color=mod_scen), size=sz, alpha=0.7) +
  labs(x="", y="Renewables (EJ)", title="") +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)

# All CCS = Coal, Gas, Oil, Biomass
#ccs_p <- ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Coal.wCCS) & !is.na(runs$SecondaryEnergy.Electricity.Gas.wCCS) & !is.na(runs$SecondaryEnergy.Electricity.Biomass.wCCS) & !is.na(runs$SecondaryEnergy.Electricity.Oil.wCCS), ],
#                aes(x=Year, 
#                    y=(SecondaryEnergy.Electricity.Coal.wCCS+SecondaryEnergy.Electricity.Gas.wCCS+SecondaryEnergy.Electricity.Biomass.wCCS+SecondaryEnergy.Electricity.Oil.wCCS), 
#                    group=mod_scen)) + 
#  geom_line(aes(color=mod_scen), size=1) +
#  labs(x="", y="All CCS (EJ)", title="") +
#  theme(legend.position = "none") +
#  lims(x=year_range) +
#  scale_color_viridis(discrete=T)

png(file=paste0("plots/elec_mix_",str_replace(t_lab,"<",""),"_",reg_lab,".png"),width=700,height=1000,res=150,type='cairo')
plot_grid(coal_woCCS_p, coal_wCCS_p, gas_woCCS_p, gas_wCCS_p, bio_woCCS_p, bio_wCCS_p, renew_p, nuc_p, nrow=4, labels=c(paste0(t_lab, ", ", reg_lab)))
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


# ---- Warming categories: distribution ----

png(file=paste0("plots/category_dist_",str_replace(t_lab,"<",""),"_",reg_lab,".png"),width=1200,height=800,res=180,type='cairo')
runs %>% group_by(category) %>%
  summarise(Count = n_distinct(mod_scen)) %>%
  ggplot(aes(x=category, y=Count)) +
  geom_bar(stat='identity', fill="deepskyblue") +
  labs(title="Distribution of selected runs over warming categories", x="") +
  geom_text(aes(x=category, y=Count-1, label=Count), size=4)
dev.off()

# ---- Fossil vs Fossil-free ONE YEAR ----
yr <- 2030

png(file=paste0("plots/fossil_vs_fossilFree_",str_replace(t_lab,"<",""),"_",reg_lab,"_",yr,".png"),width=1200,height=800,res=180,type='cairo')
runs %>% filter(Year==yr) %>%
  mutate(pc.FossilFree = pc.Renewables+pc.Nuclear+pc.Biomass) %>%
  select(pc.Fossil, pc.FossilFree) %>%
  pivot_longer(pc.Fossil:pc.FossilFree,
               names_to="Fuel",
               values_to="Gen_pc",
               names_prefix="pc.") %>%
  mutate(Fuel=ifelse(Fuel %in% "FossilFree", "Fossil free", Fuel)) %>%
  ggplot(aes(x=Gen_pc)) +
  geom_histogram(aes(fill=Fuel), bins=20) +
  facet_wrap(vars(Fuel)) +
  labs(x="Share of electricity production (%)", y="") +
  scale_fill_manual(values=c("Fossil"="grey30", "Fossil free"="deepskyblue")) +
  guides(fill=F) +
  theme(text=element_text(size=9),
        strip.text = element_text(size=9))
dev.off()

# ---- Share of electricity mix INDIVIDUAL MODELS ----
yr <- 2030
png(file=paste0("plots/elec_share_by_model_",str_replace(t_lab,"<",""),"_",reg_lab,"_",yr,".png"),width=1200,height=1000,res=200,type='cairo')
runs %>% filter(Year %in% yr) %>%
  select(mod_scen, pc.Solar, pc.Hydro, pc.Wind, pc.Coal, pc.Gas, pc.Nuclear, pc.Biomass, pc.OtherRenew, pc.OtherFossil) %>%
  pivot_longer(pc.Solar:pc.OtherFossil,
               names_to="Fuel",
               values_to="Gen_pc",
               names_prefix="pc.") %>%
  mutate(Fuel = ifelse(Fuel %in% "OtherRenew", "Other renewable", 
                       ifelse(Fuel %in% "OtherFossil", "Other fossil", Fuel))) %>%
  mutate(Fuel = factor(Fuel, ordered=T, levels=c("Wind", "Solar", "Hydro", "Other renewable", "Nuclear", "Biomass", "Gas", "Coal", "Other fossil"))) %>%
  mutate(mod_scen = str_replace(mod_scen, "\\|", "\n")) %>%
  ggplot(aes(x=mod_scen, y=Gen_pc, fill=Fuel)) +
  geom_bar(position="fill", stat="identity") +
  labs(y="", x="", title="Share of electricity production", 
       subtitle=paste0("Year: ",yr,", Region: ",reg_lab,", ",t_lab)) +
  scale_fill_manual(values=fuel_cols) +
  scale_y_continuous(breaks=c(0.2, 0.4, 0.6, 0.8, 1.0), labels=c("20%", "40%", "60%", "80%", "100%")) +
  theme(text = element_text(size=9),
        axis.text.x = element_text(angle=55, hjust=1, size=6)
        ) 
dev.off()

# ---- CCS deployment by fuel type INDIVIDUAL MODELS ----
#yr <- 2100
ccs_gen <- function(runs, yr) {
plot <- runs %>% filter(Year %in% yr) %>%
  select(mod_scen, SecondaryEnergy.Electricity.Biomass.wCCS, SecondaryEnergy.Electricity.Gas.wCCS, SecondaryEnergy.Electricity.Coal.wCCS) %>%
  pivot_longer(SecondaryEnergy.Electricity.Biomass.wCCS:SecondaryEnergy.Electricity.Coal.wCCS,
               names_to="Fuel",
               values_to="Gen",
               names_prefix="SecondaryEnergy.Electricity.") %>%
  mutate(Fuel = str_extract(Fuel, "^[A-z]+")) %>%
  mutate(Fuel = factor(Fuel, ordered=T, levels=c("Biomass", "Gas", "Coal"))) %>%
  mutate(mod_scen = str_replace(mod_scen, "\\|", "\n")) %>%
  ggplot(aes(x=mod_scen, y=Gen*278, fill=Fuel)) +
  geom_bar(stat="identity") +
  labs(y="TWh", x="", title="") + 
  lims(y=c(0, 28000)) +
       #subtitle=paste0("Year: ",yr,", Region: ",reg_lab,", ",t_lab)) +
  scale_fill_manual(values=fuel_cols) +
  #scale_y_continuous(breaks=c(0.2, 0.4, 0.6, 0.8, 1.0), labels=c("20%", "40%", "60%", "80%", "100%")) +
  theme(text = element_text(size=9),
        axis.text.x = element_blank(),
  )
return(plot)
}

ccs_gen_2030 <- ccs_gen(runs, 2030)
ccs_gen_2050 <- ccs_gen(runs, 2050)
ccs_gen_2070 <- ccs_gen(runs, 2070)

png(file=paste0("plots/ccs_fuel_model_",str_replace(t_lab,"<",""),"_",reg_lab,".png"),width=1200,height=2000,res=200,type='cairo')
plot_grid(ccs_gen_2030, ccs_gen_2050, ccs_gen_2070, nrow=3, labels=c("2030", "2050", "2070"), label_x=c(0.4, 0.4, 0.4))
dev.off()
# ==== Summarising scenarios ====
test_yr <- 2050
table(runs$mod_scen[runs$Year==test_yr], useNA="always", is.na(runs$FinalEnergy[runs$Year==test_yr]))
# all 1.5C scenarios at World level have 2020, 2030, and 2050!

# ---- TOTAL ELECTRICITY ----

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
    geom_line(data=filter(ger_sum, 
                          Type2 %in% ger_ft,
                          Region_ipcc %in% reg_lab), 
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


# ---- SHARE OF ELECTRICITY ----
coal_pc_sum <- runs %>%
  filter(Year %in% c(2020, 2030, 2040, 2050)) %>%
  group_by(Year) %>%
  summarise(med = median(pc.Coal),
            q25 = quantile(pc.Coal, 0.25),
            q75 = quantile(pc.Coal, 0.75)) %>%
  ungroup() %>%
  mutate(Type="Coal_share")

gas_woCCS_pc_sum <- runs %>%
  filter(Year %in% c(2020, 2030, 2040, 2050)) %>%
  group_by(Year) %>%
  summarise(med = median(pc.Gas.woCCS),
            q25 = quantile(pc.Gas.woCCS, 0.25),
            q75 = quantile(pc.Gas.woCCS, 0.75)) %>%
  ungroup() %>%
  mutate(Type="Gas_woCCS_share")

fossil_pc_sum <- runs %>%
  filter(Year %in% c(2020, 2030, 2040, 2050)) %>%
  group_by(Year) %>%
  summarise(med = median(pc.Fossil),
            q25 = quantile(pc.Fossil, 0.25),
            q75 = quantile(pc.Fossil, 0.75)
  ) %>%
  ungroup() %>%
  mutate(Type="Fossil_share")

renew_pc_sum <- runs %>%
  filter(Year %in% c(2020, 2030, 2040, 2050)) %>%
  group_by(Year) %>%
  summarise(med = median(pc.Renewables),
            q25 = quantile(pc.Renewables, 0.25),
            q75 = quantile(pc.Renewables, 0.75)) %>%
  ungroup() %>%
  mutate(Type="Renewables_share")

nuc_pc_sum <- runs %>%
  filter(Year %in% c(2020, 2030, 2040, 2050)) %>%
  group_by(Year) %>%
  summarise(med = median(pc.Nuclear),
            q25 = quantile(pc.Nuclear, 0.25),
            q75 = quantile(pc.Nuclear, 0.75)) %>%
  ungroup() %>%
  mutate(Type="Nuclear_share")

runs_pc_summary <- bind_rows(coal_pc_sum, gas_woCCS_pc_sum, fossil_pc_sum, renew_pc_sum, nuc_pc_sum)

med_pc_plot <- function(fuel_type, yr_max) {
  
  if (fuel_type %in% "Coal") {
    scen_pc <- runs %>% select(mod_scen, Year, pc.Coal) %>% filter(!is.na(pc.Coal), Year<=yr_max) %>% rename(pc = pc.Coal)
    ger_pc_sum <- ger_pc_sum %>% select(Year, Region_ipcc, pc.Coal) %>% rename(pc = pc.Coal) %>% mutate(col_var="Coal")
  }
  else if (fuel_type %in% "Gas_woCCS") {
    scen_pc <- runs %>% select(mod_scen, Year, pc.Gas.woCCS) %>% filter(!is.na(pc.Gas.woCCS), Year<=yr_max) %>% rename(pc = pc.Gas.woCCS)
    ger_pc_sum <- ger_pc_sum %>% select(Year, Region_ipcc, pc.Gas) %>% rename(pc = pc.Gas) %>% mutate(col_var="Gas")
  }
  else if (fuel_type %in% "Renewables") {
    scen_pc <- runs %>% select(mod_scen, Year, pc.Renewables) %>% filter(!is.na(pc.Renewables), Year<=yr_max) %>% rename(pc = pc.Renewables)
    ger_pc_sum <- ger_pc_sum %>% select(Year, Region_ipcc, pc.Renewables) %>% rename(pc = pc.Renewables) %>% mutate(col_var="Renewables")
  }
  else if (fuel_type %in% "Nuclear") {
    scen_pc <- runs %>% select(mod_scen, Year, pc.Nuclear) %>% filter(!is.na(pc.Nuclear), Year<=yr_max) %>% rename(pc = pc.Nuclear)
    ger_pc_sum <- ger_pc_sum %>% select(Year, Region_ipcc, pc.Nuclear) %>% rename(pc = pc.Nuclear) %>% mutate(col_var="Nuclear")
  }
  else {
    stop("Invalid input to 'fuel_type' variable")
  }
  
  med_pc_fuel_cols <- c("Gas"="blue", 
                     "Coal"="red", 
                     "Gas_woCCS_share"="grey20",
                     "Coal_woCCS_share"="grey20",
                     "Coal_share"="grey20",
                     "Renewables"="purple",
                     "Renewables_share"="grey20",
                     "Nuclear_share"="grey20",
                     "Nuclear"="darkorange")
  med_pc_fuel_labs <- c("Renewables"="Historic", 
                     "Renewables_share"="Scenarios",
                     "Coal"="Historic",
                     "Gas"="Historic",
                     "Nuclear"="Historic",
                     "Coal_share"="Scenarios",
                     "Coal_woCCS_share"="Scenarios",
                     "Gas_woCCS_share"="Scenarios",
                     "Gas_share"="Scenarios",
                     "Nuclear_share"="Scenarios")
  
  ggplot(filter(runs_pc_summary, Type %in% paste0(fuel_type,"_share")), aes(x=Year, y=med, colour=Type)) + #, group=Type, colour=Type)) +
    geom_errorbar(aes(ymin=q25, ymax=q75), width=1, size=1, colour="grey20") +
    geom_line(size=1) +
    geom_point(size=4, colour="grey10") +
    # individual scenarios?
    geom_line(data=scen_pc, 
              aes(x=Year, y=pc, group=mod_scen), 
              size=0.5, alpha=0.3, colour="grey20") +
    geom_line(data=filter(ger_pc_sum, Region_ipcc %in% reg_lab), 
              aes(x=Year, y=pc, colour=col_var), size=2) +
    labs(x="",
         y="% of electricity generation",
         title=paste("Share of electricity from", fuel_type),
         subtitle=paste0(t_lab,", ",reg_lab),
         colour="") +
    scale_x_continuous(breaks=c(2010, 2020, 2030, 2040, 2050), limits = c(2010, yr_max+2)) +
    scale_color_manual(values=med_pc_fuel_cols, labels=med_pc_fuel_labs)
}


png(file=paste0("plots/renewables_sum_pc_",str_replace(t_lab,"<",""),"_",reg_lab,".png"),width=900,height=600,res=150,type='cairo')
med_pc_plot("Renewables", 2050)
dev.off()

png(file=paste0("plots/coal_sum_pc_",str_replace(t_lab,"<",""),"_",reg_lab,".png"),width=900,height=600,res=150,type='cairo')
med_pc_plot("Coal", 2050)
dev.off()

png(file=paste0("plots/gas_woCCS_sum_pc_",str_replace(t_lab,"<",""),"_",reg_lab,".png"),width=900,height=600,res=150,type='cairo')
med_pc_plot("Gas_woCCS", 2050)
dev.off()

png(file=paste0("plots/nuclear_sum_pc_",str_replace(t_lab,"<",""),"_",reg_lab,".png"),width=900,height=600,res=150,type='cairo')
med_pc_plot("Nuclear", 2050)
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
# ---- Sector emissions ----

elec_CO2 <- runs %>%
  filter(Year %in% seq(2020, 2100, 10)) %>%
  group_by(Year) %>%
  summarise(med = median(Emissions.CO2.Energy.Supply.Electricity, na.rm=T),
            q25 = quantile(Emissions.CO2.Energy.Supply.Electricity, 0.25, na.rm=T),
            q75 = quantile(Emissions.CO2.Energy.Supply.Electricity, 0.75, na.rm=T)) %>%
  ungroup() %>%
  mutate(Sector="Electricity")

combust_ind_CO2 <- runs %>%
  filter(Year %in% seq(2020, 2100, 10)) %>%
  group_by(Year) %>%
  summarise(med = median(Emissions.CO2.Energy.Demand.Industry, na.rm=T),
            q25 = quantile(Emissions.CO2.Energy.Demand.Industry, 0.25, na.rm=T),
            q75 = quantile(Emissions.CO2.Energy.Demand.Industry, 0.75, na.rm=T)) %>%
  ungroup() %>%
  mutate(Sector="Combustion - Industry")

building_CO2 <- runs %>%
  filter(Year %in% seq(2020, 2100, 10)) %>%
  group_by(Year) %>%
  summarise(med = median(Emissions.CO2.Energy.Demand.ResidentialandCommercial, na.rm=T),
            q25 = quantile(Emissions.CO2.Energy.Demand.ResidentialandCommercial, 0.25, na.rm=T),
            q75 = quantile(Emissions.CO2.Energy.Demand.ResidentialandCommercial, 0.75, na.rm=T)) %>%
  ungroup() %>%
  mutate(Sector="Buildings")

afofi_CO2 <- runs %>%
  filter(Year %in% seq(2020, 2100, 10)) %>%
  group_by(Year) %>%
  summarise(med = median(Emissions.CO2.Energy.Demand.AFOFI, na.rm=T),
            q25 = quantile(Emissions.CO2.Energy.Demand.AFOFI, 0.25, na.rm=T),
            q75 = quantile(Emissions.CO2.Energy.Demand.AFOFI, 0.75, na.rm=T)) %>%
  ungroup() %>%
  mutate(Sector="Agriculture")

trans_CO2 <- runs %>%
  filter(Year %in% seq(2020, 2100, 10)) %>%
  group_by(Year) %>%
  summarise(med = median(Emissions.CO2.Energy.Demand.Transportation, na.rm=T),
            q25 = quantile(Emissions.CO2.Energy.Demand.Transportation, 0.25, na.rm=T),
            q75 = quantile(Emissions.CO2.Energy.Demand.Transportation, 0.75, na.rm=T)) %>%
  ungroup() %>%
  mutate(Sector="Transport")

afolu_CO2 <- runs %>%
  filter(Year %in% seq(2020, 2100, 10)) %>%
  group_by(Year) %>%
  summarise(med = median(Emissions.CO2.AFOLU, na.rm=T),
            q25 = quantile(Emissions.CO2.AFOLU, 0.25, na.rm=T),
            q75 = quantile(Emissions.CO2.AFOLU, 0.75, na.rm=T)) %>%
  ungroup() %>%
  mutate(Sector="AFOLU")

industry_CO2 <- runs %>%
  filter(Year %in% seq(2020, 2100, 10)) %>%
  group_by(Year) %>%
  summarise(med = median(Emissions.CO2.IndustrialProcesses, na.rm=T),
            q25 = quantile(Emissions.CO2.IndustrialProcesses, 0.25, na.rm=T),
            q75 = quantile(Emissions.CO2.IndustrialProcesses, 0.75, na.rm=T)) %>%
  ungroup() %>%
  mutate(Sector="Industrial Processes")

total_CO2 <- runs %>%
  filter(Year %in% seq(2020, 2100, 10)) %>%
  group_by(Year) %>%
  summarise(med = median(Emissions.CO2, na.rm=T),
            q25 = quantile(Emissions.CO2, 0.25, na.rm=T),
            q75 = quantile(Emissions.CO2, 0.75, na.rm=T)) %>%
  ungroup() %>%
  mutate(Sector="Total")

#mytot_CO2 <- runs %>%
#  filter(Year %in% seq(2020, 2100, 10)) %>%
#  select(Year, 
#         Emissions.CO2.Energy.Supply.Electricity,
#         Emissions.CO2.Energy.Demand.Industry,
#         Emissions.CO2.Energy.Demand.Transportation,
#         Emissions.CO2.AFOLU,
#         Emissions.CO2.IndustrialProcesses) %>%
#  drop_na() %>%
#  mutate(mytot.CO2 = rowSums(.[2:6])) %>%
#  group_by(Year) %>%
#  summarise(med = median(mytot.CO2, na.rm=T),
#            q25 = quantile(mytot.CO2, 0.25, na.rm=T),
#            q75 = quantile(mytot.CO2, 0.75, na.rm=T)) %>%
#  ungroup() %>%
#  mutate(Type="My total")

CO2_sectors <- bind_rows(elec_CO2, combust_ind_CO2, building_CO2, trans_CO2, afolu_CO2, industry_CO2, afofi_CO2)

png(file=paste0("plots/sector_emissions_",str_replace(t_lab,"<",""),"_",reg_lab,".png"),width=1400,height=900,res=200,type='cairo')
ggplot(CO2_sectors, aes(x=Year, y=med, group=Sector)) +
  geom_hline(yintercept=0, size=0.6, colour="grey50") +
  geom_line(aes(colour=Sector), size=0.8) +
  geom_ribbon(aes(ymin=q25, ymax=q75, group=Sector, fill=Sector), alpha=0.15) +
  labs(title="CO2 emissions by sector", 
       subtitle=paste0("Region: ", reg_lab, ", Warming: ", t_lab),
       x="",
       y="MtCO2/yr") +
  coord_cartesian(ylim=c(-6000, 13500))
dev.off()

# ---- Sector emissions reductions ----

# Something here to calculate the % reduction by each sector
# .........................
# Emissions by model
CO2_diff_tot <- runs %>% select(mod_scen, 
                                    Year,
                                    Emissions.CO2) %>%
  filter(Year %in% c(2020, 2030)) %>%
  pivot_wider(names_from = Year, values_from = Emissions.CO2, names_prefix = "y") %>%
  mutate(diff_tot = y2030-y2020)

# Emissions difference by model by sector
CO2_diff <- runs %>% select(mod_scen, 
                Year,
                Emissions.CO2,
                Emissions.CO2.Energy.Supply.Electricity,
                Emissions.CO2.Energy.Demand.Industry,
                Emissions.CO2.Energy.Demand.Transportation,
                Emissions.CO2.AFOLU,
                Emissions.CO2.IndustrialProcesses,
                Emissions.CO2.Energy.Demand.ResidentialandCommercial) %>%
  filter(Year %in% c(2020, 2030)) %>%
  #replace(is.na(.), 0) %>% # replace missing emissions with zero, for sums
  mutate(Emissions.CO2.Counted = rowSums(select(.,Emissions.CO2.Energy.Supply.Electricity:Emissions.CO2.Energy.Demand.ResidentialandCommercial), na.rm=T)) %>%
  mutate(Emissions.CO2.Other = Emissions.CO2 - Emissions.CO2.Counted) %>%
  select(-Emissions.CO2, -Emissions.CO2.Counted) %>%
  rename(Electricity = Emissions.CO2.Energy.Supply.Electricity,
         Industry.Combustion = Emissions.CO2.Energy.Demand.Industry,
         Transport = Emissions.CO2.Energy.Demand.Transportation,
         AFOLU = Emissions.CO2.AFOLU,
         Industry.Process = Emissions.CO2.IndustrialProcesses,
         Buildings = Emissions.CO2.Energy.Demand.ResidentialandCommercial,
         #Agriculture = Emissions.CO2.Energy.Demand.AFOFI,
         Other = Emissions.CO2.Other) %>%
  pivot_longer(Electricity:Other, names_to = "Sector", values_to="Emissions.CO2") %>%
  pivot_wider(names_from = Year, values_from = Emissions.CO2, names_prefix = "y") %>%
  mutate(diff = y2030-y2020) %>%
  left_join(CO2_diff_tot[c("mod_scen", "diff_tot")], by="mod_scen") %>%
  mutate(diff_pc = round(100*(diff/diff_tot),1))
  

CO2_diff[CO2_diff$Sector %in% "Electricity",]
median(CO2_diff$diff_pc[CO2_diff$Sector %in% "Electricity"])
mean(CO2_diff$diff_pc[CO2_diff$Sector %in% "Electricity"])


png(file=paste0("plots/emissions_co2_reduction_by_model_",str_replace(t_lab,"<",""),"_",reg_lab,".png"),width=1400,height=1200,res=200,type='cairo')
CO2_diff %>%
  mutate(mod_scen = str_replace(mod_scen, "\\|", "\n")) %>%
  ggplot(aes(x=mod_scen, y=diff/1000, fill=factor(Sector, levels=c("Other", "Industry.Process", "Transport", "Buildings", "Industry.Combustion", "AFOLU", "Electricity")))) +
  geom_bar(stat='identity', position='stack') +
  labs(y="Emissions (GtCO2)", x="", fill="Sector",
       title="Change in CO2 emissions by sector, 2020-2030", 
       subtitle=paste0(reg_lab,", ",t_lab)) +
  theme(axis.text.x = element_text(angle=55, hjust=1, size=6)) +
  scale_fill_viridis(discrete=T)
dev.off()

# ---- TWh changes by fuel ----
png(file=paste0("plots/elec_change_2020-2030_",str_replace(t_lab,"<",""),"_",reg_lab,".png"),width=1400,height=1000,res=200,type='cairo')
runs %>% select(mod_scen, 
                Year,
                SecondaryEnergy.Electricity.Coal,
                SecondaryEnergy.Electricity.Gas,
                SecondaryEnergy.Electricity.Wind,
                SecondaryEnergy.Electricity.Solar,
                SecondaryEnergy.Electricity.Biomass,
                SecondaryEnergy.Electricity.Nuclear) %>%
                #SecondaryEnergy.Electricity.Gas.wCCS) %>%
  filter(Year %in% c(2020, 2030)) %>%
  #replace(is.na(.), 0) %>% # replace missing emissions with zero, for sums
  rename(Coal = SecondaryEnergy.Electricity.Coal,
         Gas = SecondaryEnergy.Electricity.Gas,
         Wind = SecondaryEnergy.Electricity.Wind,
         Solar = SecondaryEnergy.Electricity.Solar,
         Biomass = SecondaryEnergy.Electricity.Biomass,
         Nuclear = SecondaryEnergy.Electricity.Nuclear) %>%
         #Gas.wCCS = SecondaryEnergy.Electricity.Gas.wCCS) %>%
  pivot_longer(Coal:Nuclear, names_to = "Fuel", values_to="EJ") %>%
  pivot_wider(names_from = Year, values_from = EJ, names_prefix = "y") %>%
  mutate(diff20_30 = y2030-y2020) %>%
  mutate(mod_scen = str_replace(mod_scen, "\\|", "\n")) %>%
  ggplot(aes(x=mod_scen, y=diff20_30*278, fill=Fuel)) + #levels=c("Other", "Industry.Process", "Transport", "Buildings", "Industry.Combustion", "AFOLU", "Electricity")))) +
  geom_bar(stat='identity') +
  labs(y="2030-2020 (TWh)", x="", fill="Fuel",
       title="Difference in electricity production by fuel, 2020-2030", 
       subtitle=paste0(reg_lab,", ",t_lab)) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_fill_manual(values=fuel_cols)
dev.off()

# ---- TWh changes ----
#png(file=paste0("plots/elec_change_2020-2030_",str_replace(t_lab,"<",""),"_",reg_lab,".png"),width=1400,height=1000,res=200,type='cairo')
twh_diff_coal_other <- runs %>% select(mod_scen, 
                                       Year,
                                       SecondaryEnergy.Electricity,
                                       SecondaryEnergy.Electricity.Coal) %>%
  #SecondaryEnergy.Electricity.Gas.wCCS) %>%
  filter(Year %in% c(2020, 2030)) %>%
  #replace(is.na(.), 0) %>% # replace missing emissions with zero, for sums
  mutate(SecondaryEnergy.Electricity.NotCoal = SecondaryEnergy.Electricity - SecondaryEnergy.Electricity.Coal) %>%
  select(-SecondaryEnergy.Electricity) %>%
  rename(Coal = SecondaryEnergy.Electricity.Coal,
         Other = SecondaryEnergy.Electricity.NotCoal) %>%
  #Gas.wCCS = SecondaryEnergy.Electricity.Gas.wCCS) %>%
  pivot_longer(Coal:Other, names_to = "Fuel", values_to="EJ") %>%
  pivot_wider(names_from = Year, values_from = EJ, names_prefix = "y") %>%
  mutate(diff20_30 = y2030-y2020,
         mod_scen = str_replace(mod_scen, "\\|", "\n"))

bars <- tibble(x=twh_diff_coal_other$mod_scen[twh_diff_coal_other$Fuel %in% "Coal"],
               y=-278*twh_diff_coal_other$diff20_30[twh_diff_coal_other$Fuel %in% "Coal"],
               ymin=-278*twh_diff_coal_other$diff20_30[twh_diff_coal_other$Fuel %in% "Coal"],
               ymax=-278*twh_diff_coal_other$diff20_30[twh_diff_coal_other$Fuel %in% "Coal"]
)

png(file=paste0("plots/elec_change_coal_other_2020-30_",str_replace(t_lab,"<",""),"_",reg_lab,".png"),width=1400,height=1000,res=200,type='cairo')
twh_diff_coal_other %>%
  #mutate(mod_scen = str_replace(mod_scen, "\\|", "\n")) %>%
  ggplot(aes(x=mod_scen, y=diff20_30*278, fill=Fuel)) + #levels=c("Other", "Industry.Process", "Transport", "Buildings", "Industry.Combustion", "AFOLU", "Electricity")))) +
  geom_bar(stat='identity', position='stack') +
  geom_errorbar(data=bars, aes(x=x, ymin=ymin, ymax=ymax), size=1, inherit.aes = F) +
  labs(y="2030-2020 (TWh)", x="", fill="",
       title="Changes in electricity generation, 2020-2030", 
       subtitle="As well as replacing coal, the world must add extra TWh") +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_fill_manual(values=fuel_cols) +
  scale_y_continuous(breaks=c(-5000, 0, 5000, 10000, 15000))
dev.off()
# ---- Electricity demand changes 2020 / 2030 / 2050 ----
png(file=paste0("plots/elec_demand_ppchange_2020-30-50_",str_replace(t_lab,"<",""),".png"),width=1400,height=1000,res=220,type='cairo')
runs_comp %>% 
  filter(Year %in% c(2020, 2030, 2050)) %>%
  select(mod_scen, Year, Region, FinalEnergy.Electricity, FinalEnergy) %>%
  mutate(pc.FinalEnergy.Electricity = 100*FinalEnergy.Electricity/FinalEnergy) %>%
  select(-FinalEnergy.Electricity, -FinalEnergy) %>%
  pivot_wider(names_from=Year, values_from = pc.FinalEnergy.Electricity, names_prefix = "y") %>%
  mutate(diff20.30 = y2030 - y2020, 
         diff20.50 = y2050 - y2020) %>%
  select(-y2020, -y2030, -y2050) %>%
  pivot_longer(diff20.30:diff20.50, names_to="Year", values_to="pc.diff") %>%
  mutate(Year = ifelse(Year %in% "diff20.30", "2030", 
                       ifelse(Year %in% "diff20.50", "2050", NA))) %>%
  ggplot(aes(x=Year, y=pc.diff)) +
  geom_boxplot(aes(colour=Region)) +
  labs(title="Percentage point increase in final energy from electricity",
       subtitle=paste0("With respect to 2020 (",t_lab,")"),
       x="",
       y="") +
  scale_y_continuous(breaks=c(0, 10, 20, 30, 40), labels = c("0%", "10%", "20%", "30%", "40%"))
dev.off()
  

# ---- Fraction of Gas generation abated ---- 
png(file=paste0("plots/gas_frac_abated_",str_replace(t_lab,"<",""),"_",reg_lab,".png"),width=1200,height=1000,res=220,type='cairo')
runs %>% filter(Year %in% c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)) %>%
  select(Year, mod_scen, SecondaryEnergy.Electricity, SecondaryEnergy.Electricity.Gas, SecondaryEnergy.Electricity.Gas.wCCS) %>%
  mutate(pc.Gas = 100*SecondaryEnergy.Electricity.Gas/SecondaryEnergy.Electricity,
         pc.Gas.wCCS = 100*SecondaryEnergy.Electricity.Gas.wCCS/SecondaryEnergy.Electricity.Gas) %>%
  mutate(pc.Gas.wCCS = ifelse(pc.Gas<5, NA, pc.Gas.wCCS)) %>% # make gas abated frac NA if gas provides < a certain % of all electricity
  select(Year, mod_scen, pc.Gas.wCCS) %>%
  drop_na() %>%
  ggplot(aes(x=Year, y=pc.Gas.wCCS, group=mod_scen)) +
  geom_line(size=0.8, alpha=0.8) +
  labs(y="", x="", title="Percentage of gas generation abated", 
       subtitle=paste0("While gas provides >5% of electricity.\nRegion: ",reg_lab,"\n",t_lab)) +
  theme(text=element_text(size=8),
        axis.text = element_text(size=8)) +
  scale_y_continuous(labels=c("0%", "25%", "50%", "75%", "100%"))
dev.off()

# ---- Biomass growth, Individual models ----
# INDIVIDUAL MODELS - TWh abated / unabated
png(file=paste0("plots/biomass_growth_",str_replace(t_lab,"<",""),"_",reg_lab,".png"),width=1400,height=500,res=220,type='cairo')
#runs %>% filter(Year %in% c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)) %>%
runs %>% filter(Year %in% c(2020, 2040, 2060, 2080)) %>%
  filter(!mod_scen %in% "MESSAGEix-GLOBIOM 1.0 | LowEnergyDemand") %>%
  select(Year, mod_scen, SecondaryEnergy.Electricity.Biomass.wCCS, SecondaryEnergy.Electricity.Biomass.woCCS) %>%
  rename(Abated = SecondaryEnergy.Electricity.Biomass.wCCS,
         Unabated = SecondaryEnergy.Electricity.Biomass.woCCS) %>%
  pivot_longer(Abated:Unabated, names_to="Fuel", values_to="Gen") %>%
  mutate(mod_scen = str_replace(mod_scen, "\\|", "\n")) %>%
  ggplot(aes(x=Year, y=278*Gen, fill=Fuel)) +
    geom_bar(stat='identity', width=20) +
    #geom_hline(yintercept=580, linetype='dashed') +
    labs(y="TWh", x="", fill="") +
    scale_x_continuous(breaks=c(2020, 2040, 2060, 2080)) +
    facet_grid(~mod_scen) +
    theme(text=element_text(size=6),
          axis.text.x=element_text(angle=45, hjust=1),
          panel.grid.major.y = element_blank()
          #axis.title=element_text(size=6),
          #legend.text = element_text(size=6))
    )
dev.off()

# INDIVIDUAL MODELS - % of electricity supply (abated / unabated)
png(file=paste0("plots/biomass_growth_pc.elec_",str_replace(t_lab,"<",""),"_",reg_lab,".png"),width=1200,height=600,res=220,type='cairo')
#runs %>% filter(Year %in% c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)) %>%
runs %>% filter(Year %in% c(2020, 2040, 2060, 2080)) %>%
  filter(!mod_scen %in% "MESSAGEix-GLOBIOM 1.0 | LowEnergyDemand") %>%
  select(Year, mod_scen, pc.Biomass.wCCS, pc.Biomass) %>%
  mutate(pc.Biomass.woCCS = pc.Biomass - pc.Biomass.wCCS) %>%
  select(-pc.Biomass) %>%
  rename(Abated = pc.Biomass.wCCS,
         Unabated = pc.Biomass.woCCS) %>%
  pivot_longer(Abated:Unabated, names_to="Fuel", values_to="pc.Gen") %>%
  mutate(mod_scen = str_replace(mod_scen, "\\|", "\n")) %>%
  ggplot(aes(x=Year, y=pc.Gen, fill=Fuel)) +
  geom_bar(stat='identity', width=20) +
  #geom_hline(yintercept=580, linetype='dashed') +
  labs(y="% electricity production", x="", fill="") +
  scale_x_continuous(breaks=c(2020, 2040, 2060, 2080)) +
  facet_grid(~mod_scen) +
  theme(text=element_text(size=6),
        axis.text.x=element_text(angle=45, hjust=1),
        panel.grid.major.y = element_blank()
        #axis.title=element_text(size=6),
        #legend.text = element_text(size=6))
  )
dev.off()

# ---- Biomass changes by region ----
png(file=paste0("plots/biomass_changes_region_2020-2050_",str_replace(t_lab,"<",""),".png"),width=1400,height=1000,res=200,type='cairo')
runs_comp %>% select(mod_scen, 
                Year,
                Region,
                SecondaryEnergy.Electricity.Biomass) %>%
  #SecondaryEnergy.Electricity.Gas.wCCS) %>%
  filter(Year %in% c(2020, 2050),
         !Region %in% "World",
         !mod_scen %in% "MESSAGEix-GLOBIOM 1.0 | LowEnergyDemand") %>%
  #replace(is.na(.), 0) %>% # replace missing emissions with zero, for sums
  rename(Biomass = SecondaryEnergy.Electricity.Biomass) %>%
  #Gas.wCCS = SecondaryEnergy.Electricity.Gas.wCCS) %>%
  #pivot_longer(Coal:Nuclear, names_to = "Fuel", values_to="EJ") %>%
  pivot_wider(names_from = Year, values_from = Biomass, names_prefix = "y") %>%
  mutate(diff20_50 = y2050-y2020) %>%
  mutate(mod_scen = str_replace(mod_scen, "\\|", "\n")) %>%
  ggplot(aes(x=mod_scen, y=diff20_50*278, fill=Region)) + #levels=c("Other", "Industry.Process", "Transport", "Buildings", "Industry.Combustion", "AFOLU", "Electricity")))) +
  geom_bar(stat='identity') +
  labs(y="2050-2020 (TWh)", x="",
       title="Difference in Biomass electricity, 2020-2050", 
       subtitle=t_lab) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_fill_viridis_d()
dev.off()

# ---- Nuclear changes by region ----
png(file=paste0("plots/nuclear_changes_region_2020-2030_",str_replace(t_lab,"<",""),".png"),width=1400,height=1000,res=200,type='cairo')
runs_comp %>% select(mod_scen, 
                     Year,
                     Region,
                     SecondaryEnergy.Electricity.Nuclear) %>%
  #SecondaryEnergy.Electricity.Gas.wCCS) %>%
  filter(Year %in% c(2020, 2030, 2040, 2050),
         !Region %in% "World") %>%
  #replace(is.na(.), 0) %>% # replace missing emissions with zero, for sums
  rename(Nuclear = SecondaryEnergy.Electricity.Nuclear) %>%
  #Gas.wCCS = SecondaryEnergy.Electricity.Gas.wCCS) %>%
  #pivot_longer(Coal:Nuclear, names_to = "Fuel", values_to="EJ") %>%
  pivot_wider(names_from = Year, values_from = Nuclear, names_prefix = "y") %>%
  mutate(diff20_50 = y2050 - y2020,
         diff20_30 = y2030 - y2020) %>%
  mutate(mod_scen = str_replace(mod_scen, "\\|", "\n")) %>%
  ggplot(aes(x=mod_scen, y=diff20_30*278, fill=Region)) + #levels=c("Other", "Industry.Process", "Transport", "Buildings", "Industry.Combustion", "AFOLU", "Electricity")))) +
  geom_bar(stat='identity') +
  labs(y="2030-2020 (TWh)", x="",
       title="Change in nuclear electricity production, 2020-2030", 
       subtitle=t_lab) +
  theme(text = element_text(size=8),
        axis.text.x = element_text(angle=45, hjust=1, size=6)) +
  scale_fill_viridis_d()
dev.off()
  
######### MULTI-SAMPLE PLOTS #########

# ---- Electricity demand ----
#SecondaryEnergy|Electricity
png(file=paste0("plots/elec_demand_",reg_lab,".png"),width=1200,height=800,res=200,type='cairo')
runs %>% select(mod_scen, Year, cat2, SecondaryEnergy.Electricity) %>%
  filter(!is.na(SecondaryEnergy.Electricity)) %>%
  ggplot(aes(x=Year, y=SecondaryEnergy.Electricity, group=mod_scen)) +
  geom_line(aes(colour=cat2), size=1, alpha=0.6) +
  labs(x="", y="Demand (EJ)", title="Electricity demand", colour="Warming", 
       subtitle=reg_lab) +
  coord_cartesian(xlim=c(2020,2100), ylim=c(0, 700)) +
  scale_colour_viridis(discrete=T)
#colorRampPalette()
dev.off()
# ---- Electricity emissions ----
raw_runs <- runs %>%
  select(Year,
         Emissions.CO2.Energy.Supply.Electricity,
         Emissions.CO2,
         cat2,
         mod_scen) %>%
  drop_na() 

png(file=paste0("plots/elec_emissions_",reg_lab,".png"),width=1400,height=900,res=200,type='cairo')
raw_runs %>%
  filter(Year %in% c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)) %>%
  group_by(Year, cat2) %>%
  summarise(med = median(Emissions.CO2.Energy.Supply.Electricity),
            q25 = quantile(Emissions.CO2.Energy.Supply.Electricity, 0.25),
            q75 = quantile(Emissions.CO2.Energy.Supply.Electricity, 0.75)) %>%
  ggplot(aes(x=Year, y=med)) +
  geom_hline(yintercept = 0, size = 0.8, color = "grey50") +
  #geom_vline(xintercept = 2060, size=0.8,colour="grey20") +
  #geom_line(data=tibble(x=c(2050,2050,2062,2062), y=c(-4000,-6000,-4000,-6000), g=c(1,1,2,2)), aes(x=x, y=y, group=g), size= 0.8) +
  #geom_line(data=raw_runs, aes(x=Year, y=Emissions.CO2.Energy.Supply.Electricity, colour=cat2, group=mod_scen), inherit.aes = F, alpha=0.25,size=0.3) +
  geom_ribbon(aes(ymin=q25, ymax=q75, group=cat2, fill=cat2), alpha=0.3) +
  geom_line(aes(colour=cat2), size=1) +
  lims(x=year_range) +
  #scale_x_continuous(breaks=c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)) +
  coord_cartesian(ylim=c(-6000, 13000)) +
  #scale_y_continuous(limits=c(-5000, 16000)) +
  labs(x="", y="CO2 (Mt/yr)", fill="Warming",
       title="CO2 emissions from electricity supply", 
       subtitle=paste0("Region: ",reg_lab)) +
  guides(colour=F)
  #scale_colour_manual()
dev.off()

# ---- emissins trade-offs ----
# plotting emissions in one year doesn't show anything.
# I would need to interpolate emissions, and plot these 'budgets' against each other
# Example of interpolation code:
#df <- tibble(x=c(2000:2100)) %>%
#  mutate(y=2*x-6)
#df$y[!df$x %in% seq(2000, 2100, 10)] <- NA
#a <- approx(df$x, df$y, n=101) # This provides a value for each year. 

#runs %>%
#  filter(Year %in% 2030) %>%
#  ggplot(aes(x=Emissions.CO2.Energy.Supply.Electricity, y=Emissions.CO2.Energy.Demand.Industry)) +
#  geom_point(aes(colour=cat2))
#
#runs %>%
#  filter(Year %in% 2030) %>%
#  ggplot(aes(x=Emissions.CO2.Energy.Supply.Electricity, y=Emissions.CO2.Energy.Demand.Transportation)) +
#  geom_point(aes(colour=cat2))

# ---- Generation trade-offs ----
# Box plots - Nuclear

yr <- 2030
hist(runs$pc.Nuclear[runs$Year == yr], 20, main=paste("% nuclear in",yr,"(<2C)"), xlab="", ylab="")
m_nuc <- median(runs$pc.Nuclear[runs$Year == yr], na.rm=T)
q33_nuc <- quantile(runs$pc.Nuclear[runs$Year == yr], 0.33, na.rm=T)
q66_nuc <- quantile(runs$pc.Nuclear[runs$Year == yr], 0.66, na.rm=T)
sum(runs$pc.Nuclear[runs$Year == yr] < q33_nuc)
sum(runs$pc.Nuclear[runs$Year == yr] > q66_nuc)
# 23 runs in the bottom third of nuclear in 2030
# 23 runs in the bottom third of nuclear in 2050


# Low nuclear runs, re-shaped for boxplot
low_nuc_runs <- runs %>% 
  filter(Year==yr,
         pc.Nuclear < q33_nuc) %>%
  select(pc.Coal,
         pc.Gas.woCCS,
         pc.Renewables,
         pc.Biomass) %>%
  pivot_longer(pc.Coal:pc.Biomass,
               names_to="Fuel",
               values_to="Gen_pc",
               names_prefix="pc.") %>%
  mutate(Sample="Low nuclear")

high_nuc_runs <- runs %>% 
  filter(Year==yr,
         pc.Nuclear > q66_nuc) %>%
  select(pc.Coal,
         pc.Gas.woCCS,
         pc.Renewables,
         pc.Biomass) %>%
  pivot_longer(pc.Coal:pc.Biomass,
               names_to="Fuel",
               values_to="Gen_pc",
               names_prefix="pc.") %>%
  mutate(Sample="High nuclear")
  
# Start with all, manipulate into right shape, then bind low nuclear runs

png(file=paste0("plots/nuclear_trade-off_",reg_lab,"_",yr,".png"),width=1200,height=800,res=200,type='cairo')
runs %>%
  filter(Year==yr) %>%
  select(pc.Coal,
         pc.Gas.woCCS,
         pc.Renewables,
         pc.Biomass) %>%
  pivot_longer(pc.Coal:pc.Biomass,
               names_to="Fuel",
               values_to="Gen_pc",
               names_prefix="pc.") %>%
  mutate(Sample="All") %>%
  bind_rows(low_nuc_runs) %>%
  bind_rows(high_nuc_runs) %>%
  mutate(Fuel=ifelse(Fuel %in% "Gas.woCCS", "Gas (no CCS)", Fuel)) %>%
  ggplot(aes(x=Fuel, y=Gen_pc)) +
  geom_boxplot(aes(colour=Sample), varwidth=T) +
  labs(x="", y="% electricity generation", title=paste("Share of generation in",yr), subtitle=paste0(reg_lab,", ",t_lab)) 
dev.off()

# Do 'low nuclear' and 'high nuclear' differ significantly in the amount of Gas or renewables needed?
ks.test(runs$pc.Gas.woCCS[runs$Year==yr & runs$pc.Nuclear < q33_nuc], runs$pc.Gas.woCCS[runs$Year==yr])
ks.test(runs$pc.Gas.woCCS[runs$Year==yr & runs$pc.Nuclear > q66_nuc], runs$pc.Gas.woCCS[runs$Year==yr])
ks.test(runs$pc.Renewables[runs$Year==yr & runs$pc.Nuclear < q33_nuc], runs$pc.Renewables[runs$Year==yr])
ks.test(runs$pc.Renewables[runs$Year==yr & runs$pc.Nuclear > q66_nuc], runs$pc.Renewables[runs$Year==yr])


# Fossil CCS
yr <- 2050
# Low Fossil CCS runs, re-shaped for boxplot
low_fccs_runs <- runs %>% 
  filter(Year==yr,
         pc.Fossil.wCCS < 15) %>%
  select(pc.Nuclear,
         pc.Renewables) %>%
  pivot_longer(pc.Nuclear:pc.Renewables,
               names_to="Fuel",
               values_to="Gen_pc",
               names_prefix="pc.") %>%
  mutate(Sample="Low fossil-CCS")
# High Fossil CCS runs, re-shaped for boxplot
high_fccs_runs <- runs %>% 
  filter(Year==yr,
         pc.Fossil.wCCS > 15) %>%
  select(pc.Nuclear,
         pc.Renewables) %>%
  pivot_longer(pc.Nuclear:pc.Renewables,
               names_to="Fuel",
               values_to="Gen_pc",
               names_prefix="pc.") %>%
  mutate(Sample="High fossil-CCS")

png(file=paste0("plots/fossil_ccs_trade-offs_",reg_lab,"_",yr,".png"),width=1200,height=800,res=200,type='cairo')
runs %>%
  filter(Year==yr) %>%
  select(pc.Nuclear,
         pc.Renewables,
         pc.Biomass.wCCS) %>%
  pivot_longer(pc.Nuclear:pc.Renewables,
               names_to="Fuel",
               values_to="Gen_pc",
               names_prefix="pc.") %>%
  mutate(Sample="All") %>%
  bind_rows(low_fccs_runs) %>%
  bind_rows(high_fccs_runs) %>%
  mutate(Fuel=ifelse(Fuel %in% "Biomass.wCCS", "BECCS", Fuel)) %>%
  ggplot(aes(x=Fuel, y=Gen_pc)) +
  geom_boxplot(aes(colour=Sample), varwidth=T) +
  labs(x="", y="% electricity generation", title=paste("Share of generation in",yr) 
       #subtitle=paste0(reg_lab,", ",t_lab)
       ) 
dev.off()

ks.test(runs$pc.Nuclear[runs$Year==yr & runs$pc.Fossil.wCCS < 15], runs$pc.Nuclear[runs$Year==yr]) #p=0.29
ks.test(runs$pc.Nuclear[runs$Year==yr & runs$pc.Fossil.wCCS > 15], runs$pc.Nuclear[runs$Year==yr]) #p=0.07
ks.test(runs$pc.Renewables[runs$Year==yr & runs$pc.Fossil.wCCS < 15], runs$pc.Renewables[runs$Year==yr]) #p=0.21
ks.test(runs$pc.Renewables[runs$Year==yr & runs$pc.Fossil.wCCS > 15], runs$pc.Renewables[runs$Year==yr]) #p=0.04 - this is the only significant result. High fossil CCS have lower renewables. 

# ---- Biomass changes ----
# Biomass only
bio_changes <- runs %>% 
  select(mod_scen, 
         cat2,
         Year,
         SecondaryEnergy.Electricity.Biomass.woCCS,
         SecondaryEnergy.Electricity.Biomass.wCCS) %>%
  #SecondaryEnergy.Electricity.Gas.wCCS) %>%
  filter(Year %in% c(2020, 2030, 2040, 2050)) %>%
  #replace(is.na(.), 0) %>% # replace missing emissions with zero, for sums
  rename(Abated = SecondaryEnergy.Electricity.Biomass.wCCS,
         Unabated = SecondaryEnergy.Electricity.Biomass.woCCS) %>%
  #Gas.wCCS = SecondaryEnergy.Electricity.Gas.wCCS) %>%
  pivot_longer(Abated:Unabated, names_to = "Fuel", values_to="EJ") %>%
  pivot_wider(names_from = Year, values_from = EJ, names_prefix = "y") %>%
  mutate(diff20_30 = y2030-y2020,
         diff20_50 = y2050-y2020,
         diff30_40 = y2040-y2030,
         diff40_50 = y2050-y2040)

#bio_changes_all <- bio_changes %>%
#  group_by(mod_scen) %>%
#  summarise(diff20_30=sum(diff20_30, na.rm=T),
#            diff30_40=sum(diff30_40, na.rm=T),
#            diff40_50=sum(diff40_50, na.rm=T))

#lims_30 <- c(min(bio_changes_all$diff20_30), max(bio_changes_all$diff20_30))
#lims_40 <- c(min(bio_changes_all$diff30_40), max(bio_changes_all$diff30_40))
#lims_50 <- c(min(bio_changes_all$diff40_50), max(bio_changes_all$diff40_50))

# 2030 - 2020
bio_chng_20_30_1p5 <- bio_changes %>%
  filter(cat2 %in% "<1.5C") %>%
  mutate(mod_scen = str_replace(mod_scen, "\\|", "\n")) %>%
  ggplot(aes(x=mod_scen, y=diff20_30*278, fill=Fuel)) + #levels=c("Other", "Industry.Process", "Transport", "Buildings", "Industry.Combustion", "AFOLU", "Electricity")))) +
  geom_bar(stat='identity', position='stack') +
  labs(y="", x="", fill="",
       title="") +
       #subtitle=paste0(reg_lab,", ",t_lab)) +
  theme(axis.text.x = element_blank(),
        plot.margin = margin(20, 5.5, 0, 70)) + #trbl
  lims(y=c(-300, 1000)) +
  guides(fill=F)

bio_chng_20_30_2 <- bio_changes %>%
  filter(cat2 %in% "1.5-2C") %>%
  mutate(mod_scen = str_replace(mod_scen, "\\|", "\n")) %>%
  ggplot(aes(x=mod_scen, y=diff20_30*278, fill=Fuel)) + #levels=c("Other", "Industry.Process", "Transport", "Buildings", "Industry.Combustion", "AFOLU", "Electricity")))) +
  geom_bar(stat='identity', position='stack') +
  labs(y="", x="", fill="",
       title="") +
  lims(y=c(-300, 1000)) +
  #subtitle=paste0(reg_lab,", ",t_lab)) +
  theme(axis.text.x = element_blank(),
        plot.margin = margin(20, 5.5, 0, 5.5))  #trbl)

# 2040 - 2030  
bio_chng_30_40_1p5 <- bio_changes %>%
  filter(cat2 %in% "<1.5C") %>%
  mutate(mod_scen = str_replace(mod_scen, "\\|", "\n")) %>%
  ggplot(aes(x=mod_scen, y=diff30_40*278, fill=Fuel)) + #levels=c("Other", "Industry.Process", "Transport", "Buildings", "Industry.Combustion", "AFOLU", "Electricity")))) +
  geom_bar(stat='identity', position='stack') +
  labs(y="", x="", fill="",
       title="") +
  lims(y=c(-2000, 2000)) +
  #subtitle=paste0(reg_lab,", ",t_lab)) +
  theme(axis.text.x = element_blank(),
        plot.margin = margin(20, 5.5, 0, 70)) + #trbl
  guides(fill=F)

bio_chng_30_40_2 <- bio_changes %>%
  filter(cat2 %in% "1.5-2C") %>%
  mutate(mod_scen = str_replace(mod_scen, "\\|", "\n")) %>%
  ggplot(aes(x=mod_scen, y=diff30_40*278, fill=Fuel)) + #levels=c("Other", "Industry.Process", "Transport", "Buildings", "Industry.Combustion", "AFOLU", "Electricity")))) +
  geom_bar(stat='identity', position='stack') +
  labs(y="", x="", fill="",
       title="") +
  lims(y=c(-2000, 2000)) +
  #subtitle=paste0(reg_lab,", ",t_lab)) +
  theme(axis.text.x = element_blank(),
        plot.margin = margin(20, 5.5, 0, 5.5)) #trbl)
  
# 2050 - 2040
bio_chng_40_50_1p5 <- bio_changes %>%
  filter(cat2 %in% "<1.5C") %>%
  mutate(mod_scen = str_replace(mod_scen, "\\|", "\n")) %>%
  ggplot(aes(x=mod_scen, y=diff40_50*278, fill=Fuel)) + #levels=c("Other", "Industry.Process", "Transport", "Buildings", "Industry.Combustion", "AFOLU", "Electricity")))) +
  geom_bar(stat='identity', position='stack') +
  labs(y="", x="", fill="",
       title="") +
  lims(y=c(-600, 2500)) +
  #subtitle=paste0(reg_lab,", ",t_lab)) +
  theme(axis.text.x = element_blank(),
        plot.margin = margin(0, 5.5, 0, 70)) + #trbl
  guides(fill=F)

bio_chng_40_50_2 <- bio_changes %>%
  filter(cat2 %in% "1.5-2C") %>%
  mutate(mod_scen = str_replace(mod_scen, "\\|", "\n")) %>%
  ggplot(aes(x=mod_scen, y=diff40_50*278, fill=Fuel)) + #levels=c("Other", "Industry.Process", "Transport", "Buildings", "Industry.Combustion", "AFOLU", "Electricity")))) +
  geom_bar(stat='identity', position='stack') +
  labs(y="", x="", fill="",
       title="") +
  lims(y=c(-600, 2500)) +
  #subtitle=paste0(reg_lab,", ",t_lab)) +
  theme(axis.text.x = element_blank(),
        plot.margin = margin(0, 5.5, 0, 5.5)) #trbl)

png(file=paste0("plots/biomass_twh_changes_",reg_lab,".png"),width=1400,height=1400,res=200,type='cairo')
plot_grid(bio_chng_20_30_1p5, bio_chng_20_30_2, bio_chng_30_40_1p5, bio_chng_30_40_2, bio_chng_40_50_1p5, bio_chng_40_50_2,
          nrow=3, 
          labels=c("<1.5C", "<2C", "2030 - 2020", "2040 - 2030", "2050 - 2040"), 
          rel_widths=c(1,1.9),
          label_x = c(0.5, 0.4, -0.12, -0.58, -0.12), 
          label_y = c(1.0, 1.0, 1.65, 0.6, 0.6)
)
dev.off()

# ---- 1.5C vs 2C tech timelines ----

# See 2019 regional distribution
#filter(ger_sum, Year==2019, Type2 %in% "Biomass") 

coal_sum <- runs %>%
  filter(Year %in% c(2020, 2030, 2040, 2050)) %>%
  group_by(Year, cat2) %>%
  summarise(med = median(SecondaryEnergy.Electricity.Coal.woCCS),
            q25 = quantile(SecondaryEnergy.Electricity.Coal.woCCS, 0.25),
            q75 = quantile(SecondaryEnergy.Electricity.Coal.woCCS, 0.75)) %>%
  ungroup() %>%
  mutate(Type2="Coal")

gas_sum <- runs %>%
  filter(Year %in% c(2020, 2030, 2040, 2050)) %>%
  group_by(Year, cat2) %>%
  summarise(med = median(SecondaryEnergy.Electricity.Gas.woCCS),
            q25 = quantile(SecondaryEnergy.Electricity.Gas.woCCS, 0.25),
            q75 = quantile(SecondaryEnergy.Electricity.Gas.woCCS, 0.75)
  ) %>%
  ungroup() %>%
  mutate(Type2="Gas")

biomass_sum <- runs %>%
  filter(Year %in% c(2020, 2030, 2040, 2050)) %>%
  group_by(Year, cat2) %>%
  summarise(med = median(SecondaryEnergy.Electricity.Biomass.woCCS),
            q25 = quantile(SecondaryEnergy.Electricity.Biomass.woCCS, 0.25),
            q75 = quantile(SecondaryEnergy.Electricity.Biomass.woCCS, 0.75)
  ) %>%
  ungroup() %>%
  mutate(Type2="Biomass")

#renew_sum <- runs %>%
#  filter(Year %in% c(2020, 2030, 2040, 2050)) %>%
#  group_by(Year, cat2) %>%
#  summarise(med = median(SecondaryEnergy.Electricity.Renewables),
#            q25 = quantile(SecondaryEnergy.Electricity.Renewables, 0.25),
#            q75 = quantile(SecondaryEnergy.Electricity.Renewables, 0.75)) %>%
#  ungroup() %>%
#  mutate(Type2="Renewables")

solar_sum <- runs %>%
  filter(Year %in% c(2020, 2030, 2040, 2050)) %>%
  group_by(Year, cat2) %>%
  summarise(med = median(SecondaryEnergy.Electricity.Solar),
            q25 = quantile(SecondaryEnergy.Electricity.Solar, 0.25),
            q75 = quantile(SecondaryEnergy.Electricity.Solar, 0.75)) %>%
  ungroup() %>%
  mutate(Type2="Solar")

wind_sum <- runs %>%
  filter(Year %in% c(2020, 2030, 2040, 2050)) %>%
  group_by(Year, cat2) %>%
  summarise(med = median(SecondaryEnergy.Electricity.Wind),
            q25 = quantile(SecondaryEnergy.Electricity.Wind, 0.25),
            q75 = quantile(SecondaryEnergy.Electricity.Wind, 0.75)) %>%
  ungroup() %>%
  mutate(Type2="Wind")

nuc_sum <- runs %>%
  filter(Year %in% c(2020, 2030, 2040, 2050)) %>%
  group_by(Year, cat2) %>%
  summarise(med = median(SecondaryEnergy.Electricity.Nuclear),
            q25 = quantile(SecondaryEnergy.Electricity.Nuclear, 0.25),
            q75 = quantile(SecondaryEnergy.Electricity.Nuclear, 0.75)) %>%
  ungroup() %>%
  mutate(Type2="Nuclear")


runs_summary <- bind_rows(coal_sum, gas_sum, biomass_sum, solar_sum, wind_sum, nuc_sum, filter(ger_sum, Region_ipcc %in% reg_lab)) %>%
  mutate(med = 278*med,
         q25 = 278*q25,
         q75 = 278*q75)

png(file=paste0("plots/coal_woCCS_comp_",reg_lab,".png"),width=1200,height=800,res=200,type='cairo')
ggplot(filter(runs_summary, Type2 %in% "Coal", Year %in% c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2030,2040,2050)), 
       aes(x=Year)) +
  geom_ribbon(aes(ymin=q25, ymax=q75, group=cat2, fill=cat2), alpha=0.4) +
  geom_line(size=0.8, aes(y=med, group=cat2, colour=cat2)) +
  geom_point(aes(y=med, colour=cat2), size=2) +
  # Real data
  geom_line(aes(y=Value_TWh, color=Type2, group=Type2), size=1) +
  labs(x="",
       y="Electricity production (TWh)",
       title="Unabated coal electricity in different pathways",
       subtitle=paste0("Region: ", reg_lab),
       colour="",
       fill="") +
  scale_x_continuous(breaks=c(2010, 2020, 2030, 2040, 2050), limits = c(2010, 2050)) +
  scale_color_manual(values=c("Coal"="red", "<1.5C"="skyblue3", "1.5-2C"="Coral1"),
                     labels=c("Coal"="Historic\nproduction")) +
  scale_fill_manual(values=c("<1.5C"="skyblue3", "1.5-2C"="Coral1")) +
  guides(fill=F)
dev.off()

png(file=paste0("plots/gas_woCCS_comp_",reg_lab,".png"),width=1200,height=800,res=200,type='cairo')
ggplot(filter(runs_summary, Type2 %in% "Gas", Year %in% c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2030,2040,2050)), 
       aes(x=Year)) +
  geom_line(data=filter(runs, !is.na(Year), Year>2019, !is.na(SecondaryEnergy.Electricity.Gas.woCCS)), aes(x=Year, y=SecondaryEnergy.Electricity.Gas.woCCS, group=mod_scen), colour="grey20", size=0.4, alpha=0.2, inherit.aes = F) +
  geom_ribbon(aes(ymin=q25, ymax=q75, group=cat2, fill=cat2), alpha=0.4) +
  geom_line(size=0.8, aes(y=med, group=cat2, colour=cat2)) +
  geom_point(size=2, aes(y=med, colour=cat2)) +
  # Real data
  geom_line(aes(y=Value_TWh, color=Type2, group=Type2), size=1) +
  labs(x="",
       y="Electricity production (TWh)",
       title="Unabated gas electricity in different pathways",
       subtitle=paste0("Region: ", reg_lab),
       colour="",
       fill="") +
  scale_x_continuous(breaks=c(2010, 2020, 2030, 2040, 2050), limits = c(2010, 2050)) +
  scale_color_manual(values=c("Gas"="deepskyblue3", "<1.5C"="skyblue3", "1.5-2C"="Coral1"),
                     labels=c("Gas"="Historic\nproduction")) +
  scale_fill_manual(values=c("<1.5C"="skyblue3", "1.5-2C"="Coral1")) +
  theme(panel.grid.major.y = element_blank(),
        axis.ticks.y = element_line(size=0.6, colour="grey20"),
        axis.ticks.length.y = unit(0.4,"cm"),
        axis.text.y = element_text(vjust=0, hjust=1),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  guides(fill=F)
dev.off()

png(file=paste0("plots/biomass_woCCS_comp_",reg_lab,".png"),width=1200,height=800,res=200,type='cairo')
ggplot(filter(runs_summary, Type2 %in% "Biomass", Year %in% c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2030,2040,2050)), 
       aes(x=Year)) +
  geom_ribbon(aes(ymin=q25, ymax=q75, group=cat2, fill=cat2), alpha=0.4) +
  geom_line(size=0.8, aes(y=med, group=cat2, colour=cat2)) +
  geom_point(size=3, aes(y=med, colour=cat2)) +
  # Real data
  geom_line(aes(y=Value_TWh, color=Type2, group=Type2), size=1) +
  labs(x="",
       y="Electricity production (TWh)",
       title="Biomass (unabated) electricity in different warming scenarios",
       subtitle=paste0("Region: ", reg_lab),
       colour="",
       fill="") +
  scale_x_continuous(breaks=c(2010, 2020, 2030, 2040, 2050), limits = c(2010, 2050)) +
  scale_color_manual(values=c("Biomass"="#60BC5D", "<1.5C"="skyblue3", "1.5-2C"="Coral1"),
                     labels=c("Biomass"="Historic production\n(Biomass & waste)")) +
  scale_fill_manual(values=c("<1.5C"="skyblue3", "1.5-2C"="Coral1")) +
  guides(fill=F)
dev.off()

renew_growth <- tibble(Year=c(2020:2050)) %>%
  mutate(Renew5 = 6374*(1+0.05)^(Year-2019),
         Renew6 = 6374*(1+0.06)^(Year-2019),
         Renew7 = 6374*(1+0.07)^(Year-2019)) %>%
  pivot_longer(Renew5:Renew7, names_to="Rate", values_to="EJ")

#png(file=paste0("plots/renewables_comp_",reg_lab,".png"),width=1200,height=800,res=200,type='cairo')
#ggplot(filter(runs_summary, Type2 %in% "Renewables", Year %in% c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2030,2040,2050)), 
#       aes(x=Year)) +
#  geom_ribbon(aes(ymin=q25, ymax=q75, group=cat2, fill=cat2), alpha=0.4) +
#  #geom_line(data=renew_growth, aes(x=Year, y=EJ/278, group=Rate), linetype="dashed", colour="grey50", size=1.5, alpha=0.7) +
#  geom_line(size=0.8, aes(y=med, group=cat2, colour=cat2)) +
#  geom_point(size=3, aes(y=med, colour=cat2)) +
#  #geom_line(data=renew_growth, aes(x=Year, y=EJ/278, group=Rate), linetype="dashed", colour="grey50", size=1.5, alpha=0.7) +
#  # Real data
#  geom_line(aes(y=Value_TWh/278, color=Type2, group=Type2), size=1) +
#  labs(x="",
#       y="Electricity production (EJ)",
#       title="Renewable electricity in different warming scenarios",
#       subtitle=paste0("Region: ", reg_lab),
#       colour="",
#       fill="") +
#  scale_x_continuous(breaks=c(2010, 2020, 2030, 2040, 2050), limits = c(2010, 2050)) +
#  scale_color_manual(values=c("Renewables"="purple", "<1.5C"="skyblue3", "1.5-2C"="Coral1"),
#                     labels=c("Renewables"="Historic\nproduction")) +
#  scale_fill_manual(values=c("<1.5C"="skyblue3", "1.5-2C"="Coral1")) +
#  guides(fill=F)
#dev.off()

# SOLAR
solar_proj <- tibble(Year=c(2020:2050)) %>%
  mutate(Solar_exp1 = 699*(1+0.10)^(Year-2019),
         Solar_exp2 = 699*(1+0.15)^(Year-2019)
         #Solar_lin1 = 699+((Year-2019)*250),
         #Solar_lin2 = 699+((Year-2019)*500),
         ) %>%
  pivot_longer(Solar_exp1:Solar_exp2, names_to="Rate", values_to="TWh")

png(file=paste0("plots/solar_comp_",reg_lab,".png"),width=1200,height=800,res=200,type='cairo')
ggplot(filter(runs_summary, Type2 %in% "Solar", Year %in% c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2030,2040,2050)), 
       aes(x=Year)) +
  geom_ribbon(aes(ymin=q25, ymax=q75, group=cat2, fill=cat2), alpha=0.2) +
  geom_line(size=0.8, aes(y=med, group=cat2, colour=cat2), alpha=0.7) +
  geom_point(size=2.5, aes(y=med, colour=cat2), alpha=1) +
  #geom_line(data=filter(solar_proj, Year<=2040), aes(x=Year, y=TWh, group=Rate), linetype="dotted", colour="grey50", size=1, alpha=0.7) +
  # Real data
  geom_line(aes(y=Value_TWh, color=Type2, group=Type2), size=1) +
  labs(x="",
       y="Electricity production (TWh)",
       title="Solar electricity in different warming pathways",
       subtitle=paste0("Region: ", reg_lab),
       colour="",
       fill="") +
  scale_x_continuous(breaks=c(2010, 2020, 2030, 2040, 2050), limits = c(2010, 2050)) +
  scale_color_manual(values=c("Solar"="Gold", "<1.5C"="skyblue3", "1.5-2C"="Coral1"),
                     labels=c("Solar"="Historic\nproduction")) +
  scale_fill_manual(values=c("<1.5C"="skyblue3", "1.5-2C"="Coral1")) +
  guides(fill=F)
dev.off()

# WIND
wind_proj <- tibble(Year=c(2020:2050)) %>%
  mutate(#exp1 = 1404*(1+0.10)^(Year-2019),
         #exp2 = 1404*(1+0.12)^(Year-2019)
         lin1 = 1404+((Year-2019)*350),
         lin2 = 1404+((Year-2019)*500)
  ) %>%
  pivot_longer(lin1:lin2, names_to="Rate", values_to="TWh")

png(file=paste0("plots/wind_comp_",reg_lab,".png"),width=1200,height=800,res=200,type='cairo')
ggplot(filter(runs_summary, Type2 %in% "Wind", Year %in% c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2030,2040,2050)), 
       aes(x=Year)) +
  geom_ribbon(aes(ymin=q25, ymax=q75, group=cat2, fill=cat2), alpha=0.2) +
  geom_line(size=0.8, aes(y=med, group=cat2, colour=cat2), alpha=0.7) +
  geom_point(size=2.5, aes(y=med, colour=cat2), alpha=1) +
  #geom_line(data=filter(wind_proj, Year<=2050), aes(x=Year, y=TWh, group=Rate), linetype="dotted", colour="grey20", size=0.8, alpha=0.7) +
  # Real data
  geom_line(aes(y=Value_TWh, color=Type2, group=Type2), size=1) +
  labs(x="",
       y="Electricity production (TWh)",
       title="Wind electricity in different warming pathways",
       subtitle=paste0("Region: ", reg_lab),
       colour="",
       fill="") +
  scale_x_continuous(breaks=c(2010, 2020, 2030, 2040, 2050), limits = c(2010, 2050)) +
  scale_color_manual(values=c("Wind"="firebrick1", "<1.5C"="skyblue3", "1.5-2C"="Coral1"),
                     labels=c("Wind"="Historic\nproduction")) +
  scale_fill_manual(values=c("<1.5C"="skyblue3", "1.5-2C"="Coral1")) +
  guides(fill=F)
dev.off()

png(file=paste0("plots/nuclear_comp_",reg_lab,".png"),width=1200,height=800,res=200,type='cairo')
ggplot(filter(runs_summary, Type2 %in% "Nuclear", Year %in% c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2030,2040,2050)), 
       aes(x=Year)) +
  geom_ribbon(aes(ymin=q25, ymax=q75, group=cat2, fill=cat2), alpha=0.4) +
  geom_line(size=0.8, aes(y=med, group=cat2, colour=cat2)) +
  geom_point(size=3, aes(y=med, colour=cat2)) +
  # Real data
  geom_line(aes(y=Value_TWh, color=Type2, group=Type2), size=1) +
  labs(x="",
       y="Electricity production (TWh)",
       title="Nuclear electricity in different warming scenarios",
       subtitle=paste0("Region: ", reg_lab),
       colour="",
       fill="") +
  scale_x_continuous(breaks=c(2010, 2020, 2030, 2040, 2050), limits = c(2010, 2050)) +
  scale_color_manual(values=c("Nuclear"="darkorange", "<1.5C"="skyblue3", "1.5-2C"="Coral1"),
                     labels=c("Nuclear"="Historic production")) +
  scale_fill_manual(values=c("<1.5C"="skyblue3", "1.5-2C"="Coral1")) +
  guides(fill=F)
dev.off()


# Share
renew_pc_sum <- runs %>%
  filter(Year %in% c(2020, 2030, 2040, 2050)) %>%
  group_by(Year) %>%
  summarise(med = median(pc.Renewables),
            q25 = quantile(pc.Renewables, 0.25),
            q75 = quantile(pc.Renewables, 0.75)) %>%
  ungroup() %>%
  mutate(Type="Renewables_share")


# ---- Fossil CCS ----
yr <- 2070
png(file=paste0("plots/fossil_wCCS_",yr,"_",reg_lab,".png"),width=1200,height=600,res=200,type='cairo')
runs %>% filter(Year==yr, !is.na(pc.Coal.wCCS) & !is.na(pc.Gas.wCCS)) %>%
  mutate(pc.Fossil.wCCS = pc.Gas.wCCS + pc.Coal.wCCS) %>%
  ggplot(aes(x=pc.Fossil.wCCS, fill=cat2)) +
  geom_histogram(stat='bin', binwidth=2) +
  labs(x="Electricity from Fossil + CCS (%)", 
       y="",
       title="",
       fill="Warming") +
  theme(text = element_text(size=8)) +
  scale_x_continuous(breaks=c(0,5,10,15,20,25,30)) +
  scale_fill_discrete(labels=c("<1.5C"="1.5C", "1.5-2C"="2C"))
dev.off()

png(file=paste0("plots/CarbonSeq_fossilwCCS_",yr,"_",reg_lab,".png"),width=1200,height=600,res=200,type='cairo')
runs %>% filter(Year==yr, !is.na(CarbonSequestration.CCS.Fossil)) %>%
  #mutate(pc.Fossil.wCCS = pc.Gas.wCCS + pc.Coal.wCCS) %>%
  ggplot(aes(x=CarbonSequestration.CCS.Fossil, fill=cat2)) +
  geom_histogram(stat='bin', binwidth=1000) +
  labs(title="Carbon Sequestration from Fossil + CCS (GtCO2)", 
       y="",
       x="",
       fill="Warming") +
  theme(text = element_text(size=8)) +
  #scale_x_continuous(breaks=c(0,5,10,15,20,25,30)) +
  scale_fill_discrete(labels=c("<1.5C"="1.5C", "1.5-2C"="2C"))
dev.off()

# ---- BECCS ----
yr <- 2070
png(file=paste0("plots/BECCS_",yr,"_",reg_lab,".png"),width=1200,height=600,res=200,type='cairo')
runs %>% filter(Year==yr, !is.na(SecondaryEnergy.Electricity.Biomass.wCCS)) %>%
  mutate(pc.Biomass.wCCS = 100*SecondaryEnergy.Electricity.Biomass.wCCS/SecondaryEnergy.Electricity) %>%
  ggplot(aes(x=pc.Biomass.wCCS, fill=cat2)) +
  geom_histogram(stat='bin', binwidth=1) +
  labs(x="Electricity from BECCS (%)", 
       y="",
       title="",
       fill="Warming") +
  theme(text = element_text(size=8)) +
  #scale_x_continuous(breaks=c(0,5,10,15,20,25,30)) +
  scale_fill_discrete(labels=c("<1.5C"="1.5C", "1.5-2C"="2C"))
dev.off()

png(file=paste0("plots/CarbonSeq_BECCS_",yr,"_",reg_lab,".png"),width=1200,height=600,res=200,type='cairo')
runs %>% filter(Year==yr, !is.na(CarbonSequestration.CCS.Biomass)) %>%
  #mutate(pc.Fossil.wCCS = pc.Gas.wCCS + pc.Coal.wCCS) %>%
  ggplot(aes(x=CarbonSequestration.CCS.Biomass, fill=cat2)) +
  geom_histogram(stat='bin', binwidth=1000) +
  labs(title="Carbon Sequestration from BECCS (GtCO2)", 
       y="",
       x="",
       fill="Warming") +
  theme(text = element_text(size=8)) +
  #scale_x_continuous(breaks=c(0,5,10,15,20,25,30)) +
  scale_fill_discrete(labels=c("<1.5C"="1.5C", "1.5-2C"="2C"))
dev.off()

# TEST: what is the percentage biomass?
ggplot(runs[!is.na(runs$pc.Biomass),], aes(x=Year, y=pc.Biomass)) +
  geom_line(aes(group=mod_scen, colour=cat2))
ggplot(filter(runs, Year %in% c(2040, 2050, 2070)), aes(x=pc.Biomass)) +
  geom_histogram(aes(fill=cat2), binwidth=0.2) +
  geom_vline(xintercept=1) +
  facet_wrap(~Year, nrow=3)

# Percentage abated (median + IQR)
# The >1% pc.biomass limit removes about half (11-14) runs between 2040 and 2070. 
bio_ms_keep <- runs$mod_scen[runs$pc.Biomass > 1 & runs$Year==2050]
# DATA
bio_pc_wCCS_2 <- runs %>% filter(Year %in% c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100),
                               mod_scen %in% bio_ms_keep,
                               cat2 %in% "1.5-2C") %>%
  select(Year, cat2, mod_scen, pc.Biomass, SecondaryEnergy.Electricity.Biomass.wCCS, SecondaryEnergy.Electricity.Biomass) %>%
  mutate(frac.Bio.wCCS = 100*SecondaryEnergy.Electricity.Biomass.wCCS/SecondaryEnergy.Electricity.Biomass) %>%
  #mutate(frac.Bio.wCCS = ifelse(pc.Biomass<1, NA, frac.Bio.wCCS)) %>% # make bio abated frac NA if bio provides < a certain % of all electricity
  select(Year, cat2, mod_scen, frac.Bio.wCCS) %>%
  #filter(Year %in% c(2030, 2040)) %>%
  drop_na() %>%
  rename(Pathway = cat2) %>%
  mutate(Pathway = ifelse(Pathway == "<1.5C", "1.5C", 
                          ifelse(Pathway == "1.5-2C", "2C", NA))) %>%
  group_by(Year, Pathway) %>%
  summarise(med=median(frac.Bio.wCCS),
            q25=quantile(frac.Bio.wCCS, 0.25),
            q75=quantile(frac.Bio.wCCS, 0.75))

bio_pc_wCCS_1 <- runs %>% filter(Year %in% c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100),
                                 mod_scen %in% bio_ms_keep,
                                 cat2 %in% "<1.5C") %>%
  select(Year, cat2, mod_scen, pc.Biomass, SecondaryEnergy.Electricity.Biomass.wCCS, SecondaryEnergy.Electricity.Biomass) %>%
  mutate(frac.Bio.wCCS = 100*SecondaryEnergy.Electricity.Biomass.wCCS/SecondaryEnergy.Electricity.Biomass) %>%
  #mutate(frac.Bio.wCCS = ifelse(pc.Biomass<1, NA, frac.Bio.wCCS)) %>% # make bio abated frac NA if bio provides < a certain % of all electricity
  select(Year, cat2, mod_scen, frac.Bio.wCCS) %>%
  #filter(Year %in% c(2030, 2040)) %>%
  drop_na() %>%
  rename(Pathway = cat2) %>%
  mutate(Pathway = ifelse(Pathway == "<1.5C", "1.5C", 
                          ifelse(Pathway == "1.5-2C", "2C", NA)))

# PLOT
png(file=paste0("plots/bio_frac_abated_med_",reg_lab,".png"),width=1200,height=800,res=220,type='cairo')
ggplot(bio_pc_wCCS_2, aes(x=Year)) + #filter(bio_pc_wCCS, !(Year>2060 & Pathway=="<1.5C")), 
  geom_ribbon(aes(ymin=q25, ymax=q75, group=Pathway), alpha=0.3, fill="lightblue") +
  geom_line(aes(y=med, group=Pathway, colour=Pathway), size=1) +
  geom_line(data=bio_pc_wCCS_1, aes(y=frac.Bio.wCCS, x=Year, group=mod_scen, colour=Pathway), inherit.aes = F) +
  #geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.1) +
  labs(y="Percentage abated", x="", title="Scenarios that continue to use biomass for electricity are quick to apply CCS", 
       subtitle="Models that provide >1% of electricity from biomass in 2050") +
  theme(text=element_text(size=8),
        axis.text = element_text(size=8)) +
  scale_y_continuous(labels=c("0%", "25%", "50%", "75%", "100%")) +
  scale_x_continuous(breaks=c(2020,2030,2040,2050,2060,2070,2080,2090,2100), limits=c(2020,2100)) +
  theme(text=element_text(size=8),
        axis.text = element_text(size=8),
        legend.text = element_text(size=9)) 
dev.off()


# ---- Coal CCS ----

median(runs$pc.Coal.wCCS[runs$Year==2050], na.rm=T)
median(runs$pc.Coal.wCCS[runs$Year==2070], na.rm=T)
median(runs$pc.Coal.wCCS[runs$Year==2090], na.rm=T)
mean(runs$pc.Coal.wCCS[runs$Year==2050], na.rm=T)
mean(runs$pc.Coal.wCCS[runs$Year==2070], na.rm=T)
mean(runs$pc.Coal.wCCS[runs$Year==2090], na.rm=T)
max(runs$pc.Coal.wCCS[runs$Year==2050], na.rm=T)
max(runs$pc.Coal.wCCS[runs$Year==2070], na.rm=T)
max(runs$pc.Coal.wCCS[runs$Year==2090], na.rm=T)

png(file=paste0("plots/coal_wCCS_2050_",reg_lab,".png"),width=1200,height=600,res=200,type='cairo')
runs %>% filter(Year==2050, !is.na(runs$pc.Coal.wCCS)) %>%
ggplot(aes(x=pc.Coal.wCCS, fill=cat2)) +
  geom_histogram(stat='bin') +
  labs(x="Electricity from coal + CCS (%)", y="Count", 
       title="In 2050, coal + CCS does not contribute significantly to electricity production",
       fill="Warming") +
  theme(text = element_text(size=8)) +
  scale_x_continuous(breaks=c(0,3,6,9), labels=c("0%", "3%", "6%", "9%"))
dev.off()
  #geom_line(colour="grey50", alpha=0.7, size=sz) +
  #coord_cartesian(xlim=c(2010, 2100), ylim=c(-6000, 15000)) +
  #facet_wrap(vars(cat2)) 
  #labs(y="Emissions from electricity (Mt CO2)", x="") +
  #scale_colour_viridis(discrete=T)

# TEST: what is the percentage coal?
ggplot(runs[!is.na(runs$pc.Coal),], aes(x=Year, y=pc.Coal)) +
  geom_line(aes(group=mod_scen, colour=cat2))
ggplot(filter(runs, Year %in% c(2040, 2050, 2070)), aes(x=pc.Coal)) +
  geom_histogram(aes(fill=cat2), binwidth=0.2) +
  geom_vline(xintercept=1) +
  facet_wrap(~Year, nrow=3)

# Fraction abated
min_2050 <- 1
coal_ms_keep <- runs$mod_scen[runs$pc.Coal > min_2050 & runs$Year==2050]
# DATA
coal_pc_wCCS_med <- runs %>% filter(Year %in% c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100),
                                 mod_scen %in% coal_ms_keep,
                                 cat2 %in% "1.5-2C") %>%
  select(Year, cat2, mod_scen, SecondaryEnergy.Electricity.Coal.wCCS, SecondaryEnergy.Electricity.Coal) %>%
  mutate(frac.Coal.wCCS = 100*SecondaryEnergy.Electricity.Coal.wCCS/SecondaryEnergy.Electricity.Coal) %>%
  #mutate(frac.Bio.wCCS = ifelse(pc.Biomass<1, NA, frac.Bio.wCCS)) %>% # make bio abated frac NA if bio provides < a certain % of all electricity
  select(Year, cat2, mod_scen, frac.Coal.wCCS) %>%
  #filter(Year %in% c(2030, 2040)) %>%
  drop_na() %>%
  rename(Pathway = cat2) %>%
  mutate(Pathway = ifelse(Pathway == "<1.5C", "1.5C", 
                          ifelse(Pathway == "1.5-2C", "2C", NA))) %>%
  group_by(Year, Pathway) %>%
  summarise(med=median(frac.Coal.wCCS),
            q25=quantile(frac.Coal.wCCS, 0.25),
            q75=quantile(frac.Coal.wCCS, 0.75))

coal_pc_wCCS_ind <- runs %>% filter(Year %in% c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100),
                                 mod_scen %in% coal_ms_keep,
                                 cat2 %in% "<1.5C") %>%
  select(Year, cat2, mod_scen, pc.Coal, SecondaryEnergy.Electricity.Coal.wCCS, SecondaryEnergy.Electricity.Coal) %>%
  mutate(frac.Coal.wCCS = 100*SecondaryEnergy.Electricity.Coal.wCCS/SecondaryEnergy.Electricity.Coal) %>%
  #mutate(frac.Bio.wCCS = ifelse(pc.Biomass<1, NA, frac.Bio.wCCS)) %>% # make bio abated frac NA if bio provides < a certain % of all electricity
  select(Year, cat2, mod_scen, frac.Coal.wCCS) %>%
  #filter(Year %in% c(2030, 2040)) %>%
  drop_na() %>%
  rename(Pathway = cat2) %>%
  mutate(Pathway = ifelse(Pathway == "<1.5C", "1.5C", 
                          ifelse(Pathway == "1.5-2C", "2C", NA)))

# PLOT
png(file=paste0("plots/coal_frac_abated_min_",min_2050,"_",reg_lab,".png"),width=1200,height=800,res=220,type='cairo')
ggplot(coal_pc_wCCS_med, aes(x=Year)) + #filter(bio_pc_wCCS, !(Year>2060 & Pathway=="<1.5C")), 
  geom_ribbon(aes(ymin=q25, ymax=q75, group=Pathway), alpha=0.3, fill="lightblue") +
  geom_line(aes(y=med, group=Pathway, colour=Pathway), size=1) +
  geom_line(data=coal_pc_wCCS_ind, aes(y=frac.Coal.wCCS, x=Year, group=mod_scen, colour=Pathway), inherit.aes = F, alpha=0.7) +
  #geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.1) +
  labs(y="Percentage abated", x="", title="By 2040, coal is either off the system or at least 50% abated", 
       subtitle=paste0("Models that provide >",min_2050,"% of electricity from coal in 2050")) +
  theme(text=element_text(size=8),
        axis.text = element_text(size=8)) +
  scale_y_continuous(labels=c("0%", "25%", "50%", "75%", "100%")) +
  scale_x_continuous(breaks=c(2020,2030,2040,2050,2060,2070,2080,2090,2100), limits=c(2020,2100)) +
  theme(text=element_text(size=8),
        axis.text = element_text(size=8),
        legend.text = element_text(size=9)) 
dev.off()

# ---- Gas CCS ----
# Line version
png(file=paste0("plots/gas_frac_abated_",reg_lab,".png"),width=1200,height=1000,res=220,type='cairo')
runs %>% filter(Year %in% c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)) %>%
  select(Year, cat2, mod_scen, SecondaryEnergy.Electricity, SecondaryEnergy.Electricity.Gas, SecondaryEnergy.Electricity.Gas.wCCS) %>%
  mutate(pc.Gas = 100*SecondaryEnergy.Electricity.Gas/SecondaryEnergy.Electricity,
         pc.Gas.wCCS = 100*SecondaryEnergy.Electricity.Gas.wCCS/SecondaryEnergy.Electricity.Gas) %>%
  mutate(pc.Gas.wCCS = ifelse(pc.Gas<5, NA, pc.Gas.wCCS)) %>% # make gas abated frac NA if gas provides < a certain % of all electricity
  select(Year, cat2, mod_scen, pc.Gas.wCCS) %>%
  drop_na() %>%
  ggplot(aes(x=Year, y=pc.Gas.wCCS, group=mod_scen)) +
  geom_line(aes(colour=cat2), size=0.8, alpha=0.8) +
  labs(y="", x="", title="Percentage of gas generation abated", 
       subtitle=paste0("While gas provides >5% of electricity.\nRegion: ",reg_lab,"\n",t_lab)) +
  theme(text=element_text(size=8),
        axis.text = element_text(size=8)) +
  scale_y_continuous(labels=c("0%", "25%", "50%", "75%", "100%"))
dev.off()

# Boxplot version
runs %>% filter(Year %in% c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)) %>%
  select(Year, cat2, mod_scen, SecondaryEnergy.Electricity, SecondaryEnergy.Electricity.Gas, SecondaryEnergy.Electricity.Gas.wCCS) %>%
  mutate(pc.Gas = 100*SecondaryEnergy.Electricity.Gas/SecondaryEnergy.Electricity,
         pc.Gas.wCCS = 100*SecondaryEnergy.Electricity.Gas.wCCS/SecondaryEnergy.Electricity.Gas) %>%
  mutate(pc.Gas.wCCS = ifelse(pc.Gas<5, NA, pc.Gas.wCCS)) %>% # make gas abated frac NA if gas provides < a certain % of all electricity
  select(Year, cat2, mod_scen, pc.Gas.wCCS) %>%
  #filter(Year %in% c(2030, 2040)) %>%
  drop_na() %>%
  ggplot(aes(x=factor(Year), y=pc.Gas.wCCS, colour=factor(cat2))) +
  geom_boxplot() +
  #geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.1) +
  labs(y="", x="", title="Percentage of gas generation abated", 
       subtitle=paste0("While gas provides >5% of electricity.\nRegion: ",reg_lab,"\n",t_lab)) +
  theme(text=element_text(size=8),
        axis.text = element_text(size=8)) +
  scale_y_continuous(labels=c("0%", "25%", "50%", "75%", "100%"))

# TEST: what is the percentage coal?
ggplot(runs[!is.na(runs$pc.Gas),], aes(x=Year, y=pc.Gas)) +
  geom_line(aes(group=mod_scen, colour=cat2))
ggplot(filter(runs, Year %in% c(2040, 2050, 2070)), aes(x=pc.Gas)) +
  geom_histogram(aes(fill=cat2), binwidth=0.2) +
  geom_vline(xintercept=5) +
  facet_wrap(~Year, nrow=3)

# median + IQR version
# Fraction abated
min_2050_gas <- 2
gas_ms_keep <- runs$mod_scen[runs$pc.Gas > min_2050_gas & runs$Year==2050]
# DATA
gas_pc_wCCS_med <- runs %>% filter(Year %in% c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100),
                                    mod_scen %in% gas_ms_keep,
                                    cat2 %in% "1.5-2C") %>%
  select(Year, cat2, mod_scen, SecondaryEnergy.Electricity.Gas.wCCS, SecondaryEnergy.Electricity.Gas) %>%
  mutate(frac.Gas.wCCS = 100*SecondaryEnergy.Electricity.Gas.wCCS/SecondaryEnergy.Electricity.Gas) %>%
  #mutate(frac.Bio.wCCS = ifelse(pc.Biomass<1, NA, frac.Bio.wCCS)) %>% # make bio abated frac NA if bio provides < a certain % of all electricity
  select(Year, cat2, mod_scen, frac.Gas.wCCS) %>%
  #filter(Year %in% c(2030, 2040)) %>%
  drop_na() %>%
  rename(Pathway = cat2) %>%
  mutate(Pathway = ifelse(Pathway == "<1.5C", "1.5C", 
                          ifelse(Pathway == "1.5-2C", "2C", NA))) %>%
  group_by(Year, Pathway) %>%
  summarise(med=median(frac.Gas.wCCS),
            q25=quantile(frac.Gas.wCCS, 0.25),
            q75=quantile(frac.Gas.wCCS, 0.75))

gas_pc_wCCS_ind <- runs %>% filter(Year %in% c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100),
                                    mod_scen %in% gas_ms_keep,
                                    cat2 %in% "<1.5C",
                                   pc.Gas > min_2050_gas) %>%
  select(Year, cat2, mod_scen, pc.Coal, SecondaryEnergy.Electricity.Gas.wCCS, SecondaryEnergy.Electricity.Gas) %>%
  mutate(frac.Gas.wCCS = 100*SecondaryEnergy.Electricity.Gas.wCCS/SecondaryEnergy.Electricity.Gas) %>%
  #mutate(frac.Bio.wCCS = ifelse(pc.Biomass<1, NA, frac.Bio.wCCS)) %>% # make bio abated frac NA if bio provides < a certain % of all electricity
  select(Year, cat2, mod_scen, frac.Gas.wCCS) %>%
  #filter(Year %in% c(2030, 2040)) %>%
  drop_na() %>%
  rename(Pathway = cat2) %>%
  mutate(Pathway = ifelse(Pathway == "<1.5C", "1.5C", 
                          ifelse(Pathway == "1.5-2C", "2C", NA)))

# PLOT
png(file=paste0("plots/gas_frac_abated_min_",min_2050_gas,"_",reg_lab,".png"),width=1200,height=800,res=220,type='cairo')
ggplot(gas_pc_wCCS_med, aes(x=Year)) +  
  geom_ribbon(aes(ymin=q25, ymax=q75, group=Pathway), alpha=0.3, fill="lightblue") +
  geom_line(aes(y=med, group=Pathway, colour=Pathway), size=1) +
  geom_line(data=gas_pc_wCCS_ind, aes(y=frac.Gas.wCCS, x=Year, group=mod_scen, colour=Pathway), inherit.aes = F, alpha=0.7) +
  #geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.1) +
  labs(y="Percentage abated", x="", title="Half of remaining gas generation is abated by 2040, rising to 75% by 2040.", 
       subtitle=paste0("Models that provide >",min_2050_gas,"% of electricity from gas in 2050")) +
  theme(text=element_text(size=8),
        axis.text = element_text(size=8)) +
  scale_y_continuous(labels=c("0%", "25%", "50%", "75%", "100%")) +
  scale_x_continuous(breaks=c(2020,2030,2040,2050,2060,2070,2080,2090,2100), limits=c(2020,2100)) +
  theme(text=element_text(size=8),
        axis.text = element_text(size=8),
        legend.text = element_text(size=9)) 
dev.off()

# ---- CCS by fuel and 1.5C vs 2C ----
#png(file=paste0("plots/elec_demand_ppchange_2020-30-50_",str_replace(t_lab,"<",""),".png"),width=1400,height=1000,res=220,type='cairo')
runs %>% 
  filter(Year %in% c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)) %>%
  select(mod_scen, Year, cat2, CarbonSequestration.CCS.Biomass, CarbonSequestration.CCS.Fossil) %>%
  pivot_longer(CarbonSequestration.CCS.Biomass:CarbonSequestration.CCS.Fossil, names_to="Fuel", values_to="Carbon", names_prefix = "CarbonSequestration.CCS.") %>%
  ggplot(aes(x=Fuel, y=Carbon)) +
  geom_boxplot(aes(colour=cat2)) +
  transition_time(Year) +
  ease_aes('linear') +
  labs(title = "Carbon sequestration in 1.5C and 2C models",
       subtitle = 'Year: {frame_time}',
       #subtitle=paste0("With respect to 2020 (",t_lab,")"),
       x="",
       y="GtCO2",
       colour="Warming") +
  scale_colour_discrete(labels=c("<1.5C"="1.5C", "1.5-2C"="2C")) +
  theme(text=element_text(size=18),
        title=element_text(size=14))
anim_save("plots/carbonSeq_animation.gif")
  #scale_y_continuous(breaks=c(0, 10, 20, 30, 40), labels = c("0%", "10%", "20%", "30%", "40%")) +
  #scale_x_discrete((labels=c("")))

runs %>% 
  filter(Year %in% c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)) %>%
  select(mod_scen, Year, cat2, pc.Biomass.wCCS, pc.Fossil.wCCS) %>%
  pivot_longer(pc.Biomass.wCCS:pc.Fossil.wCCS, names_to="Fuel", values_to="Gen.pc", names_prefix = "pc.") %>%
  ggplot(aes(x=Fuel, y=Gen.pc)) +
  geom_boxplot(aes(colour=cat2), varwidth=T) +
  transition_time(Year) +
  ease_aes('linear') +
  labs(title = "Share of electricity from abated sources in 1.5C and 2C models",
       subtitle = 'Year: {frame_time}',
       #subtitle=paste0("With respect to 2020 (",t_lab,")"),
       x="",
       y="% electricity",
       colour="Warming") +
  scale_colour_discrete(labels=c("<1.5C"="1.5C", "1.5-2C"="2C")) +
  theme(text=element_text(size=18),
        title=element_text(size=14))
anim_save("plots/CCS_gen_animation.gif")

#dev.off()
######### OUTLIERS ############
# ---- High coal ----
# Histogram of coal in given year <1.5C
hist(runs$SecondaryEnergy.Electricity.Coal.woCCS[runs$Year==2040 & runs$cat2 %in% "<1.5C"], 20)
# Histogram of coal in given year 1.5-2C
hist(runs$SecondaryEnergy.Electricity.Coal.woCCS[runs$Year==2030 & runs$cat2 %in% "1.5-2C"], 20)

#out1 <- c("GCAM 4.2 | SSP1-19", "AIM/CGE 2.0 | SSP1-19")
out1 <- runs$mod_scen[runs$cat2 %in% "<1.5C" & runs$Year==2040 & runs$SecondaryEnergy.Electricity.Coal.woCCS > quantile(runs$SecondaryEnergy.Electricity.Coal.woCCS[runs$cat2 %in% "<1.5C" & runs$Year==2040], 0.95, na.rm=T)]
#out2 <- c("AIM/CGE 2.0 | SSP1-26", "GCAM 4.2 | SSP1-26", "IMAGE 3.0.1 | SSP1-26", "POLES EMF33 | EMF33_tax_hi_none")
out2 <- runs$mod_scen[runs$cat2 %in% "1.5-2C" & runs$Year==2040 & runs$SecondaryEnergy.Electricity.Coal.woCCS > quantile(runs$SecondaryEnergy.Electricity.Coal.woCCS[runs$cat2 %in% "1.5-2C" & runs$Year==2040], 0.95, na.rm=T)]

runs_out <- filter(runs, mod_scen %in% out1 | mod_scen %in% out2)

sz <- 0.5
ggplot(data=runs[!is.na(runs$SecondaryEnergy.Electricity.Coal.woCCS),], 
       aes(x=Year, 
           y=SecondaryEnergy.Electricity.Coal.woCCS, 
           group=mod_scen)) +
  geom_line(colour="grey50", alpha=0.7, size=sz) +
  geom_line(data=runs_out[!is.na(runs_out$SecondaryEnergy.Electricity.Coal.woCCS),], aes(colour=mod_scen), size=2*sz) +
  coord_cartesian(xlim=c(2010, 2060)) +
  facet_wrap(vars(cat2)) +
  labs(y="Unabated coal electricity (EJ)", x="") +
  scale_colour_viridis(discrete = T)

ggplot(data=runs[!is.na(runs$pc.Coal.woCCS),],
       aes(x=Year, 
           y=pc.Coal.woCCS, 
           group=mod_scen)) +
  geom_line(colour="grey50", alpha=0.7, size=sz) +
  geom_line(data=runs_out[!is.na(runs_out$pc.Coal.woCCS),], aes(colour=mod_scen), size=2*sz) +
  coord_cartesian(xlim=c(2010, 2060)) +
  facet_wrap(vars(cat2)) +
  labs(y="Coal share of electricity (%)", x="") +
  scale_colour_viridis(discrete=T)

ggplot(data=runs[!is.na(runs$Emissions.CO2.Energy.Supply.Electricity),],
       aes(x=Year, 
           y=Emissions.CO2.Energy.Supply.Electricity, 
           group=mod_scen)) +
  geom_hline(yintercept = 0, colour="grey20", size=1) +
  geom_line(colour="grey50", alpha=0.7, size=sz) +
  geom_line(data=runs_out[!is.na(runs_out$Emissions.CO2.Energy.Supply.Electricity),], aes(colour=mod_scen), size=2*sz) +
  coord_cartesian(xlim=c(2010, 2100), ylim=c(-6000, 15000)) +
  facet_wrap(vars(cat2)) +
  labs(y="Emissions from electricity (Mt CO2)", x="") +
  scale_colour_viridis(discrete=T)

ggplot(data=runs[!is.na(runs$Emissions.CO2),],
       aes(x=Year, 
           y=Emissions.CO2, 
           group=mod_scen)) +
  geom_hline(yintercept = 0, colour="grey20", size=1) +
  geom_line(colour="grey50", alpha=0.7, size=sz) +
  geom_line(data=runs_out[!is.na(runs_out$Emissions.CO2),], aes(colour=mod_scen), size=2*sz) +
  coord_cartesian(xlim=c(2010, 2100)) + #ylim=c(-6000, 15000)) +
  facet_wrap(vars(cat2)) +
  labs(y="Emissions from transport (Mt CO2)", x="") +
  scale_colour_viridis(discrete=T)

max(runs$SecondaryEnergy.Electricity.Coal.wCCS/runs$SecondaryEnergy.Electricity, na.rm=T)

# ==== WRITE RESULTS ====

write.csv(runs_summary, file=paste0("data/runs_summary_",reg_lab,"_",str_replace(t_lab,"<",""),".csv"), row.names=F)

