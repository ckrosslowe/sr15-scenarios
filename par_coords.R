# script to read different samples of ipcc scenarios and summarise power info
library(tidyverse)
library(readxl)
library(viridis)
library(cowplot)
library(plotly)


# Variables explained here https://data.ene.iiasa.ac.at/iamc-1.5c-explorer/#/docs

# ==== READ data ====
# Read metadata
meta <- read_excel("data/sr15_metadata_indicators_r2.0.xlsx", sheet="meta") %>%
  mutate(mod_scen = str_c(model, scenario, sep=" | "))

# Read model data
runs_clean <- read.csv("data/runs_clean.csv", header=T, stringsAsFactors = F)
# duplicate (keep raw clean data)
runs <- runs_clean

# ==== Filter scenarios ====

# --- Temperature - NOT APPLIED
#temp_cats <- c("1.5C low overshoot", "Below 1.5C")
temp_cats <- c("1.5C low overshoot", "Below 1.5C", "1.5C high overshoot", "Lower 2C", "Higher 2C")

temp_ms <- meta$mod_scen[meta$category %in% temp_cats]
# Select runs that meet temp criteria
runs <- runs %>% filter(mod_scen %in% temp_ms)

#t_lab <- "<1.5C"
#t_folder <- "temp_1p5C"
#t_lab <- "<2C"
#t_folder <- "temp_2C"

# --- BECCS & Bioenergy - global
# Global bioenergy use in 2050
hist(runs$PrimaryEnergy.Biomass[runs$Year==2050 & runs$Region %in% "World"])
bio_lim <- 100

# Global BECCS use in 2050
hist(runs$CarbonSequestration.CCS.Biomass[runs$Year==2050 & runs$Region %in% "World"], 12)
beccs_lim <- 5000

# Which models meet these criteria at a global level in 2050?
keep_ms <- runs$mod_scen[runs$Year==2050 & runs$Region %in% "World" & runs$CarbonSequestration.CCS.Biomass<=beccs_lim & runs$PrimaryEnergy.Biomass<=bio_lim]

# --- Region
#reg <- "R5OECD90+EU"    # IPCC name
#reg_lab <- "OECD90_EU"  # GER name and chart label 

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

reg <- "World"
reg_lab <- "World"

# --- FILTER runs
runs <- filter(runs, mod_scen %in% keep_ms, Region %in% reg)


# --- REMOVE runs that don't include selected breakdowns
if (!reg  %in% "World") runs <- filter(runs, !mod_scen %in% "MESSAGEix-GLOBIOM 1.0 | LowEnergyDemand")


# ---- PARALLEL COORDINATES PLOT ----

runs_par <- filter(runs, Year==2050) %>%
  left_join(meta[c("mod_scen", "category")], by="mod_scen")
#runs_par$category[1:2] <- "Test"
runs_par$cat_num <- NA
runs_par$cat_num[runs_par$category %in% "Below 1.5C"] <- 1 
runs_par$cat_num[runs_par$category %in% "1.5C low overshoot"] <- 2 
runs_par$cat_num[runs_par$category %in% "1.5C high overshoot"] <- 3 
runs_par$cat_num[runs_par$category %in% "Lower 2C"] <- 4 
runs_par$cat_num[runs_par$category %in% "Higher 2C"] <- 5 



fig <- runs_par %>%
  plot_ly(width = 1000, height = 600) %>% 
  add_trace(type = 'parcoords',
                         line = list(color = ~cat_num,
                                     colorscale = 'viridis',
                                     showscale = F
                                     #reversescale = FALSE,
                                     #cmin = 1,
                                     #cmax = 2
                         ),
                         dimensions = list(
                           list(range = c(1,5),
                                tickvals = c(1,2,3,4,5),
                                ticktext = c("Below 1.5C", "1.5C low overshoot", "1.5C high overshoot", "Lower 2C", "Higher 2C"),
                                constraintrange = c(1.5,2.5),
                                visible = T,
                                #label = 'Warming', 
                                values = ~cat_num),
                           list(range = c(~min(SecondaryEnergy.Electricity),~max(SecondaryEnergy.Electricity)),
                                #constraintrange = c(100000,150000),
                                label = 'Consumption', values = ~SecondaryEnergy.Electricity),
                           list(range = c(~min(SecondaryEnergy.Electricity.Coal.woCCS),~max(SecondaryEnergy.Electricity.Coal.woCCS)),
                                #visible = TRUE,
                                label = 'Coal', 
                                values = ~SecondaryEnergy.Electricity.Coal.woCCS),
                           list(range = c(~min(SecondaryEnergy.Electricity.Gas.woCCS),~max(SecondaryEnergy.Electricity.Gas.woCCS)),
                                #visible = TRUE, #activates slider
                                label = 'Gas', 
                                values = ~SecondaryEnergy.Electricity.Gas.woCCS),
                           list(range = c(~min(SecondaryEnergy.Electricity.Nuclear),~max(SecondaryEnergy.Electricity.Nuclear)),
                                label = 'Nuclear', values = ~SecondaryEnergy.Electricity.Nuclear),
                           list(range = c(~min(SecondaryEnergy.Electricity.Renewables),~max(SecondaryEnergy.Electricity.Renewables)),
                                label = 'Renewables', values = ~SecondaryEnergy.Electricity.Renewables),
                           list(range = c(~min(SecondaryEnergy.Electricity.Biomass),~max(SecondaryEnergy.Electricity.Biomass)),
                                label = 'Biomass (all)', values = ~SecondaryEnergy.Electricity.Biomass)
                         )
) %>%
  layout(margin = list(l=150, r=50, b=50, t=10),
         font = list(size=15))

fig


#save to plotly and create API
Sys.setenv("plotly_username"="ember")
Sys.setenv("plotly_api_key"="lrXNfvv9FUMuTmp258WH")
api_create(fig, filename = "SR15 power scenarios")

