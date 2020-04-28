library(tidyverse)
library(readxl)

# Variables explained here https://data.ene.iiasa.ac.at/iamc-1.5c-explorer/#/docs

# ==== READ data ====

# Read model data
models <- read.csv("data/iamc15_scenario_data_all_regions_r2.0.csv", header=T) %>%
  mutate(mod_scen = str_c(Model, Scenario, sep=" | "))

# Read metadata
meta <- read_excel("data/sr15_metadata_indicators_r2.0.xlsx", sheet="meta") %>%
  mutate(mod_scen = str_c(model, scenario, sep=" | "))

# summarise variable availability
mod_sum <- models %>%
  left_join(meta[c("mod_scen", "category")], by="mod_scen") %>%
  #filter(Region %in% "World") %>%
  mutate(Temp_cat = ifelse(category %in% c("1.5C low overshoot", "Below 1.5C"), "1.5C", 
                       ifelse(category %in% c("1.5C high overshoot", "Lower 2C", "Higher 2C"), "2C", "Other")),
         sum = rowSums(!is.na(.[6:106]))) %>%
  mutate(given = ifelse(sum==0, 0, 1)) %>%
  group_by(Region, Variable, Temp_cat) %>%
  summarise(n_runs = sum(given)) %>%
  ungroup() %>%
  pivot_wider(id_cols=c("Region", "Variable", "Temp_cat", "n_runs"), names_from=Temp_cat, values_from=n_runs) %>% 
  replace(is.na(.), 0) %>%
  mutate(All = rowSums(.[3:5])) %>%
  write.csv("data/variables_sum.csv", row.names=F)

# Throw away some variables
models <- models %>% filter(!grepl("^Agricultural", Variable),
                            !grepl("^AR5", Variable),
                            !grepl("^Forcing", Variable),
                            !grepl("^Water", Variable)
)
                            

# Re-structure model data
runs <- models %>%
  pivot_longer(X2000:X2100, names_to="Year", values_to="Value") %>%
  pivot_wider(id_cols=c("Model", "Scenario", "Region", "Variable", "Year", "Value"), names_from=Variable, values_from=Value, names_repair="unique") %>%
  mutate(mod_scen = str_c(Model, Scenario, sep=" | "),
         Year = as.numeric(str_sub(Year,2,5)))
colnames(runs) <- gsub(" ", "", colnames(runs))
colnames(runs) <- gsub("\\|", ".", colnames(runs))
colnames(runs) <- gsub("\\/", "", colnames(runs))

# GER data
# Map GER data into model regions
OECD90_EU <- c("Albania", "Australia", "Austria",
               "Belgium", "Bosnia and Herzegovina", "Bulgaria",
               "Canada", "Croatia", "Cyprus", "Czech Republic",
               "Denmark",
               "Estonia",
               "Finland", "France",
               "Germany", "Greece", "Guam",
               "Hungary",
               "Iceland", "Ireland", "Italy",
               "Japan",
               "Kosovo",
               "Latvia", "Lithuania", "Luxembourg",
               "Malta", "Montenegro",
               "Netherlands", "New Zealand", "Norway",
               "Poland", "Portugal", "Puerto Rico",
               "Romania",
               "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland",
               "Macedonia",
               "Turkey",
               "United Kingdom", "United States"
)

MAF <- c("Algeria", "Angola",
         "Bahrain", "Benin", "Botswana", "Burkina Faso", "Burundi",
         "Cameroon", "Cape Verde", "Central African Republic", "Chad", "Comoros", "Congo-Brazzaville", "Cote dIvoire",
         "Congo-Kinshasa", "Djibouti",
         "Egypt", "Equatorial Guinea", "Eritrea", "Ethiopia",
         "Gabon", "Gambia, The", "Ghana", "Guinea", "Guinea-Bissau",
         "Iran", "Iraq", "Israel",
         "Jordan",
         "Kenya", "Kuwait",
         "Lebanon", "Lesotho", "Liberia", "Libya",
         "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique",
         "Namibia", "Niger", "Nigeria",
         "Palestinian Territories", "Oman",
         "Qatar",
         "Rwanda", "Reunion",
         "Saudi Arabia", "Senegal", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", "Swaziland", "Syria",
         "Togo", "Tunisia",
         "Uganda", "United Arab Emirates", "Tanzania", 
         "Western Sahara",
         "Yemen",
         "Zambia",
         "Zimbabwe"
)

LAM <- c("Argentina", "Aruba",
         "The Bahamas", "Barbados", "Belize", "Bolivia", "Brazil",
         "Chile", "Colombia", "Costa Rica", "Cuba",
         "Dominican Republic",
         "Ecuador", "El Salvador",
         "French Guiana",
         "Grenada", "Guadeloupe", "Guatemala", "Guyana",
         "Haiti", "Honduras",
         "Jamaica",
         "Martinique", "Mexico",
         "Nicaragua",
         "Panama", "Paraguay", "Peru",
         "Suriname",
         "Trinidad and Tobago",
         "U.S. Virgin Islands",
         "Uruguay",
         "Venezuela"
)

ASIA <- c("Afghanistan",
          "Bangladesh", "Bhutan", "Brunei",
          "Cambodia", "China",
          "North Korea",
          "Fiji", "French Polynesia",
          "Hong Kong",
          "India", "Indonesia",
          "Laos",
          "Malaysia", "Maldives", "Micronesia", "Mongolia", "Myanmar",
          "Nepal", "New Caledonia",
          "Pakistan", "Papua New Guinea", "Philippines",
          "South Korea",
          "Samoa", "Singapore", "Solomon Islands", "Sri Lanka",
          "Taiwan", "Thailand", "Timor-Leste",
          "Vanuatu", "Vietnam"
)

REF <- c("Armenia", "Azerbaijan",
         "Belarus",
         "Georgia",
         "Kazakhstan", "Kyrgyzstan",
         "Moldova", "Russia",
         "Tajikistan", "Turkmenistan",
         "Ukraine", "Uzbekistan"
)


# Read actual generation data from GER
ger <- read.csv("data/global_electricity_review_2020_v2.csv", header = T) %>%
  filter(!Country %in% c("Rest of World", "World", "EU"),
         !Type %in% c("Production", "Demand", "Net imports")) %>%
  mutate(Region_ipcc = ifelse(Country %in% OECD90_EU, "OECD90_EU", 
                              ifelse(Country %in% MAF, "MAF",
                                     ifelse(Country %in% LAM, "LAM",
                                            ifelse(Country %in% ASIA, "ASIA",
                                                   ifelse(Country %in% REF, "REF", "ROW")))))) %>%
  mutate(Type2 = ifelse(Type %in% "Coal", "Coal", 
                        ifelse(Type %in% "Gas", "Gas", 
                               ifelse(Type %in% c("Solar", "Wind", "Hydro"), "Renewables",
                                      ifelse(Type %in% "Nuclear", "Nuclear", "Other")))))


# ==== Clean data ====

# Exclude reference scenarios, and scenarios that don't account for carbon sequestration (Shell World Energy) or bioenergy (C-ROADS)
mod_exclude <- c("Reference", "C-ROADS-5.005", "Shell World Energy Model 2018")
runs <- filter(runs, !Model %in% mod_exclude)

# Throw away some variables

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
# Create Fossil, Fossil CCS, and Fossil woCCS variables
runs <- runs %>% mutate(SecondaryEnergy.Electricity.Renewables = SecondaryEnergy.Electricity.Hydro + SecondaryEnergy.Electricity.Solar + SecondaryEnergy.Electricity.Wind + SecondaryEnergy.Electricity.Ocean + SecondaryEnergy.Electricity.Geothermal,
                        SecondaryEnergy.Electricity.Fossil.wCCS = SecondaryEnergy.Electricity.Coal.wCCS + SecondaryEnergy.Electricity.Gas.wCCS + SecondaryEnergy.Electricity.Oil.wCCS,
                        SecondaryEnergy.Electricity.Fossil.woCCS = SecondaryEnergy.Electricity.Coal.woCCS + SecondaryEnergy.Electricity.Gas.woCCS + SecondaryEnergy.Electricity.Oil.woCCS,
                        SecondaryEnergy.Electricity.Fossil2 = SecondaryEnergy.Electricity.Coal + SecondaryEnergy.Electricity.Gas + SecondaryEnergy.Electricity.Oil
)

runs <- runs %>% mutate(pc.Renewables = 100*SecondaryEnergy.Electricity.Renewables/SecondaryEnergy.Electricity,
                        pc.Fossil = 100*SecondaryEnergy.Electricity.Fossil2/SecondaryEnergy.Electricity,
                        pc.Nuclear = 100*SecondaryEnergy.Electricity.Nuclear/SecondaryEnergy.Electricity,
                        pc.Biomass = 100*SecondaryEnergy.Electricity.Biomass/SecondaryEnergy.Electricity,
                        pc.Gas = 100*SecondaryEnergy.Electricity.Gas/SecondaryEnergy.Electricity,
                        pc.Coal = 100*SecondaryEnergy.Electricity.Coal/SecondaryEnergy.Electricity,
                        pc.Gas.woCCS = 100*SecondaryEnergy.Electricity.Gas.woCCS/SecondaryEnergy.Electricity
)

# ==== Write clean data ====

write.csv(runs, file="data/runs_clean.csv", row.names=F)
write.csv(ger, file="data/ger_ipcc.csv", row.names=F)

