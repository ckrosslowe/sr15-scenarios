# ONE-OFF outlier analysis
# Use runs data set already filtered by scenario_analyse.R

# coal in 2040
filter(runs, Year==2040) %>% select(Year, mod_scen, SecondaryEnergy.Electricity.Coal.woCCS)
median(runs$SecondaryEnergy.Electricity.Coal.woCCS[runs$Year==2040])

# demand in 2040
filter(runs, Year==2040) %>% select(Year, mod_scen, SecondaryEnergy.Electricity)
median(runs$SecondaryEnergy.Electricity[runs$Year==2040])

# gas in 2040
filter(runs, Year==2040) %>% select(Year, mod_scen, SecondaryEnergy.Electricity.Gas.woCCS)
median(runs$SecondaryEnergy.Electricity.Gas.woCCS[runs$Year==2040])

# Renewables in 2040
filter(runs, Year==2040) %>% select(Year, mod_scen, SecondaryEnergy.Electricity.Renewables)
median(runs$SecondaryEnergy.Electricity.Renewables[runs$Year==2040])

# Biomass in 2040
filter(runs, Year==2040) %>% select(Year, mod_scen, SecondaryEnergy.Electricity.Biomass)
median(runs$SecondaryEnergy.Electricity.Biomass[runs$Year==2040])

# BECCS in 2040
filter(runs, Year==2040) %>% select(Year, mod_scen, SecondaryEnergy.Electricity.Biomass.wCCS)
median(runs$SecondaryEnergy.Electricity.Biomass.wCCS[runs$Year==2040])

# BECCS in 2070
filter(runs, Year==2070) %>% select(Year, mod_scen, SecondaryEnergy.Electricity.Biomass.wCCS)
median(runs$SecondaryEnergy.Electricity.Biomass.wCCS[runs$Year==2070])

# Carbon seq land use in 2070
filter(runs, Year==2070) %>% select(Year, mod_scen, CarbonSequestration.LandUse)
median(runs$CarbonSequestration.LandUse[runs$Year==2070], na.rm=T)

# Carbon seq land use in 2100
filter(runs, Year==2100) %>% select(Year, mod_scen, CarbonSequestration.LandUse)
median(runs$CarbonSequestration.LandUse[runs$Year==2100], na.rm=T)

# Carbon capture total in 2070
filter(runs, Year==2070) %>% select(Year, mod_scen, CarbonSequestration.CCS)
median(runs$CarbonSequestration.CCS[runs$Year==2070], na.rm=T)

# In this scenario, gas stays below 20EJ (2015 levels) and is one of the earliest to peak (shortly after 2020)
filter(runs, mod_scen %in% "AIM/CGE 2.0 | SSP1-19", !is.na(SecondaryEnergy.Electricity.Gas)) %>% select(Year, SecondaryEnergy.Electricity.Gas)

