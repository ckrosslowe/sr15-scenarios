# ONE-OFF outlier analysis
# Use runs data set already filtered by scenario_analyse.R

outlier_runs <- runs$mod_scen[runs$Year==2040 & runs$SecondaryEnergy.Electricity.Gas.woCCS > 18]
runs_out <- filter(runs, mod_scen %in% outlier_runs)

# ---- Stats ----

# coal in 2040
filter(runs_out, Year==2040) %>% select(Year, mod_scen, SecondaryEnergy.Electricity.Coal.woCCS)
median(runs$SecondaryEnergy.Electricity.Coal.woCCS[runs$Year==2040])
quantile(runs$SecondaryEnergy.Electricity.Coal.woCCS[runs$Year==2040], 0.25)
quantile(runs$SecondaryEnergy.Electricity.Coal.woCCS[runs$Year==2040], 0.75)

# coal CCS in 2040
filter(runs, Year==2040) %>% select(Year, mod_scen, SecondaryEnergy.Electricity.Coal.wCCS)
median(runs$SecondaryEnergy.Electricity.Coal.wCCS[runs$Year==2040])
quantile(runs$SecondaryEnergy.Electricity.Coal.wCCS[runs$Year==2040], 0.25)
quantile(runs$SecondaryEnergy.Electricity.Coal.wCCS[runs$Year==2040], 0.75)

# demand in 2040
filter(runs, Year==2040) %>% select(Year, mod_scen, SecondaryEnergy.Electricity)
median(runs$SecondaryEnergy.Electricity[runs$Year==2040])
quantile(runs$SecondaryEnergy.Electricity[runs$Year==2040], 0.25)
quantile(runs$SecondaryEnergy.Electricity[runs$Year==2040], 0.75)

# gas in 2040
filter(runs_out, Year==2040) %>% select(Year, mod_scen, SecondaryEnergy.Electricity.Gas.woCCS)
median(runs$SecondaryEnergy.Electricity.Gas.woCCS[runs$Year==2040])
quantile(runs$SecondaryEnergy.Electricity.Gas.woCCS[runs$Year==2040], 0.25)
quantile(runs$SecondaryEnergy.Electricity.Gas.woCCS[runs$Year==2040], 0.75)

# Renewables in 2040
filter(runs, Year==2040) %>% select(Year, mod_scen, SecondaryEnergy.Electricity.Renewables)
median(runs$SecondaryEnergy.Electricity.Renewables[runs$Year==2040])
quantile(runs$SecondaryEnergy.Electricity.Renewables[runs$Year==2040], 0.25)
quantile(runs$SecondaryEnergy.Electricity.Renewables[runs$Year==2040], 0.75)

# Nuclear in 2040
filter(runs, Year==2040) %>% select(Year, mod_scen, SecondaryEnergy.Electricity.Nuclear)
median(runs$SecondaryEnergy.Electricity.Nuclear[runs$Year==2040])
quantile(runs$SecondaryEnergy.Electricity.Nuclear[runs$Year==2040], 0.25)
quantile(runs$SecondaryEnergy.Electricity.Nuclear[runs$Year==2040], 0.75)

# Biomass in 2040
filter(runs, Year==2040) %>% select(Year, mod_scen, SecondaryEnergy.Electricity.Biomass)
median(runs$SecondaryEnergy.Electricity.Biomass[runs$Year==2040])
quantile(runs$SecondaryEnergy.Electricity.Biomass[runs$Year==2040], 0.25)
quantile(runs$SecondaryEnergy.Electricity.Biomass[runs$Year==2040], 0.75)

# Unabated Biomass in 2040
filter(runs_out, Year==2040) %>% select(Year, mod_scen, SecondaryEnergy.Electricity.Biomass.woCCS)
median(runs$SecondaryEnergy.Electricity.Biomass.woCCS[runs$Year==2040])
quantile(runs$SecondaryEnergy.Electricity.Biomass.woCCS[runs$Year==2040], 0.25)
quantile(runs$SecondaryEnergy.Electricity.Biomass.woCCS[runs$Year==2040], 0.75)

# BECCS in 2040
filter(runs_out, Year==2040) %>% select(Year, mod_scen, SecondaryEnergy.Electricity.Biomass.wCCS)
median(runs$SecondaryEnergy.Electricity.Biomass.wCCS[runs$Year==2040])
quantile(runs$SecondaryEnergy.Electricity.Biomass.wCCS[runs$Year==2040], 0.25)
quantile(runs$SecondaryEnergy.Electricity.Biomass.wCCS[runs$Year==2040], 0.75)

# Fossil CCS in 2040
filter(runs, Year==2040) %>% select(Year, mod_scen, SecondaryEnergy.Electricity.Fossil.wCCS)
median(runs$SecondaryEnergy.Electricity.Fossil.wCCS[runs$Year==2040])
quantile(runs$SecondaryEnergy.Electricity.Fossil.wCCS[runs$Year==2040], 0.25)
quantile(runs$SecondaryEnergy.Electricity.Fossil.wCCS[runs$Year==2040], 0.75)

# BECCS in 2070
filter(runs_out, Year==2070) %>% select(Year, mod_scen, SecondaryEnergy.Electricity.Biomass.wCCS)
filter(runs_out, Year==2070) %>% select(Year, mod_scen, CarbonSequestration.CCS.Biomass)
median(runs_out$SecondaryEnergy.Electricity.Biomass.wCCS[runs$Year==2070], na.rm=T)
median(runs_out$CarbonSequestration.CCS.Biomass[runs$Year==2070])
quantile(runs_out$SecondaryEnergy.Electricity.Biomass.wCCS[runs$Year==2070], 0.25, na.rm=T)
quantile(runs_out$SecondaryEnergy.Electricity.Biomass.wCCS[runs$Year==2070], 0.75, na.rm=T)
quantile(runs_out$CarbonSequestration.CCS.Biomass[runs$Year==2070], 0.25)
quantile(runs_out$CarbonSequestration.CCS.Biomass[runs$Year==2070], 0.75)

# BECCS in 2100
filter(runs, Year==2100) %>% select(Year, mod_scen, SecondaryEnergy.Electricity.Biomass.wCCS)
filter(runs, Year==2100) %>% select(Year, mod_scen, CarbonSequestration.CCS.Biomass)
median(runs$SecondaryEnergy.Electricity.Biomass.wCCS[runs$Year==2100])
median(runs$CarbonSequestration.CCS.Biomass[runs$Year==2100])
quantile(runs$SecondaryEnergy.Electricity.Biomass.wCCS[runs$Year==2100], 0.25)
quantile(runs$SecondaryEnergy.Electricity.Biomass.wCCS[runs$Year==2100], 0.75)
quantile(runs$CarbonSequestration.CCS.Biomass[runs$Year==2100], 0.25)
quantile(runs$CarbonSequestration.CCS.Biomass[runs$Year==2100], 0.75)

# Carbon seq land use in 2070
filter(runs, Year==2070) %>% select(Year, mod_scen, CarbonSequestration.LandUse)
median(runs$CarbonSequestration.LandUse[runs$Year==2070], na.rm=T)
quantile(runs$CarbonSequestration.LandUse[runs$Year==2070], 0.25, na.rm=T)
quantile(runs$CarbonSequestration.LandUse[runs$Year==2070], 0.75, na.rm=T)


# Carbon seq land use in 2100
filter(runs, Year==2100) %>% select(Year, mod_scen, CarbonSequestration.LandUse)
median(runs$CarbonSequestration.LandUse[runs$Year==2100], na.rm=T)
quantile(runs$CarbonSequestration.LandUse[runs$Year==2100], 0.25, na.rm=T)
quantile(runs$CarbonSequestration.LandUse[runs$Year==2100], 0.75, na.rm=T)


# Carbon capture total in 2070
filter(runs, Year==2070) %>% select(Year, mod_scen, CarbonSequestration.CCS)
median(runs$CarbonSequestration.CCS[runs$Year==2070], na.rm=T)
quantile(runs$CarbonSequestration.CCS[runs$Year==2070], 0.25, na.rm=T)
quantile(runs$CarbonSequestration.CCS[runs$Year==2070], 0.75, na.rm=T)

# Carbon capture total in 2100
filter(runs, Year==2100) %>% select(Year, mod_scen, CarbonSequestration.CCS)
median(runs$CarbonSequestration.CCS[runs$Year==2100], na.rm=T)
quantile(runs$CarbonSequestration.CCS[runs$Year==2100], 0.25, na.rm=T)
quantile(runs$CarbonSequestration.CCS[runs$Year==2100], 0.75, na.rm=T)

# In this scenario, gas stays below 20EJ (2015 levels) and is one of the earliest to peak (shortly after 2020)
filter(runs, mod_scen %in% "AIM/CGE 2.0 | SSP1-19", !is.na(SecondaryEnergy.Electricity.Gas)) %>% select(Year, SecondaryEnergy.Electricity.Gas)

# ---- Plots ----

sz <- 0.5

# power generation mix
coal_p <- ggplot(runs_out[!is.na(runs_out$SecondaryEnergy.Electricity.Coal), ],
                 aes(x=Year, y=SecondaryEnergy.Electricity.Coal, group=mod_scen)) + 
  geom_line(aes(color=mod_scen), size=1) +
  geom_line(data=runs[!is.na(runs$SecondaryEnergy.Electricity.Coal), ], size=sz, colour="grey30", alpha=0.3) +
  labs(x="", y="Coal (EJ)", title="") +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)

gas_p <- ggplot(runs_out[!is.na(runs_out$SecondaryEnergy.Electricity.Gas), ],
                aes(x=Year, y=SecondaryEnergy.Electricity.Gas, group=mod_scen)) + 
  geom_line(aes(color=mod_scen), size=1) +
  geom_line(data=runs[!is.na(runs$SecondaryEnergy.Electricity.Gas), ], size=sz, colour="grey30", alpha=0.3) +
  labs(x="", y="Gas (EJ)", title="") +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)

# Make 4 other plots, then use plot_grid() to arrange them. :)
nuc_p <- ggplot(runs_out[!is.na(runs_out$SecondaryEnergy.Electricity.Nuclear), ],
                aes(x=Year, y=SecondaryEnergy.Electricity.Nuclear, group=mod_scen)) + 
  geom_line(aes(color=mod_scen), size=1) +
  geom_line(data=runs[!is.na(runs$SecondaryEnergy.Electricity.Nuclear), ], size=sz, colour="grey30", alpha=0.3) +
  labs(x="", y="Nuclear (EJ)", title="") +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)

bio_p <- ggplot(runs_out[!is.na(runs_out$SecondaryEnergy.Electricity.Biomass), ],
                aes(x=Year, y=SecondaryEnergy.Electricity.Biomass, group=mod_scen)) + 
  geom_line(aes(color=mod_scen), size=1) +
  geom_line(data=runs[!is.na(runs$SecondaryEnergy.Electricity.Biomass), ], size=sz, colour="grey30", alpha=0.3) +
  labs(x="", y="Biomass (EJ)", title="") +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)

# Wind + Solar + Hydro 
renew_p <- ggplot(runs_out[!is.na(runs_out$SecondaryEnergy.Electricity.Hydro) & !is.na(runs_out$SecondaryEnergy.Electricity.Solar) & !is.na(runs_out$SecondaryEnergy.Electricity.Wind), ],# & !is.na(runs$SecondaryEnergy.Electricity.Geothermal), ],# & !is.na(runs$SecondaryEnergy.Electricity.Ocean), ],
                  aes(x=Year, 
                      y=SecondaryEnergy.Electricity.Hydro + SecondaryEnergy.Electricity.Wind + SecondaryEnergy.Electricity.Solar,# + SecondaryEnergy.Electricity.Geothermal,# + SecondaryEnergy.Electricity.Ocean, 
                      group=mod_scen)) + 
  geom_line(aes(color=mod_scen), size=1) +
  geom_line(data=runs[!is.na(runs$SecondaryEnergy.Electricity.Hydro) & !is.na(runs$SecondaryEnergy.Electricity.Solar) & !is.na(runs$SecondaryEnergy.Electricity.Wind), ], size=sz,colour="grey30", alpha=0.3) +
  labs(x="", y="renewables (EJ)", title="") +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)

# All CCS = Coal, Gas, Oil, Biomass
ccs_p <- ggplot(runs_out[!is.na(runs_out$SecondaryEnergy.Electricity.Coal.wCCS) & !is.na(runs_out$SecondaryEnergy.Electricity.Gas.wCCS) & !is.na(runs_out$SecondaryEnergy.Electricity.Biomass.wCCS) & !is.na(runs$SecondaryEnergy.Electricity.Oil.wCCS), ],
                aes(x=Year, 
                    y=(SecondaryEnergy.Electricity.Coal.wCCS+SecondaryEnergy.Electricity.Gas.wCCS+SecondaryEnergy.Electricity.Biomass.wCCS+SecondaryEnergy.Electricity.Oil.wCCS), 
                    group=mod_scen)) + 
  geom_line(aes(color=mod_scen), size=1) +
  geom_line(data=runs[!is.na(runs$SecondaryEnergy.Electricity.Coal.wCCS) & !is.na(runs$SecondaryEnergy.Electricity.Gas.wCCS) & !is.na(runs$SecondaryEnergy.Electricity.Biomass.wCCS) & !is.na(runs$SecondaryEnergy.Electricity.Oil.wCCS), ], size=sz, colour="grey30", alpha=0.3) +
  labs(x="", y="All CCS (EJ)", title="") +
  theme(legend.position = "none") +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)

png(file="plots/outlier_gas__2deg_elec_mix.png", width=600, height=800, res=150, type='cairo')
plot_grid(coal_p, gas_p, renew_p, bio_p, nuc_p, ccs_p, nrow=3, labels="Outliers: High unabated gas")
dev.off()

# CCS
coal_lims <- c(0, 1.1*max(runs$SecondaryEnergy.Electricity.Coal, na.rm=T))
gas_lims <- c(0, 1.1*max(runs$SecondaryEnergy.Electricity.Gas, na.rm=T))
bio_lims <- c(0, 1.1*max(runs$SecondaryEnergy.Electricity.Biomass, na.rm=T))
sz <- 0.5

coal_all_p <- ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Coal),], 
                     aes(x=Year, y=SecondaryEnergy.Electricity.Coal, group=mod_scen)) +
  geom_line(colour="grey20", alpha=0.3, size=sz) +
  geom_line(data=runs_out[!is.na(runs_out$SecondaryEnergy.Electricity.Coal),], aes(colour=mod_scen), size=1) +
  labs(x="", y="all Coal (EJ)", title="") +
  theme(legend.position = "none", 
        text = element_text(size=8)) +
  lims(x=year_range, y=coal_lims) +
  scale_color_viridis(discrete=T)

coal_no_ccs_p <- ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Coal.woCCS),], 
                        aes(x=Year, y=SecondaryEnergy.Electricity.Coal.woCCS, group=mod_scen)) +
  geom_line(colour="grey20", alpha=0.3, size=sz) +
  geom_line(data=runs_out[!is.na(runs_out$SecondaryEnergy.Electricity.Coal.woCCS),], aes(colour=mod_scen), size=1) +
  labs(x="", y="Coal without CCS (EJ)", title="") +
  theme(legend.position = "none", 
        text = element_text(size=8)) +
  lims(x=year_range, y=coal_lims) +
  scale_color_viridis(discrete=T)

coal_ccs_p <- ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Coal.wCCS),], 
                     aes(x=Year, y=SecondaryEnergy.Electricity.Coal.wCCS, group=mod_scen)) +
  geom_line(colour="grey20", alpha=0.3, size=sz) +
  geom_line(data=runs_out[!is.na(runs_out$SecondaryEnergy.Electricity.Coal.wCCS),], aes(colour=mod_scen), size=1) +
  labs(x="", y="Coal with CCS (EJ)", title="") +
  theme(legend.position = "none", 
        text = element_text(size=8)) +
  lims(x=year_range, y=coal_lims) +
  scale_color_viridis(discrete=T)

gas_all_p <- ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Gas),], 
                    aes(x=Year, y=SecondaryEnergy.Electricity.Gas, group=mod_scen)) +
  geom_line(colour="grey20", alpha=0.3, size=sz) +
  geom_line(data=runs_out[!is.na(runs_out$SecondaryEnergy.Electricity.Gas),], aes(colour=mod_scen), size=1) +
  labs(x="", y="All gas (EJ)", title="") +
  theme(legend.position = "none",
        text = element_text(size=8)) +
  lims(x=year_range, y=gas_lims) +
  scale_color_viridis(discrete=T)

gas_no_ccs_p <- ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Gas.woCCS),], 
                       aes(x=Year, y=SecondaryEnergy.Electricity.Gas.woCCS, group=mod_scen)) +
  geom_line(colour="grey20", alpha=0.3, size=sz) +
  geom_line(data=runs_out[!is.na(runs_out$SecondaryEnergy.Electricity.Gas.woCCS),], aes(colour=mod_scen), size=1) +
  labs(x="", y="Gas without CCS (EJ)", title="") +
  theme(legend.position = "none",
        text = element_text(size=8)) +
  lims(x=year_range, y=gas_lims) +
  scale_color_viridis(discrete=T)

gas_ccs_p <- ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Gas.wCCS),], 
                    aes(x=Year, y=SecondaryEnergy.Electricity.Gas.wCCS, group=mod_scen)) +
  geom_line(colour="grey20", alpha=0.3, size=sz) +
  geom_line(data=runs_out[!is.na(runs_out$SecondaryEnergy.Electricity.Gas.wCCS),], aes(colour=mod_scen), size=1) +
  labs(x="", y="Gas with CCS (EJ)", title="") +
  theme(legend.position = "none",
        text = element_text(size=8)) +
  lims(x=year_range, y=gas_lims) +
  scale_color_viridis(discrete=T)

bio_all_p <- ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Biomass),], 
                    aes(x=Year, y=SecondaryEnergy.Electricity.Biomass, group=mod_scen)) +
  geom_line(colour="grey20", alpha=0.3, size=sz) +
  geom_line(data=runs_out[!is.na(runs_out$SecondaryEnergy.Electricity.Biomass),], aes(colour=mod_scen), size=1) +
  labs(x="", y="All biomass (EJ)", title="") +
  theme(legend.position = "none",
        text = element_text(size=8)) +
  lims(x=year_range, y=bio_lims) +
  scale_color_viridis(discrete=T)

bio_no_ccs_p <- ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Biomass.woCCS),], 
                       aes(x=Year, y=SecondaryEnergy.Electricity.Biomass.woCCS, group=mod_scen)) +
  geom_line(colour="grey20", alpha=0.3, size=sz) +
  geom_line(data=runs_out[!is.na(runs_out$SecondaryEnergy.Electricity.Biomass.woCCS),], aes(colour=mod_scen), size=1) +
  labs(x="", y="Biomass without CCS (EJ)", title="") +
  theme(legend.position = "none",
        text = element_text(size=8)) +
  lims(x=year_range, y=bio_lims) +
  scale_color_viridis(discrete=T)

bio_ccs_p <- ggplot(runs[!is.na(runs$SecondaryEnergy.Electricity.Biomass.wCCS),], 
                    aes(x=Year, y=SecondaryEnergy.Electricity.Biomass.wCCS, group=mod_scen)) +
  geom_line(colour="grey20", alpha=0.3, size=sz) +
  geom_line(data=runs_out[!is.na(runs_out$SecondaryEnergy.Electricity.Biomass.wCCS),], aes(colour=mod_scen), size=1) +
  labs(x="", y="Biomass with CCS (EJ)", title="") +
  theme(legend.position = "none",
        text = element_text(size=8)) +
  lims(x=year_range, y=bio_lims) +
  scale_color_viridis(discrete=T)

png(file="plots/outliers_gas_2deg_ccs_breakdown.png",width=800,height=800,res=150,type='cairo')
plot_grid(coal_all_p, coal_no_ccs_p, coal_ccs_p, gas_all_p, gas_no_ccs_p, gas_ccs_p, bio_all_p, bio_no_ccs_p, bio_ccs_p, nrow=3)
dev.off()


# Carbon sequestration
png(file=paste0("plots/outliers_gas_2deg_CS_LandUse.png"),width=600,height=500,res=200,type='cairo')
ggplot(runs[!is.na(runs$CarbonSequestration.LandUse),], 
       aes(x=Year, y=CarbonSequestration.LandUse, group=mod_scen)) +
  geom_line(colour="grey20", alpha=0.3, size=sz) +
  geom_line(data=runs_out[!is.na(runs_out$CarbonSequestration.LandUse),], aes(colour=mod_scen), size=1) +
  labs(x="", y="Carbon sequestration (Mt/yr)", 
       title="Carbon sequestation: Land use", 
       subtitle="<1.5C high-gas outliers",
       colour="Run") +
  theme(legend.position = "bottom",
        text = element_text(size=6)) +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)
dev.off()

png(file=paste0("plots/outliers_gas_2deg_CS_BECCS.png"),width=600,height=500,res=200,type='cairo')
ggplot(runs[!is.na(runs$CarbonSequestration.CCS.Biomass),], 
       aes(x=Year, y=CarbonSequestration.CCS.Biomass, group=mod_scen)) +
  geom_line(colour="grey20", alpha=0.3, size=sz) +
  geom_line(data=runs_out[!is.na(runs_out$CarbonSequestration.CCS.Biomass),], aes(colour=mod_scen), size=1) +
  labs(x="", y="Carbon sequestration (Mt/yr)", 
       title="Carbon sequestation: BECCS", 
       subtitle="<1.5C high-gas outliers", 
       colour="Run") +
  theme(legend.position = "bottom",
        text = element_text(size=6)) +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)
dev.off()

png(file=paste0("plots/outliers_gas_2deg_CS_DACS.png"),width=600,height=500,res=200,type='cairo')
ggplot(runs[!is.na(runs$CarbonSequestration.DirectAirCapture),], 
       aes(x=Year, y=CarbonSequestration.DirectAirCapture, group=mod_scen)) +
  geom_line(colour="grey20", alpha=0.3, size=sz) +
  geom_line(data=runs_out[!is.na(runs_out$CarbonSequestration.DirectAirCapture),], aes(colour=mod_scen), size=1) +
  labs(x="", y="Carbon sequestration (Mt/yr)", 
       title="Carbon sequestation: DACS", 
       subtitle="<1.5C high-gas outliers",
       colour="Run") +
  theme(legend.position = "none",
        text = element_text(size=5)) +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)
dev.off()

#png(file=paste0("plots/outliers_2deg_CS_weathering.png"),width=600,height=400,res=150,type='cairo')
#ggplot(runs[!is.na(runs$CarbonSequestration.EnhancedWeathering),], 
#       aes(x=Year, y=CarbonSequestration.EnhancedWeathering, group=mod_scen)) +
#  geom_line(colour="grey20", alpha=0.3, size=sz) +
#  geom_line(data=runs_out[!is.na(runs_out$CarbonSequestration.EnhancedWeathering),], aes(colour=mod_scen), size=1) +
#  labs(x="", y="Carbon sequestration (Mt/yr)", title="Carbon sequestation: Weathering", subtitle="<2C high-coal outliers") +
#  theme(legend.position = "none") +
#  lims(x=year_range) +
#  scale_color_viridis(discrete=T)
#dev.off()


# Other sectors

# Transport (Primary energy oil)
png(file=paste0("plots/outliers_gas_2deg_CS_PrimaryEnergyOil.png"),width=600,height=500,res=200,type='cairo')
ggplot(runs[!is.na(runs$PrimaryEnergy.Oil) & !is.na(runs$PrimaryEnergy),], 
       aes(x=Year, y=100*PrimaryEnergy.Oil/PrimaryEnergy, group=mod_scen)) +
  geom_line(colour="grey20", alpha=0.3, size=0.1) +
  geom_line(data=runs_out[!is.na(runs_out$PrimaryEnergy.Oil) & !is.na(runs_out$PrimaryEnergy),], aes(colour=mod_scen), size=1) +
  labs(x="", y="% of Primary energy", title="Primary energy from oil", 
       subtitle="<1.5C high-gas outliers", 
       colour="Run") +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        text = element_text(size=5)) +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)
dev.off()

# Gas
png(file=paste0("plots/outliers_2deg_CS_PrimaryEnergyGas.png"),width=600,height=500,res=200,type='cairo')
ggplot(runs[!is.na(runs$PrimaryEnergy.Gas) & !is.na(runs$PrimaryEnergy),], 
       aes(x=Year, y=100*PrimaryEnergy.Gas/PrimaryEnergy, group=mod_scen)) +
  geom_line(colour="grey20", alpha=0.3, size=0.1) +
  geom_line(data=runs_out[!is.na(runs_out$PrimaryEnergy.Gas) & !is.na(runs_out$PrimaryEnergy),], aes(colour=mod_scen), size=1) +
  labs(x="", y="% of Primary energy", title="Primary energy from Gas", subtitle="<2C high-coal outliers", colour="Run") +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        text = element_text(size=5)) +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)
dev.off()

# Coal
png(file=paste0("plots/outliers_2deg_CS_PrimaryEnergyCoal.png"),width=600,height=500,res=200,type='cairo')
ggplot(runs[!is.na(runs$PrimaryEnergy.Coal) & !is.na(runs$PrimaryEnergy),], 
       aes(x=Year, y=100*PrimaryEnergy.Coal/PrimaryEnergy, group=mod_scen)) +
  geom_line(colour="grey20", alpha=0.3, size=0.1) +
  geom_line(data=runs_out[!is.na(runs_out$PrimaryEnergy.Coal) & !is.na(runs_out$PrimaryEnergy),], aes(colour=mod_scen), size=1) +
  labs(x="", y="% of Primary energy", title="Primary energy from Coal", subtitle="<2C high-coal outliers", colour="Run") +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        text = element_text(size=5)) +
  lims(x=year_range) +
  scale_color_viridis(discrete=T)
dev.off()


