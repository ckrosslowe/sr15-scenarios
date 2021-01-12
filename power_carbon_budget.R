
# Read previously prepard data
# These runs have been filtered in the standard way (see scenario_analyse.R)

#runs <- read.csv("data/filtered_runs15.csv")
runs <- read.csv("data/filtered_runs2.csv")

runs <- runs %>% 
  select(mod_scen, 
         category,
         Region,
         Year,
         Emissions.CO2.Energy.Supply.Electricity) %>%
  mutate(Emissions.Elec = Emissions.CO2.Energy.Supply.Electricity)

interp_year <- function(run_name, runs=runs) {
  
  runs <- runs %>% filter(mod_scen %in% run_name)
  
  # Interpolate emissions for NA years 
  interpy <- approx(runs$Year[!is.na(runs$Emissions.Elec)], runs$Emissions.Elec[!is.na(runs$Emissions.Elec)], xout=runs$Year[is.na(runs$Emissions.Elec)])
  # Replace Na years with interpolated values
  runs$Emissions.Elec[is.na(runs$Emissions.Elec)] <- interpy$y
  
  return(runs)
}

runs_mod <- lapply(unique(runs$mod_scen), interp_year, runs)
runs_interp <- bind_rows(runs_mod)

# Calculate carbon budget for each run
budgets <- runs_interp %>% filter(Year <= 2050,
                Year > 2020) %>%
  group_by(mod_scen) %>%
  summarise(Power.CO2.budget = sum(Emissions.Elec))
# Median carbon budget
median(budgets$Power.CO2.budget)


# Plot interpolated runs
#png(file=paste0("plots/power_sector_emissions_",str_replace(t_lab,"<",""),"_",reg_lab,".png"),width=1200,height=900,res=200,type='cairo')
ggplot(runs_interp, aes(x=Year, y=Emissions.Elec)) +
  geom_hline(yintercept=0, size=0.8, colour="grey20") +
  geom_vline(xintercept=2050, size=0.8, colour="grey50", alpha=0.8, linetype=2) +
  #geom_vline(xintercept=2035, size=0.8, colour="grey50", alpha=0.8, linetype=2) +
  geom_line(aes(group=mod_scen), size=0.6, colour="#3A91D6", alpha=0.7) +
  geom_point(size=1.2, colour="#3A91D6") +
  #geom_ribbon(aes(ymin=q25, ymax=q75, group=Sector, fill=Sector), alpha=0.2) +
  labs(title="Power sector CO2 emissions (Interpolated)", 
       subtitle=paste0("Region: ", reg_lab, ", Warming: ", t_lab),
       x="",
       y="MtCO2/yr") +
  theme_ember() +
  theme(axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=9),
        axis.title.y=element_text(angle=90),
        panel.grid.major.y = element_line(colour="grey85", size=0.7)) +
  scale_x_continuous(limits=c(2010, 2100), breaks=seq(2010,2100,10)) +
  scale_y_continuous(limits=c(-5000, 5500), breaks=seq(-5000,5000,2500))

#coord_cartesian(ylim=c(-1000, 4000)) +
#scale_colour_manual(values = sector_cols) +
#scale_fill_manual(values = sector_cols)
dev.off()

