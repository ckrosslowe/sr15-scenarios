# Script to analyse two sets of scenarios (1.5C vs 2C)

run_summary1 <- read.csv("data/runs_summary_1p5C.csv", header=T) %>% mutate(Category = "1.5C")
run_summary2 <- read.csv("data/runs_summary_2C.csv", header=T) %>% mutate(Category = "2C")
run_summary <- bind_rows(run_summary1, run_summary2)

run_samp1 <- read.csv("data/run_samp_1p5C.csv", header=T, stringsAsFactors = F) %>% mutate(Category = "1.5C")
run_samp2 <- read.csv("data/run_samp_2C.csv", header=T, stringsAsFactors = F) %>% mutate(Category = "2C")
run_samp <- bind_rows(run_samp1, run_samp2)
dup_mod_scen <- run_samp$mod_scen[duplicated(run_samp$mod_scen)]
run_samp_exc <- run_samp[!(run_samp$Category %in% "2C" & run_samp$mod_scen %in% run_samp$mod_scen[run_samp$Category=="1.5C"]), ]

ger <- read.csv("data/global_electricity_review_2020_v2.csv", header = T) %>%
  filter(Country %in% "World") %>%
  mutate(Type2 = ifelse(Type %in% "Coal", "Coal", 
                        ifelse(Type %in% "Gas", "Gas", 
                               ifelse(Type %in% c("Solar", "Wind", "Hydro"), "Renewables", "Other"))))

ger_sum <- ger %>%
  group_by(Type2, Year) %>%
  summarise(Value_TWh = sum(Value_TWh)) %>%
  ungroup()


# ==== Timelines ====

png(file="plots/coal_woCCS_comp.png",width=900,height=600,res=150,type='cairo')
ggplot(filter(run_summary, Type %in% "Coal_woCCS_scen", Year %in% c(2020, 2030, 2050)), aes(x=Year, y=med, group=Category)) +
  geom_ribbon(aes(ymin=q25, ymax=q75, group=Category, fill=Category), alpha=0.4) +
  geom_line(size=1, colour="grey20") +
  geom_point(size=4, colour="grey20") +
  # Real data
  geom_line(data=filter(ger_sum, Type2 %in% "Coal"), 
            aes(x=Year, y=Value_TWh/278, color=Type2, group=Type2), size=2) +
  labs(x="",
       y="Electricity production (EJ)",
       title="Unabated coal electricity in <1.5C and <2C scenarios",
       #subtitle=,
       colour="",
       fill="") +
  scale_x_continuous(breaks=c(2000, 2010, 2020, 2030, 2040, 2050), limits = c(2000, 2050)) +
  scale_color_manual(values=c("Coal"="red"), labels=c("Coal"="Historic\nproduction"))
dev.off()

png(file="plots/gas_woCCS_comp.png",width=900,height=600,res=150,type='cairo')
ggplot(filter(run_summary, Type %in% "Gas_woCCS_scen"), aes(x=Year, y=med, group=Category)) +
  geom_ribbon(aes(ymin=q25, ymax=q75, group=Category, fill=Category), alpha=0.4) +
  geom_line(size=1, colour="grey20") +
  geom_point(size=4, colour="grey20") +
  # Real data
  geom_line(data=filter(ger_sum, Type2 %in% "Gas"), 
            aes(x=Year, y=Value_TWh/278, color=Type2, group=Type2), size=2) +
  labs(x="",
       y="Electricity production (EJ)",
       title="Unabated gas electricity in <1.5C and <2C scenarios",
       #subtitle=,
       colour="",
       fill="") +
  scale_x_continuous(breaks=c(2000, 2010, 2020, 2030, 2040, 2050), limits = c(2000, 2050)) +
  scale_color_manual(values=c("Gas"="blue"), labels=c("Gas"="Historic\nproduction"))
dev.off()

png(file="plots/renew_comp.png",width=900,height=600,res=150,type='cairo')
ggplot(filter(run_summary, Type %in% "Renew_scen", Year %in% c(2020, 2030, 2050)), aes(x=Year, y=med, group=Category)) +
  geom_ribbon(aes(ymin=q25, ymax=q75, group=Category, fill=Category), alpha=0.4) +
  geom_line(size=1, colour="grey20") +
  geom_point(size=4, colour="grey20") +
  # Real data
  geom_line(data=filter(ger_sum, Type2 %in% "Renewables"), 
            aes(x=Year, y=Value_TWh/278, color=Type2, group=Type2), size=2) +
  labs(x="",
       y="Electricity production (EJ)",
       title="Renewable electricity in <1.5C and <2C scenarios",
       #subtitle=,
       colour="",
       fill="") +
  scale_x_continuous(breaks=c(2000, 2010, 2020, 2030, 2040, 2050), limits = c(2000, 2050)) +
  scale_color_manual(values=c("Renewables"="purple"), labels=c("Renewables"="Historic\nproduction"))
dev.off()

# ==== Scatter charts ====

# 1.5C scenarios
filter(run_samp, Year==2050, Category %in% "1.5C") %>%
  select(Coal = SecondaryEnergy.Electricity.Coal, 
         Gas = SecondaryEnergy.Electricity.Gas,
         Renew = SecondaryEnergy.Electricity.Renewables,
         Biomass = SecondaryEnergy.Electricity.Biomass,
         Nuclear = SecondaryEnergy.Electricity.Nuclear) %>%
  pairs()

filter(run_samp, Year==2050, Category %in% "2C") %>%
  select(Coal = SecondaryEnergy.Electricity.Coal, 
         Gas = SecondaryEnergy.Electricity.Gas,
         Renew = SecondaryEnergy.Electricity.Renewables,
         Biomass = SecondaryEnergy.Electricity.Biomass,
         Nuclear = SecondaryEnergy.Electricity.Nuclear) %>%
  pairs()

filter(run_samp, Year==2050, Category %in% "2C") %>%
  select(Coal.CCS = SecondaryEnergy.Electricity.Coal.wCCS, 
         Gas.CCS = SecondaryEnergy.Electricity.Gas.wCCS,
         BECCS = SecondaryEnergy.Electricity.Biomass.wCCS) %>%
  pairs()

# Percentage of fossil that's abated
for (y in seq(2020,2100,10)) {
  png(file=paste0("plots/frac_abated_",y,".png"),width=700,height=550,res=150,type='cairo')
  p <- ggplot(filter(run_samp_exc, Year==y), 
         aes(x=SecondaryEnergy.Electricity.Fossil2, 
             y=100*SecondaryEnergy.Electricity.Fossil.wCCS/SecondaryEnergy.Electricity.Fossil2)) +
    geom_point(aes(color=Category), size=2, alpha=.7, position=position_jitter(3)) +
    labs(x="Fossil electricity (EJ)",
         y="% abated",
         title=as.character(y)) +
    lims(x=c(0,100), y=c(0,100))
  print(p)
  dev.off()
  #print(y)
}

for (y in seq(2020,2100,10)) {
  png(file=paste0("plots/frac_biomass_abated_",y,".png"),width=700,height=550,res=150,type='cairo')
  p <- ggplot(filter(run_samp_exc, Year==y), 
              aes(x=SecondaryEnergy.Electricity.Biomass, 
                  y=100*SecondaryEnergy.Electricity.Biomass.wCCS/SecondaryEnergy.Electricity.Biomass)) +
    geom_point(aes(color=Category), size=2, alpha=.7, position=position_jitter(3)) +
    labs(x="Biomass electricity (EJ)",
         y="% abated",
         title=as.character(y)) +
    lims(x=c(0,80), y=c(0,100))
  print(p)
  dev.off()
}
