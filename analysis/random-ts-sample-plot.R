#--------------------------------------
# This script sets out to plot a 
# random set of time series for
# demonstration in the report
#
# NOTE: setup.R must be run first
#--------------------------------------

#----------------------------------------
# Author: Trent Henderson, 18 August 2021
#----------------------------------------

# Read in data

load("data/empirical1000.Rda")

# Plot 6 time series from different domains

sampleIDs <- c("FL_fbruss_L300_N10000_IC_0.3_2_y.dat", "MP_predprey_L5000_IC_0.5_0.55_x.dat", 
               "ME_winter_rf.dat", "MD_gaitpdb_GaCo05_01l_SNIP_1256-2955.dat", 
               "CM_tpi_hob.dat", "AS_s2.1_f4_b8_l8280_135971.dat")

mySamples <- empirical1000 %>%
  filter(Name %in% sampleIDs) %>%
  mutate(name_clean = case_when(
          Name == "AS_s2.1_f4_b8_l8280_135971.dat"           ~ "A. Birdsong",
          Name == "CM_tpi_hob.dat"                           ~ "B. Trans Polar Index",
          Name == "FL_fbruss_L300_N10000_IC_0.3_2_y.dat"     ~ "C. Dissipative forced Brusselator",
          Name == "MD_gaitpdb_GaCo05_01l_SNIP_1256-2955.dat" ~ "D. Parkinson's disease gait",
          Name == "ME_winter_rf.dat"                         ~ "E. Winter river flow",
          Name == "MP_predprey_L5000_IC_0.5_0.55_x.dat"      ~ "F. Predator-prey map"))

# Draw plot

p <- mySamples %>%
  ggplot(aes(x = timepoint, y = value)) +
  geom_line() +
  labs(x = "Time",
       y = "Value") +
  theme_bw() +
  theme(strip.text = element_text(face = "bold"),
        text = element_text(size = 18)) +
  facet_wrap(~name_clean, scales = "free", ncol = 2)

print(p)

ggsave("output/sample-ts.png", p, units = "in", height = 10, width = 10)
ggsave("output/sample-ts.pdf", p, units = "in", height = 10, width = 10)
