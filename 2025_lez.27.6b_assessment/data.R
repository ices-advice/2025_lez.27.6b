## Preprocess data, write TAF data tables

## Before:
## After:

library(icesTAF)
library(ggplot2)
taf.bootstrap()

mkdir("data")


#### Load data ####

megC <- read.taf("boot/initial/data/lez6b_Catch_2025.csv")
megI <- read.taf("boot/initial/data/rockall_megrim_survey_biomass_indices_wgcse.csv")

# set up DATA.bib
draft.data(
  data.files = "lez6b_Catch_2025.csv",
  originator = "WGCSE",
  year = 2025,
  title = "lez6b_Catch_2025",
  period = "1991-2024",
  file = TRUE
)

draft.data(
  data.files = "rockall_megrim_survey_biomass_indices_wgcse.csv",
  originator = "WGCSE",
  year = 2025,
  title = "SIAMISS Survey Index",
  period = "2005-2025",
  file = TRUE,
  append = T
)


# Simple data processing
megC$catch <- megC$Landings + megC$Discards


# plot survey data
# png("data/SIAMISS Index.png", width = 8, height=6, units="in", res = 400)
# ggplot(megI, aes(year, biomass))+
#   geom_point()+
#   geom_line()+
#   geom_ribbon(data = megI, aes(min = lwr, max = upr, alpha = 0.01))+
#   theme_bw()+
#   xlab("Year")+ ylab("Biomass (tonnes)")+
#   theme(legend.position = "none")
# dev.off()


write.taf(megC, dir = "data")
write.taf(megI, dir = "data")

