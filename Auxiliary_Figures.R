library(ggplot2)
library(magrittr)

## Make folder for auxiliary plots
if(!dir.exists("plots/Auxiliary_Figures")){
  dir.create("plots/Auxiliary_Figures")
}


#----------------------------#
# DATA PROPORTION PIE CHARTS #
#----------------------------#

## Load proportion data
propData <- list(
  DataSource = read.csv("data_forUpload/DataSource_Proportions.csv"),
  FunGroup = read.csv("data_forUpload/FunctionalGroup_Proportions.csv")
)

## Set plotting variables and colors
plot.var <- c("proportion", "number")

plot.cols <- list(
  pal_propData = c("#37C8B2", "#BF3745", "#584B9F", "#0087B3"),
  pal_FunGroup = c("grey70", "#D65033", "#00B1B5", "#FBD172","#A0E7AD", "#A71B4B")
)


## Overall pie chart - Data types
pdf("plots/Auxiliary_Figures/pieChart_DataTypes_all.pdf", width = 4, height = 4)
ggplot(subset(propData[[1]], ecosystem == "Overall"), aes(x = "", y = proportion, fill = datatypeName)) + 
  geom_col() + 
  coord_polar(theta = "y") +
  scale_fill_manual(name = "", values = plot.cols[[1]]) + 
  geom_text(aes(label = paste0(round(proportion*100, digits = 1), "%")),
            position = position_stack(vjust = 0.5)) +
  theme_classic() + 
  theme(axis.line = element_blank(), axis.title = element_blank(),
        axis.text = element_blank())
dev.off()

## Ecosystem-specific pie charts - Data types
pdf("plots/Auxiliary_Figures/pieChart_DataTypes_ecosystems.pdf", width = 10, height = 8)
ggplot(subset(propData[[1]], ecosystem != "Overall"), aes(x = "", y = proportion, fill = datatypeName)) + 
  geom_col() + 
  coord_polar(theta = "y") +
  scale_fill_manual(name = "", values = plot.cols[[1]]) + 
  geom_text(aes(label = paste0(round(proportion*100, digits = 1), "%")),
            position = position_stack(vjust = 0.5)) +
  facet_wrap(~ecosystem) + 
  theme_classic() + 
  theme(axis.line = element_blank(), axis.title = element_blank(),
        axis.text = element_blank())
dev.off()

## Overall pie chart - Functional groups
pdf("plots/Auxiliary_Figures/pieChart_FunGroups_all.pdf", width = 5, height = 5)
ggplot(subset(propData[[2]], ecosystem == "Overall"), aes(x = "", y = proportion, fill = functionalGroup)) + 
  geom_col() + 
  coord_polar(theta = "y") +
  scale_fill_manual(name = "", values = plot.cols[[2]]) + 
  geom_text(aes(label = paste0(round(proportion*100, digits = 1), "%")),
            position = position_stack(vjust = 0.5)) +
  theme_classic() + 
  theme(axis.line = element_blank(), axis.title = element_blank(),
        axis.text = element_blank())
dev.off()

## Ecosystem-specific pie charts - Functional groups
pdf("plots/Auxiliary_Figures/pieChart_FunGroups_ecosystems.pdf", width = 10, height = 8)
ggplot(subset(propData[[2]], ecosystem != "Overall"), aes(x = "", y = proportion, fill = functionalGroup)) + 
  geom_col() + 
  coord_polar(theta = "y") +
  scale_fill_manual(name = "", values = plot.cols[[2]]) + 
  geom_text(aes(label = paste0(round(proportion*100, digits = 1), "%")),
            position = position_stack(vjust = 0.5)) +
  facet_wrap(~ecosystem, scales = "free") + 
  theme_classic() + 
  theme(axis.line = element_blank(), axis.title = element_blank(),
        axis.text = element_blank())
dev.off()


#--------------------------------------#
# EXTRA GRAPHS FOR SPECIFIC INDICATORS #
#--------------------------------------#

## Read in indicator index data
indInd.data <- read.csv("data_forUpload/IndicatorIndexData.csv")

## Select relevant indicators
focal_inds <- c("Tilstand semi-naturlig eng og strandeng", "Tilstand kystlynghei")

indInd.data <- indInd.data %>%
  dplyr::filter(name %in% focal_inds) %>%
  dplyr::mutate(name = ifelse(name == "Tilstand semi-naturlig eng og strandeng", "Tilstand semi-naturlig eng", name))

## Make joint plot
oll.plot <- ggplot(indInd.data, aes(x = year_t, y = median, fill = name, color = name)) + 
  geom_ribbon(aes(ymin = q025, ymax = q975), alpha = 0.25, color = NA) + 
  geom_line() + 
  geom_errorbar(aes(ymin = q025, ymax = q975), alpha = 0.8, width = 0.5) + 
  geom_point(shape = 15, size = 3) +
  xlab("År") + ylab("Indikatorverdi (arealvektet gjennomsnitt)") + 
  scale_x_continuous(breaks = unique(indInd.data$year_t), labels = unique(indInd.data$year_t)) + 
  scale_color_manual(values = c("#089392", "#CF597E")) + 
  scale_fill_manual(values = c("#089392", "#CF597E")) + 
  ylim(min(indInd.data$q025), max(indInd.data$q975)) + 
  #ylim(0, 1) + 
  theme_classic() + 
  theme(panel.grid.major.y = element_line(color = "grey80"),
        panel.grid.major.x = element_line(color = "grey80", linetype = "dotted"),
        legend.title = element_blank(), legend.position = c(0.7, 0.9))

## Save plot
ggsave(file = "plots/Auxiliary_Figures/OpenLowland_IndIdx.svg", plot = oll.plot, width = 6, height = 4)
ggsave(file = "plots/Auxiliary_Figures/OpenLowland_IndIdx.pdf", plot = oll.plot, width = 6, height = 4)

