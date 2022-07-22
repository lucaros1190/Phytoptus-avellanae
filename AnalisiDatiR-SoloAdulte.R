

# ANALYSIS ADULT PLANTS - File: "PerAnalisiR-Completo-Adulte.csv'


# Acquisition of dataset and variables

data_complete_ad = read.csv2(file.choose(), header=T, sep=";", dec=".", na.string="NA")

head(data_complete_ad)

year_ad <- as.factor(data_complete_ad$Year)
irrigation_ad <- as.factor(data_complete_ad$Irrigation)
age_ad <- as.factor(data_complete_ad$Age)
plant_ad <- as.factor(data_complete_ad$Plant)
branch_ad <- as.factor(data_complete_ad$Branch)
height_ad <- as.factor(data_complete_ad$Height)
buds_ad <- as.numeric(data_complete_ad$Buds)
galls_ad <- as.numeric(data_complete_ad$Galls)

  # Check the levels - Just to be sure that there are all!

levels(year_ad)
levels(irrigation_ad)
levels(age_ad)
levels(plant_ad)
levels(branch_ad)
levels(height_ad)



# GLM negative binomial number of galls - Comparison of the 4 years without considering the different heights

library(lme4)
library(MASS)

GenLin_galle_ad <- glmer.nb(galls_ad ~ year_ad + irrigation_ad + (1 | plant_ad), data=data_complete_ad)
summary(GenLin_galle_ad)


  # Pairwise comparison - GALLS VS YEAR:

library(multcompView)
library(emmeans)
marginal_galle_ad = emmeans(GenLin_galle_ad, ~ year_ad )
pairs(marginal_galle_ad, adjust="bonferroni")

  # Letters of significance:

library(multcomp)
lettere_galle_ad <- cld(marginal_galle_ad, alpha=0.05, Letters=letters, adjust="bonferroni")
lettere_galle_ad

# BOXPLOT - Galls vs Year

library(ggplot2)

  # Merge the 2019 letters in a single one

library(stringr)
L1_ADGY <- lettere_galle_ad$.group[1]
L1_ADGY

  # Merge the 2020 letters in a single one

L2_ADGY <- lettere_galle_ad$.group[2]
L2_ADGY

  # Merge the 2021 letters in a single one

L3_ADGY <- lettere_galle_ad$.group[4]
L3_ADGY

  # Merge the 2022 letters in a single one

L4_ADGY <- lettere_galle_ad$.group[3]
L4_ADGY

# Draw the plot

box_galle_year_ad <- ggplot(data_complete_ad, aes(x=year_ad, y=galls_ad, fill=year_ad)) + 
  geom_boxplot(width=0.55) + 
  xlab("Year") + 
  ylab("Number of galls") + 
  ggtitle("Infestation over the years") +
  theme(plot.title = element_text(hjust=0.5), text = element_text(size=21), legend.position = "none") + 
  scale_fill_manual(drop = FALSE, values= alpha(c("green", "blue", "red", "orange"), 0.5)) + 
  scale_x_discrete(labels = c("2019","2020", "2021", "2022")) +
  geom_text(aes(x=1, y=8, label = L1_ADGY), position = position_dodge(width = -0), vjust = -0.25, size = 8, hjust = 0.4) +
  geom_text(aes(x=2, y=7, label = L2_ADGY), position = position_dodge(width = 0), vjust = -0.25, size = 8, hjust = 0.4) +
  geom_text(aes(x=3, y=12.5, label = L3_ADGY), position = position_dodge(width = 0), vjust = -0.25, size = 8, hjust = 0.8) +
  geom_text(aes(x=4, y=10, label = L4_ADGY), position = position_dodge(width = 0), vjust = -0.25, size = 8, hjust = 0.6)

box_galle_year_ad


  # Pairwise comparison - GALLS VS YEAR + IRRIGATION:

library(multcompView)
library(emmeans)
marginal_galle_irrad = emmeans(GenLin_galle_ad, ~ year_ad + irrigation_ad)
pairs(marginal_galle_irrad, adjust="bonferroni")

  # Letters of significance:

library(multcomp)
lettere_galle_irrad <- cld(marginal_galle_irrad, alpha=0.05, Letters=letters, adjust="bonferroni")
lettere_galle_irrad


# BOXPLOT - Galls vs Year + Irrigation

library(ggplot2)

  # Merge the 2019 letters in a single one

library(stringr)
L1_ADGYI <- paste(str_remove(lettere_galle_irrad$.group[2], "   "), str_remove(lettere_galle_irrad$.group[1], "   "), sep = "")
L1_ADGYI

  # Merge the 2020 letters in a single one

L2_ADGYI <- paste(str_remove(lettere_galle_irrad$.group[4], "   "), str_remove(lettere_galle_irrad$.group[3], "   "), sep = "")
L2_ADGYI

  # Merge the 2021 letters in a single one

L3_ADGYI <-paste(lettere_galle_irrad$.group[8], lettere_galle_irrad$.group[7])
L3_ADGYI

  # Merge the 2022 letters in a single one

L4_ADGYI <- paste(str_remove(lettere_galle_irrad$.group[6], "   "), str_remove(lettere_galle_irrad$.group[5], "   "), sep = "")
L4_ADGYI

  # Draw the plot

box_galle_year_irrad <- ggplot(data_complete_ad, aes(x=year_ad, y=galls_ad, fill=irrigation_ad)) + 
  geom_boxplot(width=0.55) + 
  xlab("Year") + 
  ylab("Number of galls") + 
  ggtitle("Infestation over the years") +
  theme(plot.title = element_text(hjust=0.5), text = element_text(size=21)) + 
  scale_fill_manual(drop = FALSE, name = "Irrigation",  values= alpha(c("green", "blue"), 0.5)) +
  scale_x_discrete(labels = c("2019","2020", "2021", "2022")) +
  geom_text(aes(x=1, y=8, label = L1_ADGYI), position = position_dodge(width = -0), vjust = -0.25, size = 8, hjust = 0.5) +
  geom_text(aes(x=2, y=6, label = L2_ADGYI), position = position_dodge(width = 0), vjust = -0.25, size = 8, hjust = 0.6) +
  geom_text(aes(x=3, y=12.5, label = L3_ADGYI), position = position_dodge(width = 0), vjust = -0.25, size = 8, hjust = 0.6) +
  geom_text(aes(x=4, y=10, label = L4_ADGYI), position = position_dodge(width = 0), vjust = -0.25, size = 8, hjust = 0.4)

box_galle_year_irrad



# Second part of the analysis: Differences on heights vs galls - File: 'PerAnalisiR-ConfrontoAltezze.csv'

# Acquisition of the dataset and variables assignation

data_complete_redad = read.csv2(file.choose(), header=T, sep=";", dec=".", na.string="NA")

head(data_complete_redad)

year_redad <- as.factor(data_complete_redad$Year)
irrigation_redad <- as.factor(data_complete_redad$Irrigation)
age_redad <- as.factor(data_complete_redad$Age)
plant_redad <- as.factor(data_complete_redad$Plant)
branch_redad <- as.factor(data_complete_redad$Branch)
height_redad <- as.factor(data_complete_redad$Height)
buds_redad <- as.numeric(data_complete_redad$Buds)
galls_redad <- as.numeric(data_complete_redad$Galls)

  # Check the levels - Just to be sure!

levels(year_redad)
levels(irrigation_redad)
levels(age_redad)
levels(plant_redad)
levels(branch_redad)
levels(height_redad)


# GLM negative binomial number of galls vs heights

library(lme4)
library(MASS)

GenLin_galle_heiad <- glmer.nb(galls_redad ~ year_redad + height_redad + (1 | plant_redad), data=data_complete_redad)
summary(GenLin_galle_heiad)

  # Pairwise comparison - Galls vs height:

library(multcompView)
library(emmeans)
marginal_galle_heiad = emmeans(GenLin_galle_heiad, ~ height_redad )
pairs(marginal_galle_heiad, adjust="bonferroni")

  # Letters of significance:

library(multcomp)
lettere_galle_heiad <- cld(marginal_galle_heiad, alpha=0.05, Letters=letters, adjust="bonferroni")
lettere_galle_heiad

# BOXPLOT - Galls vs Height

library(ggplot2)

  # Acquisition of the H1 letters

library(stringr)
L1_ADGH <- lettere_galle_heiad$.group[1]
L1_ADGH

  # Acquisition of the H2 letters

L2_ADGH <- lettere_galle_heiad$.group[3]
L2_ADGH

  # Acquisition of the H3 letters

L3_ADGH <- lettere_galle_heiad$.group[2]
L3_ADGH

  # Draw the plot

box_galle_height_ad <- ggplot(data_complete_redad, aes(x=height_redad, y=galls_redad, fill=height_redad)) + 
  geom_boxplot(width=0.55) + 
  xlab("Plant height (m)") + 
  ylab("Number of galls") + 
  ggtitle("Infestation over height") +
  theme(plot.title = element_text(hjust=0.5), text = element_text(size=21), legend.position = "none") + 
  scale_fill_manual(drop = FALSE, values= alpha(c("green", "blue", "red"), 0.5)) + 
  scale_x_discrete(labels = c("0-1.5","1.5-3.0", ">3.0")) +
  geom_text(aes(x=1, y=6, label = L1_ADGH), position = position_dodge(width = -0), vjust = -0.25, size = 8, hjust = 0.5) +
  geom_text(aes(x=2, y=13, label = L2_ADGH), position = position_dodge(width = 0), vjust = -0.25, size = 8, hjust = 0.7) +
  geom_text(aes(x=3, y=8, label = L3_ADGH), position = position_dodge(width = 0), vjust = -0.25, size = 8, hjust = 0.5)

box_galle_height_ad


  # Pairwise comparison - Galls vs height + year:

library(multcompView)
library(emmeans)
marginal_galle_heirrad = emmeans(GenLin_galle_heiad, ~ height_redad + year_redad)
pairs(marginal_galle_heirrad, adjust="bonferroni")

  # Letters of significance:

library(multcomp)
lettere_galle_heirrad <- cld(marginal_galle_heirrad, alpha=0.05, Letters=letters, adjust="bonferroni")
lettere_galle_heirrad

  # BOXPLOT - Galls vs height + year

library(ggplot2)

  # Merge the H1 letters in a single one

library(stringr)
L1_ADGYH <- paste(lettere_galle_heirrad$.group[3], str_remove(lettere_galle_heirrad$.group[1], "   "), sep = "")
L1_ADGYH

  # Merge the H2 letters in a single one

L2_ADGYH <- paste(lettere_galle_heirrad$.group[6], str_remove(lettere_galle_heirrad$.group[5], "   "), sep = "")
L2_ADGYH

  # Merge the H3 letters in a single one

L3_ADGYH <-paste(str_remove(lettere_galle_heirrad$.group[4], "  ") , str_remove(lettere_galle_heirrad$.group[2], "  "), sep = "")
L3_ADGYH

  # Draw the plot

box_galle_year_heirrad <- ggplot(data_complete_redad, aes(x=height_redad, y=galls_redad, fill=year_redad)) + 
  geom_boxplot(width=0.55) + 
  xlab("Plant height (m)") + 
  ylab("Number of galls") + 
  ggtitle("Infestation over the years") +
  theme(plot.title = element_text(hjust=0.5), text = element_text(size=21)) + 
  scale_fill_manual(drop = FALSE, name = "Year",  values= alpha(c("green", "blue"), 0.5)) +
  scale_x_discrete(labels = c("1-1.5","1.5-3.0", ">3.0")) +
  geom_text(aes(x=1, y=6, label = L1_ADGYH), position = position_dodge(width = -0), vjust = -0.25, size = 8, hjust = 0.6) +
  geom_text(aes(x=2, y=12.5, label = L2_ADGYH), position = position_dodge(width = 0), vjust = -0.25, size = 8, hjust = 0.7) +
  geom_text(aes(x=3, y=8, label = L3_ADGYH), position = position_dodge(width = 0), vjust = -0.25, size = 8, hjust = 0.5)

box_galle_year_heirrad
