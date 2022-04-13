install.packages("ggstatsplot")

library(ggstatsplot)
library(hrbrthemes)
library(tidyverse)
install.packages("ggpubr")
library(ggpubr)
library(ggplot2)
install.packages("car")
library("car")

y <- c("scrubland","scrubland","scrubland","disturbed","disturbed","disturbed","lawn","lawn","lawn")
habitat <- matrix(y, nrow=9, ncol=1)
variable_data <- cbind(variable_data,habitat)

# Basic dot plot

ggbetweenstats(data = variable_data,x = habitat, y =spider.diversity,
               plot.type = "box",
               pairwise.display = "all",
               title = "Plot of spider diversity for different habitats",
               ggtheme = ggplot2::theme_classic(),
               xlab = "Habitats", ylab = "Spider Species richness", results.subtitle = F,
               ggsignif.args = list(textsize = 3.5,
                                    tip_length = 0.02,
                                    lwd = 1),
               point.args = list(alpha = 1, size = 4,
                                 position = ggplot2::position_jitterdodge(dodge.width = 0.8)))
                    
#for sw index
ggbetweenstats(data = variable_data,x = habitat, y = H,
               plot.type = "box",
               pairwise.display = "all",
               title = "Plot of spider diversity for different habitats",
               ggtheme = ggplot2::theme_classic(),
               xlab = "Habitats", ylab = "Shannon-Weiner Index", results.subtitle = F,
               ggsignif.args = list(textsize = 3.5,
                                    tip_length = 0.02,
                                    lwd = 1),
               point.args = list(alpha = 1, size = 4,
                                 position = ggplot2::position_jitterdodge(dodge.width = 0.8)))

scatterplot(spider.diversity ~ plant.diversity, data = variable_data, main = "Plant Diversity vs Spider Diversity",
            xlab = "Plant Species Richness", ylab = "Spider Species Richness")

scatterplot(spider.abundance ~ plant.diversity, data = variable_data, main = "Plant Diversity vs Spider Density per plot",
            xlab = "Plant Species Richness", ylab = "Spider Density")

scatterplot(H ~ plant.diversity, data = variable_data, main = "Plant Diversity vs Shannon-Weiner Index for Spiders",
            xlab = "Plant Species Richness", ylab = "Shannon-Weiner Index")

