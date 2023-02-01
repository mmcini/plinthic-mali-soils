library(extrafont)
library(tidyverse)
library(patchwork)
library(ggrepel)
library(ggpubr)
library(readxl)
library(tune)

chem_data <- read_xlsx("../data/chemical_data.xlsx")
phys_data <- read_xlsx("../data/physical_data.xlsx")

# PCA ##############################################################################################
## Chemical attributes
pca_chem_data <- chem_data %>%
                 select("Region":"Soil_hor", "pH":"Fe_Cr", -"Na")

chem_pca <- prcomp(pca_chem_data[-c(1:4)], center = T, scale = T)
chem_pca_points <- chem_pca$x %>%
                   as_tibble() %>%
                   bind_cols(region = pca_chem_data$Region,
                             soil = pca_chem_data$Soil,
                             hor = pca_chem_data$Soil_hor)
chem_pca_loadings <- chem_pca$rotation %>%
                     as_tibble(rownames = "variable")
summary(chem_pca)

chem_pca_plot <- ggplot(data = chem_pca_points, aes(x = PC1, y = PC2)) +
                        geom_point(aes(shape = region, color = soil), size = 3) +
                        xlab("PC1 (46%)") + ylab("PC2 (27%)") +
                        coord_obs_pred() +
                        geom_segment(data = chem_pca_loadings,
                                     aes(x = 0, y = 0, xend = PC1*10, yend = PC2*10),
                                     arrow = arrow(length = unit(2, "mm"))) +
                        geom_label_repel(data = chem_pca_loadings,
                                         aes(x = PC1*10, y = PC2*10, label = variable)) +
                        theme_bw() +
                        theme(text = element_text(family = "Times New Roman"),
                              legend.title = element_blank())
chem_pca_plot

## Physical attributes
pca_phys_data <- phys_data %>%
                 select("Region":"Soil_hor", "VCCS":"PD")

phys_pca <- prcomp(pca_phys_data[-c(1:4)], center = T, scale = T)
phys_pca_points <- phys_pca$x %>%
                   as_tibble() %>%
                   bind_cols(region = pca_phys_data$Region,
                             soil = pca_phys_data$Soil,
                             hor = pca_phys_data$Soil_hor)
phys_pca_loadings <- phys_pca$rotation %>%
                     as_tibble(rownames = "variable")
summary(phys_pca)

phys_pca_plot <- ggplot(data = phys_pca_points, aes(x = PC1, y = PC2)) +
                        geom_point(aes(shape = region, color = soil), size = 3) +
                        xlab("PC1 (40%)") + ylab("PC2 (25%)") +
                        coord_obs_pred() +
                        geom_segment(data = phys_pca_loadings,
                                     aes(x = 0, y = 0, xend = PC1*10, yend = PC2*10),
                                     arrow = arrow(length = unit(2, "mm"))) +
                        geom_label_repel(data = phys_pca_loadings,
                                         aes(x = PC1*10, y = PC2*10, label = variable)) +
                        theme_bw() +
                        theme(text = element_text(family = "Times New Roman"),
                              legend.title = element_blank())
phys_pca_plot

## Plot arrange
ggarrange(chem_pca_plot, phys_pca_plot, ncol = 1, common.legend = T, legend = "bottom")
ggsave("../../Figures/pca.jpeg", units = "mm", width = 130, height = 260, bg = "white")

# pXRF #############################################################################################
pxrf_data <- read_xlsx("../data/pxrf_data.xlsx")

band_data <- pxrf_data %>%
             filter(str_detect(Profile, "B")) %>%
             select("Profile":"Ti") %>%
             pivot_longer(cols = "Si":"Ti", names_to = "Elements", values_to = "Content")

band_pxrf_plot <- ggplot(band_data, aes(x = -Depth, y = Content)) +
                  xlab("Depth (cm)") + ylab("") +
                  geom_point() + geom_line() + 
                  coord_flip() + 
                  facet_grid(Profile~Elements, scales = "free") +
                  theme_bw()+
                  theme(text = element_text(family = "Times New Roman"),
                        panel.grid = element_blank(),
                        legend.title = element_blank())

siani_data <- pxrf_data %>%
              filter(str_detect(Profile, "S")) %>%
              select("Profile":"Ti") %>%
              pivot_longer(cols = "Si":"Ti", names_to = "Elements", values_to = "Content")

siani_pxrf_plot <- ggplot(siani_data, aes(x = -Depth, y = Content)) +
                   xlab("Depth (cm)") + ylab("Content (%)") +
                   geom_point() + geom_line() + 
                   coord_flip() + 
                   facet_grid(Profile~Elements, scales = "free") +
                   theme_bw()+
                   theme(text = element_text(family = "Times New Roman"),
                         panel.grid = element_blank(),
                         legend.title = element_blank())

band_pxrf_plot + siani_pxrf_plot + plot_layout(ncol = 1)

ggsave("../../Figures/depth_func.jpeg", units = "mm", width = 210, height = 297, bg = "white")
