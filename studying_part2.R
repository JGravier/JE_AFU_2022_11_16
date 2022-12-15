library(tidyverse)
library(sf)
library(spatstat)
library(spacetimeLPP)

# data: Geohistorical data and SODUCO
network <- st_read(dsn = "data/1836_jacoubet.shp")
bijoutiers <- st_read(dsn = "data/jewellers_1839.gpkg") %>%
  rename(geometry = geom)

# visu distribution spatiale
ggplot() +
  geom_sf(data = network, color = "grey85") +
  geom_sf(data = bijoutiers, alpha = 0.9, size = 0.9, color = "red", show.legend = FALSE) +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  ggthemes::theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ggtitle("Bijoutiers") +
  labs(caption = "J. Gravier 2022\ndata: GeoHistoricalData\n@_AFUrbain")

ggsave(filename = "output_fig/bijoutiers.png", plot = last_plot(), 
       dpi = 300, width = 18, height = 14, units = "cm")


#### clustering ####
clusters_650 <- dist_clustering(pp = bijoutiers, network = network, distparam = 650)

ggplot() +
  geom_sf(data = network, color = "grey85") +
  geom_sf(data = clusters_650, aes(color = cluster), alpha = 0.9, size = 0.7, show.legend = FALSE) +
  geom_sf(data = clusters_650 %>% 
            group_by(cluster, source_annee) %>% 
            summarise() %>% st_convex_hull(), 
          aes(color = cluster), show.legend = FALSE, alpha = 0.2, size = 0.5) +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  ggthemes::theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ggtitle("Bijoutiers") +
  labs(caption = "J. Gravier 2022\ndata: GeoHistoricalData\n@_AFUrbain")

ggsave(filename = "output_fig/bijoutiers_cluster.png", plot = last_plot(), 
       dpi = 300, width = 18, height = 14, units = "cm")

#### sur plusieurs distparam
distparam <- seq(500, 800, 50)

output_sf <- list()

for (i in 1:length(distparam)) {
  output_sf[[i]] <- dist_clustering(pp = bijoutiers, network = network, distparam = distparam[i]) %>%
    mutate(distparam = distparam[i])
}

output_sf_final <- data.table::rbindlist(output_sf) %>%
  as_tibble() %>%
  st_as_sf() %>%
  st_transform(x = ., crs = 2154)

# visualisation
ggplot() +
  geom_sf(data = network, color = "grey85") +
  geom_sf(data = output_sf_final, aes(color = cluster), alpha = 0.9, size = 0.4, show.legend = FALSE) +
  geom_sf(data = output_sf_final %>% 
            group_by(cluster, distparam) %>% 
            summarise() %>% st_convex_hull() , 
          aes(color = cluster), show.legend = FALSE, alpha = 0.2, size = 0.4) +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  ggthemes::theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ggtitle("Bijoutiers") +
  labs(caption = "J. Gravier 2022\ndata: GeoHistoricalData\n@_AFUrbain") +
  facet_wrap(~distparam, nrow = 2)

ggsave(filename = "output_fig/bijoutiers_cluster_explo.png", plot = last_plot(), 
       dpi = 300, width = 28, height = 18, units = "cm")

### pour logique de demonstration: quartier qui est percu uniquement d un point de vue carto ###
clusters_800 <- dist_clustering(pp = bijoutiers, network = network, distparam = 800)

pppbijoutiers <- as.ppp(X = as(object = bijoutiers, Class = "Spatial"))
matrix_bij_eucli <- pairdist(X = pppbijoutiers)
cluster_matrix <- hclust(d = as.dist(matrix_bij_eucli), method = "average")
cut_tree <- cutree(cluster_matrix, h = 800)
sf_cutree <- bijoutiers %>%
  mutate(cluster = factor(cut_tree, levels = 1:distparam))

sf_cutree <- sf_cutree %>%
  mutate(type = "euclidienne") %>%
  bind_rows(
    clusters_800 %>%
      mutate(type = "reseau")
  )

#visu
ggplot() +
  geom_sf(data = network, color = "grey85") +
  geom_sf(data = sf_cutree, aes(color = cluster), alpha = 0.9, size = 0.7, show.legend = FALSE) +
  geom_sf(data = sf_cutree %>% 
            group_by(cluster, type) %>% 
            summarise() %>% st_convex_hull(), 
          aes(color = cluster), show.legend = FALSE, alpha = 0.2, size = 0.5) +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  ggthemes::theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ggtitle("Bijoutiers") +
  labs(caption = "J. Gravier 2022\ndata: GeoHistoricalData\n@_AFUrbain") +
  facet_wrap(~type)

ggsave(filename = "output_fig/bijoutiers_cluster_disteucli.png", plot = last_plot(), 
       dpi = 300, width = 25, height = 14, units = "cm")


#### une vue d'un quartier du quotidien ####
epiciers <- st_read(dsn = "data/grocers_1839.gpkg")

# visu distribution spatiale
ggplot() +
  geom_sf(data = network, color = "grey85") +
  geom_sf(data = epiciers, alpha = 0.9, size = 0.9, color = "blue", show.legend = FALSE) +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  ggthemes::theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ggtitle("Epiciers") +
  labs(caption = "J. Gravier 2022\ndata: GeoHistoricalData\n@_AFUrbain")

ggsave(filename = "output_fig/epiciers.png", plot = last_plot(), 
       dpi = 300, width = 18, height = 14, units = "cm")


# bati Vasserot
bati_vasserot <- st_read(dsn = "data/96-batiL93/BATI_Project.shp") %>%
  filter(BATI == 1)

ggplot() +
  geom_sf(data = network, color = "grey85") +
  geom_sf(data = bati_vasserot, color = "grey20", fill = "grey20") +
  geom_sf(data = epiciers, alpha = 0.9, size = 0.9, color = "blue", show.legend = FALSE) +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  ggthemes::theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ggtitle("Epiciers et le bâti Vasserot") +
  labs(caption = "J. Gravier 2022\ndata: GeoHistoricalData et © ALPAGE: A.L. Berthe Vasserot Bati\n@_AFUrbain")

ggsave(filename = "output_fig/epiciersplusbati.png", plot = last_plot(), 
       dpi = 300, width = 18, height = 14, units = "cm")


### delimitation des quartiers du quotidien
bati_vasserot_categ1_point <- bati_vasserot %>%
  st_centroid()

# snaping points on network
bati_vasserot_categ1_point2 <- maptools::snapPointsToLines(points = as(bati_vasserot_categ1_point, "Spatial"),
                                                          lines = as(network, "Spatial"), maxDist = 500, idField = "id")


bati_vasserot_categ1_point2 <- bati_vasserot_categ1_point2 %>% 
  st_as_sf()

# distances
batilpp <- lpp(X = as.ppp(X = as(object = bati_vasserot_categ1_point2, Class = "Spatial")), 
                  L = as.linnet(X = as(object = network, Class = "Spatial")))
epicierslpp <- lpp(X = as.ppp(X = as(object = epiciers, Class = "Spatial")), 
               L = as.linnet(X = as(object = network, Class = "Spatial")))

distancesepiciersbati <- crossdist.lpp(X = batilpp, Y = epicierslpp)

distancesepiciersbati[upper.tri(x = distancesepiciersbati, diag = TRUE)] <- NA

observed_dist <- distancesepiciersbati %>%
  tibble::as_tibble() %>%
  tibble::rowid_to_column(var = "Pi") %>%
  tidyr::pivot_longer(cols = -Pi, names_to = "P", values_to = "dist_pi_p") %>%
  dplyr::filter(!is.na(dist_pi_p)) %>%
  dplyr::mutate(P = stringr::str_replace_all(string = P, pattern = "V", replacement = ""))

observed_dist %>%
  group_by(Pi) %>%
  slice(which.min(dist_pi_p)) %>%
  ungroup() %>%
  summarise(mean = mean(dist_pi_p), mediane = median(dist_pi_p),
            et = sd(dist_pi_p), min = min(dist_pi_p), max = max(dist_pi_p))

batilesplusproches <- observed_dist %>%
  group_by(Pi) %>%
  slice(which.min(dist_pi_p))

bati_vasserot <- bati_vasserot %>%
  rowid_to_column() %>%
  left_join(x = ., y = batilesplusproches, by = c("rowid" = "Pi"))


# visualisation
ggplot() +
  geom_sf(data = network, color = "grey85") +
  geom_sf(data = bati_vasserot, aes(color = P), alpha = 0.9, size = 0.7, show.legend = FALSE) +
  geom_sf(data = bati_vasserot %>% 
            st_centroid() %>%
            group_by(P) %>% 
            summarise() %>% st_convex_hull(), 
          aes(color = P), show.legend = FALSE, alpha = 0.2, size = 0.5) +
  geom_sf(data = epiciers, alpha = 0.9, size = 0.9, color = "blue", show.legend = FALSE) +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  ggthemes::theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ggtitle("Epiciers et le bâti Vasserot") +
  labs(caption = "J. Gravier 2022\ndata: GeoHistoricalData et © ALPAGE: A.L. Berthe Vasserot Bati\n@_AFUrbain")

ggsave(filename = "output_fig/epiciersbatiquartiers.png", plot = last_plot(), 
       dpi = 300, width = 18, height = 14, units = "cm")

# bounding box
bbepicier <- epiciers %>%
  rowid_to_column() %>%
  filter(rowid %in% c(932, 208, 492)) %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_as_sf() %>%
  st_transform(crs = 2154)

bbepicierbati <- st_crop(x = bati_vasserot, y = bbepicier)
bbcropnetwork <- st_crop(x = network, y = bbepicier)
bbepiciercrop <- st_crop(x = epiciers %>% st_transform(crs=2154), y = bbepicier)

# visu zoom
ggplot() +
  geom_sf(data = bbcropnetwork, color = "grey85") +
  geom_sf(data = bbepicierbati, aes(color = P, fill = P), size = 0.7, show.legend = FALSE) +
  geom_sf(data = bbepiciercrop, alpha = 0.9, size = 0.9, color = "blue", show.legend = FALSE) +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  ggthemes::theme_map() +
  ggtitle("Epiciers et le bâti Vasserot") +
  labs(caption = "J. Gravier 2022\ndata: GeoHistoricalData et © ALPAGE: A.L. Berthe Vasserot Bati\n@_AFUrbain")

ggsave(filename = "output_fig/epiciersbatiquartiers_zoom.png", plot = last_plot(), 
       dpi = 300, width = 11, height = 18, units = "cm")
