install.packages(c("remotes", "sf", "tmap", "tidyverse"))
remotes::install_github("inSileco/iseDataRetrieval")

library(sf)
library(dplyr)
library(iseDataRetrieval)

# download files in gadm
dir.create("gadm")
df_dl <- data.frame(
  iso = c(rep("THA", 2), "MMR", "LAO", "MYS", "KHM", "VNM"),
  lvl = c(0:1, 0, 0, 0, 0, 0)
)
bndr <- list()
for (i in seq_len(nrow(df_dl))) {
  bndr[[i]] <- get_gadm(df_dl$iso[i], level = df_dl$lvl[i], path = "gadm", format = "shp")
}
names(bndr) <- paste0(df_dl$iso, df_dl$lvl)

# write province names
# NB this table was manually edited... ==> "tha_provinces_zones.csv"
write.csv(
  as.data.frame(sf::st_drop_geometry(bndr$THA1)[c("NAME_1", "HASC_1")]),
  "tha_provinces.csv"
)

# zone-region relationships
dt_rg <- data.frame(
  Region = c(
    rep("Central", 4),
    rep("Northern", 3),
    rep("North-Eastern", 4),
    rep("Sourthern", 2)
  ),
  Zone = c(0, 4:6, 1:3, 7:10, 11:12)
)
#
dt_pro <-  read.csv("tha_provinces_zones.csv") %>%
  inner_join(dt_rg) %>%
  select(NAME_1, Zone, Region, ID_BC)
# provinces
pro <- bndr$THA1
pro$id <- seq_len(nrow(bndr$THA1))
pro <- pro %>% inner_join(dt_pro)
# there is one too many province (one recent split)
pro <- pro %>%
  group_by(ID_BC) %>%
  summarize(geometry = st_union(geometry), nprovinces = n())
# drop Bueng Kan (together with Nong Kai they used to be one region)
dt_pro2 <- dt_pro[dt_pro$NAME_1 != "Bueng Kan", ]
#
pro <- pro %>% inner_join(dt_pro2)
pro_cen <- st_centroid(pro)

zon <- pro %>%
  group_by(Zone) %>%
  summarize(geometry = st_union(geometry), nprovinces = n())
class(zon) <- class(pro)
zon_cen <- st_centroid(zon)

reg <- pro %>%
    inner_join(dt_rg) %>%
    group_by(Region) %>%
    summarize(
      geometry = st_union(geometry),
      nprovinces = n()
    )
reg_cen <- st_centroid(reg)


# create map
library(tmap)

pal <-  c("#1f2732", "#eeb93c", "#e4fac2", "#bcdbf6")
lwd_bd <- 0.36
lwd_bd_l <- 0.22

khm <- tm_shape(bndr$KHM0) + tm_borders(col = pal[1], lwd = lwd_bd_l) + tm_fill(col = pal[3])
lao <- tm_shape(bndr$LAO0) + tm_borders(col = pal[1], lwd = lwd_bd_l) + tm_fill(col = pal[3])
mmr <- tm_shape(bndr$MMR0) + tm_borders(col = pal[1], lwd = lwd_bd_l) + tm_fill(col = pal[3])
mys <- tm_shape(bndr$MYS0) + tm_borders(col = pal[1], lwd = lwd_bd_l) + tm_fill(col = pal[3])
vnm <- tm_shape(bndr$VNM0) + tm_borders(col = pal[1], lwd = lwd_bd_l) + tm_fill(col = pal[3])
#
thap <- tm_shape(pro) + tm_borders(col = pal[1], lwd = lwd_bd_l) + tm_fill(col = pal[2])
thap_txt <- tm_shape(pro_cen) + tm_text("ID_BC", size = .3)
#
thaz <- tm_shape(zon) + tm_borders(col = pal[1], lwd = lwd_bd) + tm_fill(col = pal[2])
thaz_txt <- tm_shape(zon_cen) + tm_text("Zone", size = .54)
#
thar <- tm_shape(reg) + tm_borders(col = pal[1], lwd = lwd_bd) + tm_fill(col = pal[2])
thar_txt <- tm_shape(reg_cen) + tm_text("Region", size = .51, ymod = c(.3, 0, 0, .65), xmod = c(0, 0, 0, -0.7))

map1 <- thap + khm + lao + mmr + mys + vnm + tm_layout(bg.color = pal[4]) + thap_txt
map2 <- thaz + khm + lao + mmr + mys + vnm + tm_layout(bg.color = pal[4]) + thaz_txt
map3 <- thar + khm + lao + mmr + mys + vnm + tm_layout(bg.color = pal[4]) + thar_txt


tmap_save(map1, "provinces.png", height = 90, width = 50, dpi = 600, units = "mm", outer.margins = 0.01)
tmap_save(map2, "zones.png", height = 90, width = 50, dpi = 600, units = "mm", outer.margins = 0.01)
tmap_save(map3, "regions.png", height = 90, width = 50, dpi = 600, units = "mm", outer.margins = 0.01)

# Use imageMagick afterwards
# convert +append regions.png zones.png provinces.png all.png
