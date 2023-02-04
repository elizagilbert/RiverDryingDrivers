library(tidyverse)
library(ggmap)
library(sf)
library(tidyverse)
library(maps)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)

#gage data
dat <- read.csv("Data/Raw/GageReturnLocations.csv")
dat_latlong <- read.csv("Data/Raw/RiverMileLatLong_And_PercentDryCalculation_FromArcGISPro.csv") %>% 
  select(!PerDry & !PercentDry & !DryPer)
dat_percentdry <- read.csv("Data/Processed/SummaryForGIS.csv")

#remove some of the gages
dat2 <- dat %>% 
  filter(Type != "Precip" & Type != "Tributary" & GageID != "GHCND:USC00293649" & GageID != "GHCND:USC00290640" 
         & GageID != "GHCND:USC00290983" & GageID != "GHCND:USW00053163" & GageID != "PERCN" & GageID != "PERCN" &
           GageID != "CACCN" & GageID !="CHACN" & GageID != "CHICN") %>%
  mutate(Long2 = case_when(Type == "Return" & Owner == "Not gaged" ~ Long - 0.035,
                             TRUE ~ Long + 0.035)) %>% 
  mutate(Long3 = case_when(Type == "TempPrecip" ~ Long - 0.025,
                           TRUE ~ Long2)) %>% 
  mutate(Long4 = case_when(Type == "Diversion" ~ Long - 0.02,
                           TRUE ~ Long3)) %>% 
  mutate(Long5 = case_when(GageID == "8358300" ~ Long - 0.02,
                           TRUE ~ Long4)) %>% 
  mutate(Lat2 = case_when(GageID == "SBYPS" ~ Lat - 0.015,
                           TRUE ~ Lat)) %>% 
  mutate(Type = case_when(GageID == "8358300" ~ "Low flow conveyance",
                          TRUE ~ Type),
         Type = case_when(Owner == "Not gaged" ~ "Return not gaged",
                          TRUE ~ Type),
         Type = case_when(Type == "River Discharge" ~ "River discharge",
                          TRUE ~ Type),
         Type = case_when(Type == "TempPrecip" ~ "Temperature and precipitation",
                          TRUE ~ Type)) 

#get river mile lat long and percent dry into dataframe called dat_dry
dat_dry <- dat_latlong %>% 
  left_join(dat_percentdry, by = "RoundRM")

dat_dry2 <- dat_dry %>% 
  mutate(PercentDry = replace_na(PerDry, 0),
         PercentDry = PercentDry*100)

# set my coordinate extents for plotting with `ggmap`
myLocation <- c(-107.5, 35.1, -106.5, 33.5)

# get the map I want to use for plotting river drainages with `ggmap` function `getmap`
# save as an object
map1 <- get_map(location=myLocation, crop = F,
                color = "bw",
                maptype="terrain",
                source="google",
                zoom=10) 

sitemap <- ggmap(map1) # use the BW terrain option
#rivers_df <- fortify(rivers10) # make river data spatial for ggplot

# plot map with river data
sitemap_pl <- sitemap +
  geom_point(data = dat_dry2, aes(x = Longitude, y = Latitude2, color = PercentDry), 
             size = 2, alpha = 1)+
  scale_color_gradient2(low = "light blue", mid = "yellow", high = "red", midpoint = 9.5, 
                        name = "Percent dry", guide = guide_colorbar(reverse = T))+

  geom_point(data = dat2, aes(x = Long5, y = Lat2, shape = Type, fill = Type, size = Type))+
  scale_shape_manual(values=c(25, 8, 23,  7, 16, 22), name = "Measurement type")+
  scale_fill_manual(values = c("purple", "red", "blue", "blue", "red", "dark green"), guide = "none")+
  scale_size_manual(values=c(2, 2, 2, 2, 2, 2), name = "Measurement type")+ 
  geom_hline(yintercept = c(34.9153, 34.67, 34.409, 33.7201), color = "black", size = 0.8,
             linetype = "dashed")+
  
  annotate(geom="text", x=-107.3, y=34.89, label="Reach 1 (RM 180.0-150.1)", color="black", size =3.5)+ 
  annotate(geom="text", x=-107.3, y=34.64, label="Reach 2 (RM 150.0-130.0)", color="black", size = 3.5)+ 
  annotate(geom="text", x=-107.3, y=34.38, label="Reach 3 (RM 130.0-74.0)", color="black", size =3.5)+
  
  annotate("segment", x = -106.7, y = 34.74, xend = -106.6, yend = 34.74) +
  annotate(geom = "text", x = -106.43, y = 34.75, label = "Los Chavez Aug 2016", size = 2.5)+
  annotate("segment", x = -106.8, y = 33.9561, xend = -106.7, yend = 33.9561) +
  annotate(geom = "text", x = -106.45, y = 33.96, 
           label = "Neil Cupp no pumping since ~ 2006\n Socorro Hub not gaged started 2020 ", size = 2.5)+

  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())+
  ylab("") + xlab("")

  

jpeg("Figures/Sitemap.jpg", units="in", width=7, height=9, res=300, quality = 100)
sitemap_pl
dev.off()


