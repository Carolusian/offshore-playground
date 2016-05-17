library(maptools)
library(dplyr)
library(plyr)
library(ggplot2)
library(ggvis)
quartz(family="STKaiti")

cnmap <- readShapePoly("bou2_4p.shp")
class(cnmap)

prov_cn <- unique(cnmap$NAME)
prov_cn <- prov_cn[!is.na(prov_cn)]
prov_en <- c("heilongjiang", "mongolia", "xinjiang", "jilin",
             "liaoning", "gansu", "hebei", "beijing", "shanxi",
             "tianjin", "shaanxi", "ningxia", "qinghai", "shandong",
             "tibet", "henan", "jiangsu", "anhui", "sichuan", "hubei",
             "chongqing", "shanghai", "zhejiang", "hunan", "jiangxi",
             "yunnan", "guizhou", "fujian", "guangxi", "taiwan", 
             "guangdong", "hongkong", "hainan")

prov <- data.frame(prov_cn, prov_en)

id_prov <- cnmap@data                                               %>%
  mutate(prov_en = sapply(NAME, 
                          function(x)
                            prov_en[which(prov_cn == x)]))     %>%
  mutate(prov_cn = as.character(NAME),
         prov_en = as.character(prov_en))                           

id <- rownames(id_prov)
id_prov <- cbind(id=id, id_prov)
id_prov <- id_prov %>% select (id, prov_cn, prov_en)

# you can borrow plyr package in a more efficient way
cnmapdf <- plyr::join(fortify(cnmap), id_prov, by = "id")

df.addr <- read.csv("AddressCNFinal.csv")
df.addr <- df.addr[df.addr$province!="", ]
df.addr <- count(df.addr, "province")
df.addr$prov_en <- df.addr$province


map2df <- cnmapdf %>%
    plyr::join(df.addr, by = "prov_en") %>%
    mutate(freq = as.numeric(freq))

map <- map2df                                                              %>%
  ggplot(aes(x = long, y = lat, group = group, fill=freq)) +
  geom_polygon(color = "grey") +
  scale_fill_gradient(low = "blue", high = "red") +
  coord_map("polyconic")
