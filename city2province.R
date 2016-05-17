library(stringr)
df.addr <- read.csv("AddressesCN.csv")
df.cityOnlyAddr <- df.addr[lapply(df.addr$city, str_length)>0 & lapply(df.addr$province, str_length)==0,]
df.province <- read.csv("city_province_cleaned.txt", header=F)
colnames(df.province) <- c("city", "provinces")
# Duplicate city cause the increase of 100 rows. approximately 2% of the totaly number of rows
df.cityAddr <- merge(df.cityOnlyAddr, df.province, by = "city")
df.cityAddr$province <- df.cityAddr$provinces
df.cityAddr <- df.cityAddr[, colnames(df.addr)]
df.addr <- rbind(df.addr[!df.addr$node_id %in% df.cityAddr$node_id,],df.cityAddr)
df.addr <- df.addr[order(df.addr$row),]
write.csv(df.addr, "AddressCNFinal.csv")

