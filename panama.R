library(tm)
library(dplyr)
library(gdata)
library(stringr)
library(stringdist)
df.addr <- read.csv('Addresses.csv')
df.area <- read.csv('city_province_cleaned.txt', header=FALSE)
provinces <- unique(df.area$V2)
cities <- unique(df.area$V1)

df.addr <- df.addr[df.addr$country_codes %in% c('CHN'), ]
print(nrow(df.addr))
df.addr$loweraddr <- tolower(df.addr$address)
addresses <- df.addr$loweraddr
corpus <- Corpus(VectorSource(addresses))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
addresses <- lapply(corpus, as.character)

# Split addresses into vector of strings, e.g. c("Chaoyang", "District", "beijing")
addrwords <- lapply(addresses, function(x)strsplit(x, " "))

# Avoid things like "beijing road" being included, 
# city or province always appears in the tail of addresses
addr.city <- lapply(addrwords, function(w)tail(intersect(unlist(w), cities), n=1))
addr.province <- lapply(addrwords, function(w)tail(intersect(unlist(w), provinces), n=1))

# intersect returns a list, but we want a string
df.addr$city <- lapply(addr.city, function(city) if(paste(city, "", sep="") == "character(0)") return("") else return(paste(city,"", sep="")))
df.addr$province <- lapply(addr.province, function(province) if(paste(province, "", sep="") == "character(0)") return("") else return(paste(province, "", sep="")))
df.addr$city <- unlist(df.addr$city, use.names=F)
df.addr$province <- unlist(df.addr$province, use.names=F)


# The above steps only found "shandong", 
# We use fuzzy match to find "shan dong"
fuzzy.province <- function(addrwords, provinces, dist = 0) {
  for(province in provinces) {
    mtr <- stringdistmatrix(addrwords, province)
    for(i in (seq_along(mtr[,1]))) {
      if(i<length(mtr[,1])) {
        # The matrix distance should be equal or shorter than string length of province
        if (mtr[,1][i] + mtr[,1][i+1] <= str_length(province)) {
          if(startsWith(province, addrwords[i]) & abs(str_length(province) - str_length(addrwords[i]) - str_length(addrwords[i+1])) <= dist ) {
            print(province)
            return(province)
          }
        }
      }
    }
  }
  return("")
}

# Similar to fuzzy.province, but match in a more strict way
fuzzy.city<- function(addrwords, cities) {
  for(city in cities) {
    mtr <- stringdistmatrix(addrwords, city)
    for(i in (seq_along(mtr[,1]))) {
      if(i<length(mtr[,1])) {
        if (mtr[,1][i] + mtr[,1][i+1] <= str_length(city)) {
          if(paste(addrwords[i], addrwords[i+1], sep="") == city){
            print(city)
            return(city)
          }
        }
      }
    }
  }
  return("")
}

attach(df.addr)
# Find province, city info for addresses that not found in previous steps
df.other <- df.addr[lapply(city, str_length)==0 & lapply(province, str_length)==0, ]
other <- df.other$loweraddr
corpus <- Corpus(VectorSource(other))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
addresses <- lapply(corpus, as.character)
addrwords <- lapply(addresses, function(x)strsplit(x, " "))
addr.province <- lapply(addrwords, function(w)fuzzy.province(unlist(w), provinces, dist=1))
addr.city<- lapply(addrwords, function(w)fuzzy.city(unlist(w), cities))
df.other$province <- paste(addr.province, "", sep="")
df.other$city <- paste(addr.city, "", sep="")
detach(df.addr)

df.addr <- rbind(df.addr[!df.addr$node_id %in% df.other$node_id,],df.other)
df.addr$row <- strtoi(row.names(df.addr))
df.addr <- df.addr[order(df.addr$row),]
write.csv(df.addr, "AddressesCN.csv")
