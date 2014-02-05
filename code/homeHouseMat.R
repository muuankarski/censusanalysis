
# Nizhni 
raw <-  read.csv("data/raw/housematerial_nizhni.csv", sep=";", skip=6)
names(raw) <- c("region","measure","material","value")

# Krasnodar
raw <-  read.csv("data/raw/housematerial_krasnodar.csv", sep=",", skip=6)
names(raw) <- c("measure","material","region","value")

# Sochi
raw <-  read.csv("data/raw/housematerial_sochi.csv", sep=",", skip=6)
names(raw) <- c("measure","material","region","value")



df <- raw
n.row <-  nrow(df)
df <- df[-n.row:-(n.row-2),]

# translate material
df$material <- as.character(df$material)
df$material[df$material == "Не указано"] <- "NotSpecified"
df$material[df$material == "Кирпич, камень"] <- "BrickStone"
df$material[df$material == "Панель"] <- "Panel"
df$material[df$material == "Блок"] <- "Block"
df$material[df$material == "Дерево"] <- "Timber"
df$material[df$material == "Смешанный материал"] <- "MixedMaterial"
df$material[df$material == "Другой материал"] <- "OtherMaterial"
df$material <- as.factor(df$material)

# make number relative
library(reshape2)
df.wide <- dcast(df, region + measure ~ material, value.var = "value")

df.wide$sum <- rowSums(df.wide[,3:9])
df.wide$ShareBlock <- round(df.wide[,3]/df.wide[,10]*100,2)
df.wide$ShareBrickStone <- round(df.wide[,4]/df.wide[,10]*100,2)
df.wide$ShareMixedMaterial <- round(df.wide[,5]/df.wide[,10]*100,2)
df.wide$ShareNotSpecified <- round(df.wide[,6]/df.wide[,10]*100,2)
df.wide$ShareOtherMaterial <- round(df.wide[,7]/df.wide[,10]*100,2)
df.wide$SharePanel <- round(df.wide[,8]/df.wide[,10]*100,2)
df.wide$ShareTimber <- round(df.wide[,9]/df.wide[,10]*100,2)

# lets order the regions by size for plotting
df.wide <- df.wide[order(df.wide$sum), ]
df.wide$region <- factor(df.wide$region, 
                         levels = as.character(df.wide[order(df.wide$sum), 1]))
df.wide.nizhni <- df.wide
# back to long for plotting
df.long <- melt(df.wide, id.vars = "region", 
                measure.vars=c("ShareBlock",
                               "ShareBrickStone",
                               "ShareMixedMaterial",
                               "ShareNotSpecified",
                               "ShareOtherMaterial",
                               "SharePanel",
                               "ShareTimber"))

df.long <- df.long[!is.na(df.long$value), ]


## Nizhni
library(RCurl)
GHurl <- getURL("https://raw.github.com/muuankarski/data/master/russia/nizhni_key_rayon.csv")
region_key_nizhni <-  read.csv(text = GHurl)
dat <- merge(df.long,region_key_nizhni,
             by="region")
save(dat, file="data/mod/houseMatNizhni.RData")

## Krasnodar Krai
library(RCurl)
GHurl <- getURL("https://raw.github.com/muuankarski/data/master/russia/krasnodar_key_rayon.csv")
region_key_krasnodar <-  read.csv(text = GHurl)
dat <- merge(df.long,region_key_krasnodar,
             by="region")
save(dat, file="data/mod/houseMatKrasnodar.RData")

## Sochi
library(ggmap)
location <- as.character(df.long$region)
code <- geocode(location, output = c("latlon", "latlona", "more", "all"), messaging = FALSE, 
                sensor = FALSE, override_limit = FALSE)
spat.data <- cbind(location, code)
dat2 <- merge(df.long, spat.data, by.x = "region", by.y = "location")
# dirty trick to remove duplicates
dat2$tempvar <- paste(dat2$region,dat2$variable)
dat2$dup <- duplicated(dat2$tempvar)
dat <- dat2[dat2$dup == FALSE,]
dat <- dat[,1:5]
dat$region[dat$region == "Сельские населенные пункты, подчиненные администрации пгт. Красная Поляна, находящегося в подчинении администрации Адлерского района г. Сочи"] <- "г. Сочи"
save(dat, file="data/mod/houseMatSochi.RData")



