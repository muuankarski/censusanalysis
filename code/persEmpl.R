library(RCurl)

## Karelia
raw <-  read.csv("data/raw/emplKarelia.csv", sep=",", skip=6)
names(raw) <- c("region","measure","variable","value")
df <- raw
## Nizhni
raw <-  read.csv("data/raw/emplNizhni.csv", sep=",", skip=6)
names(raw) <- c("region","measure","variable","value")
df <- raw

## Moscow
raw <-  read.csv("data/raw/emplMoscowOb.csv", sep=",", skip=6)
names(raw) <- c("measure","region","variable","value")
df <- raw

## Krasnodar
raw <-  read.csv("data/raw/emplKrasnodar.csv", sep=",", skip=6)
names(raw) <- c("measure","variable","region","value")
df <- raw



#######################

n.row <-  nrow(df)
df <- df[-n.row:-(n.row-2),]
df$value <- as.numeric(df$value)
df <- df[df$value > 0,]
#df <- df[df$value != "Год рождения не указан",]

df <- df[df$region != "Всего",]
df$region <- factor(df$region)
df$measure <- NULL

library(reshape2)
df.wide <- dcast(df, 
                 region ~ variable, 
                 value.var = "value")

mat <- as.matrix(df.wide[,-1])
matnorm <- mat/rowSums(mat, na.rm=TRUE)*100

dat <- as.data.frame(matnorm)
df.w <- cbind(df.wide$region,dat)

df.l <- melt(df.w, id.vars = "df.wide$region")
library(stringr)

names(df.l) <- c("region","variable","value")

df.l$variable <- as.character(df.l$variable)

df.l$variableEn[df.l$variable %in% "Не указавшие положение в занятости"] <-  "Not employed"
df.l$variableEn[df.l$variable %in% "Работающие не по найму без привлечения наемных работников"] <-  "Self-employed"
df.l$variableEn[df.l$variable %in% "Работающие не по найму - иное"] <-  "Self-employed"
df.l$variableEn[df.l$variable %in% "Работающие не по найму - не указавшие привлечение наемных работников"] <-  "Self-employed"
df.l$variableEn[df.l$variable %in% "Работающие не по найму с привлечением наемных работников"] <-  "Self-employed"
df.l$variableEn[df.l$variable %in% "Работающие по найму"] <-  "Employed"

df.l$variableEn <- factor(df.l$variableEn, levels=c("Not employed",
                                                    "Self-employed",
                                                    "Employed"))

## Karelia
GHurl <- getURL("https://raw.github.com/muuankarski/data/master/russia/karelia_key_rayon.csv")
region_key_karelia <-  read.csv(text = GHurl)
dat <- merge(df.l,region_key_karelia,
             by="region")
save(dat, file="data/mod/emplKarelia.RData")

## Nizhni
GHurl <- getURL("https://raw.github.com/muuankarski/data/master/russia/nizhni_key_rayon.csv")
region_key_nizhni <-  read.csv(text = GHurl)
dat <- merge(df.l,region_key_nizhni,
             by="region")
save(dat, file="data/mod/emplNizhni.RData")

## Moscow Oblast
GHurl <- getURL("https://raw.github.com/muuankarski/data/master/russia/moscowoblast_key_rayon.csv")
region_key_moscowOb <-  read.csv(text = GHurl)
dat <- merge(df.l,region_key_moscowOb,
             by="region")
save(dat, file="data/mod/emplMoscowOb.RData")

## Krasnodar Krai
library(RCurl)
GHurl <- getURL("https://raw.github.com/muuankarski/data/master/russia/krasnodar_key_rayon.csv")
region_key_krasnodar <-  read.csv(text = GHurl)
dat <- merge(df.l,region_key_krasnodar,
             by="region")
save(dat, file="data/mod/emplKrasnodar.RData")


