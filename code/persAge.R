library(RCurl)

## Karelia
raw <-  read.csv("data/raw/DoBKarelia.csv", sep=";", skip=6)
names(raw) <- c("measure","region","year","value")
df <- raw
## Nizhni
raw <-  read.csv("data/raw/DoBNizhni.csv", sep=";", skip=6)
names(raw) <- c("region","measure","year","value")
df <- raw
## Moscow
raw <-  read.csv("data/raw/DoBMoscowOb.csv", sep=";", skip=6)
names(raw) <- c("region","year","measure","value")
df <- raw


n.row <-  nrow(df)
df <- df[-n.row:-(n.row-2),]
df$value <- as.numeric(df$value)
df <- df[df$value > 0,]
#df <- df[df$value != "Год рождения не указан",]

df <- df[df$region != "Всего",]
df$region <- factor(df$region)
df$measure <- NULL

df$year <- paste("x",df$year,sep="")

library(reshape2)
df.wide <- dcast(df, 
                 region ~ year, 
                 value.var = "value")

df.wide <- df.wide[-102:-103]
mat <- as.matrix(df.wide[,-1])
matnorm <- mat/rowSums(mat, na.rm=TRUE)*100

dat <- as.data.frame(matnorm)
df.w <- cbind(df.wide$region,dat)

df.l <- melt(df.w, id.vars = "df.wide$region")
library(stringr)
df.l$variable <- as.character(df.l$variable)
df.l$variable <- str_replace(df.l$variable, "x","")

df.l$variable <- factor(df.l$variable)
df.l$variable <- as.numeric(levels(df.l$variable))[df.l$variable]

names(df.l) <- c("region","year","value")
df.l$age <- 2002 - df.l$year

## Karelia
GHurl <- getURL("https://raw.github.com/muuankarski/data/master/russia/karelia_key_rayon.csv")
region_key_karelia <-  read.csv(text = GHurl)
dat <- merge(df.l,region_key_karelia,
             by="region")
write.csv(dat, file="data/mod/ageKarelia.csv")

## Nizhni
GHurl <- getURL("https://raw.github.com/muuankarski/data/master/russia/nizhni_key_rayon.csv")
region_key_nizhni <-  read.csv(text = GHurl)
dat <- merge(df.l,region_key_nizhni,
             by="region")
write.csv(dat, file="data/mod/ageNizhni.csv")

## Moscow Oblast
GHurl <- getURL("https://raw.github.com/muuankarski/data/master/russia/moscowoblast_key_rayon.csv")
region_key_moscowOb <-  read.csv(text = GHurl)
dat <- merge(df.l,region_key_moscowOb,
             by="region")
write.csv(dat, file="data/mod/ageMoscowOb.csv")



