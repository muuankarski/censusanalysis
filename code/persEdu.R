library(RCurl)

## Karelia
raw <-  read.csv("data/raw/eduKarelia.csv", sep=";", skip=6)
names(raw) <- c("region","measure","educ","value")
df <- raw
## Nizhni
raw <-  read.csv("data/raw/eduNizhni.csv", sep=";", skip=6)
names(raw) <- c("region","measure","educ","value")
df <- raw
## Moscow
raw <-  read.csv("data/raw/eduMoscowOb.csv", sep=",", skip=6)
names(raw) <- c("region","measure","educ","value")
df <- raw
## Krasnodar
raw <-  read.csv("data/raw/eduKrasnodar.csv", sep=",", skip=6)
names(raw) <- c("measure","educ","region","value")
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
                 region ~ educ, 
                 value.var = "value")

mat <- as.matrix(df.wide[,-1])
matnorm <- mat/rowSums(mat, na.rm=TRUE)*100

dat <- as.data.frame(matnorm)
df.w <- cbind(df.wide$region,dat)

df.l <- melt(df.w, id.vars = "df.wide$region")
library(stringr)

names(df.l) <- c("region","variable","value")

df.l$variable <- as.character(df.l$variable)

df.l$variableEn[df.l$variable %in% "Начальное общее (начальное)"] <-  "Initial total (initial)"
df.l$variableEn[df.l$variable %in% "Не имеющие начального общего образования - не умеющие читать и писать (неграмотные)"] <-  "Not having a primary education - who can not read and write (illiterate)"
df.l$variableEn[df.l$variable %in% "Не имеющие начального общего образования - умеющие читать и писать"] <-  "Not having a primary education - can read and write"
df.l$variableEn[df.l$variable %in% "Неполное высшее профессиональное (незаконченное высшее) "] <-  "Part-time higher professional (incomplete higher education)"
df.l$variableEn[df.l$variable %in% "Не указавшие образование"] <-  "Does not have education"
df.l$variableEn[df.l$variable %in% "Основное общее (неполное среднее)"] <-  "Basic general (lower secondary)"
df.l$variableEn[df.l$variable %in% "Послевузовское профессиональное"] <-  "Graduate Professional"
df.l$variableEn[df.l$variable %in% "Среднее (полное) общее"] <-  "Secondary (complete)"
df.l$variableEn[df.l$variable %in% "Среднее профессиональное (среднее специальное)"] <-  "Vocational (secondary special)"
df.l$variableEn[df.l$variable %in% "Высшее профессиональное (высшее)"] <-  "Higher professional education (higher)"

df.l$variableEn <- factor(df.l$variableEn, levels=c("Not having a primary education - who can not read and write (illiterate)",
                                                    "Not having a primary education - can read and write",
                                                    "Does not have education",
                                                    "Initial total (initial)",
                                                    "Basic general (lower secondary)",
                                                    "Secondary (complete)",
                                                    "Vocational (secondary special)",
                                                    "Part-time higher professional (incomplete higher education)",
                                                    "Higher professional education (higher)",
                                                    "Graduate Professional"))


## Karelia
GHurl <- getURL("https://raw.github.com/muuankarski/data/master/russia/karelia_key_rayon.csv")
region_key_karelia <-  read.csv(text = GHurl)
dat <- merge(df.l,region_key_karelia,
             by="region")
save(dat, file="data/mod/eduKarelia.RData")

## Nizhni
GHurl <- getURL("https://raw.github.com/muuankarski/data/master/russia/nizhni_key_rayon.csv")
region_key_nizhni <-  read.csv(text = GHurl)
dat <- merge(df.l,region_key_nizhni,
             by="region")
save(dat, file="data/mod/eduNizhni.RData")

## Moscow Oblast
GHurl <- getURL("https://raw.github.com/muuankarski/data/master/russia/moscowoblast_key_rayon.csv")
region_key_moscowOb <-  read.csv(text = GHurl)
dat <- merge(df.l,region_key_moscowOb,
             by="region")
save(dat, file="data/mod/eduMoscowOb.RData")

## Krasnodar Krai
library(RCurl)
GHurl <- getURL("https://raw.github.com/muuankarski/data/master/russia/krasnodar_key_rayon.csv")
region_key_krasnodar <-  read.csv(text = GHurl)
dat <- merge(df.l,region_key_krasnodar,
             by="region")
save(dat, file="data/mod/eduKrasnodar.RData")

