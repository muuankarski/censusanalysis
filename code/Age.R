raw <-  read.csv("data/raw/DoBKarelia.csv", sep=";", skip=6)
names(raw) <- c("measure","region","year","value")
df <- raw
raw <-  read.csv("data/raw/DoBNizhni.csv", sep=";", skip=6)
names(raw) <- c("region","measure","year","value")
df <- raw
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

write.csv(df.l, file="data/mod/ageKarelia.csv")
write.csv(df.l, file="data/mod/ageNizhni.csv")
write.csv(df.l, file="data/mod/ageMoscowOb.csv")



