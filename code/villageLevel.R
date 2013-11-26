## Nizhni


### Age in the villages

```{rcenNizPersAgeVillage}
raw <-  read.csv("data/DoBNizhniVillage.csv", sep=";", skip=6)
names(raw) <- c("region","measure","year","value")

raw$test <- paste(raw$region,raw$year,sep="_")
raw$dup <- duplicated(raw$test)
df <- raw[raw$dup == FALSE,]

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
Rsum <- rowSums(mat, na.rm=TRUE, dims = 1)
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

```



```{rcenNizPersAgeVillageMap1}
# library(stringr)
# df.l$regionGeoCode <- str_replace(df.l$region, "й сельсовет", "")
# 
# library(ggmap)
#  location <- unique(as.character(df.l$regionGeoCode))
#  location <- location[!is.na(location)] # poistetaan NA sijainti
#  code <- geocode(location, output = c("latlon", "latlona", "more", "all"), messaging = FALSE,
#                  sensor = FALSE, override_limit = FALSE)
#  spat.data <- cbind(location, code)
# spat.data.nizhni <- spat.data
# save(spat.data.nizhni, file="data/spat.data.nizhni.RData")

load("data/spat.data.nizhni.RData")
df.w$oldShare <- rowSums(df.w[2:41], na.rm=TRUE)
dat <- df.w[,c("df.wide$region","oldShare")]
dat <- cbind(dat,Rsum)
names(dat) <- c("region","oldShare","pop")

region.dat <- merge(dat,spat.data.nizhni,by.x="region",
                    by.y="location")
# remove the missing locations
region.dat <- region.dat[!is.na(region.dat$lon), ]
# remove wrong locations

region.dat <- region.dat[region.dat$lon < 50, ]
region.dat <- region.dat[region.dat$lon > 40, ]
region.dat <- region.dat[region.dat$lat > 55, ]
library(stringr)
region.dat$region.short <- str_sub(region.dat$region, 1, 10)


ggplot(choro, aes(long,lat,group=group)) +
  #geom_polygon(aes(fill = value)) +
  geom_polygon(data = map.df, aes(long,lat), 
               fill=NA, 
               color = "dim grey",
               size=0.1) + # white borders
  geom_point(data = region.dat, aes(lon,lat, color=oldShare, group=region, size=pop)) + 
  geom_text(data = region.dat, aes(lon,lat, label=region.short, group=region), 
            size=2) + 
  coord_map(project="orthographic")

```



```{rcenNizPersAgeVillageMap2, fig.height=22, fig.width=22}
qmap("nizhni novgorod", zoom = 7, maptype="roadmap") +
  geom_polygon(aes(x = long, y = lat, group = group, fill = oldShare), data = choro,
               colour = "white", alpha = .6, size = .3) +
  scale_fill_gradient2(low=muted("blue"), high=muted("red"), midpoint=25) +
  labs(title="Share of people over 60 years (from census 2002)")  +
  geom_text(data=cnames, aes(long, lat, label = NAME_2.x, group=NAME_2.x), size=3, color="dim grey") +
  geom_point(data = region.dat, aes(lon,lat,group=region), colour="grey10", size=6) +
  geom_point(data = region.dat, aes(lon,lat, color=oldShare, group=region), size=5) + 
  scale_color_gradient2(low=muted("blue"), high=muted("red"), midpoint=22) +
  geom_text(data = region.dat, aes(lon,lat, label=region.short, group=region), 
            size=3.5, vjust=2) +
  geom_text(data = region.dat, aes(lon,lat, label=pop, group=region), 
            size=3.5, vjust=-1, color="red")


```

## ----------------------------------------------

## Moscow


### Age in the villages

```{rcenMosObPersAgeVillage}
raw <-  read.csv("data/DoBMoscowObVillage.csv", sep=";", skip=6)
names(raw) <- c("region","measure","year","value")

raw$test <- paste(raw$region,raw$year,sep="_")
raw$dup <- duplicated(raw$test)
df <- raw[raw$dup == FALSE,]

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
Rsum <- rowSums(mat, na.rm=TRUE, dims = 1)
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

```



```{rcenMosObPersAgeVillageMap1}
# library(stringr)
# df.l$regionGeoCode <- str_replace(df.l$region, "й сельсовет", "")
#  
#  library(ggmap)
#   location <- unique(as.character(df.l$regionGeoCode))
#   location <- location[!is.na(location)] # poistetaan NA sijainti
#   code <- geocode(location, output = c("latlon", "latlona", "more", "all"), messaging = FALSE,
#                   sensor = FALSE, override_limit = FALSE)
#   spat.data <- cbind(location, code)
#   spat.data.moscow <- spat.data
# save(spat.data.moscow, file="data/spat.data.moscow.RData")

load("data/spat.data.moscow.RData")
df.w$oldShare <- rowSums(df.w[2:41], na.rm=TRUE)
dat <- df.w[,c("df.wide$region","oldShare")]
dat <- cbind(dat,Rsum)
names(dat) <- c("region","oldShare","pop")

region.dat <- merge(dat,spat.data.moscow,by.x="region",
                    by.y="location")
# remove the missing locations
region.dat <- region.dat[!is.na(region.dat$lon), ]
# remove wrong locations

region.dat <- region.dat[region.dat$lon < 42, ]
region.dat <- region.dat[region.dat$lon > 35, ]
region.dat <- region.dat[region.dat$lat > 54, ]
region.dat <- region.dat[region.dat$lat < 57, ]
library(stringr)
region.dat$region.short <- str_sub(region.dat$region, 1, 10)


ggplot(choro, aes(long,lat,group=group)) +
  #geom_polygon(aes(fill = value)) +
  geom_polygon(data = map.df, aes(long,lat), 
               fill=NA, 
               color = "dim grey",
               size=0.1) + # white borders
  geom_point(data = region.dat, aes(lon,lat, color=oldShare, group=region, size=pop)) + 
  geom_text(data = region.dat, aes(lon,lat, label=region.short, group=region), 
            size=2) + 
  coord_map(project="orthographic")

```



```{rcenMosObPersAgeVillageMap2, fig.height=22, fig.width=22}
qmap("moscow", zoom = 7, maptype="roadmap") +
  geom_polygon(aes(x = long, y = lat, group = group, fill = oldShare), data = choro,
               colour = "white", alpha = .6, size = .3) +
  scale_fill_gradient2(low=muted("blue"), high=muted("red"), midpoint=22) +
  labs(title="Share of people over 60 years (from census 2002)")  +
  geom_text(data=cnames, aes(long, lat, label = NAME_2.x, group=NAME_2.x), size=3, color="dim grey") +
  geom_point(data = region.dat, aes(lon,lat,group=region), colour="grey10", size=6) +
  geom_point(data = region.dat, aes(lon,lat, color=oldShare, group=region), size=5) + 
  scale_color_gradient2(low=muted("blue"), high=muted("red"), midpoint=23) +
  geom_text(data = region.dat, aes(lon,lat, label=region.short, group=region), 
            size=3.5, vjust=2) +
  geom_text(data = region.dat, aes(lon,lat, label=pop, group=region), 
            size=3.5, vjust=-1, color="red")


```

