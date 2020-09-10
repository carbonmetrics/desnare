# find clusters

# setup
pacman::p_load(data.table,basf,raster,maditr,spatstat,
               maptools,factoextra,cluster,datasets)

load("/home/henk/Documents/PhD/Soysambu/Soysambu/data/snares.rda")
pred=raster::brick("/home/henk/Documents/PhD/Soysambu/hotspots/inst/extdata/spatial/varsbrick.grd")
load("/home/henk/Documents/PhD/Soysambu/hotspots/data/roads.rda")
load("/home/henk/Documents/PhD/Soysambu/hotspots/data/soysambu_boundaries.rda")

# add park infrastructure
load("/home/henk/Documents/PhD/Soysambu/Soysambu/data/poi.rda")  # load directly instead of via Soysambu library
infra=subset(poi,type %in% c("gate","lodge","staff_settlement"))
dist.i=distanceFromPoints(pred$dem,infra) %>%
  mask(soysambu_boundaries) %>%
  resample(.,pred)
pred=addLayer(pred,dist.i)
names(pred)[length(names(pred))]="dist.i"

# data
ns=snares %>% as.data.table %>% subset(cat=="NECK SNARE")
xy=ns[,.(x,y)]
df=as.data.table(extract(pred,xy))[,.(dem,interface,dist.roads,dist.i)]

# clustering
# prepare
nx=df %>% na.omit %>% scale

# n clusters
fviz_nbclust(nx,kmeans,method="wss")
fviz_nbclust(nx,kmeans,method="silhouette")
fviz_nbclust(nx,kmeans,method="gap_stat")

# clustering
km=kmeans(nx,centers=5,nstart=25)

fviz_cluster(km,data=nx)

df[,cl:=km$cluster]
res=df[,lapply(.SD,mean),.SDcols=c("dem","interface","dist.roads","dist.i"),keyby=cl]
res[,n:=km$size]

# plot on map
xy[,cl:=km$cluster]

plot(soysambu_boundaries)
points(xy[cl==1,.(x,y)],pch=0)
points(xy[cl==2,.(x,y)],pch=1)
points(xy[cl==3,.(x,y)],pch=2)
points(xy[cl==4,.(x,y)],pch=3)
points(xy[cl==5,.(x,y)],pch=4)
plot(roads,add=T)
