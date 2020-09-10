
# set up==========================================

pacman::p_load(raster,sp,sf,tmap,data.table,
               foreach,docstring,assertr,tinytest,
               doParallel,stringr)

registerDoParallel(4)

setwd("~/Documents/PhD/Soysambu/desnare")

## load data
load("./data/soysambu_boundaries.rda")
load("./data/soysambu_roads.rda")
load("./data/testing.rda")   # testing maxent raster ; set raster to use in new.run
load("./data/mapping.rda")   # first phase mapping snares raster
load("./data/blocks.rda")
load("../hotspots/data/allsnares.rda")

# load raster through hotspots library (Maxent)
preds=brick("/home/henk/Documents/PhD/Soysambu/hotspots/inst/extdata/spatial/predfinal.grd")
names(r)[length(names(r))]="pred"
save(r,file="./data/r.rda")

# pred_mahal=raster("../hotspots/data-raw/pred_mahal.grd")
# pred=raster::brick("../hotspots/inst/extdata/spatial/varsbrick.grd")

# utilities
colx=colorspace::sequential_hcl(10,"Blues 2",rev=T)

# initialize
snare.rast=testing

# utilities
`%nin%`=Negate(`%in%`)


# preprocessing ==================================

# 0. prepare raster

# bush=aggregate(pred$bush,fact=250/10,"mean")
# snare.rast=resample(snare.rast,bush)
# r=brick(pred_mahal,bush,snare.rast)
# names(r)[1]="pred"
#


# 1. make blocks

bounds=st_buffer(soysambu_boundaries,50)
blocks.raw=st_make_grid(bounds,cellsize=2500,square=T)
blocks=st_intersection(blocks.raw,bounds)
bl=as(blocks,"Spatial")

# 2. get values

# extract values from rasters
m=extract(r,bl,df=T,cellnumbers=T)
setDT(m)
setnames(m,"ID","block.nr")

# add coordinates
coords=xyFromCell(r,m[,cell])
m=cbind(m,coords)

# add ID
m[,idx:=.I]

# expected snare occurrence per block
m[,block.pred:={
  bl.cells=.N
  bl.preds=sum(pred)
  bl.frac=bl.preds/bl.cells
},block.nr]

m[,block.n:=sum(pred),block.nr]

# set recent to 0
m[,recent:=0]

# cleanup

# one NA for snares at pred==1
m[pred==1 & is.na(snares)]
m[is.na(snares),snares:=0]

# rm(blocks,blocks.raw,coords,testing,bush,mapping)


# snares to find ========================================

# snares available for finding
snares.raw=m[,sum(snares,na.rm=T)]

# but not all of them in pred => these were the only ones that were findable
snares.pred=m[pred==1,sum(snares,na.rm=T)]


# set up ==================================================
mx=copy(m)[pred>0 & bush>0,]


# store objects
usethis::use_data(m,overwrite=T)
usethis::use_data(r,overwrite=T)
