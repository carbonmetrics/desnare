# set up
library(desnare)

rm(list=ls())
prob=0.4
n.sample=20
desnare.reps=1
sequential.only=F
snare.cells=NULL
adj.mem=NULL
found.seq=NULL
found.rep=NULL
i=2
testmy=fread("/home/henk/Documents/PhD/Soysambu/desnare/data-raw/testmy")
a3=c(5934,5935,6022,6110,6111,6198,6199,6285,6287,5936,6024,6112,6200,6288)

# helper function
`%nin%`=Negate(`%in%`)

# set up memory
adj.mem=NULL

# only snares for which you predicted snares or cells that are not completely open area
mx=m[bush>0 & pred>0,]

# sort
setorder(mx,-block.n,-pred,-block.pred)

# assign slices
n.slices=ceiling(nrow(mx)/n.sample)
mx[,slice:=rep(1:n.slices,each=n.sample,length.out=nrow(mx))]
mx[,`:=`(
  found=0,
  found.rep=0,
  counter=0,
  repeats=0,
  from.cell=NA,
  idn=1:.N
)]

message("set up complete.")
