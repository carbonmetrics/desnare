# test multiple runs ==================
# with cutoff, repeats, replicated over n runs

library(desnare)
setwd("~/Documents/PhD/Soysambu/desnare")

rm(list=ls())
overwrite=F
reps=1000
registerDoParallel(4)

contagion.cells=resnare(m,m,r,n.clumps=4,resnare.frac=1,cells.only=T)

l=foreach(i=1:reps) %dopar% {

  # make reproducible in parallel runs............
  set.seed(i)

  # show progress (only non-parallel).............
  message("\n run ",i," =================")

  # run core function ............................
  # x=slicer(m,r,prob=0.2,n.sample=20,desnare.reps=2,sequential.only=T,cutoff=T,contagion.cells=contagion.cells,hotspots=F)   # sequential
  # x=slicer(m,r,prob=0.2,n.sample=20,desnare.reps=2,sequential.only=F,cutoff=T,contagion.cells=contagion.cells,hotspots=F) # adjacent
  # x=slicer(m,r,prob=0.2,n.sample=20,desnare.reps=2,sequential.only=F,cutoff=T,contagion.cells=contagion.cells,hotspots=T) # hotspot
  # x=slicer(m,r,prob=0.4,n.sample=20,desnare.reps=2,sequential.only=T,cutoff=T,contagion.cells=contagion.cells,hotspots=F)
  # x=slicer(m,r,prob=0.4,n.sample=20,desnare.reps=2,sequential.only=F,cutoff=T,contagion.cells=contagion.cells,hotspots=F)
  x=slicer(m,r,prob=0.4,n.sample=20,desnare.reps=2,sequential.only=F,cutoff=T,contagion.cells=contagion.cells,hotspots=T)

  # set flag for identified resnares .............
  if(nrow(resnares)>0){
    x[cell %in% resnares[,cell],flag:=T]
  }

  # catch empty runs ............................
  stopifnot(nrow(x)>0)  # entire run empty!

  # close out ...................................
  x
}
0
ll=rbindlist(l,idcol=T)
snares.pred=m[pred>0 & bush>0,sum(snares)]
pr=plot_results(m,prob=0.4,sims=ll,r=r,perc=T,onepage=T,reps=reps,printpdf=T)

if(overwrite){
  fwrite(ll,file="~/Documents/PhD/Soysambu/desnare/data-raw/hot40.txt",sep="\t")
}else{
  message("change name and save if required")
}

# # get results
# df.seq=fread("./data-raw/sequential.txt")
# df.adj=fread("./data-raw/adjacent.txt")
# df.hot=fread("./data-raw/hotspots.txt")
#
# # consolidate
# t1=plot_results(m,prob=0.4,sims=df.seq,r=r,perc=T,onepage=T,reps=reps)$tab
# t2=plot_results(m,prob=0.4,sims=df.adj,r=r,perc=T,onepage=T,reps=reps)$tab
# t3=plot_results(m,prob=0.4,sims=df.hot,r=r,perc=T,onepage=T,reps=reps)$tab
# df=rbind(t1,t2,t3)
# df[,label:=c("sequential","adjacent","hotspots")]
# mycols=c("perc.found","perc.missed","cells.visited","fraction.cells.visited","f.resnared.recovered")
# df[,(mycols):=lapply(.SD,function(x){round(x,3)}),.SDcols=mycols]
# setnames(df,"fraction.cells.visited","f.cells.visited")
# save(df,file="./data-raw/df.rda")
