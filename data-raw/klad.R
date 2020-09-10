resnares=data.table(NULL)
resnared=mx[1:4,.(cell,x,y,recent)]
resnares=rbind(resnares,resnared)

resnared=mx[6:10,.(cell,x,y,recent)]
resnares=rbind(resnares,resnared)




visits.cell=sims[,.(avg.visits=mean(visits)),.(x,y)]

# help function
rescale=function(x){
  mins=min(x,na.rm=T)
  maxs=max(x,na.rm=T)
  resc=(x-mins)/(maxs-mins)
  return(resc)
}

# rescale visits in order to allow use of cex in plot function
visits.cell[,avg.visits.scaled:=rescale(avg.visits)]

# coordinates only
xy=visits.cell[,.(x,y)]

# get cell coordinates
visits.cell[,cell:=cellFromXY(r$snares,xy)]



if(!is.null(hotspot.mem)){

  if(length(snare.cells[snare.cell %nin% hotspot.mem])>0){
    e=snare.cells[snare.cells %nin% hotspot.mem]
  }else{
    hotspot.mem=tail(hotspot.mem,mem.length)

  }

  # fails when all snare cells are in hotspot.mem

}else{
  e=snare.cells
}

# dups=df[duplicated(cell),cell] after this line in slicer.R:

if(length(dups)>0){
  # test duplication handling
  dfx=df[cell %in% dups,]
  # the sum of each counter per cell is 1
  t1=all(dfx[,sum(counter),cell][,V1]==1)
  # when the counter is zero, then all repeats are the same as the desnare.reps setting
  t2=all(dfx[counter==0,repeats]==desnare.reps)
  # adj.mem worked
  t3=!is.null(adj.mem)
  if(!all(t1,t2,t3)){
    stop("problem with de-duplication")
  }else{
    rm(t1,t2,t3,dfx)
  }
}

# roll up
# df[cell %in% dups,`:=` (counter=max(counter),
#                         found=max(found),
#                         found.rep=max(found.rep),
#                         recent=max(recent),
#                         repeats=max(repeats),
#                         snares=min(snares)
#                         ), by=cell]



# set up ==========================

library(desnare)

snares.pred=m[bush>0 & pred>0,sum(snares)]
prob=0.4
desnare.reps=2

# desnaring, single ===============
mx=sequential_desnare(m)                           # sequential: 1 visit per cell
l=desnare_adjacent(mx,desnare.reps=desnare.reps)   # inspect adjacent cells twice
ll=merge_sequential_adjacent(mx,l)                 # merge adjacent and sequential
df=sort_cutoff(ll)                                 # make the final result comparable to a single pass desnaring


# simulate ========================
reps=100
registerDoParallel(4)

# sequential ---------------------

sim=foreach(i=1:reps) %do% {
  mx=sequential_desnare(m)
}

sims=rbindlist(sim,idcol=T)
h=plot_results(snares.pred,0.4,sims,perc=F)

# adjacent ----------------------
sim=foreach(i=1:reps) %do% {
  message("run ",i," ---------------")
  mx=sequential_desnare(m)               # sequential: 1 visit per cell
  l=desnare_adjacent(mx)                 # inspect adjacent cells twice
  ll=merge_sequential_adjacent(mx,l)     # merge adjacent and sequential
  df=sort_cutoff(ll)                     # make the final result comparable to a single pass desnaring
}

sims=rbindlist(sim,idcol=T)
h=plot_results(snares.pred,0.4,sims,perc=F)


# resnare ------------------------
simx=foreach(i=1:max(sims$.id),.combine=rbind) %dopar% {
  l=sims[.id==i,]
  l=resnare(l,m,r,resnare.frac=0.5)
}

# how many snares would be picked up during normal desnaring?

# for sequential: 0 - you never revisit a cell, all counters are 1.

# for adjacent: can happen if the following conditions are met:
# 1. the counter for the cell is larger than 1, and
# 2.
# 3. the cell was revisited as a result of an adjacent search from a "from cell"
# that has a larger sequence number than the snare cell where the snare was replaced.

simx[,idn:=1:.N,.id]
simx[,cell.larger:=from.cell>cell & recent>0]
h=simx[.id==1,.(.id,idn,cell,counter,found,recent,cell.larger,from.cell)]


# start of batcher ----------------------------------------------------


library(desnare)
rm(list=ls())
reps=10
registerDoParallel(4)
snares.pred=m[pred>0 & bush>0,sum(snares)]
found.rep=NULL
found.seq=NULL
snare.cells=NULL

l=foreach(i=1:reps) %do% {
  set.seed(i)
  message("\nrun ",i,"-----")
  df=slicer(m,r,prob=0.4,desnare.reps=2,n.sample=20,sequential.only=F,cutoff=F)
}

df=rbindlist(l,idcol=T)
plot_results(snares.pred,prob=0.4,df,perc=F)

#
# sequential, 100 runs, 10.2
# with adjacent: 10.09

h=df[,.(.id,cell,snares,found.seq,found.rep,counter,repeats,found,visits,cumul.visits)]
mean(h[,sum(found.seq),.id][,V1])
mean(h[,sum(found.rep),.id][,V1])
