# plot results of simulation, simple!

library(desnare)

sims=ll

plotter=function(m,sims,runs){
  # discoverable snares
  mx=m[pred>0 & bush>0,]
  snares.pred=mx[,sum(snares)]

  # check nr of cells
  mx[,uniqueN(cell)]
  nrow(sims)/reps

  # normal snares
  found=sims[,.(found=sum(found)),.id]  # total nr of snares found in each run
  run.avg=found[,cumsum(found)/.id]/snares.pred  # cumul mov avg
  perf=sims[,.(found=sum(found)/snares.pred),.id] # perf per run
  sims[,sum(found)/reps/snares.pred] # overall avg result

  # re-snares
  resnared.found=sims[flag==T,.(found=.N),.id]  # found back
  resnared.put=sims[recent==1,.(put=.N),.id] # placed
  resnared=resnared.found[resnared.put,on=".id"] # merge

  # fraction recovered and cumulative average
  resnared[is.na(found),found:=0]
  resnared[,f:=found/put]
  run.resnared=resnared[,cumsum(f)/.id]  # perf per run
  resnared[,mean(f)] # overall perf

  # combine results snaring and resnaring
  perf[,resnared:=resnared$f]
  setnames(perf,c("run","snares.f","resnares.f"))
  perf[,`:=` (
    snares.f=round(snares.f,3),
    resnares.f=round(resnares.f,3)
  )]

  cat("performance snares:",perf[,mean(snares.f)],"\n")
  cat("performance resnares:",perf[,mean(resnares.f)],"\n\n")

  return(perf)
}


plotter.visits=function(){

}
