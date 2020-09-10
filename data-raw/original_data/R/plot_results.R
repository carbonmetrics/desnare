#' plot results
#'
#' Plot the results of a simulation.
#'
#' @param m cell overview
#' @param sims data.table with simulation result
#' @param perc Whether you plot raw snare counts or percentage of discoverable snares
#' @param prob Probability of detection a snare
#' @param r raster with snares, or any other raster that has same extent and resolution
#' @param onepage whether or not to plot summary plots on one page
#' @param printpdf whether or not to print a pdf of the performance plot
#' @param magn the amount of magnification (cex) for plot text, symbols and axes
#' @param reps the number of repetitions in the simulation
#' @return plot of stabilization of results, plot of cumulative result over number of cells visited,
#' discoverable, found and non-discoverable snares, average visits per cell
#' @examples
#' sim=foreach(i=1:reps) %dopar% {
#' sequential_desnare(m)
#' }
#' sims=rbindlist(sim,idcol=T)
#' plot_results(snares.pred,0.4,sims,perc=F)
#' @export


plot_results=function(m,prob,sims,r,perc=T,onepage=T,printpdf=F,reps,magn=2){

  # set up ---------------------------------------------
  # plot layout
  if(onepage){
    par(mfrow=c(3,2))
  }

  # helper function
  `%nin%`=Negate(`%in%`)

  # discoverable snares
  mx=m[pred>0 & bush>0,]
  snares.pred=mx[,sum(snares)]

  # test whether you have the right object
  if(!all(c(".id","found") %in% names(sims))){
    stop("I need an object with the names .id and found in it.")
  }

  # stabilization of results ------------------------------
  # calculate the average number of snares found per simulation run
  found=sims[,.(found=sum(found)),.id]
  expected.runs=data.table(.id=1:reps)

  # some runs may have been skipped: a hotspot run was required
  if(nrow(found) != nrow(expected.runs)){
    warning("some runs were skipped, filling...")
    foundx=found[expected.runs,on=".id"]
    foundx[,found:=nafill(found,type="locf")]  # fills with previous value
  }else{
    foundx=found
  }

  run.avg=foundx[,cumsum(found)/.id]

  # plot
  if(perc){
    plot(run.avg/snares.pred,
         xlim=c(0,length(run.avg)),
         xlab="simulation run index",
         ylab="fraction of snares found",
         type="b",bty="n")
  }else{
    plot(run.avg,
         xlim=c(0,length(run.avg)),
         xlab="simulation run index",
         ylab="number of snares found",
         type="b",bty="n")
  }

  # performance
  message("snares found: ",round(tail(run.avg,1),3))
  message("snare percentage found: ",round(tail(run.avg,1)/snares.pred,3))

  # efficiency of finding snares -----------------------------

  # how fast do you get to the end result (total number of snares found)?
  # cumulative sum of snares found per simulation run
  res=sims[,.(cumul=cumsum(found)),.id]

  # the length of each run can be different with cutoff (more visits per cell)
  # therefore calculate quantiles of cumsum per 0.1 interval
  a=foreach(i=1:reps) %do% {
    res[.id==i,.(qtl=quantile(cumul,seq(0,1,0.1)))]
  }
  b=rbindlist(a,idcol=T)
  b[,idx:=1:.N,.id]

  # check: the maximum idx per sim run equals length of number of quantiles
  stopifnot(all(b[,max(idx),.id][,V1]==length(seq(0,1,0.1))))

  # average over quantile steps
  d=b[,.(avg.found=mean(qtl)),idx]
  d[,qtl:=seq(0,1,0.1)]

  message("Gini coefficient: ", round(ineq::ineq(d[,avg.found],"Gini"),3))

  # plot quantile charts
  if(perc){
    with(d,plot(avg.found/snares.pred~qtl,
                type="b",bty="n",
                ylim=c(0,0.5),
                xlab="fraction of simulation runs",ylab="average fraction of snares found"))
    # abline
    xs=c(0,max(d$qtl))
    ys=c(0,prob)
    abline(lm(ys~xs),lty="dotted")
  }else{
    with(d,plot(avg.found~qtl,
                type="b",bty="n",
                ylim=c(0,signif(snares.pred*prob,1)+5),
                xlab="fraction of simulation runs",ylab="average number of snares found"))
    # abline
    xs=c(0,max(d$qtl))
    ys=c(0,snares.pred*prob)
    abline(lm(ys~xs),lty="dotted")
  }

  # make pdf plot
  if(perc & printpdf){
    myfile=paste0("performance_",as.integer(Sys.time()),".pdf")
    pdf(file=myfile,paper="a4r")
    with(d,plot(avg.found/snares.pred~qtl,
                type="b",bty="n",
                ylim=c(0,0.5),
                xlab="fraction of simulation runs",
                ylab="average fraction of snares found",
                cex=magn, cex.axis=magn, cex.lab=magn
                ))
    # abline
    xs=c(0,max(d$qtl))
    ys=c(0,prob)
    abline(lm(ys~xs),lty="dotted")
    dev.off()
  }

  # raster plots -------------------------------

  # prepare .................................
  cols=colorspace::sequential_hcl(20,"Light Grays",rev=T,alpha=1)

  # visits per raster cell ..................
  # calculate avg visits per cell (calculated via xy coordinates of the cell)
  visits.cell=sims[,.(avg.visits=mean(visits)),cell]

  # make blanco raster
  # rr=copy(r$pred)
  # rr[rr>=0]=NA
  # dummy=copy(rr)

  # add values to raster
  visit.raster=raster(r)
  # visit.raster=copy(dummy)
  visit.raster[visits.cell[,cell]]=visits.cell[,avg.visits]

  # plot
  coly=colorspace::sequential_hcl(5,"Grays",rev=T,alpha=0.5)
  plot(visit.raster,col=coly,bty="n",box=F,legend=T)
  plot(st_geometry(soysambu_boundaries),add=T)


  # discoverable or not discoverable? ...............
  # adjust prediction raster: remove bush=0
  predr=Which(r$pred>0,cells=T)
  bushr=Which(r$bush>0,cells=T)
  allr=intersect(predr,bushr)

  # make raster
  # discoverable=copy(dummy)
  discoverable=raster(r)
  discoverable[allr]=1

  # not discoverable: there are snares, but they are not covered in mx
  not.disco=m[cell %nin% mx[snares>0,cell] & snares>0,]

  # found - how likely is it that a snare is found? .......
  pfound=sims[,.(p.found=mean(found>0)),.(x,y,cell)][p.found>0,]

  # (always) missed, but findable
  missed=mx[cell %nin% pfound[,cell],][snares>0,.(x,y)]

  # plot found, discoverable and non-discoverable snares
  # to do - size cex must depend on likelihood of discovery, not whether it was ever discovered in any of the runs
  colx=colorspace::sequential_hcl(5,"Light Grays",rev=T,alpha=0.2)
  plot(discoverable,col=colx,legend=F,bty="n",box=F)
  plot(st_geometry(soysambu_boundaries),add=T)
  # points(mx[snares>0,.(x,y)],pch=21,cex=0.5,bg="lightgrey")       # discoverable
  points(not.disco[,.(x,y)],pch=2,cex=0.5)  # not discoverable
  points(pfound[,.(x,y)],cex=pfound[,p.found]*1.5,pch=1)
  points(missed[,.(x,y)],pch=3,cex=0.5)
  legend(x="bottomright",legend=c("not discoverable","found","missed"),pch=c(2,1,3),bty="n",cex=0.8)

  # how many re-snared cells were picked up? ..............

  # found back
  resnared.found=sims[flag==T,.(found=.N),.id]

  # placed
  resnared.put=sims[recent==1,.(put=.N),.id]

  # merged
  resnared=resnared.found[resnared.put,on=".id"]

  # fraction recovered and cumulative average
  resnared[is.na(found),found:=0]
  resnared[,f:=found/put]
  run.resnared=resnared[,cumsum(f)/.id]

  # plot
  plot(run.resnared,bty="n",type="b",
       xlim=c(0,resnared[,max(.id)]),
       xlab="simulation run index",
       ylab="fraction of recovered resnares"
       )

  # summary table -----------------------------

  # cells visited .............................
  n.visited=sims[,.N,.id][,mean(N)]
  f.visited=n.visited/nrow(mx)


  # summary table .............................
  tab=data.table(
    snares.found=tail(run.avg,1),
    snares.missed=nrow(missed),
    perc.found=tail(run.avg,1)/snares.pred,
    perc.missed=nrow(missed)/snares.pred,
    Gini=round(ineq::ineq(d[,avg.found],"Gini"),3),
    cells.visited=n.visited,
    fraction.cells.visited=f.visited,
    n.resnared.recovered=resnared[,mean(found)],
    f.resnared.recovered=resnared[,mean(f)]
  )


  # close out ---------------------------------
  par(mfrow=c(1,1))

  return(list(
    tab=tab,
    perf=d,
    visit.raster=visit.raster
  ))
}
