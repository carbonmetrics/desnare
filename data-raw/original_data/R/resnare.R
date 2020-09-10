#' resnare
#'
#' Takes a simulation run of desnaring and re-snares.
#' Take cells with snares in them and surrounding cells that have some bush in them.
#' Find clumps and select some of them through taking a sample.
#' Within the selected clumps, re-snare a fraction of the cells.
#' Both clump and cell selection have seeds set, so the selected cells are always the same in view of reproducibility.
#' The default is to return only cells, since this is much less computationally intensive.
#'
#' @param sim data.table of desnaring simulation of a single run
#' @param m overview of raster cells with snares
#' @param r raster overview of snares (or any raster with same extent and resolution)
#' @param n.clumps number of snare clumps to resnare
#' @param resnare.frac fraction of snare cells that will be replaced (if they were removed during desnaring)
#' @param cells.only whether to return contagion cells (default=TRUE) or whether the whole sim object is returned.
#' @return simulation object with snares replaced for selected cells. `recent` field will be set to 1
#' @export
#' @examples resnare(sim,m,r,n.clumps=3,resnare.frac=1)

resnare=function(sim,m,r,n.clumps,resnare.frac,cells.only=T){

  # snare cells and cells adjacent to them
  scells=m[snares>0 & pred>0 & bush>0,cell]                   # discoverable snare cells
  acells.raw=adjacent(r$snares,scells,directions=8,pairs=F)   # all adjacent cells
  acells=m[cell %in% acells.raw & bush>0.1,cell]              # filter out bush==0
  ccells=c(scells,acells)                                     # concatenate snare cells and adjacent cells

  # get a raster, add ccells
  dummy=copy(r$snares)
  dummy[dummy>0]=0
  dummy[ccells]=1

  # find clumps
  cl=clump(dummy,directions=8)
  cl.vals=extract(cl,ccells)
  cdf=data.table(
    cell=ccells,
    clump=cl.vals
  )
  cdf[,n:=.N,clump]

  # activate clump
  # set.seed(1964)
  cl.sample=sample(cdf$clump,size=n.clumps)
  cdx=cdf[clump %in% cl.sample,]

  # activate some cells within active clumps
  # set.seed(1964)
  cl.dice=runif(nrow(cdx))
  cont.cells=(cl.dice<resnare.frac)*cdx$cell
  cont.cells=cont.cells[cont.cells>0]

  # report number of cells to resnare
  # message("snares replaced by poachers: ", nrow(sim[found>0 & cell %in% cont.cells,]))

  # resnare - but in the recent field, do not touch the snare field
  if(cells.only){
    return(cont.cells)
  }else{
    sim[found>0 & cell %in% cont.cells,recent:=1]
    return(sim)
  }
}

