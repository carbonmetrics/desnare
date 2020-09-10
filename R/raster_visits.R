library(desnare)
setwd("~/Documents/PhD/Soysambu/desnare/data-raw/original_data")

adjacent=fread("adjacent.txt")
hotspot=fread("hotspots.txt")


# plot visits ================================================

coly=colorspace::sequential_hcl(5,"Grays",rev=T,alpha=1)

# visits per raster cell ..................

visits.plot=function(sims,r,mypdf){

  # calculate avg visits per cell (calculated via xy coordinates of the cell)
  visits.cell=sims[,.(avg.visits=mean(visits)),cell]

  # add values to raster
  visit.raster=raster(r)

  # visit.raster=copy(dummy)
  visit.raster[visits.cell[,cell]]=visits.cell[,avg.visits]

  # plot
  a=getwd()
  myfilename=paste0(mypdf,".pdf")

  setwd("~/Documents/PhD/Dissertation/figures")
  pdf(file=myfilename)
  plot(visit.raster,col=coly,bty="n",box=F,legend=F)
  legend(x="topleft",legend=c(1:5),fill=coly,bty="n",title="Visit count",cex=1.2)
  plot(st_geometry(soysambu_boundaries),add=T)
  dev.off()
  setwd(a)
}

# plot(visit.raster, col=coly, legend=FALSE, axes=FALSE)
#
# r.range <- c(minValue(visit.raster), maxValue(visit.raster))
#
# plot(visit.raster, legend.only=TRUE, coly,
#      legend.width=1, legend.shrink=0.75,
#      axis.args=list(at=seq(r.range[1], r.range[2], 25),
#                     labels=seq(r.range[1], r.range[2], 25),
#                     cex.axis=0.6),
#      legend.args=list(text='Elevation (m)', side=4, font=2, line=2.5, cex=0.8))


visits.plot(adjacent,r,"adjacent_visits")
visits.plot(hotspot,r,"hotspot_visits")

a=getwd()
setwd("~/Documents/PhD/Dissertation/figures")
system("pdfcrop adjacent_visits.pdf adjacent_visits.pdf")
system("pdfcrop hotspot_visits.pdf hotspot_visits.pdf")
