#' hotspot search
#'
#' takes a sample of adjacent cells
#' @param r raster with snares or predictions
#' @param mx cell overview with bush>0 and predicted snares prob > 0 (= mx object)
#' @param i index number of the pass in the slicer function, of which this function is part
#' @return my slice with label 'hotspot', based on sample of snare memory and adjacent cells
#' @export

hotspot_slicer=function(mx,r,i){

  restore.point("hotspot_slicer",to.global=F)

  # helper function
  `%nin%`=Negate(`%in%`)

  # sample - needs if condition due to unexpected behaviour of `sample` when x=1
  if(length(snare.cells)==1){
    s=snare.cells
  }else{
    s=sample(snare.cells,1)
  }

  # identify adjacent cells in raster where snares are predicted and where there's bush (= mx object)
  r1=adjacent(r$pred,s,directions=4,pairs=F)
  r2=adjacent(r$pred,r1,directions=4,pairs=F)
  r3=adjacent(r$pred,r2,directions=4,pairs=F)
  hcells=unique(c(s,r1,r2,r3))

  # get my object
  my=mx[cell %in% hcells,]
  my[,slice:=i]
  my[,label:="hotspot slice"]

  restore.point("hotspot_slice",to.global=F)

  # admin
  hotspot.mem<<-append(hotspot.mem,s)

  # close out
  return(my)
}
