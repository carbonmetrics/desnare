#' setup
#'
#' sets up parameters for debugging purposes; internal function for debugging purposes
#' @export

setup=function(){
 prob<<-0.4
 desnare.reps<<-2
 snare.cells<<-NULL
 n.sample<<-20
 sequential.only<<-FALSE
 i<<-2
 contagion.cells<<-resnare(m,m,r,n.clumps=4,resnare.frac=1,cells.only=T)
 snares.pred<<-26
 eps<<-0.2

 # set up memory
 adj.mem<<-NULL       # adjacent cells that were visited
 snare.cells<<-NULL   # cells where snares were found
 hotspot.mem<<-NULL   # cells selected for hotspot search
 mem.length<<-3       # memory length for hotspot search
 hotspot.run<<-NULL

 # only snares for which you predicted snares or cells that are not completely open area
 mx=m[bush>0 & pred>0,]

 # recent: assign cells that are eligible for re-snaring
 mx[cell %in% contagion.cells,recent:=1]

 # sort
 setorder(mx,-block.n,-pred,-block.pred)

 # assign slices ---------------------------------------------
 n.slices<<-ceiling(nrow(mx)/n.sample)
 mx[,slice:=rep(1:n.slices,each=n.sample,length.out=nrow(mx))]
 mx[,`:=`(
         found=0,
         found.rep=0,
         counter=0,
         repeats=0,
         from.cell=NA,
         idn=1:.N,
         flag=FALSE,
         label="no value"
 )]

 mx<<-mx

 # rm(.Random.seed, envir=globalenv())
}
