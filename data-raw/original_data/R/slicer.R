#' Slicer: sequential pass of desnaring
#'
#' Takes cell overview m, removes cells that are neither bushy nor have predicted snares (filtered data.table mx).
#' Takes slice of size n.sample and desnares. If snares are found, adjacent cells are searched
#' Optional cutoff for total visits to be equal or smaller than the filtered data.table mx you started with
#' @param m data.table with cells (prediction, snares, bushiness,xy coordinates ...)
#' @param r raster with snares (or any other raster with same resolution and extent)
#' @param prob probability of snare detection
#' @param n.sample size of slice, default = 20
#' @param desnare.reps number of visits to adjacent cells upon snare discovery in neigbouring snare cell
#' @param sequential.only whether or not you should visit adjacent cells upon finding a snare
#' @param cutoff whether or not to cutoff end result at cumulative visits <= nrows mx (= cells of mx)
#' @param contagion.cells which cells will be resnared after snares were removed from a slice; this is output from function `resnare`
#' @param hotspots whether or not hotspot desnaring (repeated desnaring of locations with known occurrences of snares) should be carried out
#' @export
#' @return df data.table with added fields snares found (sequentially and adjacent) and visits (ditto).

slicer=function(m,r,prob,n.sample=20,desnare.reps=2,sequential.only=F,cutoff=T,contagion.cells,hotspots=F){

  # helper function
  `%nin%`=Negate(`%in%`)

  # set up memory
  adj.mem<<-NULL       # adjacent cells that were visited
  snare.cells<<-NULL   # cells where snares were found
  hotspot.mem<<-NULL   # cells selected for hotspot search
  mem.length<<-10      # memory length for hotspot search
  hotspot.run<<-NULL   # for debugging
  resnares<<-data.table(NULL)    # pick up resnared cells
  eps<<-0             # epsilon -- with eps=0, you will only explore

  # only snares for which you predicted snares or cells that are not completely open area
  # mx=mz[bush>0 & pred>0,]
  mx=m[bush>0 & pred>0,]

  # recent: assign cells that are eligible for re-snaring
  mx[cell %in% contagion.cells,recent:=1]

  # sort

  # setorder(mx,-block.n,-pred,-block.pred)
  # setorder(mx,-block.pred,-pred)

  # assign slices ---------------------------------------------
  n.slices=ceiling(nrow(mx)/n.sample)
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

  # slicer core ===============================================

  l=foreach(i=1:n.slices) %do% {

    restore.point("slicer",to.global=F)

    # decide whether or not you do a hotspot search ------------
    if(hotspots){

      # condition 1: there must be snare cells
      avail.snare.cells=length(snare.cells)>0
      message("available snares: ",avail.snare.cells)

      # condition 2: dice is smaller than epsilon
      dice=runif(1)
      explore=ifelse(dice<eps,T,F)
      message("explore: ",explore)

      # if hotspot AND snare.cells AND exploit THEN hotspot slicing
      if(explore & avail.snare.cells){
        my=hotspot_slicer(mx,r,i)
        hotspot.run<<-append(hotspot.run,abs(explore))
        message("hotspot run: yes")

      }else{
        # in all other cases, do normal slicing
        my=mx[slice==i,]
        message("got my via normal or non-hotspot run")
      }
    }else{
      # if hotspots is not switched on, proceed with normal slicing
      my=mx[slice==i,]
    }

    # re-snare ----------------------------------------------

    # do I have a cell which in this slice, which is:
    # (1) eligible for re-snaring (recent=1); (2) in snare memory (snare was found)
    my[recent==1 & cell %in% snare.cells,flag:=TRUE]   # seems to be a very rare or non-existent case?
    resnared=my[flag==T,.(cell,snares,recent,x,y)]
    resnares<<-rbind(resnares,resnared)

    # desnare and admin
    dice=runif(n=nrow(my))
    if(length(dice) != nrow(my)){
      stop("length dices must be equal to my object")
    }
    my[,found:=(dice<prob)*snares]
    my[found>0,snares:=snares-found]
    my[,counter:=counter+1]

    # store snare cells (also updated via the adjacer function route)
    mysnares=as.vector(my[found>0,cell])
    snare.cells<<-append(snare.cells,mysnares)


    # update epsilon as function of nr of snares
    length.snares=ifelse(length(snare.cells)>=4,4,length(snare.cells)) # max length
    eps.table=data.table(    # ref table
      snares.found=0:4,
      eps=rev(seq(0,0.8,0.2))
    )
    eps<<-eps.table[snares.found==length.snares,eps]  # choose from ref table


    # desnaring of adjacent cells
    if(sequential.only==F){
      # message("running adjacer...")
      my=adjacer(my,mx,r,prob=prob,desnare.reps=desnare.reps)
    }

    # assign new slice number
    my[,slice.new:=i]

    # catch empty my
    # stopifnot(nrow(my)>0)

  }  # end of foreach .......................................


  restore.point("l.object",to.global=F)

  # collect results ------------------------------------------
  df=rbindlist(l,idcol=T)
  setorder(df,slice.new,idn)

  # deal with duplicated cells -------------------------------
  # if you are not carrying out hotspot search:
  # each duplicated cell occurs twice: once for normal desnaring and once for desnaring of
  # cells that were adjacent to a snare cell.
  # the de-duplication involves adding the sequential and repeat visits;
  # the sum of maxima of found rep and found max; (can therefore never be larger than 1)
  # snares must be minimum of snares for both cases, since replacement of snares occurs via the `recent` field.
  dups=df[duplicated(cell),cell]

  # roll up
  df[cell %in% dups,`:=` (counter=sum(counter),
                          repeats=sum(repeats),
                          found=max(found),
                          found.rep=max(found.rep),
                          recent=max(recent),
                          snares=min(snares)
  ), by=cell]

  df=unique(df,by="cell")

  # after dedup, the number of rows equals rows in mx if there is no hotspot desnaring
  if(hotspots==F){
    if(nrow(df)!=nrow(mx)){
      print(df)
      print(mx)
      stop("rows df and mx not the same after de-duplication, problem found at slice ",i)
    }
  }
  # to do: else condition -- how many rows do you expect in all cases?

  # clean up ----------------------------------------
  df[,.id:=NULL]
  setnames(df,"found","found.seq")
  df[,found:=found.seq+found.rep]
  df[,visits:=counter+repeats]
  df[,cumul.visits:=cumsum(visits)]
  df[snares==1 & recent==1, recent:=0]  # you can't replace a snare that was never found

  # cutoff? ----------------------------------------
  if(cutoff==T){
    tr=nrow(mx)
    df=df[cumul.visits<=tr]
  }

  restore.point("slicer_main",to.global=F)


  # close out --------------------------------------
  return(df)
}

