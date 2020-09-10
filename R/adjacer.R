#' Adjacer
#'
#' Takes a slice of cells and identifies adjacent cells.
#' @param my slice
#' @param mx all raster cells (with predicted snare occurrence and not entirely open areas)
#' @param r raster from which mx object was obtained
#' @param prob probability of detecting a snare
#' @param desnare.reps the number of repeats in surrounding snare cells
#' @return slice with adjacent cells added, including whether snares were found (snares.repeat) and updated counter (count.repeat)

adjacer=function(my,mx,r,prob,desnare.reps){

  # if you find a snare:
  if(nrow(my[found>0,])){
    # message("found one or more snares")

    # find adjacent cells -------------------------------------------
    a=adjacent(r$pred,my[found>0,cell],pairs=F,directions=8)

    # must occur in mx (pred>0 and bush>0)
    a1=mx[cell %in% a,cell]

    # remove cells that were already repeated
    a2=a1[a1 %nin% adj.mem]
    a2=a2[a2 %nin% snare.cells]

    # ensure no NA
    a3=a2[!is.na(a2)]
    # message("found adjacent cells ",paste(a3,collapse=", "))

    # if they do occur, do a repeat search -------------------------
    if(length(a3)>0){

      # message("testing adjacent cells ...")

      # update memory
      adj.mem<<-append(adj.mem,a3)
      # message("current adj.mem ",paste(adj.mem,collapse=", "))

      # throw dices equal to repeat desnaring effort and take the minimum thereof
      dices=replicate(desnare.reps,runif(length(a3)))
      if(is.matrix(dices)){
        dices=apply(dices,1,min)
      }else{
        dices=min(dices)
      }

      # check the cells against dices
      if(length(dices) != length(a3)){
        stop("length dices must be equal to mx filtered for selected adjacent cells.")
      }

      # desnare and update counter
      mx[cell %in% a3, `:=` (
        found.rep=found.rep+(dices<prob)*snares,
        repeats=desnare.reps
      )]

      # update snare memory
      adj.snares=as.vector(mx[found.rep>0,cell])
      snare.cells<<-append(snare.cells,adj.snares)

      # update snares
      mx[cell %in% a3 & found.rep>0,snares:=max(0,snares-found.rep)]

      mx[cell %in% a3,]

      # add the cells to the my object
      myz=mx[cell %in% a3,]
      my=rbind(myz,my)
      my[,label:="adjacent"]

      # message("new slice ready.")
    }
  }

  # else -----------------------------------------------------
  if(nrow(my[found>0])==0){
    # message("no snares found in repeat.")
  }

  # close out ------------------------------------------------
  return(my)
}
