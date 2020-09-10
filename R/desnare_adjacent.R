#' desnare cells adjacent to snare cells
#'
#' using the output of function `sequential`: identify snare cells (where you found cells during the sequential desnaring run);
#' for each of these cells, find the adjacent cells (Moore neighbourhood).
#' use only cells that you did not previously desnare, where there is bush and where there was predicted occurrence of snares.
#' repeat desnaring n=desnare.reps times, and update counts, snares found.
#' @param mx output of function `mx`
#' @param bush whether to include bush into the adjacent cells
#' @param desnare.reps how many times desnaring must be repeated for adjacent cells (a repeat is a proxy for spending more time in the cell for desnaring).
#' @param prob probability of finding a snare
#' @examples mx=sequential(m); l=desnare_adjacent(mx)
#' @export

desnare_adjacent=function(mx,include.bush=F,desnare.reps=2,prob=0.4){

  # reset memory adjacent memory cells
  adj.mem=NULL

  # repeat desnaring once you find a snare ----------
  # find cells surrounding snare cells
  snare.cells=mx[found>0,cell]

  l=foreach(i=1:length(snare.cells)) %do% {

    message("repeat run ",i, " .........................")

    # do for this snare cell:
    snare.cell=snare.cells[i]

    # find adjacent cells
    a=adjacent(r$pred,snare.cell,pairs=F,directions=8)

    # find predicted values for adjacent cells
    e=extract(r$pred,a)

    # remove NA values and zeros
    if(include.bush==F){
      ax=a*e
      ax=ax[ax>0 & !is.na(ax)]
    }else{
      ax=!is.na(ax)
    }

    # remove cells that were already repeated
    ax=ax[ax %nin% adj.mem]

    # remove cells that are not in the cell overview
    ax=mx[cell %in% ax,cell]

    # update adjacent cell memory
    adj.mem=append(adj.mem,ax)

    # repeat desnaring probs
    if(length(ax)>0){
      dices.throw=replicate(desnare.reps,runif(length(ax))) # matrix with 2 throws of a dice for each cell
      dices.mat=matrix(dices.throw,ncol=desnare.reps)       # ensure this is a matrix with desnare.reps columns
      dices=apply(dices.mat,1,min)

      # desnaring for the adjacent cells:
      my=mx[cell %in% ax,]
      my[,found.repeat:=(dices<prob)*snares]

      # admin
      my=my[found.repeat>0,`:=` (
        snares.repeat=snares-found.repeat,
        recent.repeat=ifelse(recent>0,recent-found.repeat,recent)
      )]

      my[,count.repeat:=2]

      # reduce data.table for later merge
      mz=my[,.(idx,cell,recent.repeat,found.repeat,count.repeat,snares.repeat)]

      # add snare cell nr
      mz[,from.cell:=snare.cell]
    }
  }  # end of foreach loop

  return(l)
}


