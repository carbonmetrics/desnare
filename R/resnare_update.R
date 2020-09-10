#' Update run with resnares
#'
#' You have run a desnaring exercise with search of adjacent cells if you found a snare (slicer with sequential.only=F).
#' Some cells were resnared (recent=1). These cells may have been visited later on, if repeats > 0.
#'
#' Here, you will filter out these cells and do a repeat desnaring.
#' (The function is not recursive: in reality, if you would find a snare, you would search the adjacent cells, which could lead
#' to finding new snares, which could lead to search of adjacent cells etc.)
#'
#' If you find a snare, update the found.rep field and the found field.
#'
#' @param x the result of running the function slicer with sequential.only=F
#' @param prob the probability of detecting a snare.
#' @return x the updated result of the slicer function output. Where resnared cells were discovered, the found.rep field was increased with the recent field.
#' @export
#' @examples
#' x=slicer(m,r,prob=0.4,n.sample=20,desnare.reps=2,sequential.only=F,cutoff=T)
#' y=resnare_update(x,prob=0.4)

resnare_update=function(x,prob){

  # identify resnared cells
  nx=x[recent>0,]

  # only cells where there were repeats are relevant here (no repeats = single pass, you would not visit this cell again, and would not see the re-snare)
  repeat.cells=nx[repeats>0,cell]

  # if there were repeat cells which were re-snared, do:
  if(length(repeat.cells)>0){

    # there was re-snaring, and you had repeats in (some of) those cells; would you find these snares?

    # throw dices equal to repeat desnaring effort and take the minimum thereof
    dices=runif(length(repeat.cells))

    # desnare; make new field for snares identified here
    nx[cell %in% repeat.cells,found.resnare:=(dices<prob)*recent]

    # update snares found and snares field
    found.resnares=nx[recent>0 & found.resnare >0,cell]
    x[cell %in% found.resnares, found:= found.seq+found.req+found.resnare]

    missed.resnares=mx[recent>0 & found.resnare==0,cell]
    x[cell %in% missed.resnares, snares:=recent]  # there was no need to update snares in the case above; they were placed and removed again.

  }else{

    # there was a re-snare, but you did not have any repeats at those cells
    x[recent>0,found.resnare:=0]
  }

  # close out - return x in both cases
  return(x)
}
