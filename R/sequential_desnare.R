#' Perform a one-pass sequential desnaring operation over a data.table
#'
#' A raster with snares found during testing a snare occurrence model was made and processed, and available as object m.
#' Here, we remove raster cells for which there is no prediction of snares, and also no bush (nothing to attach the snares to).
#' A dice is thrown for each row (cell) of the object; if it is smaller than the probability of finding the snare `prob`, then the snare is found.
#' In that case, the `snare` field is reset to zero, and `found` to 1.
#'
#' @param m data.table with raster cells, coordinates and snares (data(m))
#' @param prob probability of detecting snares, default=0.4
#' @return mx reduced object m (no bush, no cells for which no snare occurrence was predicted). Fields added: counter, found.
#' @examples sequential(m,prob=0.2)
#' @export

sequential_desnare=function(m,prob=0.4){

  # provide fresh copy
  mx=copy(m)[pred>0 & bush>0,]
  setorder(mx,-block.n,-pred,-block.pred)

  # throw dice: if
  dice=runif(nrow(mx))

  # in the majority of cases you will miss, but in 40% of the dice throws you will find the snare (for P=0.4)
  mx[,found:=(dice<prob)*snares]

  # admin
  mx[,counter:=1]
  mx[found>0,`:=` (
    snares=snares-found,
    recent=ifelse(recent>0,recent-found,recent)     # recent was set to zero in master.R
  )]

  return(mx)
}
