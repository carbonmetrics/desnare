#' Sort and cutoff
#'
#' @param z data.table after doing sequential and adjacent desnare and merge of the results
#' @return data.table cutoff at an effort equal to the number of cells visited during sequential desnare
#' @export


sort_cutoff=function(z){

  # sort --------------------------------------------

  # the snares found and repeat visits to adjacent cells must be accounted for differently.
  # the visit counts and snare founds for each snare with surrounding cells are accounted at once,
  # when the snare cell in question is encountered.

  # prepare
  # set snare cells from the first round of desnaring to id to allow grouping of snare cells and associated repeats
  z[!is.na(id),.id:=id]

  # summary table
  dt=z[,.(
    repeats=sum(count.repeat,na.rm=T),
    found.snares=sum(found.repeat,na.rm=T)   # found in repeat desnaring
  ),.id]

  # if .id=NA then there was no repeat. remove and sort on .id (order of finding snares)
  dt=dt[!is.na(.id),][order(.id),]
  setnames(dt,".id","id")

  # merge with main df - now you add repeats and found snares during these repeats
  z=dt[z,on="id"]

  # update
  z[is.na(repeats),repeats:=1]             # same as counter, repeats will be used for cumulative nr of visits
  z[,cumul:=cumsum(repeats)]               # total number of visits
  z[is.na(found.snares),found.snares:=0]

  z[,ffound:=found+found.snares]
  z[,cumul.found:=cumsum(ffound)]

  # admin
  scells=z[found>0,cell]
  # snare.mem<<-append(snare.mem,scells)

  # cutoff ----------------------------------------
  tr=nrow(z)
  # z[,cumul:=cumsum(counter)]

  finalz=z[cumul<=tr,]

  #mynames=names(mx)
  #mynames=c(mynames,"repeats","cumul","ffound")
  #finalz=finalz[,..mynames]

  return(finalz)
}


# unit testing ------------------------

# data(m)
# findable=m[pred>0 & bush>0,sum(snares)]       # snares placed
# mx=sequential_desnare(m)                      # first round of desnaring
# found.seq=mx[,sum(found)]                     # found during first round
# l=desnare_adjacent(mx)                        # check adjacent cells
# found.adj=rbindlist(l)[,sum(found.repeat)]    # found during second round
# z=merge_sequential_adjacent(mx,l)             # merge first and second desnaring rounds
# total.found=found.seq+found.adj
# final=sort_cutoff(z)
#
# expect_true(nrow(final) < nrow(mx), info="data.table should be cutoff at less rows than what you started with")  # you will find > 1 snare, therefore adjacent repeats
# # expect_true(final[,sum(found)] == final[,sum(ffound)], info="sum found (raw sums visits and snares found) must be equal to ffound (visit counts and snares found assigned in chunks at originating snare cell)")
# expect_true(all(final[snares>0,found]==0),info="if there are snares, then found must always be zero.")
# expect_true(all(final[found>0,found]==1),info="if there are no snares, then found must always be one.")

# to do: think about differences found and ffound, this is confusing.
