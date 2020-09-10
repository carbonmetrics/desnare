
merge_sequential_adjacent=function(mx,l){

  # merge ---------------------------

  # bind results in a data.table
  zz=rbindlist(l,idcol=T)
  snare.cells=mx[found>0,cell]

  # remove cells that were already snare cells
  if(nrow(zz)>0){
    froms=zz[,unique(from.cell)]
    zz=zz[cell %nin% froms,]
  }

  # overview of snare cells, because some ids may be missing (NULL in l)
  if(nrow(zz)>0){
    sdf=data.table(
      cell=snare.cells,
      id=1:length(snare.cells),
      no.vals=unlist(lapply(l,is.null))
    )
  }else{
    sdf=data.table(NULL)
  }

  # prepare merge
  z=copy(mx)

  # merge
  if(nrow(zz)>0){
    z=zz[z,on=c("cell","idx")]       # merge with repeats
    sdf=sdf[,.(cell,id)]       # merge with snare cell summary
    z=sdf[z,on="cell"]

    # same cell may be covered via different visits from snare cells, in that case, consolidate

    # clean up
    z[!is.na(count.repeat),`:=`(    # select repeat cells
      found=found+found.repeat,     # for these cells, add snares found in repeats
      counter=counter+count.repeat  # and update the counter
    )]

    z[!is.na(snares.repeat),snares:=snares.repeat]  # if snares found during repeats: update snares field
  }

  return(z)

  # notes
  #' in object z, .id comes from the repeats. so, in z[found>0,] non-NA values for .id means that the snares were found during repeat desnaring,
  #' if they are NA, then they were found during the first round of desnaring.
  #' id comes from the snare cell summary data.table sdf. here, the id stands for the snare cell, in order of identification.
  #' here, id indicates that the snare was found during the first sequential desnaring, and NA that they were found during the repeats.

}

# unit testing -------------------------

# data(m)
# findable=m[pred>0 & bush>0,sum(snares)]       # snares placed
# mx=sequential_desnare(m)                      # first round of desnaring
# found.seq=mx[,sum(found)]                     # found during first round
# l=desnare_adjacent(mx)                        # check adjacent cells
# found.adj=rbindlist(l)[,sum(found.repeat)]    # found during second round
# z=merge_sequential_adjacent(mx,l)             # merge first and second desnaring rounds
# total.found=found.seq+found.adj
#
# expect_equal(nrow(z),nrow(mx),info="Number of rows of merged object must be the same as the one you started with.")
# expect_true(all(z[found>0,found] == 1), info="All cases where a snare was found have value 1.")
# expect_true(all(z[found>0,snares] == 0),info="If the snare was found, then snare must be 0.")
#
# expect_equal(z[found>0 & is.na(.id),sum(found)],found.seq, info="Snares found from non-repeats are equal to snares found in first sequential round of desnaring.") # see notes below
# found.rep=z[found>0 & is.na(id),sum(found)]
# expect_equal(z[,sum(found)],found.seq+found.rep,info="Snares found must equal snares found during sequential desnaring + repeat desnaring.")

