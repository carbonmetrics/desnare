# runs

# set up ==================================================================

library(desnare)
setup()
mxr=m[pred>0 & bush>0,]

# helper function
`%nin%`=Negate(`%in%`)

# test starting object
expect_true(nrow(mxr[snares>1])==0, info="cannot have more than one snare in one cell.")
expect_true(nrow(mxr[recent>0])==0, info="recent reserved for re-snaring, so must be zero at this stage.")


# test single runs =======================================================

# test without cutoff and sequential only -----------------------------------
setup()
contagion.cells=resnare(m,m,r,n.clumps=4,resnare.frac=1,cells.only=T)
test1=slicer(m,r,prob=0.4,n.sample=20,desnare.reps=2,sequential.only=T,cutoff=F,contagion.cells=contagion.cells,hotspots=F)

expect_true(nrow(test1)==nrow(mxr))
expect_true(test1[,sum(visits)]==nrow(mxr))
expect_true(test1[,sum(repeats)]==0)
expect_true(test1[,sum(found.seq)]>0)
expect_true(is.vector(snare.cells))
expect_true(is.null(adj.mem))

# without cutoff and include repeats --------------------------------------
rm(list=ls())
mxr=m[pred>0 & bush>0,]
`%nin%`=Negate(`%in%`)
setup()
contagion.cells=resnare(m,m,r,n.clumps=4,resnare.frac=1,cells.only=T)
test2=slicer(m,r,prob=0.4,n.sample=20,desnare.reps=2,sequential.only=F,cutoff=F,contagion.cells=contagion.cells,hotspots=F)

# add a balance field and remove fields that you do not need
test2[,balance:=snares+found]
test2=test2[,.(cell,snares,recent,found.seq,found.rep,counter,repeats,found,visits,cumul.visits,balance)]
expect_equal(nrow(test2[balance %nin% c(0,1) ]),0, info="there are no cells with a balance other than 0 (there were never cells) or 1 (snare still there or removed.)")

# row logic
expect_true(nrow(test2[duplicated(cell),])==0, info="there are no duplicated cells.")
expect_true(all(test2[repeats>0,repeats]==2), info="all repeats equal 2, because that's how you called the function.")
expect_true(nrow(test2)==nrow(mxr),info="no cutoff, therefore rows must be the same.")
expect_true(is.vector(adj.mem))

# snare remaining and found logic
foundseq=test2[,sum(found.seq)]
foundrep=test2[,sum(found.rep)]
message("found seq: ",foundseq)
message("found rep: ", foundrep)
message("found total: ",foundseq+foundrep)
remaining.snares=test2[,sum(snares)]
found.snares=test2[,sum(found)]
findable.snares=mxr[,sum(snares)]
resnared=test2[,sum(recent)]

expect_true(test2[,sum(visits)]>nrow(mxr))
expect_true(test2[,sum(found.snares)]==foundseq+foundrep,info="sum found all equals sum of found sequential and found repeated.")
expect_true(nrow(test2[snares<0])==0,info="cannot have negative snares.")
expect_equal(found.snares+remaining.snares, findable.snares,info="sum of found and remaining must equal findable snares.")


# with cutoff and include repeats ------------------------------------------
setup()
test3=slicer(m,r,prob=0.4,n.sample=20,desnare.reps=2,sequential.only=F,cutoff=T,contagion.cells=contagion.cells,hotspots=F)
expect_true(nrow(test3[duplicated(cell),])==0)
expect_true(all(test3[repeats>0,repeats]==2))
expect_true(nrow(test3)!=nrow(mxr))
foundseq=test3[,sum(found.seq)]
foundrep=test3[,sum(found.rep)]
expect_true(test3[,sum(found)]==foundseq+foundrep)
message("found seq: ",foundseq)
message("found rep: ", foundrep)
message("found total: ",foundseq+foundrep)
expect_true(nrow(test3[cumul.visits>nrow(mxr),])==0)
expect_true(nrow(test2)!=nrow(test3))


# with desnare.reps=1 and cutoff=F ------------------------------------------
setup()
test4=slicer(m,r,prob=0.4,n.sample=20,desnare.reps=1,sequential.only=F,cutoff=F,contagion.cells=contagion.cells,hotspots=F)
cells.mxr=mxr[,cell]
cells.test4=test4[,cell]
expect_false(duplicated(cells.mxr) %>% any,info="no duplicated cells in original raster cell overview")
expect_false(duplicated(cells.test4) %>% any, info="no duplicated cells in result")
diffs=setdiff(cells.mxr,cells.test4) %>% sort
expect_true(sum(diffs)==0)  # all cells are covered
adj.mems=sort(adj.mem)
# unexpected behaviour: if there is just one repeat, then cumul.visits should be nrow(mxr)?
# no; every cell was visited, and the adjacent cell got an extra visit.
expect_true(nrow(test4[snares>1,])==0)

