# runs

# set up
library(desnare)

setup=function(){
  snare.cells<<-NULL
  adj.mem<<-NULL
  found.seq<<-NULL
  found.rep<<-NULL
  rm(list=setdiff(ls(), "setup"),envir = .GlobalEnv)
  mxr=m[pred>0 & bush>0,]
}

# test single runs ============

# test without cutoff and sequential only
setup()
test1=slicer(m,r,prob=0.4,n.sample=20,desnare.reps=2,sequential.only=T,cutoff=F)

nrow(test1)==nrow(mxr)
expect_true(test1[,sum(visits)]==nrow(mxr))
expect_true(test1[,sum(repeats)]==0)
expect_true(test1[,sum(found.seq)]>0)

# without cutoff and include repeats
setup()
test2=slicer(m,r,prob=0.4,n.sample=20,desnare.reps=2,sequential.only=F,cutoff=F)
nrow(test2[duplicated(cell),])==0
expect_true(all(test2[repeats>0,repeats]==2))
nrow(test2)==nrow(mxr)
foundseq=test2[,sum(found.seq)]
foundrep=test2[,sum(found.rep)]
expect_true(test2[,sum(found)]==foundseq+foundrep)
message("found seq: ",foundseq)
message("found rep: ", foundrep)
message("found total: ",foundseq+foundrep)
expect_true(test2[,sum(visits)]>nrow(mxr))

# with cutoff and include repeats
setup()
test3=slicer(m,r,prob=0.4,n.sample=20,desnare.reps=2,sequential.only=F,cutoff=T)
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

# with desnare.reps=1 and cutoff=F
setup()
test4=slicer(m,r,prob=0.4,n.sample=20,desnare.reps=1,sequential.only=F,cutoff=F)
cells.mxr=mxr[,cell]
cells.test4=test4[,cell]
expect_false(duplicated(cells.mxr) %>% any,info="no duplicated cells in original raster cell overview")
expect_false(duplicated(cells.test4) %>% any, info="no duplicated cells in result")
diffs=setdiff(cells.mxr,cells.test4) %>% sort
sum(diffs)==0  # all cells are covered
adj.mems=sort(adj.mem)
# unexpected behaviour: if there is just one repeat, then cumul.visits should be nrow(mxr)?
# no; every cell was visited, and the adjacent cell got an extra visit.

# test multiple runs ==================
# with cutoff, repeats, replicated over n runs
setup()
rm(l)
rm(ll)
reps=500
registerDoParallel(4)
l=foreach(i=1:reps) %dopar% {
  set.seed(i)
  message("\n run ",i," =================")
  setup()
  # slicer(m,r,prob=0.4,n.sample=20,desnare.reps=2,sequential.only=T,cutoff=T)
  # 10.58
  slicer(m,r,prob=0.4,n.sample=20,desnare.reps=2,sequential.only=F,cutoff=T)
  # 10.56
  # slicer(m,r,prob=0.4,n.sample=20,desnare.reps=2,sequential.only=F,cutoff=F)
  # 12.16
  # slicer(m,r,prob=0.4,n.sample=20,desnare.reps=1,sequential.only=F,cutoff=T)
  # 9.68  # there are more snare clusters without a neighbour than with one...so this is slightly lower.
}

ll=rbindlist(l,idcol=T)
snares.pred=m[pred>0 & bush>0,sum(snares)]
plot_results(snares.pred=snares.pred,prob=0.4,sims=ll,perc=F)
