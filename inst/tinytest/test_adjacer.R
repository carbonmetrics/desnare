library(desnare)
testmy=fread("/home/henk/Documents/PhD/Soysambu/desnare/data-raw/testmy")
a3=c(5934,5935,6022,6110,6111,6198,6199,6285,6287,5936,6024,6112,6200,6288)

# helper function
`%nin%`=Negate(`%in%`)

# set up memory
adj.mem<<-NULL
found.rep<<-NULL
found.seq<<-NULL
snare.cells<<-NULL

# only snares for which you predicted snares or cells that are not completely open area
mx=m[bush>0 & pred>0,]

# sort
setorder(mx,-block.n,-pred,-block.pred)

# assign slices
n.sample=20
n.slices=ceiling(nrow(mx)/n.sample)
mx[,slice:=rep(1:n.slices,each=n.sample,length.out=nrow(mx))]
mx[,`:=`(
  found=0,
  found.rep=0,
  counter=0,
  repeats=0,
  from.cell=NA,
  idn=1:.N
)]

# stats - found in first round and findable in second round
found.seq=testmy[,sum(found)]
findable.rep=mx[cell %in% a3,sum(snares)]

# run function and test
h=adjacer(testmy,mx,r$pred,desnare.reps=1,prob=0.4)
found.rep=h[,sum(found.rep)]
h

message("started with: ",found.seq)
message("findable in adjacent: ",findable.rep)
message("found in adjacent: ", found.rep)
message("total found: ", found.rep+found.seq)
message(found.seq,findable.rep,found.rep,found.rep+found.seq)

if(nrow(h)==nrow(testmy)){
  expect_message("no snares found in repeat.")
}else{
  expect_true(nrow(h)>nrow(testmy),info="adjacent cells added after finding a snare in repeat.")
}
expect_true(nrow(h[found>0 & snares>0,])==0,info="snares found in sequential run are set to zero.")
expect_true(nrow(h[found.rep>0 & snares>0])==0,info="snares found in repeat run are set to zero.")
expect_true(nrow(h[found.rep>0 & repeats==0,])==0,info="when you find snares in repeat, then there can be no zero repeat count.")


