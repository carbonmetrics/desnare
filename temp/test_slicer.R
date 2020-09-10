
# set up
snares.pred=m[bush>0 & pred>0,sum(snares)]
prob=0.4
desnare.reps=2
adj.mem<<-NULL
`%nin%`=Negate(`%in%`)

df=slicer(m,r,prob=0.4,desnare.reps=2,n.sample=20,cutoff=T,sequential.only=F)

# unit testing
expect_true(nrow(df[duplicated(cell),])==0,
            info="no duplicated cells left.")
expect_true(nrow(df[found.seq>0 & snares>0,])==0,
            info="cannot have found and snares both larger than 0.")
expect_true(nrow(df[found.rep>0 & snares>0,])==0,
            info="cannot have found.repeat and snares both larger than 0.")
expect_true(all(df[found.rep>0,repeats]>=desnare.reps),
            info="if you have found a snare during a repeat desnaring then all repeats must at least be equal repeat setting.")
expect_true(df[nrow(df),cumul.visits]<=nrow(m[pred>0 & bush>0]),
            info="with cutoff, the cumulative visits must be less or equal to object mx cells (is nrows mx).")

dfx=slicer(m,r,prob=0.4,desnare.reps=2,n.sample=20,cutoff=T,sequential.only=T)
expect_true(nrow(dfx[found.rep>0,])==0,
            info="with sequential only you cannot find snares in found.rep.")
expect_true(nrow(dfx[counter>1,])==0,
            info="with sequential only you cannot have counters larger than 1.")
