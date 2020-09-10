library(desnare)
data(m)
prob=0.4
include.bush=F
# `%nin%`=Negate(`%in%`)
snare.mem=NULL


# sequential desnaring =============================

mx=sequential_desnare(m)

findable=copy(m[bush>0 & pred>0,sum(snares)])  # do not use mx for this, as it was changed after initialization
message("found ",mx[,sum(found)]," snares out of ",findable, " snares in sequential run.")

expect_true(mx[,sum(snares)>0],
            info="In a single sequential run, you'd expect to find at least one snare.")
expect_true(mx[,sum(found)]/findable < (2*prob),
            info="Max fraction of snares found should be smaller than twice the prob of detection.")
expect_true(mx[,sum(found)] < findable,
            info="Cannot find more snares than there were in the first place.")
expect_true(mx[,sum(snares)] == nrow(mx[snares>0,]),
            info="Findable snares equals number of rows with snares (1 snare per cell case)")
expect_true(all(mx[found>0,snares+found]==1),
            info="When a snare is found, the sum of 'found' and 'snares' equals 1.")
expect_true(mx[,sum(recent)]==0,
            info="There are no 'recent' values at this stage.")
expect_true(mx[,sum(counter)]==nrow(mx),
            info="For sequential single-pass run, the cumulative counter equals the number of rows.")
found.seq=mx[,sum(found)]


# adjacent desnaring =============================

l=desnare_adjacent(mx,desnare.reps=2)                        # check adjacent cells
found.adj=rbindlist(l)[,sum(found.repeat)]    # found during second round
total.found=found.seq+found.adj
# the number of snare cells will not be very high, therefore sensitive to random fluctuations in dice throw.
expect_true(length(
  rbindlist(l)[duplicated(cell),cell]
) == 0,
info="No duplicated cells are allowed, since they were removed via global variable adj.mem")
expect_true(all(rbindlist(l)[,count.repeat] == 2),
            info="with desnare.reps=2 all count.repeats must be 2 as well.")


# merge adjacent and sequential desnaring =======

z=merge_sequential_adjacent(mx,l)             # merge first and second desnaring rounds

expect_equal(nrow(z),nrow(mx),
             info="Number of rows of merged object must be the same as the one you started with.")
expect_true(all(z[found>0,found] == 1),
            info="All cases where a snare was found have value 1.")
expect_true(all(z[found>0,snares] == 0),
            info="If the snare was found, then snare must be 0.")

expect_equal(z[found>0 & is.na(.id),sum(found)],found.seq,
             info="Snares found from non-repeats are equal to snares found in first sequential round of desnaring.") # see notes below
found.rep=z[found>0 & is.na(id),sum(found)]   # found during desnaring of cells that surround snare cells
expect_equal(z[,sum(found)],found.seq+found.rep,
             info="Snares found must equal snares found during sequential desnaring + repeat desnaring.")

expect_true(nrow(z[counter>1])>1,
            info="Some snares will be found, and therefore some cells will have a counter larger than 1")
expect_true(nrow(z[counter>3])==0,
            info="with desnare.reps=2, no counter can be larger than 3." )

# lx=desnare_adjacent(mx,desnare.reps=1)
# zx=merge_sequential_adjacent(mx,l)
# expect_true(nrow(zx)==zx[,sum(counter)])

# sort and cutoff ============================

final=sort_cutoff(z)

expect_true(nrow(final) < nrow(mx),
            info="data.table should be cutoff at less rows than what you started with")  # you will find > 1 snare, therefore adjacent repeats
expect_true(all(final[snares>0,found]==0),
            info="if there are snares, then found must always be zero.")
expect_true(all(final[found>0,found]==1),
            info="if there are no snares, then found must always be one.")
expect_true(final[,sum(recent)]==0,
            info="there are no values in the recent field at the start of resnaring.")

start.snares=final[,sum(snares)]
x=resnare(final,m,r,resnare.frac=1)
expect_true(x[,sum(recent)>0])

end.snares=x[,sum(snares)]
expect_equal(x[,sum(recent)] + start.snares, end.snares,
             info="the sum of the equal flags corresponds with the number of snares added.")
expect_true(final[,sum(ffound)]==final[,sum(found.snares)]+final[,sum(found)],
            info="overall sum (ffound) should be equal to sum of found (sequential) and sum of found.snares (adjacent).")
