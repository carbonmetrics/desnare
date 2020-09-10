# plot stabilization of results: running moving average

library(desnare)

sims=fread("/home/henk/Documents/PhD/Soysambu/desnare/data-raw/hot40.txt")
reps=500

# helper function
`%nin%`=Negate(`%in%`)

# discoverable snares
mx=m[pred>0 & bush>0,]
snares.pred=mx[,sum(snares)]

# test whether you have the right object
if(!all(c(".id","found") %in% names(sims))){
  stop("I need an object with the names .id and found in it.")
}

# stabilization of results ------------------------------
# calculate the average number of snares found per simulation run
found=sims[,.(found=sum(found)),.id]
expected.runs=data.table(.id=1:reps)

# some runs may have been skipped: a hotspot run was required
if(nrow(found) != nrow(expected.runs)){
  warning("some runs were skipped, filling...")
  foundx=found[expected.runs,on=".id"]
  foundx[,found:=nafill(found,type="locf")]  # fills with previous value
}else{
  foundx=found
}

run.avg=foundx[,cumsum(found)/.id]

# plot (perc)
a=getwd()
setwd("~/Documents/PhD/Dissertation/figures")
pdf(file="moving_avg.pdf")
plot(run.avg/snares.pred,
     xlim=c(0,length(run.avg)),
     xlab="",
     ylab="",
     type="b",bty="n")
mtext("simulation run index",side=1,padj=4,cex=1.2)
mtext("fraction of snares found",side=2,padj=-4,cex=1.2)
dev.off()

system("pdfcrop moving_avg.pdf moving_avg.pdf")

plot(run.avg,
     xlim=c(0,length(run.avg)),
     xlab="simulation run index",
     ylab="number of snares found",
     type="b",bty="n")
mtext("Simulation run index",side=1,padj=4,cex=1.2)
mtext("Number of snares found",side=2,padj=-4,cex=1.2)

setwd(a)
