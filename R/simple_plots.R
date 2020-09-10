# questionnaire tables

# preprocess and read
pacman::p_load(data.table,knitr,kableExtra,maditr)

d=fread("/home/henk/Documents/PhD/Soysambu/Soysambu/inst/extdata/interviews/questionnaires.csv",header=T)

# process
tab1=d[label=="closed",][,label:=NULL]
setnames(tab1,"V1","Nr")

tab2=d[label=="open",][,label:=NULL]
setnames(tab2,"V1","Nr")

tab3=d[label=="communities",][,label:=NULL]
setnames(tab3,"V1","Nr")

# latex
kable(tab1,format="latex",booktabs=T) %>% column_spec(1,"3em") %>% column_spec(2,"12em")

kable(tab2,format="latex",booktabs=T) %>% column_spec(1,"3em") %>% column_spec(2,"12em")

kable(tab3,format="latex",booktabs=T) %>% column_spec(1,"3em") %>% column_spec(2,"12em")
