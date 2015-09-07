
library(EffectLiteR)
d <- example01


######### some experiments with Wald, Score, and LRT ###########

# requires dev version of lavaan

add <- '
Eg1==0
Eg2==0
'

## restricted model
m1r <- effectLite(data=d, y="dv", z=c("z1"), k=c("k1"), x="x", 
                 control="control", add=add)
lavTestScore(m1r@results@lavresults, release=1:2)

## unrestricted model
m1u <- effectLite(data=d, y="dv", z=c("z1"), k=c("k1"), x="x",control="control")
m1u@results@hypotheses

## LRT Test
lavTestLRT(m1u@results@lavresults,m1r@results@lavresults)


############ Experiments with Order constraint stuff ###############


m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", 
                 control="control")
# sem(model=model, group="cell", missing="listwise", se="standard", 
#     group.label=c("11","12","21","22","31","32"), data=data, 
#     fixed.x=FALSE, group.w.free=TRUE, mimic="mplus")

constraints <- '
adjmean0 > adjmean1
adjmean1 > adjmean2
'
model <- m1@lavaansyntax@model
data <- m1@input@data


example <- InformativeTesting(model=model, data=data, 
                              R=10L, double.bootstrap="no",
                              constraints=constraints,
                              group="cell", missing="listwise",
                              group.label=c("11","12","21","22","31","32"), 
                              fixed.x=FALSE, group.w.free=TRUE, mimic="mplus")
example
