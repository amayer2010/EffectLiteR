######### Test Yves idea with one sided main hypothesis
## with complex example
#
# m1 <- effectLite(data=d, y="dv", z=c("z1"),
#                  k=c("k1","kateg2"), x="x",
#                  control="control")
# model <- m1@lavaansyntax@model
#
# constr <- '
# adjmean0 > 0
# adjmean0 < adjmean1
# adjmean0 < adjmean2
# '
#
# # model <- paste(model,constr,sep="\n")
#
# d <- m1@input@data
# m2 <- sem(model, group="cell", group.label=levels(d$cell), data=d,
#     fixed.x=F, group.w.free = TRUE, mimic="mplus")
# summary(m2)
#
# InformativeTesting(model, R = 1000L, double.bootstrap = "standard", missing="listwise",
#                    constraints = constr, group="cell",
#                    group.label=levels(d$cell), data=d,
#                    fixed.x=F, group.w.free = TRUE, mimic="mplus")
#
