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

#### with lavaan ####
m1 <- effectLite(y="dv", x="x", k=c("k1"), z=c("z1"), 
                 control="control", data=example01, 
                 fixed.cell=TRUE, fixed.z=TRUE)

#### step by step with ready functions ####

mm <- EffectLiteR:::computeModelMatrix(m1)  
m1.lm <- lm(dv ~ -1 + mm, data=example01) # fit model

coefs <- coef(m1.lm) 
vcovs <- vcov(m1.lm)

pnames <- m1@parnames@gammas
names(coefs) <- row.names(vcovs) <- colnames(vcovs) <- pnames
con_full <- EffectLiteR:::createLMSyntax(m1)
con <- con_full@model

# cat(con)

## compute effects
pt <- lavaanify(con)
def.function <- lavaan:::lav_partable_constraints_def(pt)
JAC <- lav_func_jacobian_complex(func=def.function, x=coefs)
info.r <- JAC %*% vcovs %*% t(JAC)
se <- sqrt(diag(info.r))
par <- def.function(coefs)

names(se) <- names(par)

m1@results@Egxgx

par[row.names(m1@results@Egxgx)]
se[row.names(m1@results@Egxgx)]



#### in one step: TODO write computeLMResults ####
m1 <- EffectLiteR:::effectLiteLM(y="dv", x="x", k=c("k1"), z=c("z1"), 
                                 control="control", data=example01, 
                                 fixed.cell=TRUE, fixed.z=TRUE)



