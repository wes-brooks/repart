# exploratory workflow


# load data ---------------------------------------------------------------
load("data/beetles.RData")
df <- beetles[,c("spider", "cue", "species")]

# sweep 'unaccounted' and 'dead' into one class, and drop the 'dead' level from the factor
beetles[[ "outcome" ]] <- forcats::fct_other(beetles[[ "outcome" ]], keep = c("alive", "emigrated"), other = "unaccounted")
outcome <- model.matrix(~-1 + outcome, data = beetles)


# call the tree -----------------------------------------------------------
mytree <- make_tree(df, outcome)


# get residuals from the fitted tree --------------------------------------
resid <- residuals.repart(mytree)


# convert residuals to full-scale -----------------------------------------
# rr <- -1 * sign(resid) * log(1 - abs(resid))
rr <- resid


# estimate a random effects model from the tree ---------------------------
require("lme4")

# make a random-intecept model for each level of the factor
df2 <- cbind(beetles, rr)
lm1 <- lmer(alive ~ 1 + (1|Trial) + (1|id), data=df3)
lm2 <- lmer(emigrated ~ 1 + (1|Trial) + (1|id), data=df2)
lm3 <- lmer(unaccounted ~ 1 + (1|Trial) + (1|id), data=df2)

### associate each random intercept with the relevant observations
ri <- matrix(0, nrow=nrow(beetles), ncol=0)

# start with lm1
re1 <- ranef(lm1)
indx1.id <- match(beetles$id, rownames(re1$id))
indx1.Trial <- match(beetles$Trial, rownames(re1$Trial))
ri1 <- cbind(re1$id$`(Intercept)`[indx1.id], re1$Trial$`(Intercept)`[indx1.Trial])
ri1.sum <- rowSums(ri1)

#back-convert r1
# random_eff1 <- sign(ri1.sum) * (1 - exp(-1 * sign(ri1.sum) * ri1.sum))
random_eff1 <- ri1.sum


# lm2
re2 <- ranef(lm2)
indx2.id <- match(beetles$id, rownames(re2$id))
indx2.Trial <- match(beetles$Trial, rownames(re2$Trial))
ri2 <- cbind(re2$id$`(Intercept)`[indx2.id], re2$Trial$`(Intercept)`[indx2.Trial])
ri2.sum <- rowSums(ri2)

#back-convert r2
# random_eff2 <- sign(ri2.sum) * (1 - exp(-1 * sign(ri2.sum) * ri2.sum))
random_eff2 <- ri2.sum


# lm3
re3 <- ranef(lm3)
indx3.id <- match(beetles$id, rownames(re3$id))
indx3.Trial <- match(beetles$Trial, rownames(re3$Trial))
ri3 <- cbind(re3$id$`(Intercept)`[indx3.id], re3$Trial$`(Intercept)`[indx3.Trial])
ri3.sum <- rowSums(ri3)

#back-convert r2
# random_eff3 <- sign(ri3.sum) * (1 - exp(-1 * sign(ri3.sum) * ri3.sum))
random_eff3 <- ri3.sum

# subtract the mean from the probability-scale random effects
p.re <- cbind(random_eff1, random_eff2, random_eff3)
p.re <- sweep(p.re, 1, rowMeans(p.re))


# adjust the observed outcome with the residuals:
new_outcome <- outcome - p.re
new_tree <- make_tree(df, new_outcome)



# cast the tree to a partykit object --------------------------------------





# using the Titanic example from the vignette to explore using the partykit methods --------
require("partykit")
data("Titanic", package = "datasets")
ttnc <- as.data.frame(Titanic)
ttnc <- ttnc[rep(1:nrow(ttnc), ttnc$Freq), 1:4]
names(ttnc)[2] <- "Gender"

(myttnc <- mytree(Survived ~ Class + Age + Gender, data = ttnc))



# now try partykit on the beetle data -------------------------------------

(btree1 <- mytree(outcome ~ cue + spider + species, data = beetles, alpha=0.063))

# get residuals from the fitted tree --------------------------------------
resid <- residuals2.repart(btree1)


# convert residuals to full-scale -----------------------------------------
# rr <- -1 * sign(resid) * log(1 - abs(resid))
rr <- resid


# estimate a random effects model from the tree ---------------------------
require("lme4")

# make a random-intecept model for each level of the factor
df2 <- cbind(beetles, rr)
lm1 <- lmer(alive ~ 1 + (1|Trial) + (1|id), data=df2)
lm2 <- lmer(emigrated ~ 1 + (1|Trial) + (1|id), data=df2)
lm3 <- lmer(unaccounted ~ 1 + (1|Trial) + (1|id), data=df2)

### associate each random intercept with the relevant observations
ri <- matrix(0, nrow=nrow(beetles), ncol=0)

# start with lm1
re1 <- ranef(lm1)
indx1.id <- match(beetles$id, rownames(re1$id))
indx1.Trial <- match(beetles$Trial, rownames(re1$Trial))
ri1 <- cbind(re1$id$`(Intercept)`[indx1.id], re1$Trial$`(Intercept)`[indx1.Trial])
ri1.sum <- rowSums(ri1)

#back-convert r1
# random_eff1 <- sign(ri1.sum) * (1 - exp(-1 * sign(ri1.sum) * ri1.sum))
random_eff1 <- ri1.sum


# lm2
re2 <- ranef(lm2)
indx2.id <- match(beetles$id, rownames(re2$id))
indx2.Trial <- match(beetles$Trial, rownames(re2$Trial))
ri2 <- cbind(re2$id$`(Intercept)`[indx2.id], re2$Trial$`(Intercept)`[indx2.Trial])
ri2.sum <- rowSums(ri2)

#back-convert r2
# random_eff2 <- sign(ri2.sum) * (1 - exp(-1 * sign(ri2.sum) * ri2.sum))
random_eff2 <- ri2.sum


# lm3
re3 <- ranef(lm3)
indx3.id <- match(beetles$id, rownames(re3$id))
indx3.Trial <- match(beetles$Trial, rownames(re3$Trial))
ri3 <- cbind(re3$id$`(Intercept)`[indx3.id], re3$Trial$`(Intercept)`[indx3.Trial])
ri3.sum <- rowSums(ri3)

#back-convert r2
# random_eff3 <- sign(ri3.sum) * (1 - exp(-1 * sign(ri3.sum) * ri3.sum))
random_eff3 <- ri3.sum

# subtract the mean from the probability-scale random effects
p.re <- cbind(random_eff1, random_eff2, random_eff3)
p.re <- sweep(p.re, 1, rowMeans(p.re))


# adjust the observed outcome with the residuals:
new_outcome <- outcome - p.re
# new_tree <- make_tree(df, new_outcome)
(btree2 <- mytree2(outcome ~ cue + spider + species, data = beetles, outcome = new_outcome))

