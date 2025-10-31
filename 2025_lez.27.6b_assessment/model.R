## Run analysis, write model results

## Before:
## After:

library(icesTAF)
library(spict)
library(FishLife)
library(corrplot)

source("functions/myretro_function.r")

mkdir("model")

megC <- read.taf("data/megC.csv")
megI <- read.taf("data/megI.csv")


### data structured as a list for SPiCT
inp <- list(obsC=megC$catch, timeC=megC$yr, obsI=megI$biomass, timeI=megI$year)

### Set the period where survey takes place
inp$timeI <- inp$timeI + (1/12)*4.5 # Apr 15th

#### intrinsic growth rate (r) from Thorsen Fishlife for Megrim ####
par(mfrow=c(1, 2))

stk.fishlife<-Plot_taxa(Search_species(Genus="Lepidorhombus",
                                       Species="whiffiagonis")$match_taxonomy, 
                        mfrow=c(1,1))

lnr <- stk.fishlife[[1]]$Mean_pred["ln_r"]
sd_lnr <- sqrt(stk.fishlife[[1]]$Cov_pred["ln_r", "ln_r"])
curve(dlnorm(x, lnr, sd_lnr), from = 0, to = 1)

exp(lnr)
exp(sd_lnr)

#### Fix model parameters ####

inp$priors$logr <- c(lnr, sd_lnr, 1) # (r prior) 
inp$ini$logn <- log(2) # shape parameter 
inp$phases$logn <- -1 # fixed to Schaefer 
inp$robflagc <- 1 # Robust estimation
inp$priors$logbkfrac <- c(log(0.5),0.5,1)# initial depletion prior
### We assume from expert discussions, the stock was moderately exploited before 1991

## Check that the 2 priors (r, logbkfrac are active)
get.no.active.priors(inp)

#### Check and plot input data ####
check.inp(inp)

png(file="model/input_data.png")
plotspict.data(inp)
dev.off()

#### Fit Spict to data ####
res <- fit.spict(inp) 
res
plot(res)

png("model/Assessment_Res.png", width = 12, height = 8, units = "in", res = 300)
plot(res)
dev.off()

png("model/Assessment_Res_brief.png")
plot2(res)
dev.off()


### Save fit summary
fitSummary <- capture.output(res)
write.csv(fitSummary ,"model/lez6b_fitsummary.csv", quote = TRUE,
          eol = "\n", na = "NA", row.names = TRUE, fileEncoding = "")

writeLines(paste("<pre>", paste(fitSummary, collapse = "\n"), "</pre>"), "model/lez6b_fitsummary.html")

#### Check Model Acceptance (NO VIOLATIONS) ####

res$opt$convergence # should equal 0
all(is.finite(res$sd)) # should be TRUE

res <- calc.osa.resid(res) # Calculate residuals
plotspict.diagnostic(res) # No Violations 


png("model/Assessment_residuals.png", width = 12, height = 8, units = "in", res = 300)
plotspict.diagnostic(res)
dev.off()


retro <- myretro(res, nretroyear=5) # Retrospective analysis
plotspict.retro(retro) # trajectories of those two quantities should be inside the confidence intervals of the base run.

png("model/Assessment_retros.png", width = 12, height = 8, units = "in", res = 300)
plotspict.retro(retro)
dev.off()


calc.bmsyk(res) # should be between 0.1 and 0.9
calc.om(res)  # should not span more than 1 order of magnitude

res <- check.ini(res) # sensitivity check
res$check.ini$resmat #the estimates should be the same for all initial values

png("model/Production_curve.png")
plotspict.production(res)
dev.off()

png("model/Biomass.png")
plotspict.biomass(res)
dev.off()

png("model/F mort.png")
plotspict.f(res)
dev.off()

#### Check parameters correlation ####
colp <- colorRampPalette(rev(c("#67001F", "#B2182B", "#D6604D", 
                               "#F4A582", "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", 
                               "#4393C3", "#2166AC", "#053061"))) ## intiutively think cold is negative and blue

## full correlation (fixed and random effects)
precision <- sdreport(res$obj, getJointPrecision = TRUE)

all_cov <- solve(precision$jointPrecision)

pars <- names(res$opt$par)
pars <- pars[pars != "logn"]
idx <- which(colnames(all_cov) %in% pars)

## first logB
idxB0 <- which(colnames(all_cov) == "logB")[1]
idall <- c(idx, idxB0)

corr_B0 <- cov2cor(all_cov[idall, idall])
colnames(corr_B0)[colnames(corr_B0) == "logB"] <- "logB0"
rownames(corr_B0)[rownames(corr_B0) == "logB"] <- "logB0"

corrplot(corr_B0, method = "ellipse",         ## no one above +-0.7
         type = "upper", col = colp(200),
         addCoef.col = "black", diag = FALSE)

dev.print(pdf, "model/correlation_plot.pdf")


