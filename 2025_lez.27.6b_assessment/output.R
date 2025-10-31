## lez.27.6b assessment - outputs for reporting
## September 2025

library(icesTAF)
library(icesAdvice)
library(xtable)

source("functions/funcs_no_retape_Kokkalis.r")

mkdir("output")

#### Strip out Model Parameters
xtab<-function(x,caption='Table X.', file=stdout(), width='"100%"', cornername='', dec=rep(1,ncol(x))){
  nc<-ncol(x)
  lin<-paste('<table width=',width,'>', sep='')
  lin<-c(lin,sub('$','</td></tr>',sub('\\. |\\.$','.</b> ',
                                      sub('^', paste('<tr><td colspan=',nc+1,'><b>',sep=''), caption))))
  hr<-paste('<tr><td colspan=',nc+1,'><hr noshade></td></tr>', sep='')
  lin<-c(lin,hr)
  cnames<-colnames(x)
  cnames<-paste(sub('$','</b></td>',sub('^','<td align=right><b>',cnames)), collapse='\t')
  lin<-c(lin,paste('<tr>',paste('<td align=left><b>',cornername,'</b></td>',sep=''),cnames,'</tr>'))
  lin<-c(lin,hr)
  rnames<-sub('$','</b></td>',sub('^','<tr> <td align=left><b>',rownames(x)))
  #x<-sapply(1:ncol(x),function(i)sub('NA','  ',format(round(x[,i],dec[i]))))
  x<-sapply(1:ncol(x),function(i)sub('NA','  ',formatC(round(x[,i],dec[i]),digits=dec[i], format='f')))
  for(i in 1:nrow(x)){
    thisline<-paste(rnames[i],paste(sub('$','</td>',sub('^','<td align=right>',x[i,])), collapse='\t'),'</tr>', sep='')
    lin<-c(lin,thisline)
  }
  lin<-c(lin,hr)
  lin<-c(lin,'</table><br>\n')
  writeLines(lin,con=file)
}


##############################
#### Management scenarios ####
##############################

# Set management scenario's agreed at benchmark

TAC <- mean(inp$obsC) # When TAC is not fully fished (Use fishing at status quo)
TAC2 <- mean(tail(inp$obsC, 3))

res <- manage(res, scenarios=c(4,2,3,8), maninterval = c(2026, 2027), intermediatePeriodCatch = TAC2)

# 
# res <- add.man.scenario2(res, "F=Fsq_ffac", ffac = Fsq_value/Fint_value, intermediatePeriodCatch = TAC2, use.ffac.for.int.c = TRUE)
# res <- add.man.scenario2(res, "F=Fint_ffac", ffac = 1, intermediatePeriodCatch = TAC2, use.ffac.for.int.c = TRUE)
# res <- add.man.scenario2(res, "F=Fmsy_ffac", breakpointB = 0.5, intermediatePeriodCatch = TAC2, use.ffac.for.int.c = TRUE)
# res <- add.man.scenario2(res, "F=Fmsy_C_fractile_ffac", fractiles = list(catch = 0.35), breakpointB = c(1/3, 1/2), intermediatePeriodCatch = TAC2, use.ffac.for.int.c = TRUE)


sumspict.manage(res)


#### F status quo fix

Fsq_value <- get.par("logFFmsy", res$man[[1]], exp = T)[as.character(2025-0.0625), 2] ### change year as needed
Fint_value <- get.par("logFFmsy", res$man[[1]], exp = T)[as.character(2026-0.0625), 2] ### change year as needed
res <- add.man.scenario2(res, "F=Fsq_ffac", ffac = Fsq_value/Fint_value, intermediatePeriodCatch = TAC2)

sumspict.manage(res) # management summary 

sumspict.predictions(res, ndigits = 2) # predictions

plotspict.catch(res)

png("output/catch options.png", width = 720)
plotspict.catch(res)
dev.off()

# plotspict.hcr(res)

plotspict.hcr(res, xlim = c(0,1))

#### Tables of estimates ####

# Parameter Estimates
tab1 <- sumspict.parest(res$man[[1]]);
xtab(tab1,caption="Parameter estimates",cornername="Parameter",
     file= "output/Parameter_estimates.html",dec=rep(4,ncol(tab1)))

# Reference Points
tab2 <- sumspict.srefpoints(res$man[[1]]);
xtab(tab2,caption="Stochastic reference points",cornername="Reference points",
     file= "output/Reference_points.html",dec=rep(4,ncol(tab2)))

# Estimated States
tab3 <- sumspict.states(res$man[[1]]);
xtab(tab3,caption="Estimated states",cornername="",
     file= "output/Estimated_states.html",dec=rep(4,ncol(tab3)))

# Forecast
tab4 <- sumspict.predictions(res$man[[1]]);
xtab(tab4,caption="Forecast",cornername="Intermediate Yr",
     file= "output/Forecast.html",dec=rep(4,ncol(tab4)))

# Relative Biomass
tab5 <- get.par("logBBmsy",res$man[[1]],exp=TRUE)
tab5_<- tab5[grep(".",rownames(tab5), fixed=TRUE, invert=TRUE),] 
xtab(tab5_,caption="Relative Biomass",cornername="B/Bmsy",
     file= "output/BBmsy.html",dec=rep(4,ncol(tab5_)))

# years for extraction for SAG
min.year <- as.numeric(min(rownames(tab5)))
max.year <-  as.numeric(max(rownames(tab5)))
yrs <- data.frame(year = c(min.year:max.year))

tab5_2 <- tab5
tab5_2 <- cbind(rownames(tab5_2), data.frame(tab5_2, row.names=NULL))
colnames(tab5_2)[1] <- "year"

tab5_3 <- merge(yrs, tab5_2)
write.csv(tab5_3, "output/BBmsy.csv")

# Absolute Biomass
tab5b <- get.par("logB",res$man[[1]],exp=TRUE)
tab5b_<- tab5b[grep(".",rownames(tab5b), fixed=TRUE, invert=TRUE),] 
xtab(tab5b_,caption="Biomass",cornername="",
     file= "output/Biomass.html",dec=rep(4,ncol(tab5b)))

# Relative Fishing Mortality
tab6 <- get.par("logFFmsy",res$man[[5]],exp=TRUE)
tab6_ <- tab6[grep("\\.0625$", rownames(tab6)), , drop = FALSE] # want to take the value just at the start of the next year
rownames(tab6_) <- sub("\\..*$", "", rownames(tab6_))

xtab(tab6_,caption="Relative Fishing Mortality",cornername="F/Fmsy",
     file= "output/FFmsy.html",dec=rep(4,ncol(tab6_)))

tab6_2 <- tab6
tab6_2 <- cbind(rownames(tab6_2), data.frame(tab6_2, row.names=NULL))
colnames(tab6_2)[1] <- "year"
tab6_2$year <- as.numeric(tab6_2$year) - 0.0625
tab6_3 <- merge(yrs, tab6_2)
write.csv(tab6_3, "output/FFmsy.csv")

# Absolute Fishing Mortality
tab6b <- get.par("logF",res$man[[1]],exp=TRUE)
tab6b_ <- tab6b[grep("\\.0625$", rownames(tab6b)), , drop = FALSE] # want to take the value just at the start of the next year
rownames(tab6b_) <- sub("\\..*$", "", rownames(tab6b_))

xtab(tab6b_,caption="Fishing Mortality",cornername="",
     file= "output/fishingMortality.html",dec=rep(4,ncol(tab6b)))

# Catch
tab6c <- get.par("logCpred",res$man[[1]],exp=TRUE)
xtab(tab6c,caption="Catch",cornername="",file= "output/Catch.html",dec=rep(4,ncol(tab6c)))

##################################
#### Intermediate year checks ####
##################################

# ffmsy <- as.data.frame(get.par("logFFmsy", res$man[[1]], exp = T))
# ffmsy <- cbind(rownames(ffmsy), data.frame(ffmsy, row.names=NULL))
# colnames (ffmsy)[1] <- "year"
# colnames (ffmsy)[3] <- "ffmsy"
# 
# bbmsy <- as.data.frame(get.par("logBBmsy", res$man[[1]], exp = T))
# colnames (bbmsy)[2] <- "bbmsy"
# 
# checkback <- cbind(ffmsy[,c(1,3)], bbmsy[,2])
# checkback$year <- as.numeric(checkback$year)
# 
# 
# min.year <- as.numeric(min(rownames(tab5)))
# max.year <-  as.numeric(max(rownames(tab5)))
# yrs <- data.frame(year = c(min.year:max.year))
# 
# 
# checkback <- merge (yrs, checkback, all.y = F)
# 
# #catch <- as.data.frame(get.par("logCpred", res$man[[1]]$man$`F=Fmsy_C_fractile`, exp = T))
# catch <- as.data.frame(get.par("logCpred", res$man[[1]], exp = T))
# 
# colnames (catch)[2] <- "catch"
# catch[nrow(catch)+1, ] <- NA
# 
# checkback <- cbind(checkback, catch[,2])
# colnames(checkback) <- c("year","ffmsy", "bbmsy", "catch")
# 
# write.csv(checkback, "output/checkback.csv")



#### Produce csv for use in standard graphs and summary of assessment ####
# add bbmsy
sg_df <- tab5_3[c(1:(nrow(tab5_3)-2)),c(1:4) ]
# add input catch
catch_sg <- data.frame(Year = sg_df$year, Catch = "" )
catch_sg$Landings <- NA
catch_sg$Discards <- NA

catch_sg$Landings[1:nrow(megC)] <- round(megC$Landings)
catch_sg$Discards[1:nrow(megC)] <- round(megC$Discards)

sg_df <- cbind(sg_df, catch_sg$Landings, catch_sg$Discards) 

# add ffmsy
sg_df <- cbind(sg_df,  tab6_3[c(1:(nrow(tab6_3)-1)),c(2:4) ])
sg_df[nrow(sg_df), c(7:9)] <- NA

colnames (sg_df) <- c("Year", "BBmsy_lower", "BBmsy_est", "BBmsy_upper",
                      "Landings", "Discards", "FFmsy_lower", "FFmsy_est", "FFmsy_upper")
sg_df[,c(2:4, 7:9)] <- round(sg_df[,c(2:4, 7:9)], digits = 3)

write.csv(sg_df, "output/summary_assessment_and_standard_graphs.csv")

sg_df$BBmsy_lower <- icesRound(sg_df$BBmsy_lower)
sg_df$BBmsy_est <- icesRound(sg_df$BBmsy_est)
sg_df$BBmsy_upper <- icesRound(sg_df$BBmsy_upper)

sg_df$FFmsy_lower <- icesRound(sg_df$FFmsy_lower)
sg_df$FFmsy_est <- icesRound(sg_df$FFmsy_est)
sg_df$FFmsy_upper <- icesRound(sg_df$FFmsy_upper)

write.csv(sg_df, "output/summary_assessment_and_standard_graphs_rounded.csv")

# Intermediate year table

ffmsy_int <- get.par("logFFmsy", res$man[[1]], exp = T)[as.character(max.year-1.9375), 2]
bbmsy_int <- get.par("logBBmsy", res$man[[1]], exp = T)[as.character(max.year-1), 2]
Catch_int <- TAC2

intyr_tab <- data.frame(Variable = c("F/FMSY", "B/BMSY", "Catch"),
                        Value = c(ffmsy_int, bbmsy_int, Catch_int))

intyr_export <- xtable(intyr_tab, digits = 4, caption = "Interim Year Assumptions")

print(intyr_export, type = "html",
      file = "output/Interim_Assumptions.html",
      include.rownames = FALSE)

# catch options not rounded

prev.advice <- 1192 ## needs to be manually updated

sumspict.manage(res, include.abs = T)

Basis = c("FMSY_35", "FMSY", "FConstant", "F0", "Fsq")

Opt_Catch <- c(get.par("logCpred", res$man$ices, exp = T)[nrow(megC)+2, 2],
               get.par("logCpred", res$man$Fmsy, exp = T)[nrow(megC)+2, 2],
               get.par("logCpred", res$man$currentF, exp = T)[nrow(megC)+2, 2],
               get.par("logCpred", res$man$noF, exp = T)[nrow(megC)+2, 2],
               get.par("logCpred", res$man$`F=Fsq_ffac`, exp = T)[nrow(megC)+2, 2])

Opt_ffmsy <- c(get.par("logFFmsy", res$man$ices, exp = T)[as.character(max.year-0.9375), 2],
               get.par("logFFmsy", res$man$Fmsy, exp = T)[as.character(max.year-0.9375), 2],
               get.par("logFFmsy", res$man$currentF, exp = T)[as.character(max.year-0.9375), 2],
               get.par("logFFmsy", res$man$noF,  exp = T)[as.character(max.year-0.9375), 2],
               get.par("logFFmsy", res$man$`F=Fsq_ffac`,  exp = T)[as.character(max.year-0.9375), 2])

Opt_bbmsy <- c(get.par("logBBmsy", res$man$ices, exp = T)[as.character(max.year), 2],
               get.par("logBBmsy", res$man$Fmsy, exp = T)[as.character(max.year), 2],
               get.par("logBBmsy", res$man$currentF, exp = T)[as.character(max.year), 2],
               get.par("logBBmsy", res$man$noF, exp = T)[as.character(max.year), 2],
               get.par("logBBmsy", res$man$`F=Fsq_ffac`, exp = T)[as.character(max.year), 2])


# tail(get.par("logFFmsy", res$man[[3]], exp = T)  )


catch_opt <- data.frame (Basis = Basis,
                         Catch = round(Opt_Catch),
                         FFMSY = round(Opt_ffmsy, digits = 3),
                         BBMSY = round(Opt_bbmsy, digits = 3),
                         B_Change = round(((Opt_bbmsy/bbmsy_int)-1)*100, digits = 3),
                         Adv_change = round(((Opt_Catch/prev.advice)-1)*100, digits = 3))

write.csv(catch_opt, "output/catch_options_for_advice_sheet.csv")


catch_opt_export<- xtable(catch_opt, caption = "Catch Scenarios")

print(catch_opt_export, type = "html",
      file = "output/Catch_Scenarios.html",      ,
      include.rownames = FALSE)

#### Final saves #### 

### Save management scenario's
write.csv(sumspict.manage(res), 
          "output/Outputsmge_Options.csv") 
write.csv(sumspict.predictions(res, ndigits = 2), 
          "output/OutputsPredictions.csv")

# Saving on res object 
save(res, file = "output/res.RData")


