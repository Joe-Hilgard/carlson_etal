library(dplyr)
library(metafor)

dat = read.csv("tabula-Carlson et al (1990).csv")
dat = dat %>% 
  filter(!is.na(n1))

# make SE cohen's d
term1 = ((dat$n1 + dat$n2)/(dat$n1*dat$n2)) + (dat$Effect.size^2)/(2*(dat$n1 + dat$n2 - 2))
term2 = (dat$n1 + dat$n2)/(dat$n1 + dat$n2 - 2)
dat$d.se = sqrt(term1*term2)

# Make weapons cue subset
dat.wep = dat %>% 
  filter(Weapon.cue == "Yes")

# mean, median cell size
summary(c(dat$n1, dat$n2))
summary(c(dat.wep$n1, dat.wep$n2))



# funnel plot and meta-analysis ----
# Overall cue effects
m1 = rma(yi = Effect.size, sei = d.se, data = dat)
m1
m1.fe = rma(yi = Effect.size, sei = d.se, data = dat, method = "FE")
m1.fe

funnel(m1.fe, 
       level = c(90, 95, 99), shade = c("white", "grey75", "grey60"), refline = 0)
funnel(m1.fe, main = "All cue effects \n Note considerable heterogeneity")

# Exposure to weapons
m2 = rma(yi = Effect.size, sei = d.se, data = dat.wep)
m2
m2.fe = rma(yi = Effect.size, sei = d.se, data = dat.wep, method = "FE")
m2.fe

funnel(m2.fe, 
       level = c(90, 95, 99), shade = c("white", "grey75", "grey60"), refline = 0)
funnel(m2.fe, main = "All weapon-exposure effects \n Note null results & heterogeneity")

# Exposure to weapons, low sophisitcation AND low apprehension
m3 = rma(yi = Effect.size, sei = d.se, data = dat.wep, subset = low.sophis.and.apprehension == "Yes")
m3
m3.fe = rma(yi = Effect.size, sei = d.se, data = dat.wep, subset = low.sophis.and.apprehension == "Yes",
            method = "FE")
m3.fe

# High sophistication OR high apprehension
m4 = rma(yi = Effect.size, sei = d.se, data = dat.wep, subset = high.sophis.or.apprehension == "Yes")
m4
m4.fe = rma(yi = Effect.size, sei = d.se, data = dat.wep, subset = high.sophis.or.apprehension == "Yes",
            method = "FE")
m4.fe

funnel(m3.fe,
       level = c(90, 95, 99), shade = c("white", "grey75", "grey60"), refline = 0)
funnel(m3.fe, main = "Low awareness subgroup \n No sig. effect \n Lots of heterogeneity")
funnel(m4.fe,
       level = c(90, 95, 99), shade = c("white", "grey75", "grey60"), refline = 0)
funnel(m4.fe, main = "High awareness subgroup \n No sig. effect \n Heterogeneity & tiny N")
