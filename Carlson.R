library(dplyr)
library(metafor)

dat = read.csv("tabula-Carlson et al (1990).csv")
dat = dat %>% 
  filter(!is.na(n1))

# mean, median cell size
summary(c(dat$n1, dat$n2))

# make SE cohen's d
term1 = ((dat$n1 + dat$n2)/(dat$n1*dat$n2)) + (dat$Effect.size^2)/(2*(dat$n1 + dat$n2 - 2))
term2 = (dat$n1 + dat$n2)/(dat$n1 + dat$n2 - 2)
dat$d.se = sqrt(term1*term2)

# funnel plot and meta-analysis?
m1 = rma(yi = Effect.size, sei = d.se, data = dat)
m1
m1.fe = rma(yi = Effect.size, sei = d.se, data = dat, method = "FE")
m1.fe

funnel(m1.fe, 
       level = c(90, 95, 99), shade = c("white", "grey75", "grey60"), refline = 0)
