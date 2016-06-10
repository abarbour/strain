library(strain)

redo <- FALSE
if (redo) rm(list=ls())
if (!exists('res') | !exists('res20') | redo){
    detach("package:strain", unload=TRUE)
    library(strain)
    res   <- hfbsm("B084",2009,215,"01:01:00", duration = 3600, sampling = 1)
    res20 <- hfbsm("B084",2009,215,"01:01:00", duration = 300, sampling = 20)
}

str(res)
dat <- load_hfbsm(res)
str(dat)

str(res20)
dat20 <- load_hfbsm(res20)
str(dat20)

plot(dat)
plot(dat20)
