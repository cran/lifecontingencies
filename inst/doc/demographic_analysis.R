## -----------------------------------------------------------------------------
#| include: false
knitr::opts_chunk$set(
  warning = FALSE,
  message = FALSE,
  collapse = TRUE,
  comment = "#>"
)
library(lifecontingencies)


## ----build--------------------------------------------------------------------
data(soa08Act)
getOmega(soa08Act)          # last attainable age
soa08Act@lx[1:5]            # survivorship at the first ages


## ----printdf------------------------------------------------------------------
lt <- new("lifetable", x = 0:5, lx = c(1000, 990, 975, 940, 880, 790),
          name = "toy")
df <- print(lt)


## ----core---------------------------------------------------------------------
pxt(soa08Act, x = 65, t = 1)     # 1-year survival probability
qxt(soa08Act, x = 65, t = 5)     # 5-year death probability
dxt(soa08Act, x = 65, t = 10)    # deaths between 65 and 75
Lxt(soa08Act, x = 65, t = 1)     # person-years lived in [65, 66)
mxt(soa08Act, x = 65, t = 1)     # central mortality rate
Tx(soa08Act, x = 65)             # total years lived from age 65


## ----mx-----------------------------------------------------------------------
m <- mxt(soa08Act, 65, 1)
c(mx2qx(m), qxt(soa08Act, 65, 1))


## ----exn----------------------------------------------------------------------
curtate  <- exn(soa08Act, x = 0, type = "curtate")
complete <- exn(soa08Act, x = 0, type = "complete")
c(curtate = curtate, complete = complete, difference = complete - curtate)


## ----exntemp------------------------------------------------------------------
exn(soa08Act, x = 50, n = 20, type = "curtate")
exn(soa08Act, x = 50, n = 20, type = "complete")


## ----finan--------------------------------------------------------------------
t80 <- new("lifetable", x = 80:86,
           lx = c(250, 217, 161, 107, 62, 28, 0), name = "Finan 23.24")
exn(t80, 80)


## ----openinterval-------------------------------------------------------------
tail2 <- new("lifetable", x = c(99, 100), lx = c(3024, 2090), name = "NCHS tail")
mOmega <- 2090 / 4696                 # published central rate at the open age
c(default_closed = Tx(tail2, 100),
  open_interval  = Tx(tail2, 100, axOmega = 1 / mOmega),
  published_L100 = 4696)


## ----var----------------------------------------------------------------------
varxn(soa08Act, x = 65, type = "Kx")        # Var(K_65)
sdxn(soa08Act,  x = 65, type = "Kx")        # sd(K_65)
varxn(soa08Act, x = 65, type = "complete")  # Var(T_65) = Var(K_65) + 1/12


## ----varfinan-----------------------------------------------------------------
varxn(t80, 80, type = "Kx")


## ----vartemp------------------------------------------------------------------
varxn(soa08Act, x = 40, n = 20, type = "Kx")


## ----dist---------------------------------------------------------------------
median(soa08Act)                                   # median age at death
quantile(soa08Act, probs = c(0.1, 0.25, 0.75, 0.9))
modalAge(soa08Act, startAge = 10)                  # adult modal age at death
modalAge(soa08Act, startAge = 10, interpolate = TRUE)


## ----distcond-----------------------------------------------------------------
median(soa08Act, age = 65)
quantile(soa08Act, probs = c(0.25, 0.75), age = 65)


## ----demoivre-----------------------------------------------------------------
dm <- new("lifetable", x = 0:100, lx = 100 - (0:100), name = "de Moivre")
c(median = median(dm), q25 = unname(quantile(dm, 0.25)),
  q75 = unname(quantile(dm, 0.75)))


## ----printopts----------------------------------------------------------------
ltSoa <- new("lifetable", x = soa08Act@x, lx = soa08Act@lx, name = "SOA 2008")
head(print(ltSoa, exType = "complete"))

