## ----echo=FALSE---------------------------------------------------------------
knitr::opts_chunk$set(
  warning = FALSE,
  message = FALSE,
  collapse = TRUE,
  error = TRUE
)
library(lifecontingencies)

## ----mdt1---------------------------------------------------------------------
valdezDf <- data.frame(
  x = c(50:54),
  lx = c(4832555, 4821937, 4810206, 4797185, 4782737),
  heart = c(5168, 5363, 5618, 5929, 6277),
  accidents = c(1157, 1206, 1443, 1679, 2152),
  other = c(4293, 5162, 5960, 6840, 7631)
)
valdezMdt <- new("mdt", name = "ValdezExample", table = valdezDf)

## ----md3, eval=FALSE----------------------------------------------------------
# print(valdezMdt)

## ----md3b, eval = requireNamespace("markovchain", quietly = TRUE)-------------
valdezDf2 <- as(valdezMdt, "data.frame")
valdezMarkovChainList <- as(valdezMdt, "markovchainList")

## ----mdt4---------------------------------------------------------------------
getOmega(valdezMdt)
getDecrements(valdezMdt)
summary(valdezMdt)

## ----mdt.dx1------------------------------------------------------------------
dxt(valdezMdt, x = 51, decrement = "other")
dxt(valdezMdt, x = 51, t = 2, decrement = "other")
dxt(valdezMdt, x = 51)
pxt(valdezMdt, x = 50, t = 3)
qxt(valdezMdt, x = 53, t = 2, decrement = 1)

## ----mdt.randomSamples--------------------------------------------------------
rmdt(n = 2, object = valdezMdt, x = 50, t = 2)

## ----mdt.udd1-----------------------------------------------------------------
qxt.prime.fromMdt(object = valdezMdt, x = 53, decrement = "accidents")

## ----mdt.udd2-----------------------------------------------------------------
qxt.fromQxprime(qx.prime = 0.01, other.qx.prime = c(0.03, 0.06))

## ----asdt.matrix1-------------------------------------------------------------
qprime <- independentRatesFromMdt(valdezMdt, x = 50:54)
qprime

## ----asdt.matrix2-------------------------------------------------------------
qxt.prime.fromMdt(valdezMdt, x = 53, decrement = "accidents")
qprime["53", "accidents"]

## ----asdt.matrix3-------------------------------------------------------------
independentRatesFromMdt(valdezMdt, x = 51:52)

## ----asdt.build1--------------------------------------------------------------
qp <- matrix(c(0.010, 0.030, 0.100,
                0.013, 0.050, 0.200),
             nrow = 2, byrow = TRUE,
             dimnames = list(NULL, c("death", "disability", "retirement")))
qp

mdt674 <- buildMdtFromIndependentRates(
  x = 60:61, qx.primes = qp,
  radix = 1000, name = "Finan 67.4"
)
print(mdt674)

## ----asdt.build2--------------------------------------------------------------
ptau60 <- prod(1 - qp[1, ])
round(ptau60, 5)
tbl674 <- mdt674@table
round(tbl674$lx[tbl674$x == 61], 2)

## ----asdt.roundtrip-----------------------------------------------------------
qprime <- independentRatesFromMdt(valdezMdt, x = 50:54)
rebuilt <- buildMdtFromIndependentRates(
  x = 50:54, qx.primes = qprime,
  radix = valdezMdt@table$lx[valdezMdt@table$x == 50],
  name = "Roundtrip"
)

## Compare lx
data.frame(
  age = 50:54,
  original = valdezMdt@table$lx[valdezMdt@table$x %in% 50:54],
  rebuilt  = round(rebuilt@table$lx[rebuilt@table$x %in% 50:54], 0)
)

## ----plot.area, fig.width=7, fig.height=4-------------------------------------
plot(valdezMdt)

## ----plot.bar, fig.width=7, fig.height=4--------------------------------------
plot(valdezMdt, type = "bar")

## ----plot.prob, fig.width=7, fig.height=4-------------------------------------
plot(valdezMdt, type = "probability")

## ----plot.cdc, fig.width=7, fig.height=5, eval=FALSE--------------------------
# plot(cdcMdt, type = "probability")

## ----mdt.act1-----------------------------------------------------------------
myTable <- data.frame(
  x = c(16, 17, 18),
  lx = c(20000, 17600, 14520),
  da = c(1300, 1870, 2380),
  doc = c(1100, 1210, 1331)
)
myMdt <- new("mdt", table = myTable, name = "Sample")

## ----mdt.act2-----------------------------------------------------------------
20000 * Axn.mdt(object = myMdt, x = 16, n = 3, i = .1, decrement = "da")

## ----mdt.act3-----------------------------------------------------------------
A691 <- Axn.mdt(myMdt, x = 16, n = 3, i = 0.10, decrement = "doc")
a691 <- axn.mdt(myMdt, x = 16, n = 3, i = 0.10)
c(APV = 20000 * A691, annuity = a691, premium = 20000 * A691 / a691)

## ----mdt.act4-----------------------------------------------------------------
m681 <- new("mdt", table = data.frame(x = 50:51, lx = c(1200, 800),
                                      d1 = c(100, 200), d2 = c(300, 300)))
Axn.mdt(m681, x = 50, n = 2, i = 0.5, decrement = c("d1", "d2"),
        benefits = c(1, 2))

m692 <- new("mdt", table = data.frame(x = 41:43, lx = c(800, 776, 752),
                                      d1 = 8, d2 = 16))
i692 <- 1 / 0.95 - 1
Axn.mdt(m692, x = 42, n = 2, i = i692, decrement = c("d1", "d2"),
        benefits = c(2000, 1000)) - 34 * axn.mdt(m692, x = 42, n = 2, i = i692)

## ----deadifalco.1a------------------------------------------------------------
axnmdt.firsttype <- function(object, x, n, i, payment = "advance", delta = 0) {
  # delta is the annuity indexing
  out <- numeric(1)
  if (!(class(object) %in% c("lifetable", "actuarialtable", "mdt")))
    stop("Error! Only lifetable, actuarialtable or mdt classes are accepted")
  if (missing(object))
    stop("Error! Need a Multiple decrement table")
  if (missing(x))
    stop("Error! Need age!")
  if (x > getOmega(object)) {
    stop("Age greater than Omega")
  }
  if (class(object) == "mdt") {
    if (x < min(object@table$x)) {
      stop("Age lower than minimum age")
    }
  }
  if (class(object) == "actuarialtable") {
    if (x < min(object@x)) {
      stop("Age lower than minimum age")
    }
  }
  if (!(missing(i))) {
    interest <- i
  } else {
    if (class(object) == "actuarialtable") {
      interest <- object@interest
    } else {
      stop("Needed Interest Rate ")
    }
  }
  if (missing(n))
    n <- (getOmega(object) - x)
  if (n == 0) {
    stop("Contract duration equal to zero")
  }
  probs <- numeric(n)
  times <- seq(from = 0, to = n - 1, by = 1)
  if (payment == "arrears") times <- times + 1
  for (j in 1:length(times)) probs[j] <- pxt(object, x, times[j])
  out <- sum(apply(cbind(probs, ((1 + interest) / (1 + delta))^-times), 1, prod))
  return(out)
}

## ----deadifalco.1b------------------------------------------------------------
data("de_angelis_di_falco")
HealthyMaleTable2013 <- de_angelis_di_falco$HealthyMaleTable2013
DAT <- new("actuarialtable",
  x = de_angelis_di_falco$DisabledMaleLifeTable$age,
  lx = de_angelis_di_falco$DisabledMaleLifeTable$'2013',
  name = "DisabledTable", i = 0.03
)
axnmdt.firsttype(DAT, x = 65, n = 10, i = 0.03, payment = "arrears", delta = 0.02)
axnmdt.firsttype(DAT, 65, 10, payment = "arrears", delta = 0.02)
axnmdt.firsttype(DAT, 65, 10, payment = "arrears", i = 0.03, delta = 0.02)
# Last case equal to axn
axnmdt.firsttype(DAT, 65, 10, payment = "arrears", delta = 0)
axn(DAT, 65, 10, payment = "arrears")

## ----cdc.data-----------------------------------------------------------------
bandStart <- c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65,
               70, 75, 80, 85, 90, 95, 100)
cdc5yr <- data.frame(
  age_group = c("0-1", "1-5", "5-10", "10-15", "15-20", "20-25", "25-30",
    "30-35", "35-40", "40-45", "45-50", "50-55", "55-60", "60-65",
    "65-70", "70-75", "75-80", "80-85", "85-90", "90-95", "95-100",
    "100 and over"),
  lx = c(10000000.0, 9930523.0, 9917600.0, 9909689.0, 9899801.0, 9866403.0,
    9820252.0, 9775095.0, 9720086.0, 9642151.0, 9527362.0, 9360142.0,
    9123237.0, 8764226.0, 8233035.0, 7489085.0, 6464402.0, 5088466.0,
    3451511.0, 1849572.0, 687944.7, 147917.1),
  total_dx = c(69477.0, 12923.0, 7911.0, 9888.0, 33398.0, 46151.0, 45157.0,
    55009.0, 77935.0, 114789.0, 167220.0, 236905.0, 359011.0, 531191.0,
    743950.0, 1024683.0, 1375936.0, 1636955.0, 1601939.0, 1161627.3,
    540027.6, 147917.1),
  partition_sum = c(20749.0, 9384.9, 6159.0, 7847.6, 29571.0, 40140.9,
    38078.3, 45424.2, 63928.2, 94335.2, 138926.7, 202948.7, 315395.2,
    470524.7, 658241.3, 900367.8, 1191618.0, 1395849.7, 1344478.8,
    958568.3, 436904.4, 116411.8),
  other = c(48728.0, 3538.1, 1752.0, 2040.4, 3827.0, 6010.1, 7078.7,
    9584.8, 14006.8, 20453.8, 28293.3, 33956.3, 43615.8, 60666.3,
    85708.7, 124315.2, 184318.0, 241105.3, 257460.2, 203059.0, 103123.2,
    31505.3),
  septicemia = c(722.6, 247.7, 91.8, 76.5, 105.3, 194.7, 257.8, 418.8,
    671.8, 1120.7, 1847.0, 2783.5, 4280.8, 6452.9, 9611.9, 13677.9,
    19662.0, 23350.0, 23250.0, 15851.9, 6872.4, 1626.4),
  hiv = c(28.3, 43.0, 58.0, 62.8, 88.2, 427.6, 1680.0, 4167.0, 6114.4,
    6706.2, 5899.3, 4113.8, 2677.1, 1727.4, 1046.5, 539.9, 257.5, 81.0,
    25.5, 19.5, 10.6, 2.7),
  cancer = c(187.9, 1060.0, 1201.0, 1242.0, 1814.4, 2526.2, 3518.9,
    6114.5, 11915.0, 23559.7, 44358.6, 77442.7, 131212.1, 198969.2,
    267824.2, 332678.3, 368548.5, 334200.1, 233736.2, 113426.7, 35785.0,
    6122.5),
  diabetes = c(6.7, 16.9, 19.3, 58.8, 120.0, 277.8, 513.9, 940.3, 1524.6,
    2623.1, 4656.5, 8029.6, 13462.8, 20843.6, 29206.0, 37986.0, 46899.8,
    48482.2, 38608.7, 21835.1, 8005.9, 1545.5),
  alzheimer = c(0.0, 0.0, 0.0, 0.0, 0.0, 0.9, 0.8, 2.4, 2.8, 8.6, 42.5,
    126.0, 482.8, 1366.0, 3477.9, 9876.6, 25418.6, 49458.4, 63722.0,
    54057.0, 25433.7, 5666.6),
  heart_disease = c(1244.1, 496.3, 257.8, 402.5, 990.5, 1632.3, 2575.5,
    4841.8, 9546.1, 19013.9, 33655.2, 56478.6, 92602.3, 141365.9,
    202096.5, 288960.8, 415183.0, 537740.9, 577672.7, 457905.7, 227285.0,
    66431.5),
  hypertension = c(3.3, 4.2, 1.6, 3.2, 11.4, 33.8, 64.9, 138.3, 220.6,
    485.8, 838.2, 1359.1, 1972.7, 3237.0, 4635.8, 6594.0, 10279.8,
    14091.1, 15639.9, 12697.0, 6581.1, 1933.8),
  cerebrovascular = c(278.6, 123.9, 71.7, 106.2, 164.9, 324.5, 513.9,
    926.9, 1870.1, 3586.8, 5965.7, 8723.2, 13848.4, 22185.8, 34745.3,
    58994.0, 100084.4, 145206.2, 161264.4, 122436.7, 54220.7, 12630.6),
  influenza_pneumonia = c(755.1, 290.7, 112.0, 107.1, 166.6, 298.6, 337.8,
    506.5, 893.9, 1322.6, 1880.1, 2539.4, 3857.3, 6272.5, 9849.1,
    17726.2, 31608.2, 51735.7, 64665.0, 58841.1, 32133.0, 10956.1),
  copd = c(94.0, 124.7, 112.0, 195.6, 218.0, 262.2, 313.4, 429.9, 686.0,
    1299.8, 2657.7, 5557.3, 13792.5, 27507.5, 49137.5, 76948.9, 100455.3,
    105576.7, 81889.6, 41622.6, 14325.5, 2877.8),
  pneumonitis = c(31.6, 31.2, 13.7, 21.7, 25.3, 62.3, 72.4, 105.1, 131.2,
    243.3, 336.5, 528.8, 866.2, 1408.2, 2466.9, 4832.3, 8971.9, 14562.1,
    17864.6, 14771.2, 7068.0, 1928.4),
  liver_disease = c(9.1, 4.2, 1.6, 4.0, 18.0, 57.1, 198.0, 769.7, 2337.2,
    4790.8, 7989.0, 8922.6, 9780.0, 11092.9, 11344.1, 10895.8, 9658.5,
    6664.8, 3434.6, 1153.5, 225.4, 24.3),
  nephritis = c(383.3, 37.9, 26.6, 33.0, 62.9, 151.5, 218.2, 365.1, 599.5,
    977.3, 1607.2, 2564.0, 4302.1, 7118.9, 11608.2, 16664.5, 24123.1,
    29663.4, 29579.4, 21013.8, 9269.0, 2227.8),
  congenital = c(13912.0, 1349.1, 472.9, 495.9, 573.3, 586.8, 543.4,
    596.6, 578.2, 636.4, 732.9, 848.9, 1003.8, 1066.8, 970.3, 879.6,
    1001.5, 1063.5, 870.6, 504.0, 236.0, 56.6),
  accidents = c(2249.1, 4586.7, 3332.5, 3845.8, 16420.4, 19138.2,
    14938.4, 14067.2, 15926.3, 17254.8, 16556.9, 14316.4, 13565.0,
    13626.6, 14401.4, 17356.4, 23649.9, 29109.6, 29211.3, 21255.5,
    9191.2, 2357.0),
  suicide = c(0.0, 0.0, 12.9, 655.5, 3959.4, 6077.0, 6007.4, 6197.6,
    6794.9, 7200.1, 7130.0, 6521.5, 6005.7, 4995.4, 4726.3, 4904.1,
    5078.7, 4297.4, 2719.2, 1042.0, 214.9, 18.9),
  homicide = c(843.2, 968.4, 373.7, 537.2, 4832.4, 8089.4, 6323.6,
    4836.4, 4115.3, 3505.5, 2773.4, 2093.2, 1683.3, 1288.2, 1093.5,
    852.3, 737.2, 566.6, 325.3, 135.1, 46.9, 5.4)
)
## sanity check: the curated partition closes exactly to total mortality
stopifnot(all(abs(cdc5yr$partition_sum + cdc5yr$other - cdc5yr$total_dx) < 1e-6))

## ----cdc.graduate-------------------------------------------------------------
causeCols <- setdiff(names(cdc5yr),
                      c("age_group", "lx", "total_dx", "partition_sum", "other"))
causeCols <- c(causeCols, "other")   # 17 named causes + residual

## monotone Hermite (Fritsch-Carlson) graduation of lx, ages 0..100
lxFun <- splinefun(x = bandStart, y = cdc5yr$lx, method = "monoH.FC")
ages <- 0:100
lxSingle <- c(lxFun(ages), 0)          # append the closing lx=0 at age 101
dxTotalSingle <- -diff(lxSingle)       # single-year total decrements, ages 0..100
band <- findInterval(ages, bandStart)  # which 5-year band each single age falls in

cdcSingle <- data.frame(x = ages, lx = lxSingle[1:101])
for (cs in causeCols) {
  shareBand <- cdc5yr[[cs]] / cdc5yr$total_dx
  cdcSingle[[cs]] <- dxTotalSingle * shareBand[band]
}

## verify exact reaggregation back to the original 5-year cause totals
recheck <- aggregate(cdcSingle[causeCols],
                      by = list(age_group = cdc5yr$age_group[band]), FUN = sum)
recheck <- recheck[match(cdc5yr$age_group, recheck$age_group), ]
max(abs(as.matrix(recheck[causeCols]) - as.matrix(cdc5yr[causeCols])))

## ----cdc.build----------------------------------------------------------------
cdcMdt <- new("mdt",
              name = "NCHS US Life Table 1999-2001, cause-specific (Table 8)",
              table = cdcSingle)
getOmega(cdcMdt)
getDecrements(cdcMdt)
isTRUE(validObject(cdcMdt))

## ----cdc.compute--------------------------------------------------------------
qxt(cdcMdt, x = 70, t = 1)                          # total decrement rate
qxt(cdcMdt, x = 70, t = 1, decrement = "cancer")     # cancer only
qxt(cdcMdt, x = 70, t = 1, decrement = "heart_disease")
qxt(cdcMdt, x = 70, t = 1, decrement = "cancer") /
  qxt(cdcMdt, x = 70, t = 1)                          # cancer's share of q70

## ----long1--------------------------------------------------------------------
valdezLong <- mdtToLong(valdezMdt, x = 50, t = 5)
head(valdezLong)

## ----long2--------------------------------------------------------------------
if (requireNamespace("survival", quietly = TRUE)) {
  fit <- survival::survfit(survival::Surv(time, status) ~ 1,
                           data = valdezLong, weights = count)
  aj <- summary(fit, times = 1:5)$pstate[, -1]
  colnames(aj) <- getDecrements(valdezMdt)
  byTable <- sapply(getDecrements(valdezMdt), function(d)
    qxt(valdezMdt, x = 50, t = 1:5, decrement = d))
  max(abs(aj - byTable))
}

## ----long3--------------------------------------------------------------------
cdcLong <- mdtToLong(cdcMdt, x = 70)
tapply(cdcLong$count, cdcLong$status, sum)[c("cancer", "heart_disease")] /
  cdcMdt@table$lx[cdcMdt@table$x == 70]

