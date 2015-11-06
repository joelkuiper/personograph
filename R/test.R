library(grImport)
data <- read.table(textConnection('
          name ev.trt n.trt ev.ctrl n.ctrl
1     Auckland     36   532      60    538
2        Block      1    69       5     61
3        Doran      4    81      11     63
4        Gamsu     14   131      20    137
5     Morrison      3    67       7     59
6 Papageorgiou      1    71       7     75
7      Tauesch      8    56      10     71
'
), header=TRUE)

cer <- w.approx.cer(data[["ev.ctrl"]], data[["n.ctrl"]])

sm <- "RR"
if (requireNamespace("meta", quietly = TRUE)) {
    ## Calculate the pooled OR or RR point estimate, we use meta package here
    m <- with(data,
             meta::metabin(ev.trt, n.trt, ev.ctrl, n.ctrl, sm=sm))
    point <- exp(m$TE.random) # meta returns outcomes on the log scale
} else {
    ## Calculated Random Effects RR, using the meta package
    point <- 0.5710092
}

ier <- calc.ier(cer, point, sm)

u <- uplift(ier, cer, F)
plot(u, fig.title="Example", fig.cap="Example from rMeta")


## Test with zero CER
data <- read.table(textConnection('
         name   ai n1i  ci  n2i days
1      Zarate    6  15   0  15     1
2    Murrough   30  47   0  25     1
3      Zarate   13  18   0  18     1
4     Lapidus    8  18   0  18     1
5 Diazgranado   10  18   0  18     1
'), header=TRUE)

cer <- w.approx.cer(data$ci, data$n2i)

## Draw documentation graphics
## u <- uplift(ier, cer, F)
## pdf("man/figures/green.pdf", 8, 10)
## plot(u, fig.title="Example", fig.cap="Example from rMeta")
## dev.off()
## png("man/figures/green.png", 800, 1000)
## plot(u, fig.title="Example", fig.cap="Example from rMeta")
## dev.off()

## u <- uplift(ier, cer, T)
## pdf("man/figures/red.pdf", 8, 10)
## plot(u, fig.title="Example", fig.cap="Example from rMeta")
## dev.off()
## png("man/figures/red.png", 800, 1000)
## plot(u, fig.title="Example", fig.cap="Example from rMeta")
## dev.off()

library(jsonlite)

json <- '{"data":[{"slab":"Thompson PL (2004)","ai":42,"n1i":1710,"ci":37,"n2i":1698,"days":28},{"slab":"Colhoun HM (2004)","ai":7,"n1i":1428,"ci":9,"n2i":1410,"days":1715.5},{"slab":"Nakamura Y (1996)","ai":0,"n1i":62,"ci":2,"n2i":62,"days":91},{"slab":"_PMSGCRP (1993)","ai":0,"n1i":530,"ci":5,"n2i":532,"days":182},{"slab":"Sever PS (2003)","ai":21,"n1i":5168,"ci":24,"n2i":5137,"days":1204.5},{"slab":"Riegger G (1999)","ai":1,"n1i":187,"ci":5,"n2i":178,"days":365},{"slab":"Schwartz GG (2001)","ai":91,"n1i":1538,"ci":106,"n2i":1548,"days":112},{"slab":"Sacks FM (1996)","ai":317,"n1i":2081,"ci":359,"n2i":2078,"days":2263},{"slab":"Herd JA (1998)","ai":8,"n1i":157,"ci":12,"n2i":164,"days":912.5},{"slab":"Durazzo AE (2004)","ai":0,"n1i":50,"ci":1,"n2i":50,"days":182},{"slab":"Downs JR (1998)","ai":60,"n1i":3304,"ci":87,"n2i":3301,"days":2628},{"slab":"Kjekshus J (2007)","ai":65,"n1i":2514,"ci":71,"n2i":2497,"days":985.5}]}'

data <- fromJSON(json)$data

cer <- w.approx.cer(data[["ci"]], data[["n2i"]])

sm <- "RR"
## Calculate the pooled OR or RR point estimate, we use meta package here
m <- with(data,
         meta::metabin(ai, n1i, ci, n2i, sm=sm))
point <- exp(m$TE.random) # meta returns outcomes on the log scale

ier <- calc.ier(cer, point, sm)

u <- uplift(ier, cer, F)
plot(u, fig.title="Example", fig.cap="Example from rMeta", n.icons=1000, dim=c(20,50))
