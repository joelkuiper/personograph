## Util
w.median <- function(x, w) {
    if (missing(w)) w <- rep(1,length(x))
    ok <- complete.cases(x, w)
    x <- x[ok]
    w <- w[ok]
    ind <- sort.list(x)
    x <- x[ind]
    w <- w[ind]
    ind1 <- min(which(cumsum(w) / sum(w) >= 0.5))
    ind2 <- if((w[1] / sum(w)) > 0.5) {
        1
    } else {
        max(which(cumsum(w) / sum(w) <= 0.5))
    }
    max(x[ind1], x[ind2])
}

approx.cer <- function(ev.ctrl, n.ctrl) {
    ## Calculate the CER (Control Event Rates)
    ## from the data, this is a weighted approximation of absolute
    ## risk with control (calculated; from 0 to 1)
    ## input:
    ##     ev.ctrl = a vector of event rates in the control group (/arm)
    ##     n.ctrl = a vector of sample sizes in the control group (/arm)
    ## output:
    ##     the approximated CER
    study_cer <- ev.ctrl / n.ctrl
    w.median(study_cer, n.ctrl)
}


calc.ier <- function(cer, point, units=sm) {
    ## IER (Intervention Event Rates)
    ## input:
    ##     cer = absolute risk with control (calculated; from 0 to 1)
    ##     point = relative risk with intervention (direct from meta-analysis)
    ##     units = RR/OR etc as string
    ## output:
    ##     absolute risk with intervention
    if (units == "RR") {
        return(cer * point)
    } else if(units == "OR") {
        return(cer * (point / (1 - (cer * (1 - point)))))
    } else {
        stop("Units need to be OR (Odds Ratios) or RR (Relative Risk)")
    }
}

uplift <- function(ier, cer, higher_is_better=NULL) {
    if(is.null(higher_is_better)) {
        higher_is_better <- T
        warning("Setting higher_is_better as outcome direction to TRUE")
    }

    ## The adopted terminology is similar to that of Uplift modelling
    ## https://en.wikipedia.org/wiki/Uplift_modelling
    if (higher_is_better == F) {
        ## Always orient the numbers so that higher events represents a good outcome
        ier <- 1 - ier
        cer <- 1 - cer
    }

    ## [healthy] people who are happy no matter what treatment
    healthy <- min(ier, cer)

    ## [lost] people who are sad no matter what treatment
    lost <- 1-max(ier, cer)

    ## [treated] people who would be saved by treatment
    treated <- max(ier-cer, 0)

    ## [harmed] people who would be harmed by treatment
    harmed <- max(cer-ier, 0)

    list(healtyh=healthy, treated=treated, harmed=harmed, lost=lost)
}

## Plotting code
library("grImport")
as.colors <- function(lst, palette=rainbow) {
    n <- names(lst)
    colors <- palette(length(n))
    sapply(n, function(name) { colors[[which(n == name)]]} ,simplify = FALSE, USE.NAMES = TRUE)
}

round.with.warn <- function(x, f=round, name=NULL) {
    rounded <- f(x)
    if(x > 0 && rounded == 0) {
        warning(paste("truncating",ifelse(is.null(name), "a", name), "non-zero value of", x, "to 0"))
    }
    rounded
}

personograph <- function(data,
                icon=NULL,
                n.icons=100,
                dimension=ceiling(sqrt(c(n.icons, n.icons))),
                colors=as.colors(data),
                ask=dev.interactive(orNone=TRUE), ...) {
    devAskNewPage(FALSE)
    plot.new()
    devAskNewPage(ask)

    if(is.null(icon)) {
        icon <- readPicture("~/Desktop/man214.ps.xml") # FIXME use inst
    }

    vp <- viewport(layout.pos.row=1, name="vp", width=unit(0.8, "npc"), height=unit(0.8, "npc"))
    pushViewport(vp)

    cols <- dimension[2]
    rows <- dimension[1]

    icon.height <- 1 / rows
    icon.width <- 1 / cols

    n <- names(data)
    counts <- sapply(n, function(name) {
        x <- data[[which(n == name)]]
        round.with.warn(x * n.icons, name=name)} ,simplify = FALSE, USE.NAMES = TRUE)


    ordered_names <- names(counts)

    if(is.null(colors)) {
        colors <- as.colors(data)
    }

    flat <- unlist(lapply(ordered_names, function(name) { rep(name, counts[[name]])}))

    row_height <- icon.height
    total <- 0
    grobs <- c(NULL)
    for (i in rows:1) {
        for (j in 1:cols) {
            total <- total + 1

            j_snake <- ifelse((i %% 2==1), j, cols - j + 1) # to group like icons together

            x <- (j_snake * icon.width) - icon.width + (j_snake * (icon.width / cols))
            y <- i * row_height
            if(total < length(flat) + 1) {
                type <- flat[[total]]
                grob <- pictureGrob(icon, x, y,
                                   gp=gpar(fill=colors[[type]], col=NA), use.gc=F,
                                   width=icon.width,
                                   height=icon.height)
                grobs[[total]] <- grob
            }
        }
    }

    grid.draw(do.call(gList, grobs))

    popViewport()

    dev.flush()
}


## Demo
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

cer <- approx.cer(data[["ev.ctrl"]], data[["n.ctrl"]])

## Calculate the OR or RR, we use meta package here
library(meta)
sm <- "RR"
m <- metabin(data[, "ev.trt"], data[, "n.trt"], data[, "ev.ctrl"], data[, "n.ctrl"], sm=sm)

# Random effects meta analysis result
point <- exp(m$TE.random) # meta returns outcomes on the log scale

ier <- calc.ier(cer, point)

d <- uplift(ier, cer, F)

personograph(d, colors=list(harmed="red", treated="green", lost="black", healtyh="gray"))
