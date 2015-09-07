## Util
w.median <- function(x, w) {
    ## Lifted from cwhmisc, http://www.inside-r.org/packages/cran/cwhmisc/docs/w.median
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

#' Calculate the CER (Control Event Rates)
#'
#' Calculates the CER from the data, this is a weighted approximation of absolute
#' risk with control (from 0 to 1)
#'
#' @export
#' @param ev.ctrl Vector of event rates in the control group (/arm)
#' @param n.ctrl A vector of sample sizes in the control group (/arm)
#' @return Approximated Control Event Rates (CER)
w.approx.cer <- function(ev.ctrl, n.ctrl) {
    study_cer <- ev.ctrl / n.ctrl
    w.median(study_cer, n.ctrl)
}

#' Calculate the IER (Intervention Event Rates)
#'
#' @export
#' @seealso \code{\link{w.approx.cer}}
#' @param cer Absolute risk with control (calculated; from 0 to 1)
#' @param point Relative risk with intervention (direct from meta-analysis)
#' @param units The outcome measure, RR or OR as string
#' @return Absolute risk with intervention as Intervention Event Rates (IER)
calc.ier <- function(cer, point, units=sm) {
    if (units == "RR") {
        return(cer * point)
    } else if(units == "OR") {
        return(cer * (point / (1 - (cer * (1 - point)))))
    } else {
        stop("Units need to be OR (Odds Ratios) or RR (Relative Risk)")
    }
}

#' "Uplift" from IER and CER
#'
#' Calculates the percentage (from 0 to 1) of people helped, harmed, sad, and happy
#' from the Intervention Event Rates (IER) and Control Event Rates (CER).
#' Note that the result depends on the direction of the outcome measure,
#' e.g. higher_is_better = T (default) for treatment efficacy, higher_is_better = F for
#' adverse events.
#'
#' The adopted terminology is similar to that of Uplift modelling
#' https://en.wikipedia.org/wiki/Uplift_modelling
#'
#' @export
#' @param ier Intervention Event Rates
#' @param cer Control Event Rates
#' @param higher_is_better bool indicating the direction of the outcome measure, default TRUE
#' @return A list of S3 class \code{personograph.uplift} with the following elements:
#' \itemize{
#' \item{happy}{people who are happy no matter what treatment}
#' \item{sad}{people who are sad no matter what treatment}
#' \item{helped}{people who would be helped by treatment}
#' \item{harmed}{people who would be harmed by treatment}
#' }
#' @examples
#' ier <- 0.06368133
#' cer <- 0.1115242
#' uplift(ier, cer, T)
uplift <- function(ier, cer, higher_is_better=NULL) {
    if(is.null(higher_is_better)) {
        higher_is_better <- T
        warning("Setting higher_is_better as outcome direction to TRUE")
    }
    if (higher_is_better == F) {
        ## Always orient the numbers so that higher events represents a happy outcome
        ier <- 1 - ier
        cer <- 1 - cer
    }

    ## [happy] people who are happy no matter what treatment
    happy <- min(ier, cer)

    ## [sad] people who are sad no matter what treatment
    sad <- 1-max(ier, cer)

    ## [helped] people who would be saved by treatment
    helped <- max(ier-cer, 0)

    ## [harmed] people who would be harmed by treatment
    harmed <- max(cer-ier, 0)

    result <- list(happy=happy, helped=helped, harmed=harmed, sad=sad)
    class(result) <- "personograph.uplift"
    result
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
        warning(paste("truncating", ifelse(is.null(name), "a", name), "non-zero value of", x, "to 0"))
    }
    rounded
}

#' Plots a personograph
#'
#' Plots a personogram from a list with with percentages.
#' A personograph is a graphical represenation of relative benefit or harm, using a grid of icons with different colors.
#' Its intended use is similar to that of Cates Plots (Visual Rx, Number Needed to Treat visualization).
#' Although these could be seen as Marshall-Kuiper plots.
#'
#' @export
#' @param data A list of names to percentages (from 0 to 1)
#' @param icon A \code{grImport} \code{Picture} for the icon used
#' @param n.icons Number of icons to draw, defaults to 100
#' @param dimension A vector of c(rows, columns) for the dimensions of the grid
#' @param dimension A vector of names to colors, must match the names in data. Uses the "rainbow" style if none supplied
#' @param ask If TRUE, a prompt will be displayed before generating the next page of a multi-page plot.
#' @return None.
#' @examples
#' data <- list(happy= 0.8884758, helped = 0.04784283, harmed = 0, sad = 0.06368133)
#' personograph(data)
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
        icon <- readPicture(system.file("icon.ps.xml", package="personograph"))
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

#' @export
#' @seealso \code{\link{personograph}}
plot.personograph.uplift <- function(x, ...) {
    personograph(x, colors=list(harmed="firebrick3", helped="olivedrab3", sad="azure4", happy="azure3"), ...)
}
