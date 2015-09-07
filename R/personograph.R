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
#' @param sm The outcome measure, RR or OR as string
#' @return Absolute risk with intervention as Intervention Event Rates (IER)
calc.ier <- function(cer, point, sm) {
    if (sm == "RR") {
        return(cer * point)
    } else if(sm == "OR") {
        return(cer * (point / (1 - (cer * (1 - point)))))
    } else {
        stop("Sm need to be OR (Odds Ratios) or RR (Relative Risk)")
    }
}

#' "Uplift" from IER and CER
#'
#' Calculates the percentage (from 0 to 1) of people helped, harmed, bad, and good
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
#' @param higher_is_better logical indicating the direction of the outcome measure, default TRUE
#' @return A list of S3 class \code{personograph.uplift} with the following elements:
#' \itemize{
#' \item{good}{people who are good no matter what treatment}
#' \item{bad}{people who are bad no matter what treatment}
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
        ## Always orient the numbers so that higher events represents a good outcome
        ier <- 1 - ier
        cer <- 1 - cer
    }

    ## [good] people who are good no matter what treatment
    good <- min(ier, cer)

    ## [bad] people who are bad no matter what treatment
    bad <- 1-max(ier, cer)

    ## [helped] people who would be saved by treatment
    helped <- max(ier-cer, 0)

    ## [harmed] people who would be harmed by treatment
    harmed <- max(cer-ier, 0)

    result <- list(good=good, helped=helped, harmed=harmed, bad=bad)
    class(result) <- "personograph.uplift"
    result
}

## Plotting code
as.colors <- function(lst, palette=gray.colors) {
    n <- names(lst)
    colors <- palette(length(n))
    sapply(n, function(name) { colors[[which(n == name)]]}, simplify = FALSE, USE.NAMES = TRUE)
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
#' Plots a personograph from a list with with percentages.
#' A personograph is a graphical represenation of relative benefit or harm, using a grid of icons with different colors.
#' Its intended use is similar to that of Cates Plots (Visual Rx, Number Needed to Treat visualization).
#' Although these could be seen as Kuiper-Marshall plots.
#'
#' @export
#' @param data A list of names to percentages (from 0 to 1)
#' @param icon.style A numeric from 1-11 indicating which of the included icons to use
#' @param icon A \code{grImport} \code{Picture} for the icon, overwrites \code{icon.style}
#' @param n.icons Number of icons to draw, defaults to 100
#' @param plot.width The percentage of width that the main plotting area should take (with respect to the frame)
#' @param dimension A vector of c(rows, columns) for the dimensions of the grid
#' @param colors A vector of names to colors, must match the names in data. Uses the "gray.colors" style if none supplied
#' @param ask If TRUE, a prompt will be displayed before generating the next page of a multi-page plot.
#' @param fig.cap Figure caption
#' @param fig.title Figure title
#' @param draw.legend Logical indicating whether to draw the legend
#' @return None.
#' @examples
#' data <- list(good= 0.8884758, helped = 0.04784283, harmed = 0, bad = 0.06368133)
#' personograph(data)
personograph <- function(data,
                 fig.title=NULL,
                 fig.cap=NULL,
                 draw.legend=T,
                 icon=NULL,
                 icon.style=1,
                 n.icons=100,
                 plot.width=0.6,
                 dimension=ceiling(sqrt(c(n.icons, n.icons))),
                 colors=as.colors(data),
                 ask=dev.interactive(orNone=TRUE), ...) {
    devAskNewPage(FALSE)
    plot.new()
    devAskNewPage(ask)

    fontfamily <- c("Open Sans", "Helvatica", "Arial")

    if(is.null(icon)) {
        icon <- readPicture(system.file(paste0(icon.style, ".ps.xml"), package="personograph"))
    }

    master.rows <- sum(!is.null(fig.title), !is.null(draw.legend), !is.null(fig.cap))
    master.heights <- c(0.2,
                       0.9 - (master.rows * 0.1),
                       ifelse(draw.legend, .1, 0),
                       ifelse(!is.null(fig.cap), .1, 0))

    masterLayout <- grid.layout(
        nrow    = 4,
        ncol    = 1,
        heights = unit(master.heights, rep("null", 4)))

    vp1 <- viewport(layout.pos.row=1, name="title")
    vp2 <- viewport(layout.pos.row=2, name="plot")
    vp3 <- viewport(layout.pos.row=3, name="legend")
    vp4 <- viewport(layout.pos.row=4, name="caption")

    pushViewport(vpTree(viewport(layout = masterLayout, name = "master"), vpList(vp1, vp2, vp3, vp4)))

    if(!is.null(fig.title)) {
        seekViewport("title")
        grid.text(fig.title,
                  gp = gpar(fontsize = 18, fontfamily=fontfamily, fontface="bold"))
        popViewport()
    }

    seekViewport("plot")
    pushViewport(viewport(width=unit(plot.width, "npc")))

    cols <- dimension[2]
    rows <- dimension[1]

    icon.height <- 1 / rows
    icon.width <- 1 / cols

    n <- names(data)
    counts <- sapply(n, function(name) {
        x <- data[[which(n == name)]]
        round.with.warn(x * n.icons, name=name)}, simplify = FALSE, USE.NAMES = TRUE)


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

            j_snake <- ifelse((i %% 2 == 1), j, cols - j + 1) # to group like icons together

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
    plotGrobs <- do.call(grobTree, grobs)
    grid.draw(plotGrobs)
    popViewport(2)

    font <- gpar(fontsize=10, fontfamily, col="azure4")

    if(draw.legend) {
        seekViewport("legend")
        legend.cols <- length(n)
        pushViewport(viewport(
            width  = unit(0.8, "npc"),
            x      = unit(0.55, "npc"),
            layout = grid.layout(ncol=legend.cols * 2,
                                 nrow=1,
                                 heights=unit(0.25, "npc"))))

        idx <- 0

        for(name in n)  {
            idx <- idx + 1
            pushViewport(viewport(layout.pos.row=1, layout.pos.col=idx))
            grid.circle(x=0, r=0.35, gp=gpar(fill=colors[[name]], col=NA))
            popViewport()
            idx <- idx + 1
            pushViewport(viewport(layout.pos.row=1, layout.pos.col=idx))
            grid.text(x=unit(-0.8, "npc"), paste(name, "=", formatC(data[[name]], digits=2)), gp=font, just="left")
            popViewport()
        }

        popViewport(2)
    }

    if(!is.null(fig.cap)) {
        seekViewport("caption")
        grid.text(fig.cap, gp = font)
        popViewport()
    }

    popViewport()
    dev.flush()
}

#' @export
#' @seealso \code{\link{personograph}}
plot.personograph.uplift <- function(x, ...) {
    personograph(x, colors=list(harmed="firebrick3", helped="olivedrab3", bad="azure4", good="azure3"), ...)
}
