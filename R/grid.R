
################
# This stuff should migrate to the 'grid' package

################
# Bezier curves

# X-Spline approx to Bezier
Ms <- 1/6*rbind(c(1, 4, 1, 0),
                c(-3, 0, 3, 0),
                c(3, -6, 3, 0),
                c(-1, 3, -3, 1))
Msinv <- solve(Ms)
# Bezier control matrix
Mb <- rbind(c(1, 0, 0, 0),
            c(-3, 3, 0, 0),
            c(3, -6, 3, 0),
            c(-1, 3, -3, 1))

splinePoints <- function(xb, yb) {
    xs <- Msinv %*% Mb %*% xb
    ys <- Msinv %*% Mb %*% yb
    list(x=xs, y=ys)
}

grid.bezier <- function(x, y, arrow=NULL, gp=NULL) {
    if (length(x) != 4 || length(y) != 4)
        stop("Must have exactly 4 control points")
    xx <- convertX(x, "inches", valueOnly=TRUE)
    yy <- convertY(y, "inches", valueOnly=TRUE)
    sp <- splinePoints(xx, yy)
    grid.xspline(sp$x, sp$y, default.units="inches",
                 shape=1, repEnds=FALSE,
                 arrow=arrow, gp=gp)
}

bezierPoints <- function(x, y) {
    xx <- convertX(x, "inches", valueOnly=TRUE)
    yy <- convertY(y, "inches", valueOnly=TRUE)
    sp <- splinePoints(xx, yy)
    grid:::xsplinePoints(xsplineGrob(sp$x, sp$y, default.units="inches",
                                     shape=1, repEnds=FALSE))
}


