
# This needs to disappear once R 2.11.0 is out

drawCurveOLD <- function(start, end, xy, dir, incoming, arr, rag) {
    # Calculate control points based on start and end node locations
    sx <- xy$x[start]
    sy <- xy$y[start]
    ex <- xy$x[end]
    ey <- xy$y[end]
    tempCP <- calcControlPoints(sx, sy, ex, ey, dir, incoming)
    # Use those to calculate "real" start/end (one of which will
    # be the *edge* of a node)
    if (incoming) {
        angle <- atan2(tempCP[2] - sy, tempCP[1] - sx)
        sx <- convertX(grobX(grid.get(start), angle/pi*180),
                       "native", valueOnly=TRUE)
        sy <- convertY(grobY(grid.get(start), angle/pi*180),
                       "native", valueOnly=TRUE)
    } else {
        angle <- atan2(tempCP[4] - ey, tempCP[3] - ex)
        ex <- convertX(grobX(grid.get(end), angle/pi*180),
                       "native", valueOnly=TRUE)
        ey <- convertY(grobY(grid.get(end), angle/pi*180),
                       "native", valueOnly=TRUE)
    }
    finalCP <- calcControlPoints(sx, sy, ex, ey, dir, incoming)
    lwd <- edgeData(rag, start, end, "lwd")
    if (is.na(lwd)) {
        lwd <- 1
    } 
    col <- edgeData(rag, start, end, "color")
    if (is.na(col)) {
        col <- "black"
    }
    grid.bezier(unit(c(sx, finalCP[1], finalCP[3], ex), "native"),
                unit(c(sy, finalCP[2], finalCP[4], ey), "native"),
                arrow=arr, gp=gpar(col=col, fill=col, lwd=lwd))
}

