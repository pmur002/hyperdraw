
#############
# All the nasty details about drawing

# 'xy' is a set of xy-locations for nodes

# 'rag' is an Ragraph object

edgeNodeTangent <- function(edgeNode, edgeNodeIncoming, edgeNodeOutgoing, xy) {
    # Calculate the average angle for all incoming and outgoing
    # edges
    # NOTE  that we do this by ...
    # - calculating all incoming and outgoing angles
    # - converting angles to x-y vectors (unit length)
    # - summing the x-y vectors
    # - then arctan of those averages
    nin <- length(edgeNodeIncoming)
    nout <- length(edgeNodeOutgoing)
    xin <- xy$x[edgeNode] - xy$x[edgeNodeIncoming]
    xout <- xy$x[edgeNodeOutgoing] - xy$x[edgeNode]
    yin <- xy$y[edgeNode] - xy$y[edgeNodeIncoming]
    yout <- xy$y[edgeNodeOutgoing] - xy$y[edgeNode]
    # Special case 1:  NO edges enter OR outgoing
    if (nin == 0 && nout == 0) {
        # No problem because no edges will be drawn
        # to or from this rnode
        NA 
    } else {
        # Special case 2:  NO edges incoming
        if (nin == 0) {
            angles <- atan2(yout, xout)
        # Special case 3:  NO edges outgoing
        } else if (nout == 0) {
            angles <- atan2(yin, xin)
        # Normal:  average in and out
        } else {    
            angles <- atan2(c(yin, yout), c(xin, xout))
        }
        dx <- cos(angles)
        dy <- sin(angles)
        atan2(sum(dy), sum(dx))
    }
}

# Draw a normal node
drawNode <- function(node, xy, rag) {
    margin <- eval(parse(text=nodeData(rag, node, "margin")))
    if (is.na(margin)) {
        margin <- unit(1, "mm")
    }
    shape <- nodeData(rag, node, "shape")
    if (is.na(shape)) {
        shape <- "plain"
    }
    cex <- nodeData(rag, node, "cex")
    if (is.na(cex)) {
        cex <- 1
    }
    col <- nodeData(rag, node, "color")
    if (is.na(col)) {
        col <- "black"
    }    
    grid.draw(node(node,
                   unit(xy$x[node], "native"),
                   unit(xy$y[node], "native"),
                   shape=shape,
                   margin=margin,
                   gp=gpar(cex=cex, col=col)))
}

drawEdgeNode <- function(edgeNode, xy, edgeNodeIO) {
    # If no incoming nodes, then draw a dot
    nin <- length(edgeNodeIO$incoming[[edgeNode]])
    if (nin == 0) {
        grid.circle(unit(xy$x[edgeNode], "native"),
                    unit(xy$y[edgeNode], "native"),
                    r=unit(.5, "mm"),
                    gp=gpar(fill="black"))
    }
}

drawLabel <- function(edgeNode, xy, edgeNodeIO, edgeNodeTangent, rag,
                      offset=unit(1, "mm")) {
    # If no incoming or outgoing nodes, then draw to the left
    nin <- length(edgeNodeIO$incoming[[edgeNode]])
    nout <- length(edgeNodeIO$outgoing[[edgeNode]])
    if (nin == 0 && nout == 0) {
        dir <- pi
    } else {
        # Determine location perpendicular to the
        # tangent that all edges pass through at this node
        dir <- edgeNodeTangent[[edgeNode]] + pi/2
        while (dir > 2*pi)
            dir <- dir - 2*pi
        while (dir < 0)
            dir <- dir + 2*pi
    }
    # Justify the text based on the direction
    hjust <- ifelse(dir > pi/2 && dir < 3*pi/2, 1, 0)
    vjust <- ifelse(dir > 0  && dir < pi, 0, 1)
    col <- nodeData(rag, edgeNode, "color")
    if (is.na(col)) {
        col <- "black"
    }    
    cex <- nodeData(rag, edgeNode, "cex")
    if (is.na(cex)) {
        cex <- 1
    }
    label <- nodeData(rag, edgeNode, "label")
    grid.text(label,
              unit(xy$x[edgeNode], "native") + cos(dir)*offset,
              unit(xy$y[edgeNode], "native") + sin(dir)*offset,
              hjust=hjust, vjust=vjust,
              gp=gpar(col=col, cex=cex))
}

# Given start and end points, calc intermediate control points
calcControlPoints <- function(sx, sy, ex, ey, dir, incoming) {
    dx <- ex - sx
    dy <- ey - sy
    # Angle of start -> end
    angle <- atan2(dy, dx)
    # Rotate end about start by -angle
    trans <- translateM(sx, sy) %*% rotateM(-angle) %*% translateM(-sx, -sy)
    newEnd <- transform(ex, ey, trans)
    # Add -angle to dir
    newDir <- dir - angle
    if (incoming) {
        snd <- sin(-newDir)
        cnd <- cos(-newDir)
    } else {
        snd <- sin(newDir)
        cnd <- cos(newDir)
    }
    # Distance between start and end
    D <- sqrt(dx^2 + dy^2)
    # Amount of that distance that we will use to position control points
    # (use more as the newDir points further away from start node)
    Dangle <- abs(newDir)
    while (Dangle > pi) Dangle <- pi*2 - Dangle
    Dfrac <- D/4 + (3*D/4)*tan(Dangle/4)
    # P2
    # If the tangent that this curve has to meet points "away" from the
    # start node, then make P2 at 90 degree angle to P3 about end node
    # (rather than mirroring P3 from start node)
    if (incoming && cnd < 0) {
        p2x <- sx - Dfrac*cnd
    } else {
        p2x <- sx + Dfrac*cnd
    }
    p2y <- sy + Dfrac*snd
    # P3
    if (!incoming && cnd < 0) { 
        p3x <- newEnd[1] + Dfrac*cnd
    } else {
        p3x <- newEnd[1] - Dfrac*cnd
    }
    p3y <- newEnd[2] + Dfrac*snd
    # Rotate P2 and P3 back
    invtrans <- solve(trans)
    p2Final <- transform(p2x, p2y, invtrans)
    p3Final <- transform(p3x, p3y, invtrans)
    c(p2Final[1:2], p3Final[1:2])
}

trimPts <- function(pts, start, end, fromStart) {
    node <- start
    ref <- 1
    step <- 1
    if (!fromStart) {
        node <- end
        step <- -1
        ref <- length(pts$x)
    }
    getAngle <- function(pts, ref) {
        dx <- convertX(pts$x[ref + step] - pts$x[ref], "inches",
                       valueOnly=TRUE)
        dy <- convertY(pts$y[ref + step] - pts$y[ref], "inches",
                       valueOnly=TRUE)
        # What is direction from start/end to point (start + i)/(end - i)?
        atan2(dy, dx)
    }        
    compareDist <- function(pts, node, ref) {
        dx <- convertX(pts$x[ref + step] - pts$x[ref], "inches",
                       valueOnly=TRUE)
        dy <- convertY(pts$y[ref + step] - pts$y[ref], "inches",
                       valueOnly=TRUE)
        angle <- getAngle(pts, ref)
        # What is the distance to the edge of the node in that direction?
        edx <- convertX(grobX(grid.get(node), angle/pi*180) - pts$x[ref],
                        "inches", valueOnly=TRUE)
        edy <- convertY(grobY(grid.get(node), angle/pi*180) - pts$y[ref],
                        "inches", valueOnly=TRUE)
        dist <- edx^2 + edy^2
        dist > (dx^2 + dy^2)
    }
    # Is that distance greater than the distance from start/end
    # to (start + i)/(end - i)?
    # FIXME: vectorize this!?
    while (compareDist(pts, node, ref)) {
        if (fromStart)
            step <- step + 1
        else
            step <- step - 1
    }
    newx <- grobX(grid.get(node), getAngle(pts, ref)/pi*180)
    newy <- grobY(grid.get(node), getAngle(pts, ref)/pi*180)
    if (fromStart) {
        list(x=unit.c(newx, pts$x[-(ref:(ref+step-1))]),
             y=unit.c(newy, pts$y[-(ref:(ref+step-1))]))
    } else {
        list(x=unit.c(pts$x[-(ref:(ref+step+1))], newx),
             y=unit.c(pts$y[-(ref:(ref+step+1))], newy))        
    }
}

drawCurve <- function(start, end, xy, dir, incoming, arr, rag) {
    # Calculate control points based on start and end node locations
    sx <- xy$x[start]
    sy <- xy$y[start]
    ex <- xy$x[end]
    ey <- xy$y[end]
    cp <- calcControlPoints(sx, sy, ex, ey, dir, incoming)
    # Calculate locations to draw lines through
    pts <- bezierPoints(unit(c(sx, cp[1], cp[3], ex), "native"),
                        unit(c(sy, cp[2], cp[4], ey), "native"))
    tpts <- trimPts(pts, start, end, incoming)
    lwd <- edgeData(rag, start, end, "lwd")
    if (is.na(lwd)) {
        lwd <- 1 # edgeDataDefaults(rag, "lwd")
    }
    col <- edgeData(rag, start, end, "color")
    if (is.na(col)) {
        col <- "black"
    }
    grid.lines(tpts$x, tpts$y, arrow=arr,
               gp=gpar(col=col, fill=col, lwd=lwd))
}

drawEdge <- function(start, end, xy, dir, incoming, arr, rabg) {
    # If using development version of R, make use of
    # new xsplinePoints() function
    if (as.numeric(R.version$major) >= 2 &&
        as.numeric(R.version$minor) >= 11)
        drawCurve(start, end, xy, dir, incoming, arr, rabg)
    else
        drawCurveOLD(start, end, xy, dir, incoming, arr, rabg)
}

drawHyperEdge <- function(edge, xy, edgeNodes, edgeNodeDir, rag, eps=5) {
    start <- gsub("(^.+)~.+$", "\\1", edge)
    end <- gsub("^.+~(.+)$", "\\1", edge)
    incoming <- end %in% edgeNodes
    arrLoc <- graphData(rag, "arrowLoc")
    if (is.na(arrLoc))
        arrLoc <- "middle"
    if (incoming) {
        dir <- edgeNodeDir[[end]]
        if (arrLoc == "middle") {
            # Arrows drawn at edgeNodes
            arr <- arrow(length=unit(1.5, "mm"), type="closed")
        } else {
            arr <- NULL
        }
    } else {
        dir <- edgeNodeDir[[start]]
        if (arrLoc == "end") {
            arr <- arrow(length=unit(1.5, "mm"), type="closed")
        } else {
            arr <- NULL
        }
    }
    drawEdge(start, end, xy, dir, incoming, arr, rag)
}

