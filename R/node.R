
# Code for "node" grobs

node <- function(label, x=.5, y=.5,
                 shape="plain",  
                 margin=unit(1, "mm"),
                 name=label, gp=NULL) {
    lab <- textGrob(label, x, y, gp=gp, name="label")
    if (shape == "circle") {
        box <- circleGrob(x, y, r=0.5*grobWidth(lab) + margin, name="box",
                         gp=gpar(fill="grey90"))
    } else if (shape == "box") {
        box <- rectGrob(x, y,
                        width=grobWidth(lab) + margin,
                        height=grobHeight(lab) + margin,
                        name="box", gp=gpar(fill="grey90"))        
    } else { # plain
        box <- rectGrob(x, y,
                        width=grobWidth(lab) + 2*margin,
                        height=grobHeight(lab) + 2*margin,
                        name="box", gp=gpar(col=NA, fill=NA))
    }
    gTree(children=gList(box, lab), name=name, cl="node")
}

grobX.node <- function(x, theta) {
    grobX(getGrob(x, "box"), theta)
}

grobY.node <- function(x, theta) {
    grobY(getGrob(x, "box"), theta)
}
