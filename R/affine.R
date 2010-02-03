
###################
# Affine transformations
translateM <- function(tx, ty) {
    rbind(c(1, 0, tx),
          c(0, 1, ty),
          c(0, 0, 1))
}

translate <- function(x, y, tx, ty) {
    temp <- translateM(tx, ty) %*% cbind(c(x, y, 1))
    temp[1:2]/temp[3]
}

rotateM <- function(angle) {
    ca <- cos(angle)
    sa <- sin(angle)
    rbind(c(ca, -sa, 0),
          c(sa, ca, 0),
          c(0, 0, 1))
}

rotate <- function(x, y, angle) {
    temp <- rotateM(angle) %*% cbind(c(x, y, 1))
    temp[1:2]/temp[3]    
}

transform <- function(x, y, t) {
    t %*% cbind(c(x, y, 1))
}
