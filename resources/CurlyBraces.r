# function for drawing curly brackets
CurlyBraces <- function(x0, x1, y0, y1, pos = 1, direction = 1, depth = 1) {
    a=c(1,2,3,48,50)    # set flexion point for spline
    b=c(0,.2,.28,.7,.8) # set depth for spline flexion point
    curve = spline(a, b, n = 50, method = "natural")$y * depth
    curve = c(curve,rev(curve))
    if (pos == 1){
        a_sequence = seq(x0,x1,length=100)
        b_sequence = seq(y0,y1,length=100)
    }
    if (pos == 2){
        b_sequence = seq(x0,x1,length=100)
        a_sequence = seq(y0,y1,length=100)
    }
    # direction
    if(direction==1)
        a_sequence = a_sequence+curve
    if(direction==2)
        a_sequence = a_sequence-curve
    # pos
    if(pos==1)
        lines(a_sequence,b_sequence, lwd=1.5,   xpd=NA) # vertical
    if(pos==2)
        lines(b_sequence,a_sequence, lwd=1.5, xpd=NA) # horizontal
}
