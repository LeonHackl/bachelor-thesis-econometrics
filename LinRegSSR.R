library(tikzDevice)

bowl <- function(x, y){x**2+y**2}

x <- y <- seq(-5, 5, length = 0.8)
z <- outer(x, y, bowl)

tikz(file = "SSR.tex", width = 10, height = 5)

par(bg = "white")
nrz <- nrow(z)
ncz <- ncol(z)
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
nbcol <- 100
color <- jet.colors(nbcol)
zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
facetcol <- cut(zfacet, nbcol)

plot <- persp(x, y, z, zlab = "", xlab = "", ylab = "", 
              theta = 30, phi = 20, shade = 0.15, expand = 0.6, col = color[facetcol], border = NA)

minimum <- trans3d(0, 0, 0.1, pmat = plot)
points(minimum, pch = 19, col = "red", cex = 1.5)

print(plot)
dev.off()

library(plot3D)

u <- function(x1,x2,A,b)
{
  n_row <- length(x1)
  n_col <- length(x2)
  X <- cbind(rep(x1,each=n_col),rep(x2,n_row))
  f <- function(x)
  {   
    x <- cbind(x)
    out <- 0.5*t(x)%*%A%*%x + t(x)%*%b
    return(out)
  }
  out <- cbind(X,apply(X,FUN=f,MAR=1))
}

x1 <- seq(-4,4,length.out=30)
x2 <- x1
A <- matrix(c(1,0,0,1),2,2)
b <- cbind(c(.5,.5))

-solve(A)%*%b
dt <- u(x1,x2,A,b)
x <- dt[,1]
y <- dt[,2]
z <- matrix(dt[,3],ncol=length(x2),nrow=length(x1))
persp(x1,x2, z = z, theta = 50, phi = 30, box = T, axes=TRUE, 
        nticks=2, ticktype="detailed", 
        scale = FALSE, expand = 0.3, contour = list(nlevels = 15, col = "black"),
        image = list(col = grey (seq(0.2, 0.8, length.out = 100))),shade=0.1,
        zlim = range(z)+c(-8,1), clim = range(z), plot = TRUE)

