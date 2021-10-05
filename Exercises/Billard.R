# Title     : Billard (Cue sport, pool)
# Objective : Metrics on billiard ball
# Created by: Pascal L
# Created on: 19-10-20

#Ref. https://en.wikipedia.org/wiki/Billiard_ball
#     https://en.wikipedia.org/wiki/Glossary_of_cue_sports_terms#cue_ball
#     plot (graphics) - ref. https://www.rdocumentation.org/packages/graphics/versions/3.6.2
library(plotrix)
library(R.utils)
library(stringi)

#####################
# American - 15 balls
billes_america <- 1:15
billes_america_colors <- c("yellow","blue","red","violet","orange","green","maroon","black",
                           "yellow","blue","red","violet","orange","green","maroon")
billes_america_pattern <- c("solid","solid","solid","solid","solid","solid","solid","solid",
                           "stripe","stripe","stripe","stripe","stripe","stripe","stripe")

str(billes_america)
str(billes_america_colors)
sum(billes_america)      # 120 points
summary(billes_america)  # median = mean = 8 = 120 / 15 !!!

billesA <- as.data.frame(billes_america)
billesA$color <- as.factor(billes_america_colors)
billesA$decor <- as.factor(billes_america_pattern)
summary(billesA); str(billesA);nrow(billesA)
billesA; billesA$billes_america; billesA$color;billesA$decor
# Mix/blend the balls randomly (http://www.cookbook-r.com/Numbers/Generating_random_numbers/)
r <- sample(1:15)
billesAR <- billesA[r,]
# Draw american balls
plot(0:5, seq(0,5,length=6), type="n", xlab="X", ylab="Y", main="Blackball - Pool 8")
x <- 1; y <- 0.5; n <- 5; j <- 0
for(i in 1:nrow(billesAR)) {
  j <- j + 1
  bille <- billesAR[i,]
  sprintf("%s\n", str(bille))
  d <- ifelse(stri_compare(bille$decor,"solid") == 0,NA,10)
  draw.circle(x-0.5, y,0.50, border=bille$color, col=bille$color, density=d, lty=1, lwd=3)
  # ref. density - https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/polygon
  c <- ifelse(stri_compare(bille$color,"black") == 0,0,1)
  text(x-0.5, y,0.50, labels=bille$billes_america, cex=4, col=c)
  x <- x + 1
  if (j == n) {
    x <- x - n + 0.5  # ifelse(j%%2, y+1, y+0.5);
    y <- y + 1;
    n <- n - 1;
    j <- 0;
  }
}
# + ref. https://sites.harding.edu/fmccown/r/


###########
# Blackpool
billes_blackpool <- c("yellow"=1,"red"=2,"yellow"=3,"red"=4,"red"=5,"red"=6,"yellow"=7,"red"=8,"yellow"=9,"yellow"=10,"black"=11,"red"=12,"red"=13,"yellow"=14,"yellow"=15)
billes_blackpool2 <- c("red"=1,"yellow"=2,"red"=3,"yellow"=4,"yellow"=5,"yellow"=6,"red"=7,"yellow"=8,"red"=9,"red"=10,"black"=11,"yellow"=12,"yellow"=13,"red"=14,"red"=15)
names(billes_blackpool)
str(billes_blackpool)
names(billes_blackpool2)
str(billes_blackpool2)

billes <- as.data.frame(billes_blackpool)
billes2 <- as.data.frame(billes_blackpool2)
billes$color <- as.factor(names(billes_blackpool))
billes2$color <- as.factor(names(billes_blackpool2))
dim(billes); dim(billes2)
str(billes); str(billes2)
summary(billes)
summary(billes2)

# Draw pool 8 balls
plot(0:5, seq(0,5,length=6), type="n", xlab="", ylab="", main="Blackball - Pool 8")
balls <- billes # billes vs. billes2
x <- 1; y <- 0.5; n <- 5; j <- 0
for(i in 1:dim(balls)[1]) {
  j <- j + 1
  draw.circle(x-0.5, y,0.50,border=balls[i,2], col=balls[i,2], lty=1, lwd=1)
  if (balls[i,2] == "black") {
    draw.circle(x-0.5, y,0.29,border=balls[i,2], col="white", lty=1, lwd=1)
    draw.circle(x-0.5, y+0.11,0.10,border=balls[i,2], col="white", lty=1, lwd=6)
    draw.circle(x-0.5, y-0.13,0.12,border=balls[i,2], col="white", lty=1, lwd=6)
  }
  # printf(c("%.1f) ", " x=%.1f", " y=%.1f", " n=%.1f \n"), c(j,x,y,n))
  x <- x + 1
  if (j == n) {
    #printf(c("* j=%.1f "," x=%.1f", " y=%.1f", " n=%.1f \n\n"), c(j,x,y,n))
    x <- x - n + 0.5  # ifelse(j%%2, y+1, y+0.5);
    y <- y + 1;
    n <- n - 1;
    j <- 0;
    # printf(c("** i=%.1f ", "j=%.1f "," x=%.1f", " y=%.1f", " n=%.1f \n\n"), c(i,j,x,y,n))
  }
}
# Ref. https://www.rdocumentation.org/packages/plotrix/versions/3.7-7/topics/draw.circle
# draw.circle(2,4,c(1,0.66,0.33),border="purple", col=c("#ff00ff","#ff77ff","#ffccff"),lty=1,lwd=1)


## Note that example(trees)  shows more sensible plots!
N <- nrow(trees)
with(trees, {
## Girth is diameter in inches
symbols(Height, Volume, circles = Girth/24, inches = FALSE,
        main = "Trees' Girth") # xlab and ylab automatically
## Colours too:
op <- palette(rainbow(N, end = 0.9))
symbols(Height, Volume, circles = Girth/16, inches = FALSE, bg = 1:N,
        fg = "gray30", main = "symbols(*, circles = Girth/16, bg = 1:N)")
palette(op)
})
#ref. https://cran.r-project.org/web/packages/pals/vignettes/pals_examples.html
#     http://www.sthda.com/french/wiki/couleurs-dans-r
