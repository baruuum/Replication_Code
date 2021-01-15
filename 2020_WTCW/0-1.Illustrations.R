###############################################################################
##                                                                           ##
##  Hypothetical Scenarioes, Graphics for Illustration                       ##
##                                                                           ##
###############################################################################


if (!exists('wd.base')) {
   # base directory
   stop('Specify "wd.base"!')
}

# set paths
if (!exists('code.path'))
   code.path <- paste0(wd.base, 'code/')
if (!exists('raw.data.path')) 
   raw.data.path <- paste0(wd.base, 'raw_data/')
if (!exists('data.path'))
   data.path <- paste0(wd.base, 'data/')
if (!exists('graphics.path')) 
   graphics.path <- paste0(wd.base, 'plots/')
if (!exists('stan.path'))
   stan.path <- paste0(wd.base, 'stan/')



# line widths
l.width <- 1.5
lab.dist <- .7
seg.wd <- 1
# old plotting parameters
old.par <- par()


# Symmetric Issue partisanship
pdf(paste0(graphics.path, 'Figure1A.pdf'), height=7, width=3)

par(mfrow=c(3,1))
par(mar=rep(.5,4))
par(oma=rep(2,4))

plot(c(0,1), c(0,1), type="n", axes=F)
box()
mtext('Issue Partisanship',font=2,side=3,outer=F, line=lab.dist)
abline(h=.5, lwd=l.width)
mtext('% Liberal', side=2, outer=F, line=lab.dist)

plot(c(0,1), c(0,1), type="n", axes=F)
box()
abline(.5, .3, lwd=l.width, lty=1)
abline(.5, -.3, lwd=l.width, lty=2, col='darkgray')
for (v in seq(0,1,.1)) {
   segments(v,.5 + -.3*v, v, .5+.3*v, col='gray80', lwd=seg.wd, lty=3)
}
mtext('% Liberal (by Party ID)', side=2, outer=F, line=lab.dist)

plot(c(0,1), c(-1,1), type="n", axes=F)
box()
abline(0, .6, lwd=l.width)
mtext('% Liberal (Dem - Rep)', side=2, outer=F, line=lab.dist)
mtext('Time', side=1, outer=F, line=lab.dist)

dev.off()


# Secular Trend (I)

pdf(paste0(graphics.path, 'Figure1B.pdf'), height=7, width=3)

par(mfrow=c(3,1))
par(mar=rep(.5,4))
par(oma=rep(2,4))

plot(c(-4,4), c(0,1), type="n", axes=F)
box()
mtext('Secular Trend', outer=F, side=3, font=2, line=lab.dist)
curve(pnorm(x), xlim=c(-4,4), add=T, lwd=l.width)

plot(c(-3,3), c(0,1), type="n", axes=F)
box()
curve(pnorm(x), xlim=c(-4,4), add=T, lwd=l.width)
curve(pnorm(x), add=T, lty=2, col='darkgray', lwd=l.width)

plot(c(0,1), c(-1,1), type="n", axes=F)
box()
abline(h=0, lwd=l.width)
mtext('Time', side=1, outer=F, line=lab.dist)

dev.off()


# Partisan Secular Trend
pdf(paste0(graphics.path, 'Figure1C.pdf'), height=7, width=3)

par(mfrow=c(3,1))
par(mar=rep(.5,4))
par(oma=rep(2,4))

plot(c(-4,4), c(0,1), type="n", axes=F)
box()
mtext('Partisan Secular Trend', font=2, side=3, outer=F, line=lab.dist)
curve(pnorm(x), xlim=c(-4,4), add=T, lwd=l.width)

plot(c(-4,4), c(0,1), type="n", axes=F)
box()
curve(pnorm(x+.75), xlim=c(-4,4), add=T, lwd=l.width)
curve(pnorm(x-.75), add=T, col='darkgray', lwd=l.width, lty=2)
for (v in seq(-4,4,.8)) {
   segments(v,pnorm(v-.75), v, pnorm(v+.75), col='gray80', lwd=seg.wd, lty=3)
}

plot(c(0,1), c(-1,1), type="n", axes=F)
box()
curve(3*x + -3*x^2, add=T, lwd=l.width)
# plot(c(-1,1), c(-1,1), type = 'n', axes =F)
# box()
# curve(exp(-x^2/.5) - exp(-1/.5), add = T, lwd=l.width)
mtext('Time', side=1, outer=F, line=lab.dist)

dev.off()

suppressWarnings(par(old.par))

#### END OF CODE ####