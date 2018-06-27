
################################################
################################################
####                                        ####
####            HOW ARE WE APART            ####
####                                        ####
####          HYPOTHETICAL GRAPHS           ####
####                                        ####
####                                        ####
#### Last Update: 08/09/2016                ####
####                                        ####
################################################
################################################


rm(list=ls())
#set wd
wd <- "Working Directory Here"

# wd for graphs
g.wd <- paste0("Path to directory where figures should be placed")
dir.create(g.wd)
setwd(g.wd)


### Graph for Hypothetical Example (Polarization vs. Sorting) ###

pdf('polsort.pdf', family='CM Roman', width=12, height=8)
line.w <- 3
line.t1 <- 2
line.c1 <- 'black'
line.t2 <- 2
line.c2 <- 'grey60'

m.line.w <- 2
m.line.t <- 1
m.line.c <- 'grey90'
n <- 1000
x<-seq(-5,5,length.out=n)
y1 <- dnorm(x,0,1)

par(mfrow=c(2,2), mar=c(2,1,3,1), oma=c(0,0,0,0)+.5)
plot(x, y1+.02, type='l', xlab='', ylab='', xaxt='n', yaxt='n', xlim=c(-5,5), ylim=c(0,.5), lwd=m.line.w, col=m.line.c, lty=m.line.t, main='I. No Polarization, No Sorting', frame.plot=F, cex.main=2)
polygon(x, y1+.02, col=m.line.c, border=m.line.c)
points((rnorm(n/2,0,1)), rep(.01,n/2), col=line.c1, pch='|', cex=.6, font=2) 
points((rnorm(n/2,0,1)),rep(-.005,n/2), col=line.c2, pch='|', cex=.6, font=2) 
lines((x2<-sort(runif(n,-5,5))), dnorm(x2,0,1)/2.02+.02, col=line.c2, lty=1, lwd=line.w)
lines((x1<-sort(runif(n,-5,5))), dnorm(x1,0,1)/2.02+.02, col=line.c1, lty=line.t1, lwd=line.w)
abline(h=.02, lty=1, col='black', lwd=3)

plot(x, y1+.02, type='l', xlab='', ylab='', xaxt='n', yaxt='n', xlim=c(-5,5), ylim=c(0,.5), main='II. No Polarization, Sorting', frame.plot=F, cex.main=2, lwd=m.line.w, col=m.line.c, lty=m.line.t)
polygon(x, y1+.02, col=m.line.c, border=m.line.c)
lines(x1<- -abs(sort(rnorm(n,0,1))), dnorm(x1,0,1)/1 + .019, lty=line.t1, lwd=line.w, col=line.c1)
segments(0.-.02,.019,-.02,dnorm(0.02,0,1)/1+.019, lty=line.t1, lwd=line.w, col=line.c1)
segments(0.02,.019,0.02,dnorm(0.02,0,1)/1+.019, lty=line.t1, lwd=line.w, col=line.c2)

points(-abs(rnorm(n/2,0,1)), rep(.01,n/2), pch='|', cex=.6, font=2, col=line.c1)
points(abs(rnorm(n/2,0,1)), rep(-.005,n/2), col=line.c2,pch='|', cex=.6, font=2)
lines(x2<- abs(sort(rnorm(n,0,1))), dnorm(x2,0,1)/1 + .019, col=line.c2, lty=line.t2, lwd=line.w)
abline(h=.02, lty=1, col='black', lwd=3)

y2 <- .5*dnorm(x,-2,1)+.5*dnorm(x,2,1)

plot(x, y2+.02, type='l', xlab='', ylab='', xaxt='n', yaxt='n', xlim=c(-5,5), ylim=c(0,.3), main='III. Polarization, No Sorting', frame.plot=F, cex.main=2, lwd=m.line.w, col=m.line.c, lty=m.line.t)
polygon(x, y2+.02, col=m.line.c, border=m.line.c)
points((rnorm(n/2,-2,1)), rep(0.005,n/2), col=line.c2, pch='|', cex=.6, font=2) 
points((rnorm(n/2,2,1)), rep(0.005,n/2), col=line.c2, pch='|', cex=.6, font=2) 
lines(x2<-sort(runif(n,-5,5)), (.5*dnorm(x2,-2,1)+.5*dnorm(x2,2,1))/2 + .02, lty=1, col=line.c2, lwd=line.w)
points((rnorm(n/2,-2,1)), rep(.014,n/2), pch='|', cex=.6, font=2, col=line.c1) 
points((rnorm(n/2,2,1)), rep(.014,n/2), pch='|', cex=.6, font=2, col=line.c1) 
lines(x1<-sort(runif(n,-5,5)), (.5*dnorm(x1,-2,1)+.5*dnorm(x1,2,1))/2 + .02, lty=line.t1, lwd=line.w, col=line.c1)
abline(h=.02, lty=1, col='black', lwd=3)

plot(x, y2+.02, type='l', xlab='', ylab='', xaxt='n', yaxt='n', xlim=c(-5,5), ylim=c(0,.3), main='IV. Polarization & Sorting', frame.plot=F, cex.main=2, lwd=m.line.w, col=m.line.c, lty=m.line.t)
polygon(x, y2+.02, col=m.line.c, border=m.line.c)
points((rnorm(n,-2,1)), rep(.014,n), pch='|', cex=.6, col=line.c1) 
points((rnorm(n,2,1)), rep(.005,n), col=line.c2, pch='|', cex=.6) 
lines(x1 <- sort(seq(-5,2,.1)), dnorm(x1, -2,1)/2 + .02, lty=line.t1, lwd=line.w,col=line.c1)
lines(x2 <- sort(seq(-2,5,.1)), dnorm(x2,2,1)/2 + .02, col=line.c2, lty=line.t2, lwd=line.w)
abline(h=.02, lty=1, col='black', lwd=3)

dev.off()


### Function for Sampling from Bivariate Normal Mixtures ###

# Code modified from Ruscio & Kaczetow (2008)

GenData <- function(Supplied.Data, N.Factors = 0, Max.Trials = 5, Initial.Multiplier = 1, seed = 0, nn=1000,kk=4, rho=.5, dist=NULL)
{
    # Initialize variables and (if applicable) set random number seed (step 1) -------------------------------------
    N <- nn
    k <- kk
    Data <- matrix(0, nrow = N, ncol = k) # Matrix to store the simulated data
    Distributions <- matrix(0, nrow = N, ncol = k) # Matrix to store each variable's score distribution
    Iteration <- 0 # Iteration counter
    Best.RMSR <- 1 # Lowest RMSR correlation
    Trials.Without.Improvement <- 0 # Trial counter
    if (seed != 0) set.seed(seed) # If user specified a nonzero seed, set it
    # Generate distribution for each variable (step 2) -------------------------------------------------------------
    if (length(dist) != 0) {
        if (dist[1]=='rnorm') {
            Distributions[,1] <- sort(rnorm(n=N))
        } else {
            Distributions[,1] <- sort(rbinorm(n=N))
        }
        
        if (dist[2]=='rnorm') {
            Distributions[,2] <- sort(rnorm(n=N))
        } else { 
            Distributions[,2] <- sort(rbinorm(n=N))
        }
    } else {
        Distributions[,1] <- sort(rbinorm(n=N))
        Distributions[,2] <- sort(rbinorm(n=N))
    }
    
    # This implementation of GenData bootstraps each variable's score distribution from a supplied data set.
    # Users should modify this block of the program, as needed, to generate the desired distribution(s).
    #
    # For example, to sample from chi-square distributions with 2 df, replace the 2nd line in this block with:
    # Distributions[,i] <- sort(rchisq(N, df = 2))
    #
    # Or, one can drop the loop and use a series of commands that samples variables from specified populations:
    # Distributions[,1] <- sort(rnorm(N, 0, 1)) # Standard normal distribution
    # Distributions[,2] <- sort(runif(N, 0, 1)) # Uniform distribution ranging from 0 - 1
    # Distributions[,3] <- sort(rlnorm(N, 0, 1)) # Log-normal distribution, log scale M = 0, SD = 1
    # Distributions[,4] <- sort(rexp(N, rate = 1)) # Exponential distribution with rate = 1
    # Distributions[,5] <- sort(rpois(N, lambda = 4)) # Poisson distribution with lambda = 4
    # Distributions[,6] <- sort(rbinom(N, 10, .25) # Binominal distribution, size = 10 and p = .25
    # Distributions[,7] <- sort(rbinom(N, 2, .25) # Binary distribution with p = .25
    #
    # All of the commands shown above draw random samples from specified population distributions. As an
    # alternative, one can reproduce distributions without sampling error. For example, working with a
    # supplied data set, one can replace the 2nd line in this block with:
    # Distributions[,i] <- Supplied.Data[,i]
    # Alternatively, idealized distributions can be reproduced. For example, uniform quantiles can be
    # created and used to generate data from common distributions:
    # Uniform.Quantiles <- seq(from = 0, to = 1, length = (N + 2))[2:(N + 1)] # quantiles 0, 1 dropped
    # Distributions[,1] <- qnorm(Uniform.Quantiles, 0, 1) # Standard normal distribution
    # Distributions[,2] <- qunif(Uniform.Quantiles, 0, 1) # Uniform distribution ranging from 0 to 1
    # Distributions[,3] <- qchisq(Uniform.Quantiles, df = 2) # Chi-square distribution with 2 df
    #
    # Note that when score distributions are generated from specified populations rather than bootstrapped from
    # a supplied data set, the user must provide the target correlation matrix (see the next block). This is
    # true regardless of whether the distributions incorporate sampling error.
    # Calculate and store a copy of the target correlation matrix (step 3) -----------------------------------------
    Target.Corr <- matrix(c(rep(c(1, rep(rho,k)),k-1),1), nrow=k)
    Intermediate.Corr <- Target.Corr
    # This implementation of GenData calculates the target correlation matrix from a supplied data set.
    # Alternatively, the user can modify the program to generate data with user-defined sample size, number of
    # variables, and target correlation matrix by redefining the function as follows:
    # GenData <- function(N, k, Target.Corr, N.Factors = 0, Max.Trials = 5, Initial.Multiplier = 1, seed = 0)
    # In this case, one would also remove the program lines that calculate N, k, and Target.Corr.
    # To generate data in which variables are uncorrelated, one would remove the SsortT function from step 2
    # and terminate the program before step 3 begins by returning the Distributions object as the data set.
    # If number of latent factors was not specified, determine it through parallel analysis (step 4) ---------------
    if (N.Factors == 0)
    {
        Eigenvalues.Observed <- eigen(Intermediate.Corr)$values
        Eigenvalues.Random <- matrix(0, nrow = 100, ncol = k)
        Random.Data <- matrix(0, nrow = N, ncol = k)
        for (i in 1:100)
        {
            for (j in 1:k)
                Random.Data[,j] <- sample(Distributions[,j], size = N, replace = TRUE)
            Eigenvalues.Random[i,] <- eigen(cor(Random.Data))$values
        }
        Eigenvalues.Random <- apply(Eigenvalues.Random, 2, mean) # calculate mean eigenvalue for each factor
        N.Factors <- max(1, sum(Eigenvalues.Observed > Eigenvalues.Random))
    }
    # Generate random normal data for shared and unique components, initialize factor loadings (steps 5, 6) --------
    Shared.Comp <- matrix(rnorm(N * N.Factors, 0, 1), nrow = N, ncol = N.Factors)
    Unique.Comp <- matrix(rnorm(N * k, 0, 1), nrow = N, ncol = k)
    Shared.Load <- matrix(0, nrow = k, ncol = N.Factors)
    Unique.Load <- matrix(0, nrow = k, ncol = 1)
    # Begin loop that ends when specified number of iterations pass without improvement in RMSR correlation --------
    while (Trials.Without.Improvement < Max.Trials)
    {
        Iteration <- Iteration + 1
        # Calculate factor loadings and apply to reproduce desired correlations (steps 7, 8) ---------------------------
        Fact.Anal <- Factor.Analysis(Intermediate.Corr, Corr.Matrix = TRUE, N.Factors = N.Factors)
        if (N.Factors == 1) Shared.Load[,1] <- Fact.Anal$loadings
        else Shared.Load <- Fact.Anal$loadings
        Shared.Load[Shared.Load > 1] <- 1
        Shared.Load[Shared.Load < -1] <- -1
        if (Shared.Load[1,1] < 0) Shared.Load <- Shared.Load * -1
        Shared.Load.sq <- Shared.Load * Shared.Load
        for (i in 1:k)
            if (sum(Shared.Load.sq[i,]) < 1) Unique.Load[i,1] <- (1 - sum(Shared.Load.sq[i,]))
        else Unique.Load[i,1] <- 0
        Unique.Load <- sqrt(Unique.Load)
        for (i in 1:k)
            Data[,i] <- (Shared.Comp %*% t(Shared.Load))[,i] + Unique.Comp[,i] * Unique.Load[i,1]
        # the %*% operator = matrix multiplication, and the t() function = transpose (both used again in step 13)
        # Replace normal with nonnormal distributions (step 9) ---------------------------------------------------------
        for (i in 1:k)
        {
            Data <- Data[sort.list(Data[,i]),]
            Data[,i] <- Distributions[,i]
        }
        # Calculate RMSR correlation, compare to lowest value, take appropriate action (steps 10, 11, 12) --------------
        Reproduced.Corr <- cor(Data)
        Residual.Corr <- Target.Corr - Reproduced.Corr
        RMSR <- sqrt(sum(Residual.Corr[lower.tri(Residual.Corr)] * Residual.Corr[lower.tri(Residual.Corr)]) /
                         (.5 * (k * k - k)))
        if (RMSR < Best.RMSR)
        {
            Best.RMSR <- RMSR
            Best.Corr <- Intermediate.Corr
            Best.Res <- Residual.Corr
            Intermediate.Corr <- Intermediate.Corr + Initial.Multiplier * Residual.Corr
            Trials.Without.Improvement <- 0
        }
        else
        {
            Trials.Without.Improvement <- Trials.Without.Improvement + 1
            Current.Multiplier <- Initial.Multiplier * .5 ^ Trials.Without.Improvement
            Intermediate.Corr <- Best.Corr + Current.Multiplier * Best.Res
        }
    } # end of the while loop
    # Construct the data set with the lowest RMSR correlation (step 13) --------------------------------------------
    Fact.Anal <- Factor.Analysis(Best.Corr, Corr.Matrix = TRUE, N.Factors = N.Factors)
    if (N.Factors == 1) Shared.Load[,1] <- Fact.Anal$loadings
    else Shared.Load <- Fact.Anal$loadings
    Shared.Load[Shared.Load > 1] <- 1
    Shared.Load[Shared.Load < -1] <- -1
    if (Shared.Load[1,1] < 0) Shared.Load <- Shared.Load * -1
    Shared.Load.sq <- Shared.Load * Shared.Load
    for (i in 1:k)
        if (sum(Shared.Load.sq[i,]) < 1) Unique.Load[i,1] <- (1 - sum(Shared.Load.sq[i,]))
    else Unique.Load[i,1] <- 0
    Unique.Load <- sqrt(Unique.Load)
    for (i in 1:k)
        Data[,i] <- (Shared.Comp %*% t(Shared.Load))[,i] + Unique.Comp[,i] * Unique.Load[i,1]
    Data <- apply(Data, 2, scale) # standardizes each variable in the matrix
    for (i in 1:k)
    {
        Data <- Data[sort.list(Data[,i]),]
        Data[,i] <- Distributions[,i]
    }
    # Report the results and return the simulated data set (step 14) -----------------------------------------------
    Iteration <- Iteration - Max.Trials
    cat("\nN =",N,", k =",k,",",Iteration,"Iterations,",N.Factors,"Factors, RMSR r =",round(Best.RMSR,3),"\n")
    return(Data)
}

Factor.Analysis <- function(Data, Corr.Matrix = FALSE, Max.Iter = 50, N.Factors = 0)
{
    Data <- as.matrix(Data)
    k <- dim(Data)[2]
    if (N.Factors == 0) N.Factors <- k
    if (!Corr.Matrix) Cor.Matrix <- cor(Data)
    else Cor.Matrix <- Data
    Criterion <- .001
    Old.H2 <- rep(99, k)
    H2 <- rep(0, k)
    Change <- 1
    Iter <- 0
    Factor.Loadings <- matrix(nrow = k, ncol = N.Factors)
    while ((Change >= Criterion) & (Iter < Max.Iter))
    {
        Iter <- Iter + 1
        Eig <- eigen(Cor.Matrix)
        L <- sqrt(Eig$values[1:N.Factors])
        for (i in 1:N.Factors)
            Factor.Loadings[,i] <- Eig$vectors[,i] * L[i]
        for (i in 1:k)
            H2[i] <- sum(Factor.Loadings[i,] * Factor.Loadings[i,])
        Change <- max(abs(Old.H2 - H2))
        Old.H2 <- H2
        diag(Cor.Matrix) <- H2
    }
    if (N.Factors == k) N.Factors <- sum(Eig$values > 1)
    return(list(loadings = Factor.Loadings[,1:N.Factors], factors = N.Factors))
}


rbinorm <- function(n,mu1=-2,mu2=2,sigma1=1, sigma2=1) {
    uu <- runif(n)
    x <- vector('numeric',n)
    for (nn in 1:n) {
        if (uu[nn] > .5) {
            x[nn] <- rnorm(1, mu1, sigma1)
        } else {
            x[nn] <- rnorm(1, mu2, sigma2)
        }
    }
    return(x)
}



## Generate Graphs

library(ggplot2)
library(grid)
library(gridExtra)

nn <- 5000
kk <- 2


xx <- matrix(NA, nrow=nn, ncol=kk)
set.seed(123)
xx[,1] <- rbinorm(nn)
xx[,2] <- rbinorm(nn)
df <- data.frame(xx)
df$con1 <- with(df, ifelse(X1 > mean(X1),1,0))
df$con2 <- with(df, ifelse(X2 > mean(X2),1,0))
df$con <- abs(df$X1+df$X2)


mm <- .5

p1 <- ggplot(df, aes(x=X1, y=X2)) + geom_point(alpha=.7, size=2, shape=19) + theme_bw()  + labs(x='Economic Dimension', y='Social Dimension') + theme(plot.margin=unit(rep(mm,4),'lines'), legend.position='none', axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank()) +xlim(-6,6)+ylim(-6,6)+ geom_hline(yintercept=0, linetype=2) + geom_vline(xintercept=0, linetype=2) 

p2 <- ggplot(df, aes(x=X1)) + geom_histogram(aes(y=..density..), binwidth=.1,fill='gray30', alpha=.4) + theme_bw() + labs(x=NULL, y=" ") + theme(plot.margin=unit(c(1,mm,mm-.5,mm+.2), 'lines'), axis.text.x = element_blank(), axis.text.y=element_blank(), axis.ticks= element_blank(),axis.title.x = element_blank(), axis.title.y = element_text(size = 15)) + xlim(-6,6)

p3 <- ggplot(df, aes(x=X2)) + geom_histogram(aes(y=..density..),binwidth=.1,fill='gray30', alpha=.4) + theme_bw() + labs(x=NULL, y=" " ) + coord_flip() + theme(axis.text.y = element_blank(), axis.ticks= element_blank(), axis.title.x = element_text(size = rel(1.8)), axis.title.y = element_text(size = rel(1.8)),plot.margin=unit(c(mm,1,mm,0), 'lines'), axis.text.x=element_blank()) + xlim(-6,6)

blankPlot <- ggplot()+geom_blank(aes(1,1))+ theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),  axis.text.y = element_blank(), axis.ticks = element_blank(), axis.line = element_blank(), plot.margin=unit(c(1,1,mm,0), 'lines'))

pdf('pol0.pdf', family='CM Roman', width=12, height=12)
grid.arrange(p2, blankPlot, p1, p3, nrow=2, widths=c(5, 1), heights=c(1, 5))
dev.off()
embed_fonts('pol0.pdf')


xx <- GenData(Supplied.Dat=NULL, N.Factors=0, Max.Trials=20, Initial.Multiplier=1, seed=123, nn=nn, kk=kk, rho=.3)

df <- data.frame(xx)
df$con1 <- with(df, ifelse(X1 > mean(X1),1,0))
df$con2 <- with(df, ifelse(X2 > mean(X2),1,0))
df$con <- abs(df$X1+df$X2)

p1 <- ggplot(df, aes(x=X1, y=X2)) + geom_point(alpha=.7, size=2, shape=19) + theme_bw()  + labs(x='Economic Dimension', y='Social Dimension') + theme(plot.margin=unit(rep(mm,4),'lines'), legend.position='none', axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank()) +xlim(-6,6)+ylim(-6,6)+ geom_hline(yintercept=0, linetype=2) + geom_vline(xintercept=0, linetype=2) 

p2 <- ggplot(df, aes(x=X1)) + geom_histogram(aes(y=..density..), binwidth=.1,fill='gray30', alpha=.4) + theme_bw() + labs(x=NULL, y=" ") + theme(plot.margin=unit(c(1,mm,mm-.5,mm+.2), 'lines'), axis.text.x = element_blank(), axis.text.y=element_blank(), axis.ticks= element_blank(),axis.title.x = element_blank(), axis.title.y = element_text(size = 15)) + xlim(-6,6)

p3 <- ggplot(df, aes(x=X2)) + geom_histogram(aes(y=..density..),binwidth=.1,fill='gray30', alpha=.4) + theme_bw() + labs(x=NULL, y=" " ) + coord_flip() + theme(axis.text.y = element_blank(), axis.ticks= element_blank(), axis.title.x = element_text(size = rel(1.8)), axis.title.y = element_text(size = rel(1.8)),plot.margin=unit(c(mm,1,mm,0), 'lines'), axis.text.x=element_blank()) + xlim(-6,6)

blankPlot <- ggplot()+geom_blank(aes(1,1))+ theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),  axis.text.y = element_blank(), axis.ticks = element_blank(), axis.line = element_blank(), plot.margin=unit(c(1,1,mm,0), 'lines'))

pdf('pol3.pdf', family='CM Roman', width=12, height=12)
grid.arrange(p2, blankPlot, p1, p3, nrow=2, widths=c(5, 1), heights=c(1, 5))
dev.off()

xx <- GenData(Supplied.Dat=NULL, N.Factors=0, Max.Trials=20, Initial.Multiplier=1, seed=123, nn=nn, kk=kk, rho=.6)

df <- data.frame(xx)
df$con1 <- with(df, ifelse(X1 > mean(X1),1,0))
df$con2 <- with(df, ifelse(X2 > mean(X2),1,0))
df$con <- abs(df$X1+df$X2)

p1 <- ggplot(df, aes(x=X1, y=X2)) + geom_point(alpha=.7, size=2, shape=19) + theme_bw()  + labs(x='Economic Dimension', y='Social Dimension') + theme(plot.margin=unit(rep(mm,4),'lines'), legend.position='none', axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank()) +xlim(-6,6)+ylim(-6,6)+ geom_hline(yintercept=0, linetype=2) + geom_vline(xintercept=0, linetype=2) 

p2 <- ggplot(df, aes(x=X1)) + geom_histogram(aes(y=..density..), binwidth=.1,fill='gray30', alpha=.4) + theme_bw() + labs(x=NULL, y=" ") + theme(plot.margin=unit(c(1,mm,mm-.5,mm+.2), 'lines'), axis.text.x = element_blank(), axis.text.y=element_blank(), axis.ticks= element_blank(),axis.title.x = element_blank(), axis.title.y = element_text(size = 15)) + xlim(-6,6)

p3 <- ggplot(df, aes(x=X2)) + geom_histogram(aes(y=..density..),binwidth=.1,fill='gray30', alpha=.4) + theme_bw() + labs(x=NULL, y=" " ) + coord_flip() + theme(axis.text.y = element_blank(), axis.ticks= element_blank(), axis.title.x = element_text(size = rel(1.8)), axis.title.y = element_text(size = rel(1.8)),plot.margin=unit(c(mm,1,mm,0), 'lines'), axis.text.x=element_blank()) + xlim(-6,6)

blankPlot <- ggplot()+geom_blank(aes(1,1))+ theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),  axis.text.y = element_blank(), axis.ticks = element_blank(), axis.line = element_blank(), plot.margin=unit(c(1,1,mm,0), 'lines'))

pdf('pol6.pdf', family='CM Roman', width=12, height=12)
grid.arrange(p2, blankPlot, p1, p3, nrow=2, widths=c(5, 1), heights=c(1, 5))
dev.off()

xx <- GenData(Supplied.Dat=NULL, N.Factors=0, Max.Trials=20, Initial.Multiplier=1, seed=123, nn=nn, kk=kk, rho=.9)

df <- data.frame(xx)
df$con1 <- with(df, ifelse(X1 > mean(X1),1,0))
df$con2 <- with(df, ifelse(X2 > mean(X2),1,0))
df$con <- abs(df$X1+df$X2)

p1 <- ggplot(df, aes(x=X1, y=X2)) + geom_point(alpha=.7, size=2, shape=19) + theme_bw()  + labs(x='Economic Dimension', y='Social Dimension') + theme(plot.margin=unit(rep(mm,4),'lines'), legend.position='none', axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank()) +xlim(-6,6)+ylim(-6,6)+ geom_hline(yintercept=0, linetype=2) + geom_vline(xintercept=0, linetype=2) 

p2 <- ggplot(df, aes(x=X1)) + geom_histogram(aes(y=..density..), binwidth=.1,fill='gray30', alpha=.4) + theme_bw() + labs(x=NULL, y=" ") + theme(plot.margin=unit(c(1,mm,mm-.5,mm+.2), 'lines'), axis.text.x = element_blank(), axis.text.y=element_blank(), axis.ticks= element_blank(),axis.title.x = element_blank(), axis.title.y = element_text(size = 15)) + xlim(-6,6)

p3 <- ggplot(df, aes(x=X2)) + geom_histogram(aes(y=..density..),binwidth=.1,fill='gray30', alpha=.4) + theme_bw() + labs(x=NULL, y=" " ) + coord_flip() + theme(axis.text.y = element_blank(), axis.ticks= element_blank(), axis.title.x = element_text(size = rel(1.8)), axis.title.y = element_text(size = rel(1.8)),plot.margin=unit(c(mm,1,mm,0), 'lines'), axis.text.x=element_blank()) + xlim(-6,6)

blankPlot <- ggplot()+geom_blank(aes(1,1))+ theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),  axis.text.y = element_blank(), axis.ticks = element_blank(), axis.line = element_blank(), plot.margin=unit(c(1,1,mm,0), 'lines'))

pdf('pol9.pdf', width=12, height=12)
grid.arrange(p2, blankPlot, p1, p3, nrow=2, widths=c(5, 1), heights=c(1, 5))
dev.off()




### Hypothetical Graph for Illustration of Coefficient of Overlapping ###
library(reshape)

n <- 5000
x <- seq(0,1,length.out=n)
y1 <- dbeta(x,2,4) + .1
y2 <- dbeta(x,4,2) + .1
dat <- data.frame(x=x, y1=y1, y2=y2)
dat$p.min <- apply(dat[,2:3], 1, min)

df <- melt(dat, id.vars='x')

pdf('ovexample.pdf', family='CM Roman',width=10, height=7)
ggplot(df[df$variable!='p.min',], aes(x=x, y=value,col=variable))+ geom_ribbon(data=df[df$variable=='p.min',], aes(ymin=.1, ymax=value), alpha=.2) +  geom_line(size=1) +theme_bw() + geom_hline(yintercept=.1) + theme(axis.ticks=element_blank(),axis.text= element_blank(),axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position='none', plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank()) + ylim(0,3) +scale_color_manual(values=c('white','black','black')) + annotate('text', x=.05, y=0, label='Liberal', size=10) + annotate('text', x=.92, y=0, label='Conservative', size=10)
dev.off()



### End of Code ###