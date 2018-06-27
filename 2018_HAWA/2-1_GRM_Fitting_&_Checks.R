
################################################
################################################
####                                        ####
####            HOW ARE WE APART            ####
####                                        ####
####        GRM Models: Fit & Checks        ####
####                                        ####
####                                        ####
#### Last Update: 08/09/2016                ####
####                                        ####
################################################
################################################

# remove all objects
rm(list=ls())


# load packages
library(coda)

n.chains <- 6
library(rstan)
rstan_options(auto_write=TRUE)
options(mc.cores=n.chains)

library(ggplot2)
library(grid)
library(gridExtra)
library(xtable)

# set working directory
wd <- "Working Directory Here!"
if (!isTRUE(getwd()==wd)) setwd(wd)


# generate some new folders

if ('MCMC' %in% dir()==FALSE) dir.create('MCMC')
wd.mcmc <- paste0(wd,'/MCMC')

setwd(wd.mcmc)
fl <- c('dim1','dim2_corr','dim2_corr_2','dim_corr_cross', 'dim3_corr','dim3_corr_year','dim3_corr_year_sd','dim3_corr_year_sd_mean')
lapply(fl, function(x) if (x %in% dir()==FALSE) dir.create(x))

setwd(wd)

###### RUN MODELS ######
# cautions: running the models is extremely time-consuming!!


# read data
long.ord <- readRDS('grm.dat.Rda')

# read cut point data
cut.dat <- readRDS('cut.dat.Rda')

cats <- cut.dat[[1]]
cuts <- cut.dat[[2]]
end.cuts <- cut.dat[[3]]
start.cuts <- cut.dat[[4]]
n.cuts <- cut.dat[[5]]


### Model 1: Unidimensional

# long.ord <- readRDS('grm.dat.Rda')
# cut.dat <- readRDS('cut.dat.Rda')
# cats <- cut.dat[[1]]
# cuts <- cut.dat[[2]]
# end.cuts <- cut.dat[[3]]
# start.cuts <- cut.dat[[4]]
# n.cuts <- cut.dat[[5]]



# stand model
stan.string <- "
data{

int<lower=1> J;
int<lower=1> K;
int<lower=1> N;
int<lower=1> jj[N];
int<lower=1> kk[N];
int<lower=0, upper=7> y[N];
int<lower=1> start[K];
int<lower=1> n_cuts[K];
int tot_cuts;
}

parameters {

vector[J] theta;
vector[K-1] gamma_star;
vector[tot_cuts] kappa_star;
real<lower=0> sigma;

}

transformed parameters {

vector[N] xb;
vector[tot_cuts] kappa;
vector[K] gamma;

gamma[1] <- 1;
for (k in 2:K) gamma[k] <- gamma_star[k-1];

{
    
    int co;
    
    co <- 1;
    for (k in 1:K) {
    for (cc in 1:n_cuts[k]) {
    kappa[co] <- sort_asc(segment(kappa_star,start[k],n_cuts[k]))[cc];
    co <- co + 1;
    }
    }
    
}

for (n in 1:N) 
xb[n] <- gamma[kk[n]] * theta[jj[n]] ;


}

model {

theta ~ normal(0,sigma);
sigma ~ uniform(0,50);
gamma_star ~ normal(0, 10);
kappa_star ~ normal(0, 10);    


for (n in 1:N) 
y[n] ~ ordered_logistic(xb[n], segment(kappa,start[kk[n]],n_cuts[kk[n]]));

}

generated quantities {

vector[N] log_lik;

for (n in 1:N)
log_lik[n] <- ordered_logistic_log(y[n],xb[n], segment(kappa, start[kk[n]], n_cuts[kk[n]]));

}

"

stan.dat <- list(J=length(unique(long.ord$unique.no)), K=length(unique(long.ord$item)), N=nrow(long.ord), jj=long.ord$unique.no, kk=long.ord$item, y=long.ord$response, start=start.cuts, n_cuts=cuts, tot_cuts=n.cuts)
stan.pars <- c('gamma','kappa','theta','log_lik','sigma')

# n.chains <- 6
# library(rstan)
# rstan_options(auto_write=TRUE)
# options(mc.cores=n.chains)

temp <- stan(model_code=stan.string, data=stan.dat, pars=stan.pars, chains=n.chains, warmup=10000, iter=15000, thin=20, refresh=100)

setwd(paste0(wd.mcmc,'/dim1'))

saveRDS(temp, 'dim1.Rda')

for (v in stan.pars) {
	file.name <- paste0(v,'.Rda')
	assign(v, extract(temp, v))
	saveRDS(get(v), file.name)
}






### Model 2: Two dims (correlated, civil rights load on moral dimensions)

# long.ord <- readRDS('grm.dat.Rda')
# cut.dat <- readRDS('cut.dat.Rda')
# cats <- cut.dat[[1]]
# cuts <- cut.dat[[2]]
# end.cuts <- cut.dat[[3]]
# start.cuts <- cut.dat[[4]]
# n.cuts <- cut.dat[[5]]


# stan model

stan.string <- "
data{

int<lower=1> J;
int<lower=1> K;
int<lower=1> N;
int<lower=1> D;
int<lower=1> jj[N];
int<lower=1> kk[N];
int<lower=0, upper=7> y[N];
int<lower=1> start[K];
int<lower=1> n_cuts[K];
int tot_cuts;

}

parameters {

matrix[J,D] theta;
vector[D] tau;
vector[16] gamma_econ_star;
vector[25] gamma_social_star;
vector[tot_cuts] kappa_star;
cholesky_factor_corr[D] L_Rho;

}

transformed parameters {

vector[N] xb;
vector[tot_cuts] kappa;
matrix[K,D] gamma;

gamma <- rep_matrix(0,K,D);
gamma[1,1] <- 1;
gamma[K,2] <- 1;
for (k in 18:33) gamma[k,1] <- gamma_econ_star[k-17];
for (k in 2:17) gamma[k,2] <- gamma_social_star[k-1];
for (k in 34:(K-1)) gamma[k,2] <- gamma_social_star[k-18];

{
    int co;
    
    co <- 1;
    for (k in 1:K) {
    for (cc in 1:n_cuts[k]) {
    kappa[co] <- sort_asc(segment(kappa_star,start[k],n_cuts[k]))[cc];
    co <- co + 1;
    }
    }
}

for (n in 1:N) 
xb[n] <- dot_product(gamma[kk[n]], theta[jj[n]]);

}

model {
vector[D] mu;
matrix[D,D] L_Sigma;

tau ~ uniform(0,50);
L_Rho ~ lkj_corr_cholesky(1);

mu <- rep_vector(0,D);
L_Sigma <- diag_pre_multiply(tau,L_Rho);

for (j in 1:J) 
theta[j] ~ multi_normal_cholesky(mu,L_Sigma);

kappa_star ~ normal(0,10);    
gamma_econ_star ~ normal(0,10);
gamma_social_star ~ normal(0,10);

for (n in 1:N) 
y[n] ~ ordered_logistic(xb[n], sort_asc(segment(kappa,start[kk[n]],n_cuts[kk[n]])));

}

generated quantities {

vector[N] log_lik;
real rho;	

for (n in 1:N)
log_lik[n] <- ordered_logistic_log(y[n],xb[n], segment(kappa, start[kk[n]], n_cuts[kk[n]]));

{
    matrix[D,D] Rho;
    Rho <- multiply_lower_tri_self_transpose(L_Rho);
    rho <- Rho[1,2];
}
}

"

# n.chains <- 6
# 
# library(rstan)
# rstan_options(auto_write=TRUE)
# options(mc.cores=n.chains)

stan.dat <- list(J=length(unique(long.ord$unique.no)), K=length(unique(long.ord$item)), N=nrow(long.ord), D=2, jj=long.ord$unique.no, kk=long.ord$item, y=long.ord$response, start=start.cuts, n_cuts=cuts, tot_cuts=n.cuts)
stan.pars <- c('gamma','kappa','theta','rho','log_lik', 'tau')

temp <- stan(model_code=stan.string, data=stan.dat, pars=stan.pars, chains=n.chains, warmup=10000, iter=15000, thin=20, refresh=100)

setwd(paste0(wd.mcmc,'/dim2_corr'))
saveRDS(temp, 'dim2_corr.Rda')

for (v in stan.pars) {
	file.name <- paste0(v,'.Rda')
	assign(v, extract(temp, v))
	saveRDS(get(v), file.name)
}




### Model 3: 2 Dims (correlated, civil rights load on economic dimension)

# long.ord <- readRDS('grm.dat.Rda')
# cut.dat <- readRDS('cut.dat.Rda')
# cats <- cut.dat[[1]]
# cuts <- cut.dat[[2]]
# end.cuts <- cut.dat[[3]]
# start.cuts <- cut.dat[[4]]
# n.cuts <- cut.dat[[5]]

# stan model

stan.string <- "
data{

int<lower=1> J;
int<lower=1> K;
int<lower=1> N;
int<lower=1> D;
int<lower=1> jj[N];
int<lower=1> kk[N];
int<lower=0, upper=7> y[N];
int<lower=1> start[K];
int<lower=1> n_cuts[K];
int tot_cuts;

}

parameters {

matrix[J,D] theta;
vector[D] tau;
vector[32] gamma_econ_star;
vector[9] gamma_social_star;
vector[tot_cuts] kappa_star;
cholesky_factor_corr[D] L_Rho;

}

transformed parameters {

vector[N] xb;
vector[tot_cuts] kappa;
matrix[K,D] gamma;

gamma <- rep_matrix(0,K,D);
gamma[1,1] <- 1;
gamma[K,2] <- 1;
for (k in 2:33) gamma[k,1] <- gamma_econ_star[k-1];
for (k in 34:(K-1)) gamma[k,2] <- gamma_social_star[k-33];

{
    int co;
    
    co <- 1;
    for (k in 1:K) {
    for (cc in 1:n_cuts[k]) {
    kappa[co] <- sort_asc(segment(kappa_star,start[k],n_cuts[k]))[cc];
    co <- co + 1;
    }
    }
}

for (n in 1:N) 
xb[n] <- dot_product(gamma[kk[n]], theta[jj[n]]);

}

model {
vector[D] mu;
matrix[D,D] L_Sigma;

tau ~ uniform(0,50);
L_Rho ~ lkj_corr_cholesky(1);

mu <- rep_vector(0,D);
L_Sigma <- diag_pre_multiply(tau,L_Rho);

for (j in 1:J) 
theta[j] ~ multi_normal_cholesky(mu,L_Sigma);

kappa_star ~ normal(0,10);    
gamma_econ_star ~ normal(0,10);
gamma_social_star ~ normal(0,10);

for (n in 1:N) 
y[n] ~ ordered_logistic(xb[n], sort_asc(segment(kappa,start[kk[n]],n_cuts[kk[n]])));

}

generated quantities {

vector[N] log_lik;
real rho;	

for (n in 1:N)
log_lik[n] <- ordered_logistic_log(y[n],xb[n], segment(kappa, start[kk[n]], n_cuts[kk[n]]));

{
    matrix[D,D] Rho;
    Rho <- multiply_lower_tri_self_transpose(L_Rho);
    rho <- Rho[1,2];
}
}

"

# n.chains <- 6
# 
# library(rstan)
# rstan_options(auto_write=TRUE)
# options(mc.cores=n.chains)

stan.dat <- list(J=length(unique(long.ord$unique.no)), K=length(unique(long.ord$item)), N=nrow(long.ord), D=2, jj=long.ord$unique.no, kk=long.ord$item, y=long.ord$response, start=start.cuts, n_cuts=cuts, tot_cuts=n.cuts)
stan.pars <- c('gamma','kappa','theta','rho','log_lik', 'tau')

temp <- stan(model_code=stan.string, data=stan.dat, pars=stan.pars, chains=n.chains, warmup=10000, iter=15000, thin=20, refresh=100)

setwd(paste0(wd.mcmc,'/dim2_corr_2'))
saveRDS(temp, 'dim2_corr_2.Rda')

for (v in stan.pars) {
	file.name <- paste0(v,'.Rda')
	assign(v, extract(temp, v))
	saveRDS(get(v), file.name)
}





### Model 4: 2 Dims (correlated, cross-loadings allowed for civil rights items)

# long.ord <- readRDS('grm.dat.Rda')
# cut.dat <- readRDS('cut.dat.Rda')
# cats <- cut.dat[[1]]
# cuts <- cut.dat[[2]]
# end.cuts <- cut.dat[[3]]
# start.cuts <- cut.dat[[4]]
# n.cuts <- cut.dat[[5]]


# stan model

stan.string <- "
data{

int<lower=1> J;
int<lower=1> K;
int<lower=1> N;
int<lower=1> D;
int<lower=1> jj[N];
int<lower=1> kk[N];
int<lower=0, upper=7> y[N];
int<lower=1> start[K];
int<lower=1> n_cuts[K];
int tot_cuts;

}

parameters {

matrix[J,D] theta;
vector[D] tau;
vector[16] gamma_econ_star;
vector[16] gamma_civil_star1;
vector[16] gamma_civil_star2;
vector[9] gamma_moral_star;
vector[tot_cuts] kappa_star;
cholesky_factor_corr[D] L_Rho;

}

transformed parameters {

vector[N] xb;
vector[tot_cuts] kappa;
matrix[K,D] gamma;

gamma <- rep_matrix(0,K,D);
gamma[1,1] <- 1;
gamma[K,2] <- 1;
for (k in 18:33) gamma[k,1] <- gamma_econ_star[k-17];
for (k in 2:17) gamma[k,1] <- gamma_civil_star1[k-1];
for (k in 2:17) gamma[k,2] <- gamma_civil_star2[k-1];
for (k in 34:(K-1)) gamma[k,2] <- gamma_moral_star[k-33];

{
    int co;
    
    co <- 1;
    for (k in 1:K) {
    for (cc in 1:n_cuts[k]) {
    kappa[co] <- sort_asc(segment(kappa_star,start[k],n_cuts[k]))[cc];
    co <- co + 1;
    }
    }
}

for (n in 1:N) 
xb[n] <- dot_product(gamma[kk[n]], theta[jj[n]]);

}

model {
vector[D] mu;
matrix[D,D] L_Sigma;

tau ~ uniform(0,50);
L_Rho ~ lkj_corr_cholesky(1);

mu <- rep_vector(0,D);
L_Sigma <- diag_pre_multiply(tau,L_Rho);

for (j in 1:J) 
theta[j] ~ multi_normal_cholesky(mu,L_Sigma);

kappa_star ~ normal(0,10);    
gamma_econ_star ~ normal(0,10);
gamma_civil_star1 ~ normal(0,10);
gamma_civil_star2 ~ normal(0,10);
gamma_moral_star ~ normal(0,10);

for (n in 1:N) 
y[n] ~ ordered_logistic(xb[n], sort_asc(segment(kappa,start[kk[n]],n_cuts[kk[n]])));

}

generated quantities {

vector[N] log_lik;
real rho;	

for (n in 1:N)
log_lik[n] <- ordered_logistic_log(y[n],xb[n], segment(kappa, start[kk[n]], n_cuts[kk[n]]));

{
    matrix[D,D] Rho;
    Rho <- multiply_lower_tri_self_transpose(L_Rho);
    rho <- Rho[1,2];
}
}

"

# n.chains <- 6
# 
# library(rstan)
# rstan_options(auto_write=TRUE)
# options(mc.cores=n.chains)

stan.dat <- list(J=length(unique(long.ord$unique.no)), K=length(unique(long.ord$item)), N=nrow(long.ord), D=2, jj=long.ord$unique.no, kk=long.ord$item, y=long.ord$response, start=start.cuts, n_cuts=cuts, tot_cuts=n.cuts)
stan.pars <- c('gamma','kappa','theta','rho','log_lik', 'tau')

temp <- stan(model_code=stan.string, data=stan.dat, pars=stan.pars, chains=n.chains,  warmup=10000, iter=15000, thin=20, refresh=100)


setwd(paste0(wd.mcmc,'/dim2_corr_cross'))
saveRDS(temp, 'dim2_corr_cross.Rda')

for (v in stan.pars) {
    file.name <- paste0(v,'.Rda')
    assign(v, extract(temp, v))
    saveRDS(get(v), file.name)
}




### Model 5: 3 Dims (correlated)

# long.ord <- readRDS('grm.dat.Rda')
# cut.dat <- readRDS('cut.dat.Rda')
# cats <- cut.dat[[1]]
# cuts <- cut.dat[[2]]
# end.cuts <- cut.dat[[3]]
# start.cuts <- cut.dat[[4]]
# n.cuts <- cut.dat[[5]]


# stan model

stan.string <- "
data{

int<lower=1> J;
int<lower=1> K;
int<lower=1> N;
int<lower=1> D;
int<lower=1> jj[N];
int<lower=1> kk[N];
int<lower=0, upper=7> y[N];
int<lower=1> start[K];
int<lower=1> n_cuts[K];
int tot_cuts;

}

parameters {

vector[D] theta[J];
vector[D] tau;
vector[16] gamma_econ_star;
vector[15] gamma_civil_star;
vector[9] gamma_moral_star;
vector[tot_cuts] kappa_star;
cholesky_factor_corr[D] L_Rho;

}

transformed parameters {

vector[N] xb;
vector[tot_cuts] kappa;
matrix[K,D] gamma;

gamma <- rep_matrix(0,K,D);
gamma[1,1] <- 1;
gamma[5,2] <- 1;
gamma[K,3] <- 1;
for (k in 18:33) gamma[k,1] <- gamma_econ_star[k-17];
for (k in 2:4) gamma[k,2] <- gamma_civil_star[k-1];
for (k in 6:17) gamma[k,2] <- gamma_civil_star[k-2];
for (k in 34:(K-1)) gamma[k,3] <- gamma_moral_star[k-33];

{
    int co;
    
    co <- 1;
    for (k in 1:K) {
    for (cc in 1:n_cuts[k]) {
    kappa[co] <- sort_asc(segment(kappa_star,start[k],n_cuts[k]))[cc];
    co <- co + 1;
    }
    }
}

for (n in 1:N) 
xb[n] <- dot_product(gamma[kk[n]], theta[jj[n]]);

}

model {
vector[D] mu;
matrix[D,D] L_Sigma;

tau ~ uniform(0,50);

mu <- rep_vector(0,D);
L_Sigma <- diag_pre_multiply(tau, L_Rho);

theta ~ multi_normal_cholesky(mu,L_Sigma);

kappa_star ~ normal(0,10);    
gamma_econ_star ~ normal(0,10);
gamma_civil_star ~ normal(0,10);
gamma_moral_star ~ normal(0,10);

for (n in 1:N) 
y[n] ~ ordered_logistic(xb[n], sort_asc(segment(kappa,start[kk[n]],n_cuts[kk[n]])));

}

generated quantities {

vector[N] log_lik;
real rho_econ_civil;	
real rho_econ_moral;
real rho_civil_moral;

for (n in 1:N)
log_lik[n] <- ordered_logistic_log(y[n],xb[n], segment(kappa, start[kk[n]], n_cuts[kk[n]]));

{
    matrix[D,D] Rho;
    Rho <- multiply_lower_tri_self_transpose(L_Rho);
    rho_econ_civil <- Rho[1,2];
    rho_econ_moral <- Rho[1,3];
    rho_civil_moral <- Rho[2,3];
}
}

"
# 
# n.chains <- 6
# 
# library(rstan)
# rstan_options(auto_write=TRUE)
# options(mc.cores=n.chains)

stan.dat <- list(J=length(unique(long.ord$unique.no)), K=length(unique(long.ord$item)), N=nrow(long.ord), D=3, jj=long.ord$unique.no, kk=long.ord$item, y=long.ord$response, start=start.cuts, n_cuts=cuts, tot_cuts=n.cuts)
stan.pars <- c('gamma','kappa','theta','rho_econ_civil','rho_econ_moral','rho_civil_moral','log_lik', 'tau')

temp <- stan(model_code=stan.string, data=stan.dat, pars=stan.pars, chains=n.chains,  warmup=10000, iter=15000, thin=20, refresh=100)

setwd(paste0(wd.mcmc,'/dim3_corr'))
saveRDS(temp, 'dim3_corr.Rda')

for (v in stan.pars) {
	file.name <- paste0(v,'.Rda')
	assign(v, extract(temp, v))
	saveRDS(get(v), file.name)
}


### Model 6: DIM 3 (correlated, year-specific correlations)


# read data (NEW)
setwd(wd)
long.ord <- readRDS('grm.year.dat.Rda')
time.id.dat <- readRDS('time.id.dat.Rda')

# old cut data

# cut.dat <- readRDS('cut.dat.Rda')
# cats <- cut.dat[[1]]
# cuts <- cut.dat[[2]]
# end.cuts <- cut.dat[[3]]
# start.cuts <- cut.dat[[4]]
# n.cuts <- cut.dat[[5]]


# stan model

stan.string <- "
data{

int<lower=1> J;
int<lower=1> K;
int<lower=1> N;
int<lower=1> D;
int<lower=1> T;
int<lower=1> jj[N];
int<lower=1> kk[N];
int<lower=1> tt[J];
int<lower=0, upper=7> y[N];
int<lower=1> start[K];
int<lower=1> n_cuts[K];
int tot_cuts;

}

parameters {

vector[D] theta[J];
vector[D] tau;
vector[K-3] gamma_star;
vector[tot_cuts] kappa_star;
cholesky_factor_corr[D] L_Rho[T];

}

transformed parameters {

matrix[K,D] gamma;
vector[tot_cuts] kappa;
vector[N] xb;


gamma <- rep_matrix(0,K,D);
gamma[1,1] <- 1;
gamma[5,2] <- 1;
gamma[K,3] <- 1;
for (k in 2:4) gamma[k,2] <- gamma_star[k-1];
for (k in 6:17) gamma[k,2] <- gamma_star[k-2];
for (k in 18:33) gamma[k,1] <- gamma_star[k-2];
for (k in 34:(K-1)) gamma[k,3] <- gamma_star[k-2];

{
    int co;
    
    co <- 1;
    for (k in 1:K) {
    for (cc in 1:n_cuts[k]) {
    kappa[co] <- sort_asc(segment(kappa_star,start[k],n_cuts[k]))[cc];
    co <- co + 1;
    }
    }
}

for (n in 1:N) xb[n] <- dot_product(gamma[kk[n]], theta[jj[n]]);

}

model {
vector[D] mu;
matrix[D,D] L_Sigma[T];

tau ~ uniform(0,50);
kappa_star ~ normal(0,10);    
gamma_star ~ normal(0,10);

mu <- rep_vector(0,D);

for (t in 1:T) 
L_Sigma[t] <- diag_pre_multiply(tau,L_Rho[t]);


for (j in 1:J) 
theta[j] ~ multi_normal_cholesky(mu, L_Sigma[tt[j]]);

for (n in 1:N) 
y[n] ~ ordered_logistic(xb[n], sort_asc(segment(kappa,start[kk[n]],n_cuts[kk[n]])));

}

generated quantities {

vector[N] log_lik;
vector[T] rho_econ_civil;	
vector[T] rho_econ_moral;
vector[T] rho_civil_moral;

for (n in 1:N)
log_lik[n] <- ordered_logistic_log(y[n],xb[n], segment(kappa, start[kk[n]], n_cuts[kk[n]]));

{
    matrix[D,D] Rho;
    for (t in 1:T) {
    Rho <- multiply_lower_tri_self_transpose(L_Rho[t]);
    rho_econ_civil[t] <- Rho[1,2];
    rho_econ_moral[t] <- Rho[1,3];
    rho_civil_moral[t] <- Rho[2,3];
    }
}
}

"
# 
# n.chains <- 6
# 
# library(rstan)
# rstan_options(auto_write=TRUE)
# options(mc.cores=n.chains)

stan.dat <- list(J=length(unique(long.ord$unique.no)), 
		K=length(unique(long.ord$item)), 
		N=nrow(long.ord), 
		D=3, 
		T=length(unique(long.ord$year)),
		jj=long.ord$unique.no, 
		kk=long.ord$item, 
		tt=time.id.dat$year,
		y=long.ord$response, 
		start=start.cuts, 
		n_cuts=cuts, 
		tot_cuts=n.cuts)
stan.pars <- c('gamma','kappa','theta','rho_econ_civil','rho_econ_moral','rho_civil_moral','log_lik', 'tau')

temp <- stan(model_code=stan.string, data=stan.dat, pars=stan.pars, chains=n.chains,  warmup=10000, iter=15000, thin=20, refresh=100)

setwd(paste0(wd.mcmc,'/dim3_corr_year'))
saveRDS(temp, 'dim3_corr_year.Rda')

for (v in stan.pars) {
	file.name <- paste0(v,'.Rda')
	assign(v, extract(temp, v))
	saveRDS(get(v), file.name)
}




### Model 7: 3 Dim (correlated, year-specific correlations and variances)


# long.ord <- readRDS('grm.year.dat.Rda')
# time.id.dat <- readRDS('time.id.dat.Rda')
# cut.dat <- readRDS('cut.dat.Rda')
# cats <- cut.dat[[1]]
# cuts <- cut.dat[[2]]
# end.cuts <- cut.dat[[3]]
# start.cuts <- cut.dat[[4]]
# n.cuts <- cut.dat[[5]]



# stan model

stan.string <- "
data{

int<lower=1> J;
int<lower=1> K;
int<lower=1> N;
int<lower=1> D;
int<lower=1> T;
int<lower=1> jj[N];
int<lower=1> kk[N];
int<lower=1> tt[J];
int<lower=0, upper=7> y[N];
int<lower=1> start[K];
int<lower=1> n_cuts[K];
int tot_cuts;

}

parameters {

vector[D] theta[J];
vector[K-3] gamma_star;
vector[tot_cuts] kappa_star;
cholesky_factor_corr[D] L_Rho[T];
matrix<lower=0>[T,3]  tau;

}

transformed parameters {

matrix[K,D] gamma;
vector[tot_cuts] kappa;
vector[N] xb;

gamma <- rep_matrix(0,K,D);
gamma[1,1] <- 1;
gamma[5,2] <- 1;
gamma[K,3] <- 1;

for (k in 2:4) gamma[k,2] <- gamma_star[k-1];
for (k in 6:17) gamma[k,2] <- gamma_star[k-2];
for (k in 18:33) gamma[k,1] <- gamma_star[k-2];
for (k in 34:(K-1)) gamma[k,3] <- gamma_star[k-2];

{
    int co;
    
    co <- 1;
    for (k in 1:K) {
    for (cc in 1:n_cuts[k]) {
    kappa[co] <- sort_asc(segment(kappa_star,start[k],n_cuts[k]))[cc];
    co <- co + 1;
    }
    }
}

for (n in 1:N) xb[n] <- dot_product(gamma[kk[n]], theta[jj[n]]);

}

model {
vector[D] mu;
matrix[D,D] L_Sigma[T];

mu <- rep_vector(0,D);

kappa_star ~ normal(0,10);    
gamma_star ~ normal(0,10);

to_vector(tau) ~ uniform(0,50);

for (t in 1:T) 
L_Sigma[t] <- diag_pre_multiply(tau[t],L_Rho[t]);

for (j in 1:J) 
theta[j] ~ multi_normal_cholesky(mu, L_Sigma[tt[j]]);

for (n in 1:N) 
y[n] ~ ordered_logistic(xb[n], sort_asc(segment(kappa,start[kk[n]],n_cuts[kk[n]])));

}

generated quantities {

vector[N] log_lik;
vector[T] rho_econ_civil;	
vector[T] rho_econ_moral;
vector[T] rho_civil_moral;

for (n in 1:N)
log_lik[n] <- ordered_logistic_log(y[n],xb[n], segment(kappa, start[kk[n]], n_cuts[kk[n]]));

{
    matrix[D,D] Rho;
    for (t in 1:T) {
    Rho <- multiply_lower_tri_self_transpose(L_Rho[t]);
    rho_econ_civil[t] <- Rho[1,2];
    rho_econ_moral[t] <- Rho[1,3];
    rho_civil_moral[t] <- Rho[2,3];
    }
}
}

"
# 
# n.chains <- 6
# 
# library(rstan)
# rstan_options(auto_write=TRUE)
# options(mc.cores=n.chains)

stan.dat <- list(J=length(unique(long.ord$unique.no)), 
                 K=length(unique(long.ord$item)), 
                 N=nrow(long.ord), 
                 D=3, 
                 T=length(unique(long.ord$year)),
                 jj=long.ord$unique.no, 
                 kk=long.ord$item, 
                 tt=time.id.dat$year,
                 y=long.ord$response, 
                 start=start.cuts, 
                 n_cuts=cuts, 
                 tot_cuts=n.cuts)
stan.pars <- c('gamma','kappa','theta','rho_econ_civil','rho_econ_moral','rho_civil_moral','tau','log_lik')

temp <- stan(model_code=stan.string, data=stan.dat, pars=stan.pars, chains=n.chains,  iter=15000, warmup=10000, thin=20, refresh=10)

setwd(paste0(wd.mcmc,'/dim3_corr_year_sd'))
saveRDS(temp, 'dim3_corr_year_sd.Rda')

for (v in stan.pars) {
    file.name <- paste0(v,'.Rda')
    assign(v, extract(temp, v))
    saveRDS(get(v), file.name)
}




### Model 8: 3 Dim (correlated, year-specific variances and means)

# long.ord <- readRDS('grm.year.dat.Rda')
# cut.dat <- readRDS('cut.dat.Rda')
# time.id.dat <- readRDS('time.id.dat.Rda')
# cats <- cut.dat[[1]]
# cuts <- cut.dat[[2]]
# end.cuts <- cut.dat[[3]]
# start.cuts <- cut.dat[[4]]
# n.cuts <- cut.dat[[5]]


# stan model

stan.string <- "
data{

int<lower=1> J;
int<lower=1> K;
int<lower=1> N;
int<lower=1> D;
int<lower=1> T;
int<lower=1> jj[N];
int<lower=1> kk[N];
int<lower=1> tt[J];
int<lower=0, upper=7> y[N];
int<lower=1> start[K];
int<lower=1> n_cuts[K];
int tot_cuts;

}

parameters {

vector[D] theta[J];
vector[K-3] gamma_star;
vector[tot_cuts] kappa_star;
cholesky_factor_corr[D] L_Rho[T];
matrix<lower=0>[T,D]  tau;
matrix[T-1,D] mu;

}

transformed parameters {

matrix[K,D] gamma;
vector[tot_cuts] kappa;
vector[N] xb;

gamma <- rep_matrix(0,K,D);
gamma[1,1] <- 1;
gamma[5,2] <- 1;
gamma[K,3] <- 1;
for (k in 2:4) gamma[k,2] <- gamma_star[k-1];
for (k in 6:17) gamma[k,2] <- gamma_star[k-2];
for (k in 18:33) gamma[k,1] <- gamma_star[k-2];
for (k in 34:(K-1)) gamma[k,3] <- gamma_star[k-2];

{
    int co;
    
    co <- 1;
    for (k in 1:K) {
    for (cc in 1:n_cuts[k]) {
    kappa[co] <- sort_asc(segment(kappa_star,start[k],n_cuts[k]))[cc];
    co <- co + 1;
    }
    }
}

for (n in 1:N) xb[n] <- dot_product(gamma[kk[n]], theta[jj[n]]);

}

model {
matrix[D,D] L_Sigma[T];
matrix[T,D] mu_full;

kappa_star ~ normal(0,10);    
gamma_star ~ normal(0,10);

to_vector(mu) ~ normal(0,10);
to_vector(tau) ~ uniform(0,50);

L_Sigma[1] <- diag_pre_multiply(tau[1],L_Rho[1]);
mu_full[1] <- rep_row_vector(0,D);

for (t in 2:T) {
L_Sigma[t] <- diag_pre_multiply(tau[t],L_Rho[t]);
mu_full[t] <- mu[t-1];
}

for (j in 1:J) 
theta[j] ~ multi_normal_cholesky(mu_full[tt[j]], L_Sigma[tt[j]]);

for (n in 1:N) 
y[n] ~ ordered_logistic(xb[n], sort_asc(segment(kappa,start[kk[n]],n_cuts[kk[n]])));

}

generated quantities {

vector[N] log_lik;
vector[T] rho_econ_civil;	
vector[T] rho_econ_moral;
vector[T] rho_civil_moral;

for (n in 1:N)
log_lik[n] <- ordered_logistic_log(y[n],xb[n], segment(kappa, start[kk[n]], n_cuts[kk[n]]));

{
    matrix[D,D] Rho;
    for (t in 1:T) {
    Rho <- multiply_lower_tri_self_transpose(L_Rho[t]);
    rho_econ_civil[t] <- Rho[1,2];
    rho_econ_moral[t] <- Rho[1,3];
    rho_civil_moral[t] <- Rho[2,3];
    }
}
}

"

# n.chains <- 6
# 
# library(rstan)
# rstan_options(auto_write=TRUE)
# options(mc.cores=n.chains)

stan.dat <- list(J=length(unique(long.ord$unique.no)), 
                 K=length(unique(long.ord$item)), 
                 N=nrow(long.ord), 
                 D=3, 
                 T=length(unique(long.ord$year)),
                 jj=long.ord$unique.no, 
                 kk=long.ord$item, 
                 tt=time.id.dat$year,
                 y=long.ord$response, 
                 start=start.cuts, 
                 n_cuts=cuts, 
                 tot_cuts=n.cuts)
stan.pars <- c('gamma','kappa','theta','rho_econ_civil','rho_econ_moral','rho_civil_moral','mu','tau','log_lik')

temp <- stan(model_code=stan.string, data=stan.dat, pars=stan.pars, chains=n.chains,  iter=15000, warmup=10000, thin=20, refresh=100)

setwd(paste0(wd.mcmc,'/dim3_corr_year_sd_mean'))
saveRDS(temp, 'dim3_corr_year_sd_mean.Rda')

for (v in stan.pars) {
    file.name <- paste0(v,'.Rda')
    assign(v, extract(temp, v))
    saveRDS(get(v), file.name)
}



##### Convergence Statistics #####

setwd(paste0(wd.mcmc, '/dim3_corr_year_sd_mean'))
if ('summary' %in% dir()==FALSE) dir.create('summary')


fit <- readRDS('dim3_corr_year_sd_mean.Rda')
stan.pars <- stan.pars[!(stan.pars %in% 'log_lik')]

# stan.pars <- c('gamma','kappa','theta','rho_econ_civil','rho_econ_moral','rho_civil_moral','mu','tau')

setwd(paste0(getwd(),'/summary'))
for (str in stan.pars) {
    temp.str <- paste0('s.',str)
    assign(temp.str, summary(fit, pars=str)$summary[, c('n_eff','Rhat')])
    colMeans(get(temp.str))
    print(get(temp.str))
    saveRDS(get(temp.str), paste0(str,'.Rda'))
}

# discrimination parameters
res.mat <- matrix(nrow=length(stan.pars), ncol=12)
rownames(res.mat) <- c(stan.pars)
stats <- c('(Min)','(25%)', '(50%)', '(Mean)', '(75%)','(Max)')
colnames(res.mat) <- paste(rep(c('eff.N','R.hat'),each=6),stats)
cc<-0
for (str in stan.pars) {
    cc<-cc+1
    temp.res <- readRDS(paste0(str,'.Rda'))
    if (str=='gamma') temp.res <- temp.res[!is.na(temp.res[,2]),]
    summ <- apply(temp.res,2,summary)
    
    res.mat[cc,1:6] <- summ[,1]
    res.mat[cc,7:12] <- summ[,2]
    
}

library(xtable)
p.res.mat <- formatC(rbind(res.mat[,1:6],res.mat[,7:12]), digits=3, format='f')
colnames(p.res.mat) <- substr(colnames(p.res.mat),7,nchar(colnames(p.res.mat)))
p.res.mat <- cbind(paste0("$\\",rownames(p.res.mat),'$'), p.res.mat)
tt <- xtable(p.res.mat, digits=3, align=c(c('l','l'),rep('c',ncol(p.res.mat)-1)))
print.xtable(tt, include.rownames=F, sanitize.text.function=function(x){x})




##### WAIC calculations & Posterior Predictive Checks #####

## WAIC

library(loo)

pp <- c('dim1','dim2_corr','dim2_corr_2','dim2_cross','dim3_corr','dim3_corr_year','dim3_corr_year_sd','dim3_corr_year_sd_mean')

for (ii in pp) {
    setwd(paste0(wd.mcmc,'/',ii))
    ll <- readRDS('log_lik.Rda')
    temp.str <- paste0('waic.',ii)
    assign(temp.str, waic(ll[[1]]))
    rm(ll)
    setwd(wd)
}


w.comp <- compare(waic.dim1, waic.dim2_corr, waic.dim2_corr_2, waic.dim2_cross,waic.dim3_corr,waic.dim3_corr_year, waic.dim3_corr_year_sd, waic.dim3_corr_year_sd_mean)

setwd(wd.mcmc)
if('gof' %in% dir()==FALSE) dir.create('gof')
setwd(paste0(wd.mcmc,'/gof'))
if('waic' %in% dir()==FALSE) dir.create('waic')
wd.res <- paste0(wd,'/MCMC/gof/waic')
setwd(wd.res)
saveRDS(w.comp, 'waic.res.Rda')
waic.res <- readRDS('waic.res.Rda')

waic.mat <- as.matrix(waic.res)
waic.mat <- waic.mat[c(1,2,3,4,8,7,6,5),c(1,3,5)]
rownames(waic.mat) <- paste0('Model',nrow(waic.mat):1)

model.dim <- c(3,3,3,3,2,2,2,1)
model.desc <- matrix(c(rep('Y',3), c('C','Y','Y'), c('C','C','Y'), rep('C',3),rep('C',3),rep('C',3),rep('C',3),c('C','C','---')), ncol=3, byrow=T)

w.mat <- cbind(model.dim, model.desc, formatC(waic.mat[,1:ncol(waic.mat)], digits=2, format='f'))

print(xtable(w.mat), sanitize.text.function=function(x){x})


## PPC

library(doRNG)
library(doParallel)

setwd(wd)

full.dat <- readRDS('full.dat.Rda')
cut.dat <- readRDS('cut.dat.Rda')
i.vars.summ <- readRDS('summary.Rda')


setwd(paste0(wd.mcmc,'/dim3_corr_year_sd_mean/'))

theta.mcmc <- readRDS('theta.Rda')
gamma.mcmc <- readRDS('gamma.Rda')
kappa.mcmc <- readRDS('kappa.Rda')

setwd(paste0(wd,'/MCMC/gof'))
if ('ppc' %in% dir()==FALSE) dir.create('ppc')
setwd(paste0(wd,'/MCMC/gof/ppc'))


temp.dat <- full.dat[full.dat$year>1984 & full.dat$year != 2002,1:43]
n.cats <- apply(temp.dat, 2, function(x){sum(!is.na(unique(x)))})
temp.dat[, n.cats==2] <- temp.dat[,n.cats==2]+1
data.mean <- colMeans(temp.dat, na.rm=T)

gen.rep <- function (x) {
    vec.xb <- xb.dat[,x]
    vec.cuts <- sim.cuts[[x]]
    logis.trans <- plogis(matrix(vec.cuts, nrow=length(vec.xb), ncol=length(vec.cuts), byrow=T) - vec.xb)
    probs <- apply(logis.trans, 1, function(w) diff(c(0,w,1)))
    rep.samp <- apply(probs, 2, function(w) {sample(1:length(w), size=1, prob=w)})
    return(rep.samp)
}

cl <- makePSOCKcluster(10)
registerDoParallel(cl)

n.sims <- 500
rep.set <- foreach (i=1:n.sims, .combine='rbind', .export=c('gen.rep'), .options.RNG=321) %dorng% {
    sims <- nrow(theta.mcmc$theta)
    g1 <- gamma.mcmc$gamma[(sims-n.sims+i),,]
    t1 <- theta.mcmc$theta[(sims-n.sims+i),,]
    k1 <- kappa.mcmc$kappa[(sims-n.sims+i),]
    n.pars <- nrow(g1)
    sim.cuts <- vector('list',n.pars)
    for (p in 1:n.pars) {
        sim.cuts[[p]] <- k1[cut.dat$start.cuts[p]:cut.dat$end.cuts[p]]
    }
    xb.dat <- t1 %*% t(g1)
    
    samp <- sapply(1:n.pars, gen.rep)
    samp[is.na(temp.dat)] <- NA
    res <- colMeans(samp, na.rm=T)
    return(res)
}

saveRDS(rep.set,'rep.set.Rda')
saveRDS(data.mean, 'data.mean.Rda')


library(reshape)
library(ggplot2)
library(grid)
library(gridExtra)

dat.g.mean <- mean(colMeans(temp.dat, na.rm=T))
g.mean <- rowMeans(rep.set)
colnames(rep.set) <- i.vars.summ[,5]
df <- melt(rep.set)
names(df) <- c('draw','variable','mean')

pdf('gof.pdf', width=17, height=12)
g.list <- vector('list', nrow(i.vars.summ))
g.order <- (1:nrow(i.vars.summ))[order(as.numeric(i.vars.summ[,3]))]
for (n in g.order) {
    
    temp <- df[df$variable==i.vars.summ[n,5],]
    p.val <- sum(data.mean[n] > temp[,3])/length(temp[,3])
    print(paste0(i.vars.summ[n,5],': ',p.val))
    if (p.val < .05 | p.val > .95) {
        g.list[[n]] <- ggplot(temp, aes(x=mean)) + geom_histogram(aes(y=..density..), col='grey', alpha=.5) + theme_bw() + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), plot.margin= unit(rep(.2, 4), "lines"), panel.background = element_rect(fill = 'grey80', linetype=2)) + xlim(min(temp[,3])-.01, max(temp[,3])+.01) + ggtitle(i.vars.summ[n,5]) + geom_vline(xintercept=data.mean[n],size=1)
    } else {
        g.list[[n]] <- ggplot(temp, aes(x=mean)) + geom_histogram(aes(y=..density..), col='grey', alpha=.5) + theme_bw() + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), plot.margin= unit(rep(.2, 4), "lines")) + xlim(min(temp[,3])-.01, max(temp[,3])+.01) + ggtitle(i.vars.summ[n,5]) + geom_vline(xintercept=data.mean[n],size=1)
    }
}

grid.arrange(grobs=g.list, ncol=7)

dev.off()


### End of Code ###