###############################################################################
##                                                                           ##
##  Compile STAN models                                                      ##
##                                                                           ##
###############################################################################


## Basic Setup ----------------------------------------------------------------

if (!exists('wd.base'))
   stop('Specify wd.base!')

# set paths
if (!exists('code.path'))
   code.path <- paste0(wd.base, 'code/')
if (!exists('data.path'))
   data.path <- paste0(wd.base, 'data/')
if (!exists('raw.data.path')) 
   raw.data.path <- paste0(wd.base, 'raw_data/')
if (!exists('graphics.path')) 
   graphics.path <- paste0(wd.base, 'plots/')
if (!exists('stan.path'))
   stan.path <- paste0(wd.base, 'stan/')

# set path to stan models
stan.model.path <- paste0(stan.path, 'models/')

# set capture.output NULL file
if (.Platform$OS.type=='windows') {
   c.out <- 'NUL'
} else c.out <- '/dev/null'


## Compile Models -------------------------------------------------------------

message('\n\nCompiling STAN Models ---------------')

if (!dir.exists(stan.model.path)) {
   
   message(
      paste0('Creating "~/models/" directory into ',
             stan.path)
   )
   dir.create(stan.model.path,
              recursive = T)
}

message('Compiling Normal-Normal Model ...')

### Normal-Normal Model

# code
nn.code <- "
data {

   int<lower=1> N;               // no. obs.
   int<lower=1> K;               // predictors
   int<lower=1> J;               // no. groups
   int<lower=1> jj[N];           // group index
   vector[K] x[N];               // predictor matrix
   real<lower=0, upper=1> y[N];  // outcome
}

parameters {

   vector[K] gamma_star[J];       
   vector[K] mu_gamma;
   vector<lower=0>[K] sigma_gamma;
   cholesky_factor_corr[K] L_rho; 
   real<lower=0> sigma_e;              

}

transformed parameters {

   vector[K] gamma[J];
   real mu[N];

   {
      matrix[K,K] L_Sigma;
      L_Sigma = diag_pre_multiply(sigma_gamma, L_rho);
		
      for (j in 1:J) 
         gamma[j] = mu_gamma + L_Sigma * gamma_star[j];
   }


   // linear predictor
   for (i in 1:N) 
      mu[i] = dot_product(gamma[jj[i]], x[i]);
   
}

model {

   // priors
   for (j in 1:J) 
      gamma_star[j] ~ normal(0,1);
   mu_gamma ~ normal(0,1);
   sigma_gamma ~ normal(0,1);
   L_rho ~ lkj_corr_cholesky(2.0);
   sigma_e ~ normal(0,1);

   // likelihood
   y ~ normal(mu, sigma_e);

}

generated quantities {
   real log_lik[N];
   corr_matrix[K] Rho;

   for (i in 1:N) 
      log_lik[i] = normal_lpdf(y[i] | mu[i],sigma_e);
		
   Rho = multiply_lower_tri_self_transpose(L_rho);
}
"

# compile and save
capture.output(
	{
		nn.model <- stan_model(
   		   model_code=nn.code,
   		   model_name='Normal-Normal'
		   )
	},
	file=c.out
)

saveRDS(nn.model,
        paste0(stan.model.path,
               'stan.nn.model.rds'))

### Beta-Normal Model
message('Compiling Beta-Normal Model ...')

bn.code <- "
data {

   int<lower=1> N;               // no. obs.
   int<lower=1> K;               // predictors
   int<lower=1> J;               // no. groups
   int<lower=1> jj[N];           // group index
   vector[K] x[N];               // predictor matrix
   real<lower=0, upper=1> y[N];  // outcome
	
}

parameters {
   
   vector[K] gamma_star[J];       
   vector[K] mu_gamma;
   vector<lower=0>[K] sigma_gamma;
   cholesky_factor_corr[K] L_rho; 
   real<lower=0> nu;    

   // Note: the parameter nu corresponds to 1/nu in the paper

}

transformed parameters {

   vector[K] gamma[J];
   vector<lower=0>[N] alpha;
   vector<lower=0>[N] beta;

   {
      matrix[K,K] L_Sigma;
      L_Sigma = diag_pre_multiply(sigma_gamma, L_rho);
		
      for (j in 1:J) 
         gamma[j] = mu_gamma + L_Sigma * gamma_star[j];
   }

   {
      vector[N] mu;
 
      // generate linear predictor 
      for (i in 1:N) 
         mu[i] = inv_logit(dot_product(gamma[jj[i]], x[i]));
      
      // transform into original two shape parameters
      alpha = mu * (1/nu);
      beta = (1-mu) * (1/nu);
   }
}

model {

   // priors
   for (j in 1:J) 
      gamma_star[j] ~ normal(0,1);
   mu_gamma ~ normal(0,5);
   sigma_gamma ~ cauchy(0,3);
   L_rho ~ lkj_corr_cholesky(2.0);
   nu ~ cauchy(0,3); 
   
   // likelihood
   y ~ beta(alpha,beta);

}

generated quantities {
   real log_lik[N];
   corr_matrix[K] Rho;
   
   for (i in 1:N) 
   log_lik[i] = beta_lpdf(y[i] | alpha[i],beta[i]);
   Rho = multiply_lower_tri_self_transpose(L_rho);
}
"


capture.output(
	{
		bn.model <- stan_model(
   		      model_code=bn.code,
   		      model_name='Beta-Normal'
		      )
	},
	file=c.out
)

saveRDS(bn.model,
        paste0(stan.model.path,
               'stan.bn.model.rds'))

message('Done!\n')

### END OF CODE ###

