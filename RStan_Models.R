### Set Working Direct, Clear Environment, and Load Packages
  setwd("/Universe/GitHub/MCMCtab")
  source("check_packages.R")
  check_packages(c("rstan","rjags", "R2jags", "xtable", "foreign", "data.table", "parallel", "dplyr", "epicalc","gmodels", "foreign", "stringr", "knitr"))
  detachAllData( ); rm(list=ls())

### Load in 2006 data with 25 countries
  cdata <- read.csv( "Data/06data_25cntries.csv" , header = T)

### Subset Variables for models
  imm_vars <- c("foreignpct", "migpct", "foreigndif", "dforeignpctnew")

  for (i in imm_vars){
    c.data <- cdata[,c("dspendlaw", "cntry",  "police", "homicide", i, "inczscore", "age", "agesq", "female", "nevermar", "divorced", "widow", "kidshh", "rural", "suburb", "lesshs", "univ", "ptemp", "unemp", "nolabor", "selfemp", "pubemp", "protestant", "catholic", "otherrel", "relreligion","foreignpct", "migpct", "foreigndif", "dforeignpctnew" )]

### Attach data
  attach(c.data)

### Build Matrix for Predictors
  x   <- data.frame(cbind(c.data[,3:ncol(c.data)]))
### Number of countries (level 2 units)
  M  <- length(unique(cntry))
### Number of predictors
  K     <- dim(x)[2]
### Recode Country Values as sequential (still need to equate with no labels)
  levels(cntry) <- seq(1,25,1)
  cntry <- as.numeric(as.character(cntry))

### Prepare data for Stan
  polimm_dat  <- list(N= length(dspendlaw), M=M, K=K, y=dspendlaw, x=x,g=cntry)

### Code the Model
  mod_code <- '
  data {
      int N; // number of obs 
      int M; // number of countries
      int K; // number of predictors
      
      int y[N]; // outcome
      row_vector[K] x[N]; // predictors
      int g[N];    // map obs to countries
  }
  parameters {
      real alpha;
      real a[M]; 
      vector[K] beta;
      real sigma;  
  }
  model {
    alpha ~ normal(0,100);
    a ~ normal(0,sigma);
    beta ~ normal(0,100);
    for(n in 1:N) {
      y[n] ~ bernoulli(inv_logit( alpha + a[g[n]] + x[n]*beta));
    }
  }'

### Random Effects Logistic Regression using the Hamiltonian Monte Carlo (HMC) algorithm called the "No-U Turn Sampler (NUTS)" of Hoffman and Gelman (2011) for 2006 w/25 countries including foreignpct, migpct, homicide, and police & individual controls
  # Translate model code to C++
    stk_mig_hp_06 <- stan(model_code=mod_code, model_name="Stock and MigPct w/Police and Homicide 2006", data=polimm_dat, chains = 0)

  # Run two HMC chains in parallel for 1000 iterations # WARNING runs for >10min
    CL = makeCluster(2, outfile = 'Local_Only/parallel.log')
    clusterExport(cl = CL, c("polimm_dat", "stk_mig_hp_06")) 
    sflist <- mclapply(1:2, mc.cores = 2, function(i)  stan(fit = stk_mig_hp_06, data=polimm_dat, chains=1, iter=1000,chain_id = i))
    stopCluster(CL)

  # Merge the two HMC chains 
    fit <- sflist2stanfit(sflist)

  # Save Model 
    save(fit, file= "Model_Results/stk_mig_ph06.dat")

  # Create diagnostic plots 
    print(fit)
    pdf("Plots/stk_mig_ph06_MCMC.pdf")
    t <- rstan::traceplot(fit)
    dev.off()
