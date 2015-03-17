polls.subset <- read.table ("polls.subset.dat")
attach(polls.subset)

 # Load in data for region indicators
 # Use "state", an R data file (type ?state from the R command window for info)
 #
 # Regions:  1=northeast, 2=south, 3=north central, 4=west, 5=d.c.
 # We have to insert d.c. (it is the 9th "state" in alphabetical order)
setwd("/Universe/GitHub/MCMCtab/Key_Model_References/Gelman_Ch19(ParamExpansion)")
data (state)                  # "state" is an R data file
state.abbr <- c (state.abb[1:8], "DC", state.abb[9:50])
dc <- 9
not.dc <- c(1:8,10:51)
region <- c(3,4,4,3,4,4,1,1,5,3,3,4,4,2,2,2,2,3,3,1,1,1,2,2,3,2,4,2,4,1,1,4,1,3,2,2,3,4,1,1,3,2,3,3,4,1,3,4,1,2,4)

 # define other data summaries
ok <- !is.na(bush)
y <- bush[ok]                  # 1 if support bush, 0 if support dukakis
n <- length(y)             # of survey respondents
n.age <- max(age)          # of age categories
n.edu <- max(edu)          # of education categories
n.state <- max(state)      # of states
n.region <- max(region)    # of regions
age.ok <- age[ok]
edu.ok <- edu[ok]
state.ok <- state[ok]
region.ok <- region[ok]
female.ok <- female[ok]
black.ok <- black[ok]

 # also include a measure of previous vote as a state-level predictor

library (foreign)
presvote <- read.dta ("presvote.dta")
attach (presvote)
v.prev <- presvote$g76_84pr
age.edu <- n.edu*(age-1) + edu
age.edu.ok <- age.edu[ok]
n.age.edu <- 16

dataList.2 <- list(N=n, n_age=n.age,n_edu=n.edu,n_state=n.state,
                   n_region=n.region, n_age_edu=n.age.edu,
                   y=y,female=female.ok,black=black.ok,
                   age=age.ok,edu=edu.ok, state=state.ok,region=region,
                   v_prev=v.prev,age_edu=age.edu.ok)

str(dataList.2)
election88_expansion.sf1 <- stan(file='election88_expansion.stan',
                                 data=dataList.2, iter=1000, chains=4)
print(election88_expansion.sf1, pars = c("beta","b_age", "b_edu","b_state","b_region","b_age_edu", "lp__"))


## item response model -- FIXME: DATA??
