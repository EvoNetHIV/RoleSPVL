#script to 1) estimate and 2) save to file a "network" object to use in evonet
#this step can not be done in hyak and must be done on linux and then object transferred
# to hyak
#
# NOTE! Very important!!!!
# "network" related parameters used to estimate network here need to be same on hyak run
# These "network" parameters include (if different from default)
# initital population size, target stats, relationship duration, role ,
# generic attribute, nw_form_terms, nw_coef_form, nw_constraints

#--------------------------------------------------------------
library(evonet)

#--------------------------------------------------------------
# change warnings to errors and open browser on error
options(warn=2) # turn warnings into error
options(error=browser) # go into debug mode on error

# change error setting back to default if desired
#options(error=NULL)
#options(warn=1)

#--------------------------------------------------------------
#Load default parameters

primary_parameters  <- input_parameters_primary()
cd4_data            <- input_parameters_cd4_data()

#--- combine individual parameters into single list
evoparams <- c(primary_parameters, cd4_data)
#--------------------------------------------------------------

# Creates values corresponding to 0-20% of nodes currently in concurrent partnerships
target_stats_vec <- c(0,250,500,750,1000)
modname_vec=c("conc0","conc5","conc10","conc15","conc20")

for(ii in 1:length(target_stats_vec)){
evoparams$initial_pop       = 5000
evoparams$initial_infected  = 500
evoparams$birth_model       = "poisson_birth_numbers"
evoparams$poisson_birth_lambda = 0.685
evoparams$trans_RR_age         = 1.0
evoparams$target_stats         = 5000*0.7/2
evoparams$relation_dur  			      	= 200
evoparams$trans_RR_insertive_anal_msm = 2.9  
evoparams$trans_RR_receptive_anal_msm = 17.3
evoparams$transmission_model  	      = "hughes"
evoparams$Heritability                = 0.36     #0.36 default
evoparams$VarianceLogSP0              = 0.8      #0.8 default
evoparams$nw_form_terms = "~edges + concurrent + offset(nodematch('role', diff=TRUE, keep=1:2))"  

evoparams$target_stats   <- c(1750, target_stats_vec[ii])
modname=modname_vec[ii]
#add parameters that are functions of other input parameters
evoparams  <- input_parameters_derived(evoparams)

#convert raw parameter list into EpiModel object
evoparams <- do.call(EpiModel::param.net,evoparams)


#--------------------------------------------------------------
# initialize network

nw <- setup_initialize_network(evoparams)

#--------------------------------------------------------------

#run qaqc on input parameters
input_parameters_qaqc(evoparams)

#--------------------------------------------------------------

#estimate initial network (create argument list, then call fxn)
netest_arg_list <- list(
  nw            =  nw,
  formation     =  as.formula(evoparams$nw_form_terms),
  target.stats  =  evoparams$target_stats,
  coef.form     =  evoparams$nw_coef_form,
  constraints   =  as.formula(evoparams$nw_constraints),
  verbose       =  FALSE,
  coef.diss     =  dissolution_coefs( dissolution =  ~offset(edges),
                                      duration    =  evoparams$relation_dur,
                                      d.rate      =  3e-05) )

estimated_nw <- do.call(EpiModel::netest, netest_arg_list)

#--------------------------------------------------------------

save(estimated_nw,
     file = paste("evo_nw_",modname,".RDATA",sep=""))
remove(estimated_nw)
}
# change error setting back to default
options(error=NULL)
options(warn=1)
