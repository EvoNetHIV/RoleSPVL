#--------------------------------------------------------------
#package building tools (most users can ignore)

#library(devtools)
#devtools::document(file.path(getwd(),"trunk","evonet"))
#ld=devtools::load_all("trunk/evonet",quiet=T)

#--------------------------------------------------------------
library(evonet)

#--------------------------------------------------------------
# change warnings to errors and open browser on error
#options(warn=2) # turn warnings into error
#options(error=browser) # go into debug mode on error

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

#overide default parameters, re-initialize network with this code
#used as example, users should create own file to source.
source('~/Documents/RoleSPVL/parameters.R')

#add parameters that are functions of other input parameters
evoparams  <- input_parameters_derived(evoparams)

#convert raw parameter list into EpiModel object
evoparams <- do.call(EpiModel::param.net,evoparams)

reldur_vec <- c(50, 100, 500, 800, 1000, 1500, 2000, 2500, 3000)

for(ii in 1:length(reldur_vec)) {
  
  model_name <- paste("evomodel_reldur_",ii,sep="")
  
  evoparams$relation_dur   <- reldur_vec[ii]
  
  nw <- setup_initialize_network(evoparams)
  
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
  
  #will always estimate nw on first iteration of loop
  #and subsequent iterations if "estimate_new_nw == TRUE" 
  #if "estimate_new_nw == FALSE", only single nw estimated for loop
  if(ii==1 || evoparams$estimate_new_nw == TRUE)
    estimated_nw <- do.call(EpiModel::netest, netest_arg_list)
  
  #--------------------------------------------------------------
  
  #-- create initial vector of infection status as an epimodel object
  infected_list <- EpiModel::init.net(i.num=evoparams$initial_infected,
                                      status.rand = FALSE)
  
  #--------------------------------------------------------------
  
  #---  Create list of modules to run for input into epimodel_control_fxn() below
  
  # ***   Note: initialize fxn must always be first and verbose fxn last AND death fxn
  # ***   must precede birth fxn (these conditions may change in future)
  # ***   treatment_fxn must be before update_vl and update_cd4
  
  evo_module_list<- list(
    "initialize.FUN"     = initialize_module,
    "plot_nw.FUN"        = plot_network_fxn,  
    "aging.FUN"          = vital_aging_module,
    "testing.FUN"        = social_testing_diagnosis_module,
    "treatment.FUN"      = social_treatment_module_john,
    "update_vl.FUN"      = viral_update_gamma,
    "update_cd4.FUN"     = viral_update_cd4_daily,
    "coital_acts.FUN"    = social_coital_acts_module,
    "trans.FUN"          = transmission_main_module,
    "trans_book.FUN"     = transmission_bookkeeping_module,
    "trans_cd4.FUN"      = transmission_cd4_module,
    "deaths.FUN"         = vital_deaths_module,
    "births.FUN"         = vital_births_module,
    "social_trans.FUN"   = social_attribute_transition_module,
    "summary.FUN"        = summary_module,
    "resim_nets.FUN"     = EpiModel::resim_nets,
    "verbose.FUN"        = NULL)
  
  
  #--- call epimodel's control fxn (load evonet modules into epimodel)
  evocontrol <- setup_epimodel_control_object(evonet_params = evoparams,
                                              module_list   = evo_module_list)
  
  #--------------------------------------------------------------
  
  runtime <- system.time({
    
    evomodel  <- EpiModel::netsim(x = estimated_nw,
                                  param = evoparams,
                                  init = infected_list,
                                  control = evocontrol)
  })
  
  print(runtime)
  
  assign(model_name,evomodel)
  remove(evomodel)
  file_name <- paste(model_name,".RData",sep="")
  save(list=model_name,
       file = file.path(evoparams$output_path,file_name ))
  
  if(evoparams$save_vl_list){
    plot_vl_trajectories(model=get(model_name),sim=1,
                         outpath=evoparams$output_path,
                         name=model_name)
  }  
  
  if(evoparams$save_coital_acts){
    plot_disc_coital_acts_history(model=get(model_name),
                                  outpath=evoparams$output_path,
                                  name=model_name)
  }
  
  
  #--------------------------------------------------------------
  
}#end of loop
#--------------------------------------------------------------

# change error setting back to default
options(error=NULL)
options(warn=1)
