
hyak=T
hyak_par=T

#hyak=T, hyak_par=F => Hyak interactive
#hyak=T, hyak_par=T => Hyak parallel PBS 
#hyak=F, hyak_par=F => non-hyak job (local or linux,windows server)  

if(!isTRUE(hyak) & isTRUE(hyak_par)){stop("hyak flags incorrect")}

hyak_path= '/gscratch/csde/sestans/conc39'
local_path="~/Hyak/Concurrency_Runs/Run39"

if(hyak)outpath=hyak_path else 
  outpath=local_path

#--------------------------------------------------------------
library(evonet)
library(EpiModelHPC)
#--------------------------------------------------------------
#Load default parameters

primary_parameters  <- input_parameters_primary()
cd4_data            <- input_parameters_cd4_data()

#--- combine individual parameters into single list
evoparams <- c(primary_parameters, cd4_data)

#--------------------------------------------------------------
evoparams$nsims            = 16

if(hyak_par){
  evoparams$ncores=16
}else{evoparams$ncores=1}

evoparams$popsumm_frequency =25
evoparams$n_steps           = 365*50
evoparams$initial_pop       = 5000
evoparams$initial_infected  = 500
evoparams$birth_model       = "poisson_birth_numbers"
evoparams$poisson_birth_lambda = 0.0137*(evoparams$initial_pop/100)
evoparams$trans_RR_age         = 1.0
evoparams$target_stats         = 5000*0.7/2
evoparams$relation_dur  			      	= 200
evoparams$trans_RR_insertive_anal_msm = 2.9  
evoparams$trans_RR_receptive_anal_msm = 17.3
evoparams$transmission_model  	      = "hughes"
evoparams$nw_form_terms = "~edges + concurrent + offset(nodematch('role', diff=TRUE, keep=1:2))"  
evoparams$age_dist      <- seq(50, 10, -10/9)/1110

#add parameters that are functions of other input parameters
evoparams  <- input_parameters_derived(evoparams)

#convert raw parameter list into EpiModel object
evoparams <- do.call(EpiModel::param.net,evoparams)

#params to loop through----------------------------
target_stats_vec <- c(0,250,500,750,1000)
#output name
suffix=(rep("",5))
model_names=paste("run_concurrency_level_",target_stats_vec,suffix,sep="")

#estiamted_nw object names
nw_names=c("evo_nw_conc0.RDATA","evo_nw_conc5.RDATA","evo_nw_conc10.RDATA","evo_nw_conc15.RDATA",
           "evo_nw_conc20.RDATA")

#make sure param vecs same length


for(ii in 1:length(target_stats_vec))
{  
  
  load(file.path(outpath,nw_names[ii]))
  
  model_name = model_names[ii]
  evoparams$target_stats   <- c(1750, target_stats_vec[ii])
  
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
    #    "plot_nw.FUN"        = plot_network_fxn,  
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
  
  if(isTRUE(hyak_par)){
    evomodel  <- EpiModelHPC::netsim_par(x = estimated_nw, 
                                         param = evoparams, 
                                         init = infected_list, 
                                         control = evocontrol)
  }else{
    evomodel  <- EpiModel::netsim(x = estimated_nw,
                                  param = evoparams,
                                  init = infected_list,
                                  control = evocontrol)
  }
  
  evomodel$epi <- NULL
  evomodel$stats <- NULL
  evomodel$control <- NULL
  
  
  plots_popsumm(evomodel,outpath=outpath,
                name=model_name,nw_stats=TRUE,max_points_rep=100,
                evoparams$popsumm_frequency)

  assign(model_name,evomodel)
  file_name <- paste(model_name,".RData",sep="")
  save(list=model_name,
       file = file.path(outpath,file_name) )
  remove(evomodel)
  
}#end of loop

#--------------------------------------------------------------
