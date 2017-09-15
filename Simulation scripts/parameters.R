##Default script n= 1000

evoparams$nsims            = 25
evoparams$n_steps          = 365*10

evoparams$initial_pop       = 1000
evoparams$initial_infected  = 200
evoparams$birth_model       = "poisson_birth_numbers"
evoparams$poisson_birth_lambda = 0.1370
evoparams$target_stats      = 1000*0.7/2

evoparams$Heritability                = 0.5
evoparams$condom_prob                 = 0
evoparams$mean_sex_acts_day           = 0.4
evoparams$trans_RR_insertive_anal_msm = 1.8  
evoparams$trans_RR_receptive_anal_msm = 23
evoparams$transmission_model  	      = "hill"

evoparams$popsumm_frequency = 10
evoparams$output_path <- getwd()



