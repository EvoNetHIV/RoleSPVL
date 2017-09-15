# Sexual role and HIV-1 set point viral load among men who have sex with men

This repository contains the code to reproduce the analysis of how sexual role impacts set point viral load (SPVL) evolution.     

This model is written in the R programming language.  It requires [EpiModel](http://www.epimodel.org), the epidemic modeling software, to be installed first, as well as [EvoNetHIV](https://github.com/EvoNetHIV).      

Within R:  
`install.packages("EpiModel")`     
`if(!require(devtools)) { install.packages("devtools") }`     
`library(devtools)`         
`devtools::install_github("EvoNetHIV/RoleSPVL")`     

### Citation
> Stansfield SE, Mittler JE, Gottlieb GS, Murphy JT, Hamilton DT, Detels R, Wolinsky SM, Jacobson LP, Margolick JB, Rinaldo CR, Herbeck JT, Goodreau SM. (submitted). Sexual role and HIV-1 set point viral load among men who have sex with men. Journal of Infectious Diseases.

### Abstract
*Background*     
HIV-1 set point viral load (SPVL) is a highly variable trait that influences disease progression and transmission risk.  Men who are exclusively insertive (EI) during anal intercourse require more sexual contacts to become infected than exclusively receptive (ER) men. Thus, we hypothesize that EIs are more likely to acquire their viruses from highly infectious partners (i.e., with high SPVLs) and to have higher SPVLs than infected ERs.     
*Methods*    
We used a one-generation Bernoulli model, a dynamic network model, and data from the Multicenter AIDS Cohort Study (MACS) to examine whether and under what circumstances MSM differ in SPVL by sexual role.     
*Results*    
Both models predicted higher SPVLs in EIs than role versatile (RV) or ER men, but only in scenarios where longer-term relationships predominated.  ER and RV men displayed similar SPVLs. When the MACS data were limited by some estimates of lower sex partner counts (a proxy for longer relationships), EI men had higher SPVLs; these differences were clinically relevant (>0.3 log10 copies/mL) and statistically significant (p<0.05).     
*Conclusions*    
Mode of acquisition may be an important aspect of SPVL evolution in MSM, with clinical implications.      
    
*Keywords*     
HIV-1, network modeling, mathematical modeling, men who have sex with men (MSM), sexual role, MACS study, set point viral load

