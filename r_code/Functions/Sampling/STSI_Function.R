stsi_function <- function(population.data,
                          strata.variables,
                          psu.variable,
                          ssu.variable,
                          sampling.fraction1,
                          sampling.fraction2)
{
  
  sample <- unique(population.data, by = c(strata.variables,psu.variable))
  sample[,sampled:=(1:.N)%in%sample.int(.N,ceiling(sampling.fraction1*.N)),by=strata.variables]
  sample[,(paste0("pi_",strata.variables)):=1]
  sample[,(paste0("pi_",psu.variable)):=sum(sampled)/.N,by=strata.variables]
  sample <-sample[(sampled),]

  sample[,sampled:=NULL]
  
  variables.table <- data.frame('variable'          = c(strata.variables,psu.variable),
                                'sampling.fraction' = c(rep(1,length(strata.variables)),sampling.fraction1),
                                'allocation'        = c(rep("prop",length(strata.variables)),NA))
  
  setattr(sample,"design",variables.table)
  
  return(sample)
}