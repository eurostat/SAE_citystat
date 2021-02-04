ts_function <- function(population.data,
                        strata.variables,
                        psu.variable,
                        ssu.variable,
                        sampling.fraction1,
                        sampling.fraction2)
{
  sample_stage1 <- unique(population.data, by = c(strata.variables,psu.variable))
  sample_stage1[,sampled:=(1:.N)%in%sample.int(.N,ceiling(sampling.fraction1*.N)),by=strata.variables]
  sample_stage1[,(pi0_name <- paste0("pi_",strata.variables)):=1]
  sample_stage1[,(pi1_name <- paste0("pi_",psu.variable)):=sum(sampled)/.N,by=strata.variables]
  sample_stage1 <-sample_stage1[(sampled),]

  sample <- unique(data, by = c(strata.variables,psu.variable,ssu.variable))[get(psu.variable)%in%sample_stage1[[psu.variable]],]
  sample <- merge(sample,sample_stage1[,.SD,.SDcols = c(strata.variables,psu.variable,pi0_name,pi1_name)], by = c(strata.variables,psu.variable))
  
  sample[,sampled:=(1:.N)%in%sample.int(.N,(sampling.fraction2*.N)),by=c(strata.variables,psu.variable)]
  sample[,(paste0("pi_",ssu.variable)):=sum(sampled)/.N,by=c(strata.variables,psu.variable)]
  sample <-sample[(sampled),]
  sample[,sampled:=NULL]
  
  variables.table <- data.frame('variable'          = c(strata.variables,psu.variable,ssu.variable),
                                'sampling.fraction' = c(rep(1,length(strata.variables)),sampling.fraction1,sampling.fraction2),
                                'allocation'        = c(rep("prop",length(strata.variables)),NA,NA))
  
  setattr(sample,"design",variables.table)
  
  
  return(sample)
}