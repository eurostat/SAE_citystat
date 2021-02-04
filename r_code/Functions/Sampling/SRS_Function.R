srs_function <- function(population.data,
                         strata.variables,
                         psu.variable,
                         ssu.variable,
                         sampling.fraction1,
                         sampling.fraction2)
  
{
  sample <- unique(population.data, by = psu.variable)
  sample[,sampled:=.I%in%sample.int(.N,sampling.fraction1*.N)]
  sample[,(paste0("pi_",psu.variable)):=sum(sampled)/.N]
  sample <-sample[(sampled),]
  sample[,sampled:=NULL]
  
  variables.table <- data.frame('variable'          = psu.variable,
                                'sampling.fraction' = sampling.fraction1,
                                'allocation'        = NA)
  
  setattr(sample,"design",variables.table)
  
  return(sample)
}



