setwd("C:\\holger\\SEM\\modelfit\\stanversion")

library(mvtnorm)
library(matrixcalc)
library(lavaan)

library(foreach)
library(doParallel)

registerDoParallel(16) 

#detectCores()
################################################################
est <- 
  foreach (i=1:16) %dopar% {
    library(mvtnorm)
    library(matrixcalc)
    library(lavaan)
    
    
    #setwd("C:\\holger\\SEM\\modelfit\\stanversion")
    source("C:/holger/SEM/modelfit/stanversion/funs/lavaan_dsem_models.R")
    source("C:/holger/SEM/modelfit/stanversion/funs/lavaan_dsem_models_comp1.R")
    source("C:/holger/SEM/modelfit/stanversion/funs/lavaan_dsem_models_comp2.R")
    source("C:/holger/SEM/modelfit/stanversion/funs/gen_data_version01.R")
    
    ##########################
    person_size_SIMULATE <- c(31, 61, 91,121, 151, 181,211)
    time_point_SIMULATE <- c(1:5,10,15) # Nt
    #model_TRUE_MISS_SIMULATE <- c(0, 0.3)
    model_TRUE_MISS_SIMULATE <- 0.3
    run_Samples_SIMULATE <- 7
    ##########################
    
    
    ##########################
    phi0 <- diag(2)*.5+.5 # cov(eta)
    mu0  <- c(0,0)        #mean(eta)
    ar0  <- c(.5,.5)      # ar(1) structure
    ly0  <- matrix(c(1,1,1,0,0,0,
                     0,0,0,1,1,1),6,2,byrow=F) # factor loadings
    td   <- diag(6)*.25 # cond. var(y) -> res var
    ##########################
    
    #set core specific seed
    set.seed(131212023+i)
    
    for (person_size in person_size_SIMULATE) {
      for (time_point in time_point_SIMULATE) {
        for (model_TRUE_MISS in model_TRUE_MISS_SIMULATE){
        #person_size <- 200
        #time_point <- 15
        #model_TRUE_MISS <- .3
        #model_TRUE_MISS <- .0
        #######################################
        if(time_point*6<person_size){
          # empty matrix for results
          fitm1 <- fitm2 <- fitm3 <- matrix(NA,run_Samples_SIMULATE,46)
          fitm12 <- fitm13 <- matrix(NA,run_Samples_SIMULATE,2)
          # set N and Nt and missfit
          N <- person_size  # persons
          Nt <- time_point
          #if(time_point<5){Nt<-5}else{Nt <- time_point}
          ly1  <- matrix(c(0,0,0,0,0,model_TRUE_MISS,
                           0,0,0,0,0,0),6,2,byrow=F) # factor loadings
          
          #######################################
          # start looping over 
          for (SAMPLING in 1:run_Samples_SIMULATE){ ### All Samples run !!!!!!!!!!!!!!!!!
            #SAMPLING <-1
            dat1 <- gendata01(N,Nt,phi0,mu0,ar0,ly0,ly1,td)
            
            # check pos def. again
            if(dat1[["is_positive_def"]]==T){
              
              y0 <- dat1[["y0"]]
              #dim(y0)
              # loop with error back-up
              tryCatch({
                res1 <- sem(dsem[[time_point]], data=y0,std.lv = TRUE)
                res2 <- sem(dsem.mf[[time_point]], data=y0,std.lv = TRUE)
                res3 <- sem(dsem.ar[[time_point]], data=y0,std.lv = TRUE)
                
                #summary(res1)
                #summary(res2)
                #summary(res3)
                diff1 <- anova(res1,res2)
                diff2 <- anova(res1,res3)
                
                fitm12[SAMPLING,] <- c(diff1$`Chisq diff`[2],diff1$`Pr(>Chisq)`[2])
                fitm13[SAMPLING,] <- c(diff2$`Chisq diff`[2],diff2$`Pr(>Chisq)`[2])
                
                fitm1[SAMPLING,] <- fitmeasures(res1)
                fitm2[SAMPLING,] <- fitmeasures(res2)
                fitm3[SAMPLING,] <- fitmeasures(res3)
              },
              error=function(error_message) {
                #return(res1 <- NA)
              })
              
              
            }#end pos.def
          }# end looping sampling
          #colnames(fitm1) <- names(fitmeasures(res1))
          #######################################
          
          # save files outside
          name_local_SIMULATE_Info <- paste("C:\\holger\\SEM\\modelfit\\stanversion\\results_lavaan_version01a\\local", as.character(person_size), as.character(time_point),as.character(model_TRUE_MISS),i ,"_version01", sep = "_")
          #write.csv(fitm1, file = name_local_SIMULATE_Info, row.names = FALSE) # Save samples run
          saveRDS(list(fitm1,fitm2,fitm3,fitm12,fitm13), file = paste0(name_local_SIMULATE_Info,".RDS")) # Save samples run
          
        }
        }
      }# end of if identification
    }
  }

