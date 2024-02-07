



#par(mfrow = c(3, 5))
#plot(list.IG.means[[1]]$alpha_on_W1)
#plot(list.IG.means[[1]]$alpha_on_W2)
#plot(list.IG.means[[1]]$alpha_var)
#plot(list.IG.means[[1]]$beta_on_W1)
#plot(list.IG.means[[1]]$beta_on_W2)
#plot(list.IG.means[[1]]$beta_var)
#plot(list.IG.means[[1]]$deviance)
#plot(list.IG.means[[1]]$int_beta)
#plot(list.IG.means[[1]]$int_phi)
#plot(list.IG.means[[1]]$lnV_on_W1)
#plot(list.IG.means[[1]]$lnV_on_W2)
#plot(list.IG.means[[1]]$ln_var_var)
#plot(list.IG.means[[1]]$phi_on_W1)
#plot(list.IG.means[[1]]$phi_on_W2)
#plot(list.IG.means[[1]]$phi_var)



{list.IG.means[[1]]$alpha_on_W1<- unlist(lapply(list.IG.means[[1]]$alpha_on_W1 , function(x) {
  x[abs(x) >= 10] <- NA  # Replace values with absolute value >= 10 with NA
  return(x)
}))


  list.IG.means[[1]]$beta_on_W2<- unlist(lapply(list.IG.means[[1]]$beta_on_W2 , function(x) {
  x[abs(x) >= 10] <- NA  # Replace values with absolute value >= 10 with NA
  return(x)
}))
list.IG.means[[1]]$int_beta <- unlist(lapply(list.IG.means[[1]]$int_beta , function(x) {
  x[abs(x) >= 10] <- NA  # Replace values with absolute value >= 10 with NA
  return(x)
}))
list.IG.means[[1]]$beta_on_W1 <- unlist(lapply(list.IG.means[[1]]$beta_on_W1 , function(x) {
  x[abs(x) >= 10] <- NA  # Replace values with absolute value >= 10 with NA
  return(x)
}))

list.IG.means[[1]]$alpha_var <- unlist(lapply(list.IG.means[[1]]$alpha_var , function(x) {
  x[abs(x) >= 10] <- NA  # Replace values with absolute value >= 10 with NA
  return(x)
}))

list.IG.means[[1]]$alpha_on_W2 <- unlist(lapply(list.IG.means[[1]]$alpha_on_W2 , function(x) {
  x[abs(x) >= 10] <- NA  # Replace values with absolute value >= 10 with NA
  return(x)
}))

list.IG.means[[1]]$ln_var_var <- unlist(lapply(list.IG.means[[1]]$ln_var_var , function(x) {
  x[abs(x) >= 6] <- NA  # Replace values with absolute value >= 10 with NA
  return(x)
}))

na_positions <- which(is.na(list.IG.means[[1]]$alpha_on_W2)|is.na(list.IG.means[[1]]$beta_on_W2)
                      |is.na(list.IG.means[[1]]$int_beta)|is.na(list.IG.means[[1]]$beta_on_W1)
                      |is.na(list.IG.means[[1]]$alpha_var)|is.na(list.IG.means[[1]]$ln_var_var)
                      |is.na(list.IG.means[[1]]$alpha_on_W1))
}
list.IG.means[[1]]$alpha_on_W1 <-list.IG.means[[1]]$alpha_on_W1[-na_positions]
list.IG.means[[1]]$alpha_on_W2 <-list.IG.means[[1]]$alpha_on_W2[-na_positions]
list.IG.means[[1]]$alpha_var<-list.IG.means[[1]]$alpha_var[-na_positions]
list.IG.means[[1]]$beta_on_W1 <-list.IG.means[[1]]$beta_on_W1[-na_positions]
list.IG.means[[1]]$beta_on_W2 <-list.IG.means[[1]]$beta_on_W2[-na_positions]
list.IG.means[[1]]$beta_var   <-list.IG.means[[1]]$beta_var[-na_positions]
list.IG.means[[1]]$deviance <-list.IG.means[[1]]$deviance[-na_positions]
list.IG.means[[1]]$int_beta <-list.IG.means[[1]]$int_beta[-na_positions]
list.IG.means[[1]]$int_phi<-list.IG.means[[1]]$int_phi[-na_positions]
list.IG.means[[1]]$lnV_on_W1<-list.IG.means[[1]]$lnV_on_W1[-na_positions]
list.IG.means[[1]]$lnV_on_W2<-list.IG.means[[1]]$lnV_on_W2[-na_positions]
list.IG.means[[1]]$ln_var_var <-list.IG.means[[1]]$ln_var_var[-na_positions]
list.IG.means[[1]]$phi_on_W1  <- list.IG.means[[1]]$phi_on_W1[-na_positions]
list.IG.means[[1]]$phi_on_W2<- list.IG.means[[1]]$phi_on_W2[-na_positions]
list.IG.means[[1]]$phi_var  <- list.IG.means[[1]]$phi_var[-na_positions]



{list.IG.means[[2]]$alpha_on_W1<- unlist(lapply(list.IG.means[[2]]$alpha_on_W1 , function(x) {
  x[abs(x) >= 10] <- NA  # Replace values with absolute value >= 10 with NA
  return(x)
}))



  list.IG.means[[2]]$beta_on_W2<- unlist(lapply(list.IG.means[[2]]$beta_on_W2 , function(x) {
  x[abs(x) >= 10] <- NA  # Replace values with absolute value >= 10 with NA
  return(x)
}))
  list.IG.means[[2]]$int_beta <- unlist(lapply(list.IG.means[[2]]$int_beta , function(x) {
    x[abs(x) >= 10] <- NA  # Replace values with absolute value >= 10 with NA
    return(x)
  }))
  list.IG.means[[2]]$beta_on_W1 <- unlist(lapply(list.IG.means[[2]]$beta_on_W1 , function(x) {
    x[abs(x) >= 10] <- NA  # Replace values with absolute value >= 10 with NA
    return(x)
  }))

  list.IG.means[[2]]$alpha_var <- unlist(lapply(list.IG.means[[2]]$alpha_var , function(x) {
    x[abs(x) >= 10] <- NA  # Replace values with absolute value >= 10 with NA
    return(x)
  }))

  list.IG.means[[2]]$alpha_on_W2 <- unlist(lapply(list.IG.means[[2]]$alpha_on_W2 , function(x) {
    x[abs(x) >= 10] <- NA  # Replace values with absolute value >= 10 with NA
    return(x)
  }))

  list.IG.means[[2]]$ln_var_var <- unlist(lapply(list.IG.means[[2]]$ln_var_var , function(x) {
    x[abs(x) >= 6] <- NA  # Replace values with absolute value >= 10 with NA
    return(x)
  }))

na_positions_2 <- which(is.na(list.IG.means[[2]]$alpha_on_W2)|is.na(list.IG.means[[2]]$beta_on_W2)
                        |is.na(list.IG.means[[2]]$int_beta)|is.na(list.IG.means[[2]]$beta_on_W1)
                        |is.na(list.IG.means[[2]]$alpha_var)|is.na(list.IG.means[[2]]$ln_var_var)
                        |is.na(list.IG.means[[2]]$alpha_on_W2))

}
list.IG.means[[2]]$alpha_on_W1 <-list.IG.means[[1]]$alpha_on_W1[-na_positions_2]
list.IG.means[[2]]$ln_var_var<-list.IG.means[[1]]$ln_var_var[-na_positions_2]
list.IG.means[[2]]$alpha_on_W2 <-list.IG.means[[1]]$alpha_on_W2[-na_positions_2]
list.IG.means[[2]]$alpha_var<-list.IG.means[[1]]$alpha_var[-na_positions_2]
list.IG.means[[2]]$beta_on_W1 <-list.IG.means[[1]]$beta_on_W1[-na_positions_2]
list.IG.means[[2]]$beta_on_W2 <-list.IG.means[[1]]$beta_on_W2[-na_positions_2]
list.IG.means[[2]]$beta_var   <-list.IG.means[[1]]$beta_var[-na_positions_2]
list.IG.means[[2]]$deviance <-list.IG.means[[1]]$deviance[-na_positions_2]
list.IG.means[[2]]$int_beta <-list.IG.means[[1]]$int_beta[-na_positions_2]
list.IG.means[[2]]$int_phi<-list.IG.means[[1]]$int_phi[-na_positions_2]
list.IG.means[[2]]$lnV_on_W1<-list.IG.means[[1]]$lnV_on_W1[-na_positions_2]
list.IG.means[[2]]$lnV_on_W2<-list.IG.means[[1]]$lnV_on_W2[-na_positions_2]
list.IG.means[[2]]$ln_var_var <-list.IG.means[[1]]$ln_var_var[-na_positions_2]
list.IG.means[[2]]$phi_on_W1  <- list.IG.means[[1]]$phi_on_W1[-na_positions_2]
list.IG.means[[2]]$phi_on_W2<- list.IG.means[[1]]$phi_on_W2[-na_positions_2]
list.IG.means[[2]]$phi_var  <- list.IG.means[[1]]$phi_var[-na_positions_2]

{list.IG.means[[3]]$alpha_on_W1<- unlist(lapply(list.IG.means[[3]]$alpha_on_W1 , function(x) {
  x[abs(x) >= 10] <- NA  # Replace values with absolute value >= 10 with NA
  return(x)
}))


list.IG.means[[3]]$alpha_var <- unlist(lapply(list.IG.means[[3]]$alpha_var , function(x) {
  x[abs(x) >= 10] <- NA  # Replace values with absolute value >= 10 with NA
  return(x)
}))

list.IG.means[[3]]$alpha_on_W2 <- unlist(lapply(list.IG.means[[3]]$alpha_on_W2 , function(x) {
    x[abs(x) >= 10] <- NA  # Replace values with absolute value >= 10 with NA
    return(x)
  }))

na_positions_3 <- which(is.na(list.IG.means[[3]]$alpha_on_W2)|is.na(list.IG.means[[3]]$beta_on_W2)
                        |is.na(list.IG.means[[3]]$int_beta)|is.na(list.IG.means[[3]]$beta_on_W1)
                        |is.na(list.IG.means[[3]]$alpha_var)|is.na(list.IG.means[[3]]$ln_var_var)
                        |is.na(list.IG.means[[3]]$alpha_on_W1))

}
list.IG.means[[3]]$alpha_on_W1 <-list.IG.means[[1]]$alpha_on_W1[-na_positions_3]
list.IG.means[[3]]$ln_var_var<-list.IG.means[[1]]$ln_var_var[-na_positions_3]
list.IG.means[[3]]$alpha_on_W2 <-list.IG.means[[1]]$alpha_on_W2[-na_positions_3]
list.IG.means[[3]]$alpha_var<-list.IG.means[[1]]$alpha_var[-na_positions_3]
list.IG.means[[3]]$beta_on_W1 <-list.IG.means[[1]]$beta_on_W1[-na_positions_3]
list.IG.means[[3]]$beta_on_W2 <-list.IG.means[[1]]$beta_on_W2[-na_positions_3]
list.IG.means[[3]]$beta_var   <-list.IG.means[[1]]$beta_var[-na_positions_3]
list.IG.means[[3]]$deviance <-list.IG.means[[1]]$deviance[-na_positions_3]
list.IG.means[[3]]$int_beta <-list.IG.means[[1]]$int_beta[-na_positions_3]
list.IG.means[[3]]$int_phi<-list.IG.means[[1]]$int_phi[-na_positions_3]
list.IG.means[[3]]$lnV_on_W1<-list.IG.means[[1]]$lnV_on_W1[-na_positions_3]
list.IG.means[[3]]$lnV_on_W2<-list.IG.means[[1]]$lnV_on_W2[-na_positions_3]
list.IG.means[[3]]$ln_var_var <-list.IG.means[[1]]$ln_var_var[-na_positions_3]
list.IG.means[[3]]$phi_on_W1  <- list.IG.means[[1]]$phi_on_W1[-na_positions_3]
list.IG.means[[3]]$phi_on_W2<- list.IG.means[[1]]$phi_on_W2[-na_positions_3]
list.IG.means[[3]]$phi_var  <- list.IG.means[[1]]$phi_var[-na_positions_3]




{list.IG.means[[4]]$alpha_on_W1<- unlist(lapply(list.IG.means[[4]]$alpha_on_W1 , function(x) {
  x[abs(x) >= 10] <- NA  # Replace values with absolute value >= 10 with NA
  return(x)
}))

list.IG.means[[4]]$beta_on_W2<- unlist(lapply(list.IG.means[[4]]$beta_on_W2 , function(x) {
  x[abs(x) >= 10] <- NA  # Replace values with absolute value >= 10 with NA
  return(x)
}))
  list.IG.means[[4]]$int_beta <- unlist(lapply(list.IG.means[[4]]$int_beta , function(x) {
    x[abs(x) >= 10] <- NA  # Replace values with absolute value >= 10 with NA
    return(x)
  }))
  list.IG.means[[4]]$beta_on_W1 <- unlist(lapply(list.IG.means[[4]]$beta_on_W1 , function(x) {
    x[abs(x) >= 10] <- NA  # Replace values with absolute value >= 10 with NA
    return(x)
  }))

  list.IG.means[[4]]$alpha_var <- unlist(lapply(list.IG.means[[4]]$alpha_var , function(x) {
    x[abs(x) >= 10] <- NA  # Replace values with absolute value >= 10 with NA
    return(x)
  }))

  list.IG.means[[4]]$alpha_on_W2 <- unlist(lapply(list.IG.means[[4]]$alpha_on_W2 , function(x) {
    x[abs(x) >= 10] <- NA  # Replace values with absolute value >= 10 with NA
    return(x)
  }))

  list.IG.means[[4]]$ln_var_var <- unlist(lapply(list.IG.means[[4]]$ln_var_var , function(x) {
    x[abs(x) >= 6] <- NA  # Replace values with absolute value >= 10 with NA
    return(x)
  }))

  list.IG.means[[4]]$int_phi <- unlist(lapply(list.IG.means[[4]]$int_phi , function(x) {
    x[abs(x) >= 0.4] <- NA  # Replace values with absolute value >= 10 with NA
    return(x)
  }))

  list.IG.means[[4]]$phi_var <- unlist(lapply(list.IG.means[[4]]$phi_var , function(x) {
    x[abs(x) >= 0.025] <- NA  # Replace values with absolute value >= 10 with NA
    return(x)
  }))

na_positions_4 <- which(is.na(list.IG.means[[4]]$alpha_on_W2)|is.na(list.IG.means[[4]]$beta_on_W2)
                          |is.na(list.IG.means[[4]]$int_beta)|is.na(list.IG.means[[4]]$beta_on_W1)
                          |is.na(list.IG.means[[4]]$alpha_var)|is.na(list.IG.means[[4]]$ln_var_var)
                        |is.na(list.IG.means[[4]]$phi_var)|is.na(list.IG.means[[4]]$int_phi)
                        |is.na(list.IG.means[[4]]$alpha_on_W1))
}
list.IG.means[[4]]$alpha_on_W1 <-list.IG.means[[1]]$alpha_on_W1[-na_positions_4]
list.IG.means[[4]]$ln_var_var<-list.IG.means[[1]]$ln_var_var[-na_positions_4]
list.IG.means[[4]]$alpha_on_W2 <-list.IG.means[[1]]$alpha_on_W2[-na_positions_4]
list.IG.means[[4]]$alpha_var<-list.IG.means[[1]]$alpha_var[-na_positions_4]
list.IG.means[[4]]$beta_on_W1 <-list.IG.means[[1]]$beta_on_W1[-na_positions_4]
list.IG.means[[4]]$beta_on_W2 <-list.IG.means[[1]]$beta_on_W2[-na_positions_4]
list.IG.means[[4]]$beta_var   <-list.IG.means[[1]]$beta_var[-na_positions_4]
list.IG.means[[4]]$deviance <-list.IG.means[[1]]$deviance[-na_positions_4]
list.IG.means[[4]]$int_beta <-list.IG.means[[1]]$int_beta[-na_positions_4]
list.IG.means[[4]]$int_phi<-list.IG.means[[1]]$int_phi[-na_positions_4]
list.IG.means[[4]]$lnV_on_W1<-list.IG.means[[1]]$lnV_on_W1[-na_positions_4]
list.IG.means[[4]]$lnV_on_W2<-list.IG.means[[1]]$lnV_on_W2[-na_positions_4]
list.IG.means[[4]]$ln_var_var <-list.IG.means[[1]]$ln_var_var[-na_positions_4]
list.IG.means[[4]]$phi_on_W1  <- list.IG.means[[1]]$phi_on_W1[-na_positions_4]
list.IG.means[[4]]$phi_on_W2<- list.IG.means[[1]]$phi_on_W2[-na_positions_4]
list.IG.means[[4]]$phi_var  <- list.IG.means[[1]]$phi_var[-na_positions_4]




{list.IG.means[[5]]$alpha_on_W1 <- unlist(lapply(list.IG.means[[5]]$alpha_on_W1 , function(x) {
  x[abs(x) >= 10] <- NA  # Replace values with absolute value >= 10 with NA
  return(x)
}))


list.IG.means[[5]]$alpha_var <- unlist(lapply(list.IG.means[[5]]$alpha_var , function(x) {
    x[abs(x) >= 10] <- NA  # Replace values with absolute value >= 10 with NA
    return(x)
  }))

  list.IG.means[[5]]$alpha_on_W2 <- unlist(lapply(list.IG.means[[5]]$alpha_on_W2 , function(x) {
    x[abs(x) >= 10] <- NA  # Replace values with absolute value >= 10 with NA
    return(x)
  }))

  list.IG.means[[5]]$ln_var_var <- unlist(lapply(list.IG.means[[5]]$ln_var_var , function(x) {
    x[abs(x) >= 6] <- NA  # Replace values with absolute value >= 10 with NA
    return(x)
  }))

  list.IG.means[[5]]$int_phi <- unlist(lapply(list.IG.means[[5]]$int_phi , function(x) {
    x[abs(x) >= 0.4] <- NA  # Replace values with absolute value >= 10 with NA
    return(x)
  }))

  list.IG.means[[5]]$phi_var <- unlist(lapply(list.IG.means[[5]]$phi_var , function(x) {
    x[abs(x) >= 0.025] <- NA  # Replace values with absolute value >= 10 with NA
    return(x)
  }))


na_positions_5 <- which(is.na(list.IG.means[[5]]$alpha_on_W2)|is.na(list.IG.means[[5]]$beta_on_W2)
                          |is.na(list.IG.means[[5]]$int_beta)|is.na(list.IG.means[[5]]$beta_on_W1)
                          |is.na(list.IG.means[[5]]$alpha_var)|is.na(list.IG.means[[5]]$ln_var_var)
                          |is.na(list.IG.means[[5]]$phi_var)|is.na(list.IG.means[[5]]$int_phi)
                        |is.na(list.IG.means[[5]]$alpha_on_W2))}
list.IG.means[[5]]$alpha_on_W1 <-list.IG.means[[1]]$alpha_on_W1[-na_positions_5]
list.IG.means[[5]]$ln_var_var<-list.IG.means[[1]]$ln_var_var[-na_positions_5]
list.IG.means[[5]]$alpha_on_W2 <-list.IG.means[[1]]$alpha_on_W2[-na_positions_5]
list.IG.means[[5]]$alpha_var<-list.IG.means[[1]]$alpha_var[-na_positions_5]
list.IG.means[[5]]$beta_on_W1 <-list.IG.means[[1]]$beta_on_W1[-na_positions_5]
list.IG.means[[5]]$beta_on_W2 <-list.IG.means[[1]]$beta_on_W2[-na_positions_5]
list.IG.means[[5]]$beta_var   <-list.IG.means[[1]]$beta_var[-na_positions_5]
list.IG.means[[5]]$deviance <-list.IG.means[[1]]$deviance[-na_positions_5]
list.IG.means[[5]]$int_beta <-list.IG.means[[1]]$int_beta[-na_positions_5]
list.IG.means[[5]]$int_phi<-list.IG.means[[1]]$int_phi[-na_positions_5]
list.IG.means[[5]]$lnV_on_W1<-list.IG.means[[1]]$lnV_on_W1[-na_positions_5]
list.IG.means[[5]]$lnV_on_W2<-list.IG.means[[1]]$lnV_on_W2[-na_positions_5]
list.IG.means[[5]]$ln_var_var <-list.IG.means[[1]]$ln_var_var[-na_positions_5]
list.IG.means[[5]]$phi_on_W1  <- list.IG.means[[1]]$phi_on_W1[-na_positions_5]
list.IG.means[[5]]$phi_on_W2<- list.IG.means[[1]]$phi_on_W2[-na_positions_5]
list.IG.means[[5]]$phi_var  <- list.IG.means[[1]]$phi_var[-na_positions_5]



