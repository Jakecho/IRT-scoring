## ---------------------------
##
## Script name: Scoring methods
##
## Purpose of script:
##
## Author: Jake Cho
##
## Date Created: 2023-08-25
##
## Copyright (c) Jake Cho, 2023
## Email: u_cho@uncg.edu
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

library(shiny)
library(shinythemes)
library(DT)

#### define functions #### 

model_gendata <- function(np=100, ni=30,
                          t_dist=c(0, 1), 
                          t_bound=c(-3, 3),
                          a_dist=c(0, .2), 
                          b_dist=c(0, 1), 
                          c_dist=c(5, 17),
                          model ="3PL",
                          D=1.702) 
                              {
  truet_vec <- truncnorm::rtruncnorm(np, a=t_bound[1], b=t_bound[2], mean=t_dist[1], sd=t_dist[2]) 
  a_vec <- rlnorm(ni, meanlog=a_dist[1], sdlog=a_dist[2])
  b_vec <- rnorm(ni, mean=b_dist[1], sd=b_dist[2])
  c_vec <- rbeta(ni, shape1=c_dist[1], shape2=c_dist[2])
  
  if(model=="Rasch"){
    p <- model_3pl_prob(t=truet_vec, a=rep(1, ni), b=b_vec, c=rep(0, ni), D=1.702)
    x <- array(runif(length(p)), dim(p))
    u <- (p >= x) * 1L
    u <- as.data.frame(u)
    u_df <- cbind(pid=1:np, theta=truet_vec, u=u) 
    ip_df <- data.frame(a=rep(1, ni), b=b_vec, g=rep(0, ni))
  }
  if(model=="2PL"){
    p <- model_3pl_prob(t=truet_vec, a=a_vec, b=b_vec, c=rep(0, ni), D=1.702)
    x <- array(runif(length(p)), dim(p))
    u <- (p >= x) * 1L
    u <- as.data.frame(u)
    u_df <- cbind(pid=1:np, theta=truet_vec, u=u) 
    ip_df <- data.frame(a=a_vec, b=b_vec, g=rep(0, ni))
  }
  if(model=="3PL"){
    p <- model_3pl_prob(t=truet_vec, a=a_vec, b=b_vec, c=c_vec, D=1.702)
    x <- array(runif(length(p)), dim(p))
    u <- (p >= x) * 1L
    u <- as.data.frame(u)
    u_df <- cbind(pid=1:np, theta=truet_vec, u=u) 
    ip_df <- data.frame(a=a_vec, b=b_vec, g=c_vec)
  }
  return(list(u_df=u_df, ip_df=ip_df))
}

model_3pl_prob <- function(t, a, b, c, D=1.702){
  p <- c + (1 - c) / (1 + exp(D * a * outer(b, t, '-')))
  t(p)
}

ss_conversion <-function(theta_mat, theta_vec, ss_vec){
  model <- lm(ss_vec ~ poly(theta_vec, 2, raw = TRUE))
  b0 <- coefficients(model)[1]
  b1 <- coefficients(model)[2]
  b2 <- coefficients(model)[3]
  
  ss_mat <- theta_mat
  ss_mat$SS <- b0 + b1 * theta_mat[, 3] + b2 *theta_mat[, 3] * theta_mat[,3]
  # ss_mat$SS_se <- b0 + b1 * theta_mat[, 4]  + b2 *theta_mat[, 4]  * theta_mat[, 4] 
  return(ss_mat)
} 

# quad is a df with "Q.points", and "Weights"
NormalQWs <- function(Q.p=49, bound=c(-3, 3), prior=c(0,1)){
  Qs <- seq(bound[1], bound[2], length.out=Q.p)
  Ws <- dnorm(Qs, mean = prior[1], sd=prior[2])
  Ws <- Ws/sum(Ws)
  return(data.frame(Q_points = Qs, Weights = Ws))
}

####Computing conditional and marginal distribution with LW algorithm #####
lw.fun=function(D=1.702, param, quadrature){
  n.item <- nrow(param)
  theta <- quadrature$Q_points
  q.weights <-  quadrature$Weights
  
  prob.mat=matrix(0, nrow = n.item, ncol = length(theta))
  a=param[, 1]
  b=param[, 2]
  g=param[, 3]
  
  #prob(x=1|theta)
  for(i in 1:length(theta)){
    t=D*a*(theta[i]-b)
    prob.mat[, i] = g + (1-g)*exp(t)/(1+exp(t))
  }
  
  prev.run=cond.xt=matrix(0, nrow = n.item+1, ncol = length(theta))
  
  prev.run[1, ] = 1-prob.mat[1, ]
  prev.run[2, ] = prob.mat[1, ]
  
  # recursive with swapping
  for(i in 2:n.item){
    cond.xt[1, ] = prev.run[1, ] * (1-prob.mat[i, ])
    for(j in 2:i){
      cond.xt[j, ] = prev.run[j, ] * (1-prob.mat[i, ]) + prev.run[j-1, ] * prob.mat[i, ]
    }
    cond.xt[i+1, ] = prev.run[i, ] * prob.mat[i, ]
    prev.run=cond.xt
  }
  
  row.names(cond.xt) <- c(0, 1:n.item)
  colnames(cond.xt) <- round(theta, 1)
  names(q.weights) <- round(theta, 1)
  marg.xt <- colSums(q.weights*t(cond.xt))
  lw.out <- list(cond.prob=t(cond.xt), marg.prob=marg.xt, cuml.prob=cumsum(marg.xt))
  
  return(lw.out)
}

##### Scoring Procedures ####
model_3pl_mle_scoring <- function(u_mat, ip_mat, ccrit=0.0001, 
                                  iter_max=100, bound=c(-3, 3), max_se=5, D=1.702){
  mle_df <- data.frame(pid=u_mat[, 1], SumS=rowSums(u_mat[, -1]), MLE=rep(0, dim(u_mat)[1]), MLE_se=rep(0, dim(u_mat)[1]))
  
    a <- ip_mat[, 1]
    b <- ip_mat[, 2]
    g <- ip_mat[, 3]
  
  for(i in 1:nrow(u_mat)){ # i <- 59
    u <- u_mat[i, -1]
    if(sum(u)==length(u)){
      mle_df[i, 2] <- bound[2]
      mle_df[i, 3] <- max_se
    } else if(sum(u)==0){
      mle_df[i, 2] <- bound[1]
      mle_df[i, 3] <- max_se
    } else {
      s <- 0
      th <- 0
      sumnum <- 0.0
      sumdem <- 0.0
      repeat{
        k <- exp(D*a*(th-b))
        phat <-  g + (1-g)*k/(1+k)
        sumnum <- sum(D*a * ((phat - g)/(1 - g)) * ((u - phat) / phat)) 
        sumdem <- sum((D*a)^2 * ((phat-g)/phat) * ((1.0 - phat)/phat)^2) 
        delta <- sumnum / -sumdem
        th <- th - delta
        if (abs(delta) < ccrit | s == iter_max) {
          se <- 1 / sqrt(sumdem)
          mle_df[i, 3] <- th
          mle_df[i, 4] <- se
          break
        }
        s <- s+1
      }
    }
  }
  mle_df[which(mle_df$MLE > bound[2]), "MLE"] <- bound[2]
  mle_df[which(mle_df$MLE < bound[1]), "MLE"] <- bound[1]
  mle_df[which(mle_df$MLE_se > max_se), "MLE_se"] <- max_se
  mle_df <- ss_conversion(mle_df, theta_vec, ss_vec)
  return(mle_df)
}

model_3pl_map_scoring <- function(u_mat, ip_mat, D=1.702, 
                                  prior=c(0, 1), bound=c(-3, 3), max_se=5, iter_max=100, ccrit=0.0001){
  map_df <- data.frame(pid=u_mat[, 1], SumS=rowSums(u_mat[, -1]), MAP=rep(0, dim(u_mat)[1]), MAP_se=rep(0, dim(u_mat)[1]))
  a <- ip_mat[, 1]
  b <- ip_mat[, 2]
  g <- ip_mat[, 3]
  
  for(i in 1:nrow(u_mat)){ # i <- 1
    u <- u_mat[i, -1]
    sumnum <- 0.0
    sumdem <- 0.0
    th <- 0
    s <- 0
    repeat{
      k <- exp(D*a*(th-b))
      phat <-  g + (1-g)*k/(1+k)
      sumnum <- sum(D*a * ((phat - g)/(1 - g)) * ((u - phat) / phat)) - ((th-prior[1])/prior[2]^2)
      sumdem <- -sum((D*a)^2 * ((phat-g)/phat) * ((1.0 - phat)/phat)^2) - prior[2]^2
      delta <- sumnum / sumdem
      th <- th - delta
      if (abs(delta) < ccrit | s == iter_max) {
        se <- 1 / sqrt(-sumdem)
        map_df[i, 3] <- th
        map_df[i, 4] <- se
        break
      }
      s <- s+1
      }
    }
  map_df[map_df$MAP > bound[2]] <- bound[2]
  map_df[map_df$MAP < bound[1]] <- bound[1]
  map_df[map_df$MAP_se > max_se] <- max_se
  map_df <- ss_conversion(map_df, theta_vec, ss_vec)
  return(map_df)
}

model_3pl_eap_scoring <- function(u_mat, ip_mat, quad=NULL, D=1.702, 
                                  prior=c(0, 1), bound=c(-3, 3), max_se=5, Q.p=49){
  eap_df <- data.frame(pid=u_mat[, 1], SumS=rowSums(u_mat[, -1]), EAP=rep(0, dim(u_mat)[1]), EAP_se=rep(0, dim(u_mat)[1]))
  a <- ip_mat[, 1]
  b <- ip_mat[, 2]
  g <- ip_mat[, 3]
  u <- as.matrix(u_mat[, -1])

  if(is.null(quad)) {quad <- NormalQWs(Q.p, bound, prior)}
  p <- model_3pl_prob(quad$Q_points, a, b, g, D=1.702)
  lh <- exp(u %*% t(log(p)) + (1 - u) %*% t(log(1 - p)))
  t <- colSums(t(lh) * quad$Weights * quad$Q_points) / colSums(t(lh) * quad$Weights)
  t[t < bound[1]] <- bound[1]
  t[t > bound[2]] <- bound[2]
  t_sd <- colSums(t(lh) * quad$Weights * outer(quad$Q_points, t, '-')^2) / colSums(t(lh) * quad$Weights)
  t_sd <- sqrt(t_sd)
  eap_df$EAP <- t
  eap_df$EAP_se <- t_sd
  eap_df[which(eap_df$EAP > bound[2]), "EAP"] <- bound[2]
  eap_df[which(eap_df$EAP < bound[1]), "EAP"] <- bound[1]
  eap_df[which(eap_df$EAP_se > max_se), "EAP_se"] <- max_se
  eap_df <- ss_conversion(eap_df, theta_vec, ss_vec)
  return(eap_df)
}

model_3pl_sum_eap_scoring <- function(u_mat, ip_mat, quad=NULL, D=1.702, 
                                      prior=c(0, 1), bound=c(-3, 3), max_se=5, Q.p=49){
  sum_eap_df <- data.frame(pid=u_mat[, 1], SumS=rowSums(u_mat[, -1]), 
                           EAP=rep(0, dim(u_mat)[1]), EAP_se=rep(0, dim(u_mat)[1]))

  if(is.null(quad)) {quad <- NormalQWs(Q.p, bound, prior)}
  lw.out <- lw.fun(D=1.702, param=ip_mat, quadrature=quad)
  post <- t(t(lw.out$cond.prob*quad$Weights)/colSums(lw.out$cond.prob*quad$Weights))
  t <- colSums(post*quad$Q_points)
  t_sd <- sqrt(colSums(post * outer(quad$Q_points, t, '-')^2))
  sum_eap_df$EAP <- t[(sum_eap_df$SumS+1)]
  sum_eap_df$EAP_se <- t_sd[(sum_eap_df$SumS+1)]
  sum_eap_df[which(sum_eap_df$EAP > bound[2]), "EAP"] <- bound[2]
  sum_eap_df[which(sum_eap_df$EAP < bound[1]), "EAP"] <- bound[1]
  sum_eap_df[which(sum_eap_df$EAP_se > max_se), "EAP_se"] <- max_se
  sum_eap_df <- ss_conversion(sum_eap_df, theta_vec, ss_vec)
  return(sum_eap_df)
}


# Define the TCC for 3PL model
tcc_3pl <- function(a, b, g, theta, D=1.702){
  DL = D * t(outer(theta, a, "*") - b)
  p.mat = g + (1 - g) / (1 + exp(-DL))
  colSums(p.mat)
}

# Calculate the first derivative of TCC for 3PL model
tccp_3pl <- function(a, b, g, theta, D=1.702){
  DL = D * t(outer(theta, a, "*") - b)
  pp.mat = (a * (1 - g) * exp(-DL)) / (1 + exp(-DL))^2
  colSums(pp.mat)
}

# Implement the Newton-Raphson method to find theta estimates for 3PL model
findtheta_3pl <- function(a, b, g, sum_score, prec=0.0001, D=1.702){
  told = 0
  tol = prec
  repeat {
    tcc_values = tcc_3pl(a, b, g, told, D)
    tccp_values = tccp_3pl(a, b, g, told, D)
    tnew = told - (sum_score - tcc_values) / (-1 * tccp_values)
    delta = abs(tnew - told)
    cat(delta, '\n')
    if (is.na(delta) || is.nan(delta)) {
      stop("NA or NaN encountered. Stopping the iteration.")
    }
    if (delta <= tol) {
      break
    } else {
      told = tnew
    }
  }
  return(tnew)
}

find_theta_with_interpolation <- function(sum_score, item_parameters, 
                                          lower_bound = bound[1], upper_bound = bound[2]) {
  a <- item_parameters[, 1]
  b <- item_parameters[, 2]
  g <- item_parameters[, 3]
  sum_guessing = sum(g)
  max_sum_score = nrow(item_parameters)
  
  # Case when sum_score is zero : sum_score=1
  if (sum_score == 0) {
    theta_estimate <- lower_bound
  } else if (sum_score == max_sum_score) {
    theta_estimate <- upper_bound
  } else if ((sum_score > sum_guessing) && (sum_score < max_sum_score)) {
    theta_estimate <- findtheta_3pl(a, b, g, sum_score, prec=0.0001, D=1.702)
  } else {
    min_theta = lower_bound 
    max_theta <- findtheta_3pl(a, b, g, sum_score=ceiling(sum_guessing + 1.5), prec=0.0001, D=1.702)
    interpolated_theta = ((sum_score - 0) / (sum_guessing - 0)) * (max_theta - min_theta) + min_theta
    theta_estimate <- interpolated_theta
  }
  return(theta_estimate)
}

model_3pl_inv_tcc_scoring  <- function(u_mat, ip_mat, quad=NULL, D=1.702, 
                                       prior=c(0, 1), bound=c(-3, 3), max_se=5, Q.p=49){
  inv_t_df <- data.frame(pid=u_mat[, 1], SumS=rowSums(u_mat[, -1]), 
                           Inv_t=rep(0, dim(u_mat)[1]), Inv_t_se=rep(0, dim(u_mat)[1]))

  test_raw_scores <- 0:nrow(ip_mat)
  theta_estimate <- data.frame(sum_score=test_raw_scores, 
                               inv_t = rep(0, length(test_raw_scores)),
                               inv_t_se = rep(0, length(test_raw_scores)))
  for (score in test_raw_scores) {  # score = 1
    theta_estimate$inv_t[(score+1)] <- 
      find_theta_with_interpolation(score, ip_mat, lower_bound = bound[1], upper_bound = bound[2])
  }
  
  if(is.null(quad)) {quad <- NormalQWs(Q.p, bound, prior)}
  lw.out <- lw.fun(D=1.702, param=ip_mat, quadrature=quad)
  post <- t(t(lw.out$cond.prob*quad$Weights)/colSums(lw.out$cond.prob*quad$Weights))
  t_sd <- sqrt(colSums(post * outer(quad$Q_points, theta_estimate$inv_t, '-')^2))
  inv_t_df$Inv_t <- theta_estimate$inv_t[(inv_t_df$SumS+1)]
  inv_t_df$Inv_t_se <- t_sd[(inv_t_df$SumS+1)]
  inv_t_df[which(inv_t_df$Inv_t > bound[2]), "Inv_t"] <- bound[2]
  inv_t_df[which(inv_t_df$Inv_t < bound[1]), "Inv_t"] <- bound[1]
  inv_t_df[which(inv_t_df$Inv_t_se > max_se), "Inv_t_se"] <- max_se
  inv_t_df <- ss_conversion(inv_t_df, theta_vec, ss_vec)
  return(inv_t_df)
}


###### Example Case ####
sample_set <- model_gendata()
u_df <- sample_set$u_df
u_mat <- u_df[, -2]
ip_mat <- as.matrix(sample_set$ip_df)
theta_vec <- c(-3, 1, 3)
ss_vec <- c(1, 130, 200)

MLE_Theta <- model_3pl_mle_scoring(u_mat, ip_mat)
MLE_Theta <- MLE_Theta[order(MLE_Theta$SumS),]
MAP_Theta <- model_3pl_map_scoring(u_mat, ip_mat)
MAP_Theta <- MAP_Theta[order(MAP_Theta$SumS),]
EAP_Theta <- model_3pl_eap_scoring(u_mat, ip_mat)
EAP_Theta <- EAP_Theta[order(EAP_Theta$SumS),]
SUM_EAP_Theta <- model_3pl_sum_eap_scoring(u_mat, ip_mat)
SUM_EAP_Theta <- SUM_EAP_Theta[order(SUM_EAP_Theta$SumS),]
INV_TCC_Theta <- model_3pl_inv_tcc_scoring(u_mat, ip_mat)
INV_TCC_Theta <- INV_TCC_Theta[order(INV_TCC_Theta$SumS),]

L <- nrow(ip_mat)
TCC_t <- c()
for(i in 1:L){ # i <- 1
  X <- sum(u_mat[i, -1])
  P <- X/L
  if(P==1) p <- .99
  if(P<=g) P <- g+0.001
  TCC_t[i] <- sum((1/-a)*log(((1-P))/(P-g))+b)
}

