
## Libraries ####

library(readr)
library(igraph)
library(bipartite)
library(ggplot2) 
library(MASS)
library(foreach)
library(mclust)
library(doParallel)
library(plotrix)
library(cds)


## Load COVID-19 data ####

zip.url <- "https://https://github.com/dfailli/MLTA/blob/main/Data.zip"
zip.file <- "Data.zip"
dir <- getwd()
zip.combine <- as.character(paste(dir, zip.file, sep = "/"))
download.file(zip.url, destfile = zip.combine)
italy <- unzip(zip.file)


## Nodal attributes ####

id <- c(1:36106)
italy$id <- id

italy$gender <- as.factor(italy$gender) # gender
round(prop.table(table(italy$gender)),digits=2)


italy$age <- as.numeric(italy$age) # age
summary(italy$age)


reg <- rep(NA, nrow(italy)) # region
reg[italy$region=="North east" | italy$region=="North west"] <- "North"
reg[italy$region=="Centre"] <- "Centre"
reg[italy$region=="South" | italy$region=="Islands"] <- "South and Islands"
italy$region <- as.factor(reg)
round(prop.table(table(italy$region)),digits=2)


empl <- rep(NA, nrow(italy)) # employment status
empl[italy$employment_status=="Full time employment" | italy$employment_status=="Part time employment"] <- "employed"
empl[italy$employment_status=="Full time student" | italy$employment_status=="Retired" |
       italy$employment_status=="Other" | italy$employment_status=="Not working" |
       italy$employment_status=="Unemployed"] <- "not employed"
italy$employment_status <- as.factor(empl)
round(prop.table(table(italy$employment_status)),digits=4)


symptoms <- rep(NA, nrow(italy))  # symptoms
symptoms[italy$i5_health_1=="Yes" | italy$i5_health_5=="Yes" | italy$i5_health_2=="Yes" |
           italy$i5_health_3=="Yes" | italy$i5_health_4=="Yes"] <- "yes"
symptoms[italy$i5_health_99=="Yes"] <- "none"
italy$symptoms <- as.factor(symptoms)
sym <- table(italy$symptoms)
round(prop.table(sym),digits=4)


illness <- rep(NA, nrow(italy)) # illness
illness[italy$d1_health_13=="Yes" | italy$d1_health_11=="Yes" | italy$d1_health_7=="Yes" |
          italy$d1_health_5=="Yes" | italy$d1_health_4=="Yes" | italy$d1_health_1=="Yes" |
          italy$d1_health_2=="Yes" | italy$d1_health_3=="Yes" | italy$d1_health_6=="Yes" |
          italy$d1_health_8=="Yes" | italy$d1_health_9=="Yes" | italy$d1_health_10=="Yes" |
          italy$d1_health_12=="Yes"] <- "Yes"
illness[italy$d1_health_98=="Yes"] <- "No answer"
illness[italy$d1_health_99=="Yes"] <- "None"
italy$illness <- as.factor(illness)
ill <- table(italy$illness)
round(prop.table(ill),digits=4)


num <- rep(0, nrow(italy)) # number of diseases
for(i in 1:nrow(italy)){
  for(m in 57:69){
    if(is.na(italy$illness[i])){
      num[i] <- NA
    }
    if(italy[i,m]=="Yes"){
      num[i] = num[i] + 1
    }
    else{
      num[i] = num[i] + 0
    }
  }
}
italy$num.ill <- num


conf <- rep(NA, nrow(italy)) # confidence in authorities
conf[italy$WCRex2=="A fair amount of confidence" | italy$WCRex2=="A lot of confidence"] <- "confident"
conf[italy$WCRex2=="No confidence at all" | italy$WCRex2=="Not very much confidence"] <- "no confident"
conf[italy$WCRex2=="Don't know"] <- "other"
italy$conf <- conf
round(prop.table(table(italy$conf)),digits=4)


op <- rep(NA, nrow(italy)) # opinion towards covid-19
op[italy$WCRV_4=="I am fairly scared that I will contract the Coronavirus (COVID-19)" | italy$WCRV_4=="I am very scared that I will contract the Coronavirus (COVID-19)"] <- "scared"
op[italy$WCRV_4=="I am not at all scared that I will contract the Coronavirus (COVID-19)" | italy$WCRV_4=="I am not very scared that I will contract the Coronavirus (COVID-19)"] <- "not scared"
op[italy$WCRV_4=="Don't know" | italy$WCRV_4=="Not applicable - I have already contracted Coronavirus (COVID-19)"] <- "other"
italy$op <-  op
round(prop.table(table(italy$op)),digits=4)



## Design Matrix ####

dat <- data.frame(cbind(italy$age,italy$gender, italy$employment_status, italy$region,
                         italy$illness, italy$num.ill, italy$symptoms, italy$op, italy$conf, italy$id))
df <- dat[complete.cases(dat),]
italy <- italy[df[,10],]
df <- df[,-10]
colnames(df) <- c("age","gender","empl","region","illness","num.illness","symptoms","opinion","confidence")

df$gender <- as.factor(df$gender)
df$empl <- as.factor(df$empl)
df$region <- as.factor(df$region)
df$illness <- as.factor(df$illness)
df$symptoms <- as.factor(df$symptoms)
df$opinion <- as.factor(df$opinion)
df$confidence <- as.factor(df$confidence)
df$age <- as.numeric(df$age)
df$num.illness <- as.numeric(df$num.illness)
DesMat <- model.matrix(~ age + gender + empl + region + illness + num.illness + symptoms +
                         opinion + confidence, data=df)


## Preventive measures ####

mask <- rep(NA, nrow(italy))
mask[italy$i12_health_1=="Not at all"] <- 0
mask[italy$i12_health_1=="Rarely"] <- 0
mask[italy$i12_health_1=="Sometimes"] <- 0
mask[italy$i12_health_1=="Frequently"] <- 1
mask[italy$i12_health_1=="Always"] <- 1
italy$mask <- mask
sum(is.na(italy$mask))

wash <- rep(NA, nrow(italy))
wash[italy$i12_health_2=="Not at all"] <- 0
wash[italy$i12_health_2=="Rarely"] <- 0
wash[italy$i12_health_2=="Sometimes"] <- 0
wash[italy$i12_health_2=="Frequently"] <- 1
wash[italy$i12_health_2=="Always"] <- 1
italy$wash <- wash
sum(is.na(italy$wash))

sanitiser <- rep(NA, nrow(italy))
sanitiser[italy$i12_health_3=="Not at all"] <- 0
sanitiser[italy$i12_health_3=="Rarely"] <- 0
sanitiser[italy$i12_health_3=="Sometimes"] <- 0
sanitiser[italy$i12_health_3=="Frequently"] <- 1
sanitiser[italy$i12_health_3=="Always"] <- 1
italy$sanitiser <- sanitiser
sum(is.na(italy$sanitiser))

sneeze <- rep(NA, nrow(italy))
sneeze[italy$i12_health_4=="Not at all"] <- 0
sneeze[italy$i12_health_4=="Rarely"] <- 0
sneeze[italy$i12_health_4=="Sometimes"] <- 0
sneeze[italy$i12_health_4=="Frequently"] <- 1
sneeze[italy$i12_health_4=="Always"] <- 1
italy$sneeze <- sneeze
sum(is.na(italy$sneeze))

contact <- rep(NA, nrow(italy))
contact[italy$i12_health_5=="Not at all"] <- 0
contact[italy$i12_health_5=="Rarely"] <- 0
contact[italy$i12_health_5=="Sometimes"] <- 0
contact[italy$i12_health_5=="Frequently"] <- 1
contact[italy$i12_health_5=="Always"] <- 1
italy$contact <- contact
sum(is.na(italy$contact))

out <- rep(NA, nrow(italy))
out[italy$i12_health_6=="Not at all"] <- 0
out[italy$i12_health_6=="Rarely"] <- 0
out[italy$i12_health_6=="Sometimes"] <- 0
out[italy$i12_health_6=="Frequently"] <- 1
out[italy$i12_health_6=="Always"] <- 1
italy$out <- out
sum(is.na(italy$out))

hospital <- rep(NA, nrow(italy))
hospital[italy$i12_health_7=="Not at all"] <- 0
hospital[italy$i12_health_7=="Rarely"] <- 0
hospital[italy$i12_health_7=="Sometimes"] <- 0
hospital[italy$i12_health_7=="Frequently"] <- 1
hospital[italy$i12_health_7=="Always"] <- 1
italy$hospital <- hospital
sum(is.na(italy$hospital))

transport <- rep(NA, nrow(italy))
transport[italy$i12_health_8=="Not at all"] <- 0
transport[italy$i12_health_8=="Rarely"] <- 0
transport[italy$i12_health_8=="Sometimes"] <- 0
transport[italy$i12_health_8=="Frequently"] <- 1
transport[italy$i12_health_8=="Always"] <- 1
italy$transport <- transport
sum(is.na(italy$transport))

guests <- rep(NA, nrow(italy))
guests[italy$i12_health_11=="Not at all"] <- 0
guests[italy$i12_health_11=="Rarely"] <- 0
guests[italy$i12_health_11=="Sometimes"] <- 0
guests[italy$i12_health_11=="Frequently"] <- 1
guests[italy$i12_health_11=="Always"] <- 1
italy$guests <- guests
sum(is.na(italy$guests))

small_gatherings <- rep(NA, nrow(italy))
small_gatherings[italy$i12_health_12=="Not at all"] <- 0
small_gatherings[italy$i12_health_12=="Rarely"] <- 0
small_gatherings[italy$i12_health_12=="Sometimes"] <- 0
small_gatherings[italy$i12_health_12=="Frequently"] <- 1
small_gatherings[italy$i12_health_12=="Always"] <- 1
italy$small_gatherings <- small_gatherings
sum(is.na(italy$small_gatherings))

medium_gatherings <- rep(NA, nrow(italy))
medium_gatherings[italy$i12_health_13=="Not at all"] <- 0
medium_gatherings[italy$i12_health_13=="Rarely"] <- 0
medium_gatherings[italy$i12_health_13=="Sometimes"] <- 0
medium_gatherings[italy$i12_health_13=="Frequently"] <- 1
medium_gatherings[italy$i12_health_13=="Always"] <- 1
italy$medium_gatherings <- medium_gatherings
sum(is.na(italy$medium_gatherings))

large_gatherings <- rep(NA, nrow(italy))
large_gatherings[italy$i12_health_14=="Not at all"] <- 0
large_gatherings[italy$i12_health_14=="Rarely"] <- 0
large_gatherings[italy$i12_health_14=="Sometimes"] <- 0
large_gatherings[italy$i12_health_14=="Frequently"] <- 1
large_gatherings[italy$i12_health_14=="Always"] <- 1
italy$large_gatherings <- large_gatherings
sum(is.na(italy$large_gatherings))

crowd <- rep(NA, nrow(italy))
crowd[italy$i12_health_15=="Not at all"] <- 0
crowd[italy$i12_health_15=="Rarely"] <- 0
crowd[italy$i12_health_15=="Sometimes"] <- 0
crowd[italy$i12_health_15=="Frequently"] <- 1
crowd[italy$i12_health_15=="Always"] <- 1
italy$crowd <- crowd
sum(is.na(italy$crowd))

shops <- rep(NA, nrow(italy))
shops[italy$i12_health_16=="Not at all"] <- 0
shops[italy$i12_health_16=="Rarely"] <- 0
shops[italy$i12_health_16=="Sometimes"] <- 0
shops[italy$i12_health_16=="Frequently"] <- 1
shops[italy$i12_health_16=="Always"] <- 1
italy$shops <- shops
sum(is.na(italy$shops))



## Edgelist ####

edgelist <- NULL
for(i in 1:nrow(italy)){
  start=Sys.time()
  if(italy$mask[i]==1){
    edgelist <- rbind(edgelist, cbind(i,"mask"))
  }
  if(italy$wash[i]==1){
    edgelist <- rbind(edgelist, cbind(i,"wash"))
  }
  if(italy$sanitiser[i]==1){
    edgelist <- rbind(edgelist, cbind(i,"sanitiser"))
  }
  if(italy$sneeze[i]==1){
    edgelist <- rbind(edgelist, cbind(i,"cover sneeze"))
  }
  if(italy$contact[i]==1){
    edgelist <- rbind(edgelist, cbind(i,"avoid contact"))
  }
  if(italy$out[i]==1){
    edgelist <- rbind(edgelist, cbind(i,"avoid out"))
  }
  if(italy$hospital[i]==1){
    edgelist <- rbind(edgelist, cbind(i,"avoid hospital"))
  }
  if(italy$transport[i]==1){
    edgelist <- rbind(edgelist, cbind(i,"avoid transport"))
  }
  if(italy$guests[i]==1){
    edgelist <- rbind(edgelist, cbind(i,"avoid guests"))
  }
  if(italy$small_gatherings[i]==1){
    edgelist <- rbind(edgelist, cbind(i,"avoid small gatherings"))
  }
  if(italy$medium_gatherings[i]==1){
    edgelist <- rbind(edgelist, cbind(i,"avoid medium gatherings"))
  }
  if(italy$large_gatherings[i]==1){
    edgelist <- rbind(edgelist, cbind(i,"avoid large gatherings"))
  }
  if(italy$crowd[i]==1){
    edgelist <- rbind(edgelist, cbind(i,"avoid crowd"))
  }
  if(italy$shops[i]==1){
    edgelist <- rbind(edgelist, cbind(i,"avoid shops"))
  }
  end=Sys.time()
  time=end-start
}
edgelist

events <- c("mask", "wash", "sanitiser", "cover sneeze", "avoid contact",
            "avoid out", "avoid hospital", "avoid transport", "avoid guests",
            "avoid small gatherings", "avoid medium gatherings", "avoid large gatherings",
            "avoid crowd", "avoid shops")
events



## Network ####

nrow(italy)
g <- graph.empty()  # graph
g <- add_vertices(g, nv=nrow(italy), attr=list(name=1:nrow(italy), type=rep(TRUE,nrow(italy))))
g <- add_vertices(g, nv=length(events), attr=list(name=events, type=rep(FALSE,length(events))))
edgelist.vec <- as.vector(t(as.matrix(edgelist)))
g <- add_edges(g, edgelist.vec)
is_bipartite(g)

m <- as_incidence_matrix(g) # incidence matrix
nrow(m)
m <- t(m)
nrow(m)
dim(m) # NxM matrix



## MLTA functions ####


AitkenAcc <- function(l, lA, iter){
  a <- (l[3] - l[2]) / (l[2] - l[1])
  lA[1] <- lA[2]
  
  if (a > 0 & a < 1) 
  {
    lA[2] <- l[2] + (l[3] - l[2]) / (1 - a)
  }else{
    lA[2] <- l[3]
  }
  
  diff <- abs(lA[2] - lA[1])
  
  if ((l[3] < l[2]) & (iter > 5))
  {
    stop("Decrease in log-likelihood")
  }	
  
  Out <- c(diff, lA)
  Out
}




ResTable <- function(bicG, restype)
{
  if (restype == 'll') {
    resBIC <- vector('list', 1)
    names(resBIC) <- 'Table of LL (G-H Quadrature correction)'
    resBIC[[1]] <- bicG
  }
  
  if (restype == 'llva') {
    resBIC <- vector('list', 1)
    names(resBIC) <- 'Table of LL (variational approximation)'
    resBIC[[1]] <- bicG
  }
  
  if (restype == 'BIC') {
    resBIC <- vector('list', 2)
    names(resBIC) <- c('Table of BIC Results', 'Model Selection')
    resBIC[[1]] <- bicG
    resBIC[[2]] <-
      paste(colnames(bicG)[t(bicG == min(bicG)) * seq(1:ncol(bicG))], rownames(bicG)[(bicG ==
                                                                                        min(bicG)) * seq(1:nrow(bicG))], sep = ', ')
    names(resBIC[[2]]) <- 'Model with lower BIC:'
  }
  
  resBIC
}



cls <- function(lab) 
{
  gr <- sort(unique(lab))
  z <- matrix(0, length(lab), length(gr))
  for (i in 1:length(gr)) z[lab == gr[i], i] <- 1
  z
}



f_lca <-function(X, DM, G, tol, maxiter, beta0){ 
  
  N <- nrow(X)
  M <- ncol(X) 
  J <- ncol(DM)
  
  # Initialize EM Algorithm
  
  if(!is.null(beta0)) beta = beta0 else{
    beta <- rep(0,J*(G-1))
    beta <- matrix(beta,ncol=G-1)
  }
  
  exb <- exp(DM %*% beta)
  eta <- cbind(1,exb)/(rowSums(exb)+1)  # priors
  
  
  z <- matrix(NA, nrow=N, ncol=G)
  for(i in 1:N)   z[i,] <- t(rmultinom(1, size = 1, prob = eta[i,]))
  
  # Set up
  
  p <- (t(z) %*% X) / colSums(z) # proportion of success in group g
  
  # Initialize Aitken acceleration
  
  l <- numeric(3)
  lA <- numeric(2)
  
  # Iterative process
  
  v <- matrix(0, N, G)
  W <- list()
  ll <- -Inf
  diff <- 1
  iter <- 0
  cond <- TRUE
  
  tol <- 0.1 ^ 6
  print(c(beta))
  
  while(diff > tol & iter < maxiter)
  {
    iter <- iter + 1
    beta.old=beta
    #diff.ll <- 0
    
    
    # M-step 
    
    lk = sum(z*log(eta))
    it = 0; lko = lk
    XXdis = array(0,c(G,(G-1)*ncol(DM),N))
    for(i in 1:N){
      XXdis[,,i] = diag(G)[,-1]%*%(diag(G-1)%x%t(DM[i,]))
    }
    while((lk-lko>10^-6 & it<100) | it==0){
      it = it+1; lko = lk 
      sc = 0; Fi = 0
      for(i in 1:N){
        pdis = eta[i,]
        sc = sc+t(XXdis[,,i])%*%(z[i,]-pdis)
        Fi = Fi+t(XXdis[,,i])%*%(diag(pdis)-pdis%o%pdis)%*%XXdis[,,i]
      }
      
      dbe = as.vector(ginv(Fi)%*%sc)
      mdbe = max(abs(dbe))
      if(mdbe>0.5) dbe = dbe/mdbe*0.5
      be0 = c(beta)
      flag = TRUE
      while(flag){
        beta = be0+dbe
        Eta = matrix(0,N,G)
        for(i in 1:N){
          
          if(ncol(DM)==1) Eta[i,] = XXdis[,,i]*beta
          else Eta[i,] = XXdis[,,i]%*%beta
        }	
        if(max(abs(Eta))>100){
          dbe = dbe/2
          flag = TRUE	
        }else{
          flag = FALSE
        }	        	
      }
      if(iter/10 == floor(iter/10))       print(beta)
      
      beta = matrix(beta, J, G-1)    
      exb <- exp(DM %*% beta) # updfe priors
      eta <- cbind(1,exb)/(rowSums(exb)+1)
      
      lk = sum(z*log(eta))
    }
    
    
    # E-step
    PS = array(apply(p, 1, function(xx) dbinom(c(X), 1, xx)), c(N, M, G))
    v <- eta *  apply(PS, c(1,3), prod)
    # v[is.nan(v)] <- 0
    # v[v < 0] <- 0
    vsum <- rowSums(v)
    z <- v / vsum      # posterior
    ll <- rep(1, N) %*% log(vsum) # log-likelihood
    
    
    # Aitken acceleration
    
    l <- c(l[-1], ll)
    Out <- AitkenAcc(l, lA, iter)
    diff <- Out[1]
    # print(diff)
    lA <- Out[-1]
    
    # diff.ll <- ll-ll.old
    
    print(iter)
    
  } # end EM
  
  p <- (t(z) %*% X) / colSums(z)	
  
  BIC <- -2*ll + (J*(G-1)+G*M) * log(N)
  
  expected <- vsum * N
  
  q <- qnorm(0.975) # CI
  u.beta <- beta + q*se.beta
  l.beta <- beta - q*se.beta
  
  colnames(p) <- NULL
  rownames(p) <- rownames(p, do.NULL = FALSE, prefix = "Group ")
  colnames(p) <- colnames(p, do.NULL = FALSE, prefix = "p_g")
  colnames(beta) <- 2:G
  rownames(beta) <- c(colnames(DM))
  names(ll)<- "Log-Likelihood:"
  names(BIC)<-"BIC:"
  
  out <- list(p = p, eta = eta, LL = ll, BIC = BIC)
  list(p = p, eta = eta, LL = ll, BIC = BIC, z = z, expected = expected,
       beta = beta, se.beta = se.beta, rrr=exp(beta), u.beta=u.beta, l.beta=l.beta) 
}



f_lca_nstarts <- function(X, DM, G, nstarts, tol, maxiter, beta0) {
  
  out <- f_lca(X, DM, G, tol, maxiter, beta0)
  
  foreach(i=2:nstarts, .packages = c("MASS","igraph"), .export = "f_lca") %dopar% {
    out1 <- f_lca(X, DM, G, tol, maxiter, beta0)
    if(out1$LL > out$LL) out <- out1
  }
  out
}



lca <- function(X, DM, G, nstarts = 3, tol = 0.1^2, maxiter = 250, beta0=NULL) {
  
  if (any(G < 1)) {
    print("Specify G > 0!")
    return("Specify G > 0!")
  }
  
  if (any(G == 1)) {
    out <- f_lca_nstarts(X, DM, G, nstarts, tol, maxiter, beta0 = beta0)
  } else{
    if (length(G) == 1) {
      out <- f_lca_nstarts(X, DM, G, nstarts, tol, maxiter, beta0 = beta0)
    } else{
      out <- vector("list", length(G) + 1)
      names(out) <- c(paste('G', G, sep = '='), 'BIC')
      i <- 0
      for (g in G) {
        i <- i + 1
        out[[i]] <- f_lca_nstarts(X, DM, g, nstarts, tol, maxiter, beta0 = beta0)
      }
      out[[length(G) + 1]] <- tableBIC(out)
    }
  }
  out
}



buildYYh <- function(YY, mu, dimy, N){
  
  YYh <- matrix(NA, (dimy + 1) * (dimy + 1), N)
  
  for(ddi in 1:dimy){
    YYh[((ddi - 1) * dimy + ddi):(ddi - 1 + (ddi * dimy)),] <- YY[((ddi - 1) * dimy + 1):(ddi * dimy),]
    YYh[ddi * dimy + ddi,] <- mu[ddi,]
  }
  
  YYh[(dimy * (dimy + 1) + 1):((dimy + 1) * (dimy + 1) - 1),] <- mu[1:ddi,]
  
  YYh[(dimy + 1) * (dimy + 1),] <- 1
  
  YYh		
}



f_mlta_nstarts <- function(X, DM, G, D, nstarts, tol, maxiter, pdGH, beta0)
{
  out <- try(f_mlta(X, DM, G, D, tol, maxiter, pdGH, beta0))
  
  if(nstarts > 1){ 
    foreach(i=2:nstarts, .packages = c("MASS","igraph"), .export = "f_mlta") %dopar% {
      out1 <- try(f_mlta(X, DM, G, D, tol, maxiter, pdGH, beta0))
      if(out1$LL > out$LL) out <- out1
    }
  }
  return(out)
}



f_mlta_wfix <- function(X, DM, G, D, tol, maxiter, pdGH, beta0)
{
  
  N <- nrow(X)
  M <- ncol(X) 
  J <- ncol(DM)
  
  # Initialize EM Algorithm
  
  if(!is.null(beta0)) beta = beta0 else{
    beta <- rep(0,J*(G-1))
    beta <- matrix(beta,ncol=G-1)
  }
  
  exb <- exp(DM %*% beta)
  eta <- cbind(1,exb)/(rowSums(exb)+1)  # priors
  
  
  z <- matrix(NA, nrow=N, ncol=G)
  for(i in 1:N)   z[i,] <- t(rmultinom(1, size = 1, prob = eta[i,]))
  
  # Set up
  
  p <- (t(z) %*% X) / colSums(z)
  
  
  ###
  
  xi <- array(20, c(N, M, G))
  sigma_xi <- 1 / (1 + exp(-xi))
  lambda_xi <- (0.5 - sigma_xi) / (2 * xi)
  
  w <- matrix(rnorm(M * D), D, M)
  b <- matrix(rnorm(M, G), G, M)
  
  
  wh <- matrix(0, D + G, M)
  
  C <- array(0, c(D * D, N, G))
  
  mu <- array(0, c(N, D, G))
  
  YY <- array(0, c(D * D, N, G))
  
  gam <- matrix(0, D + G, M)
  K <- array(diag(D + G), c(D + G, D + G, M))
  
  lxi <- matrix(0, G, N)
  
  # Iterative process
  
  v <- matrix(0, N, G)
  W <- list()
  beta_change <- rep(NA, (J)*(G-1))
  beta_change <- matrix(beta_change, ncol=(G-1), nrow=J)
  se.beta <- rep(NA, (J)*(G-1))
  se.beta <- matrix(se.beta, ncol=(G-1), nrow=J)
  ll <- -Inf
  diff <- 1
  iter <- 0
  cond <- TRUE
  
  tol <- 0.1 ^ 4
  print(c(beta))
  while (diff > tol & iter < maxiter)
  {
    iter <- iter + 1
    beta.old=beta
    ll_old <- ll
    
    if (D == 1) {
      for (g in 1:G)
      {
        # # STEP 1: Computing the Latent Posterior Statistics
        
        C[, , g] <-
          1 / (1 - 2 * rowSums(sweep(lambda_xi[, , g], MARGIN = 2, w ^ 2, `*`)))
        mu[, , g] <-
          C[, , g] * rowSums(sweep(
            X - 0.5 + 2 * sweep(lambda_xi[, , g], MARGIN = 2, b[g, ], `*`),
            MARGIN = 2, w, `*`))
        
        YY[, , g] <- matrix(C[, , g] + mu[, , g] ^ 2, ncol = 1)
        
        xi[, , g] <- YY[, , g] %*% w^2 + mu[, , g] %*% (2 * b[g, ] * w) + 
          matrix(b[g, ]^2, nrow = N, ncol = M, byrow = TRUE)
      }
      
      # STEP 3: Optimising the Model Parameters (w and b)
      
      xi <- sqrt(xi)
      sigma_xi <- 1 / (1 + exp(-xi))
      lambda_xi <- (0.5 - sigma_xi) / (2 * xi)
      
      gam[1, ] <- colSums(crossprod(z * mu[, 1, ], X - 0.5))
      gam[2:(1 + G), ] <- crossprod(z, X - 0.5)
      
      K[1, 1, ] <- apply(aperm(array(z * YY[1, , ], c(N, G, M)), c(1, 3, 2)) * lambda_xi, 2, sum)
      
      K[2:(G + 1), 1, ] <- t(apply(aperm(array(
        z * mu[, 1, ], c(N, G, M)
      ), c(1, 3, 2)) * lambda_xi, c(2, 3), sum))
      K[1, 2:(G + 1), ] <- K[2:(G + 1), 1, ]
      
      for (m in 1:M)
      {
        for (g in 1:G)
          K[1 + g, 1 + g, m] <- sum(z[, g] * lambda_xi[, m, g])
        
        wh[, m] <- -solve(2 * K[, , m]) %*% gam[, m]
      }
      
      w <- wh[1:D, ]
      
      w <- as.matrix(t(w))
      
      for (g in 1:G)
      {
        b[g, ] <- wh[D + g, ]
        
        lxi[g, ] <-
          0.5 * log(C[1, , g]) + mu[, 1, g] ^ 2 / (2 * C[1, , g]) + rowSums(
            log(sigma_xi[, , g]) - 0.5 * xi[, , g] - lambda_xi[, , g] * xi[, , g] ^ 2 + 
              sweep(lambda_xi[, , g], MARGIN = 2, b[g, ] ^ 2, `*`) + 
              sweep(X - 0.5, MARGIN = 2, b[g, ], `*`))
      } # end for (g in 1:G)
    }
    
    if (D == 2) {
      for (g in 1:G)
      {
        # # STEP 1: Computing the Latent Posterior Statistics
        
        C[, , g] <-
          apply(lambda_xi[, , g], 1, function(x)
            solve(diag(D) - 2 * crossprod(x * t(w), t(w))))
        
        mu[, , g] <-
          t(apply(rbind(C[, , g], 
                        tcrossprod(w, X - 0.5 + 2 * sweep(lambda_xi[, , g], MARGIN = 2, b[g, ], `*`))), 
                  2, function(x) matrix(x[1:4], nrow = D) %*% x[-(1:4)]))
        
        # # STEP 2: Optimising the Variational Parameters (xi)
        
        YY[, , g] <- C[, , g] + apply(mu[, , g], 1, tcrossprod)
        
        xi[, , g] <-
          t(
            apply(YY[, , g], 2, function(x)
              rowSums(crossprod(w, matrix(x, ncol = D)) * t(w))) + 
              tcrossprod(2 * b[g, ] * t(w), mu[, , g]) + matrix(
                b[g, ] ^ 2,
                nrow = M,
                ncol = N,
                byrow = FALSE
              )
          )
      }
      
      # STEP 3: Optimising the Model Parameters (w and b)
      
      xi <- sqrt(xi)
      sigma_xi <- 1 / (1 + exp(-xi))
      lambda_xi <- (0.5 - sigma_xi) / (2 * xi)
      
      gam[3:(2 + G), ] <- crossprod(z, X - 0.5)
      
      aa <- aperm(array(z, c(N, G, D)), c(1, 3, 2)) * mu
      bb <- aperm(array(z, c(N, G, D * D)), c(3, 1, 2)) * YY
      
      K[3:(2 + G), 3:(2 + G), ] <-
        apply(apply(aperm(array(z, c(
          N, G, M
        )), c(1, 3, 2)) * lambda_xi, c(2, 3), sum), 1, diag)
      
      kk <- 0
      for (g in 1:G) {
        K[2 + g, 1:2, ] <- crossprod(aa[, , g], lambda_xi[, , g])
        K[1:2, 2 + g, ] <- K[2 + g, (1:2), ]
        kk <- kk + bb[, , g] %*% lambda_xi[, , g]
      }
      
      K[1:2, 1:2, ] <- kk
      
      for (m in 1:M)
      {
        gam[1:D, m] <- apply(aa * (X[, m] - 0.5), 2, sum)
        wh[, m] <- -solve(2 * K[, , m]) %*% gam[, m]
      }
      
      w <- wh[1:D, ]
      
      for (g in 1:G)
      {
        b[g, ] <- wh[D + g, ]
        
        # Approximation of log(p(x|z))
        
        detC <- C[1, , g] * C[4, , g] - C[3, , g] * C[2, , g]
        
        lxi[g, ] <-
          0.5 * log(detC) + 0.5 * apply(rbind(C[4, , g] / detC, -C[2, , g] / detC, -C[3, , g] /
                                                detC, C[1, , g] / detC, t(mu[, , g])), 2, function(x)
                                                  t((x[-(1:4)])) %*% matrix(x[1:4], nrow = D) %*% (x[-(1:4)])) + 
          rowSums(
            log(sigma_xi[, , g]) - 0.5 * xi[, , g] - lambda_xi[, , g] * xi[, , g]^2 + 
              sweep(lambda_xi[, , g], MARGIN = 2, b[g, ] ^ 2, `*`) + 
              sweep(X - 0.5, MARGIN = 2, b[g, ], `*`))
      } # end for (g in 1:G)
      
    }# end D=2
    
    if (D > 2) {
      for (g in 1:G)
      {
        # # STEP 1: Computing the Latent Posterior Statistics
        
        C[, , g] <-
          apply(lambda_xi[, , g], 1, function(x)
            solve(diag(D) - 2 * crossprod(x * t(w), t(w))))
        
        mu[, , g] <-
          t(apply(rbind(C[, , g], w %*% t(
            X - 0.5 + 2 * sweep(lambda_xi[, , g], MARGIN = 2, b[g, ], `*`)
          )), 2, function(x)
            matrix(x[1:(D * D)], nrow = D) %*% x[-(1:(D * D))]))
        
        # # STEP 2: Optimising the Variational Parameters (xi)
        
        YY[, , g] <- C[, , g] + apply(mu[, , g], 1, tcrossprod)
        
        xi[, , g] <-
          t(
            apply(YY[, , g], 2, function(x)
              rowSums((t(w) %*% matrix(x, ncol = D)) * t(w))) + 
              (2 * b[g, ] * t(w)) %*% t(mu[, , g]) + 
              matrix(
                b[g, ] ^ 2,
                nrow = M,
                ncol = N,
                byrow = FALSE
              )
          )
        
      }
      
      # STEP 3: Optimising the Model Parameters (w and b)
      
      xi <- sqrt(xi)
      sigma_xi <- 1 / (1 + exp(-xi))
      lambda_xi <- (0.5 - sigma_xi) / (2 * xi)
      
      gam[(D + 1):(D + G), ] <- t(z) %*% (X - 0.5)
      
      aa <- aperm(array(z, c(N, G, D)), c(1, 3, 2)) * mu
      bb <- aperm(array(z, c(N, G, D * D)), c(3, 1, 2)) * YY
      
      K[(D + 1):(D + G), (D + 1):(D + G), ] <-
        apply(apply(aperm(array(z, c(
          N, G, M
        )), c(1, 3, 2)) * lambda_xi, c(2, 3), sum), 1, diag)
      
      kk <- 0
      for (g in 1:G) {
        K[D + g, 1:D, ] <- crossprod(aa[, , g], lambda_xi[, , g])
        K[1:D, D + g, ] <- K[D + g, 1:D, ]
        kk <- kk + bb[, , g] %*% lambda_xi[, , g]
      }
      
      K[1:D, 1:D, ] <- kk
      
      for (m in 1:M)	{
        gam[1:D, m] <- apply(aa * (X[, m] - 0.5), 2, sum)
        wh[, m] <- -solve(2 * K[, , m]) %*% gam[, m]
      }
      
      w <- wh[1:D, ]
      
      for (g in 1:G)
      {
        b[g, ] <- wh[D + g, ]
        
        # Approximation of log(p(x|z))
        
        detC <- apply(C[, , g], 2, function(x)
          det(matrix(x, D, D)))
        
        lxi[g, ] <-
          0.5 * log(detC) + 0.5 * apply(rbind(C[, , g], t(mu[, , g])), 2, function(x)
            t((x[-(1:(D * D))])) %*% solve(matrix(x[1:(D * D)], nrow = D)) %*% (x[-(
              1:(D * D))])) + rowSums(
                log(sigma_xi[, , g]) - 0.5 * xi[, , g] - lambda_xi[, , g] *
                  xi[, , g]^2 + sweep(lambda_xi[, , g], MARGIN = 2, b[g, ] ^ 2, `*`) + 
                  sweep(X - 0.5, MARGIN = 2, b[g, ], `*`)
              )
      } # end for (g in 1:G)
      
    } # end if D > 2
    
    # M-step 
   
    lk = sum(z*log(eta))
    it = 0; lko = lk
    XXdis = array(0,c(G,(G-1)*ncol(DM),N))
    for(i in 1:N){
      XXdis[,,i] = diag(G)[,-1]%*%(diag(G-1)%x%t(DM[i,]))
    }
    while((lk-lko>10^-6 & it<100) | it==0){
      it = it+1; lko = lk 
      sc = 0; Fi = 0
      for(i in 1:N){
        pdis = eta[i,]
        sc = sc+t(XXdis[,,i])%*%(z[i,]-pdis)
        Fi = Fi+t(XXdis[,,i])%*%(diag(pdis)-pdis%o%pdis)%*%XXdis[,,i]
      }
      
      dbe = as.vector(ginv(Fi)%*%sc)
      mdbe = max(abs(dbe))
      if(mdbe>0.5) dbe = dbe/mdbe*0.5
      be0 = c(beta)
      flag = TRUE
      while(flag){
        beta = be0+dbe
        Eta = matrix(0,N,G)
        for(i in 1:N){
          
          if(ncol(DM)==1) Eta[i,] = XXdis[,,i]*beta
          else Eta[i,] = XXdis[,,i]%*%beta
        }	
        if(max(abs(Eta))>100){
          dbe = dbe/2
          flag = TRUE	
        }else{
          flag = FALSE
        }	        	
      }
      if(iter/10 == floor(iter/10))       print(beta)
      
      beta = matrix(beta, J, G-1)    
      exb <- exp(DM %*% beta) # updfe priors
      eta <- cbind(1,exb)/(rowSums(exb)+1)
      
      lk = sum(z*log(eta))
    }
    
    
    # E-step
    v <- eta * t(exp(lxi))
    if(any(is.nan(v))) browser()
    #v[is.nan(v)] <- 0
    vsum <- apply(v, 1, sum)
    z <- v / vsum
    ll <- sum(rep(1, N) * log(vsum))
    
    # Stopping Criteria
    
    diff <- sum(abs(ll - ll_old))
  
  } # end while(diff>tol)
  
  # Correction to the log-likelihood 
  
  # Gauss-Hermite Quadrature
  
  npoints <- round(pdGH ^ (1 / D))
  ny <- npoints ^ D
  GaHer <- glmmML::ghq(npoints, FALSE)
  Ygh <- expand.grid(rep(list(GaHer$zeros), D))
  Ygh <- as.matrix(Ygh)
  Wgh <-
    apply(as.matrix(expand.grid(rep(
      list(GaHer$weights), D
    ))), 1, prod) * apply(exp(Ygh ^ 2), 1, prod)
  
  Hy <- apply(Ygh, 1, mvtnorm::dmvnorm)
  Beta <- Hy * Wgh / sum(Hy * Wgh)
  
  fxy <- array(0, c(N, ny, G))
  
  for (g in 1:G)
  {
    Agh <- t(tcrossprod(t(w), Ygh) + b[g, ])
    pgh <- 1 / (1 + exp(-Agh))
    fxy[, , g] <-
      exp(tcrossprod(X, log(pgh)) + tcrossprod(1 - X, log(1 - pgh)))
  }
  
  
  LLva <- ll
  BICva <- -2 * LLva + {
    G * M + M * D - D * {
      D - 1
    } / 2 + J*(G - 1)
  } * log(N)
  
  llGH1 <- apply(rep(Beta, each = N) * fxy, c(1, 3), sum)
  LL <- sum(log(rowSums(eta * (llGH1))))
  
  BIC <- -2 * LL + {
    G * M + M * D - D * {
      D - 1
    } / 2 + J*(G - 1)
  } * log(N)
  
  expected <- colSums(eta * (llGH1)) * N
  
  q <- qnorm(0.975) # CI
  u.beta <- beta + q*se.beta
  l.beta <- beta - q*se.beta
  
  rownames(b) <- NULL
  colnames(b) <- colnames(b, do.NULL = FALSE, prefix = "Item ")
  rownames(b) <- rownames(b, do.NULL = FALSE, prefix = "Group ")
  
  rownames(w) <- rownames(w, do.NULL = FALSE, prefix = "Dim ")
  colnames(w) <- colnames(w, do.NULL = FALSE, prefix = "Item ")
  
  colnames(beta) <- 2:G
  rownames(beta) <- c(colnames(DM))
  
  names(LLva) <- c("Log-Likelihood (variational approximation):")
  names(BICva) <- c("BIC (variational approximation):")
  names(LL) <- c("Log-Likelihood (G-H Quadrature correction):")
  names(BIC) <- c("BIC (G-H Quadrature correction):")
  
  out1 <-
    list(
      b = b,
      w = w,
      eta = eta,
      LL = LL,
      BIC = BIC,
      LLva = LLva,
      BICva = BICva,
      expected = expected,
      mu = mu,
      C = C,
      z = z,
      beta = beta, 
      se.beta = se.beta, 
      rrr=exp(beta), 
      u.beta=u.beta, 
      l.beta=l.beta
    )
  
  out1
  
}


f_mlta_nstarts_wfix <- function(X, DM, G, D, nstarts, tol, maxiter, pdGH, beta0)
{
  
  out <- try(f_mlta_wfix(X, DM, G, D, tol, maxiter, pdGH, beta0))
  
  if(nstarts > 1){ 
    for(i in 2:nstarts) {
      
      out1 <- try(f_mlta_wfix(X, DM, G, D, tol, maxiter, pdGH, beta0))
      if(out1$LL > out$LL)  out <- out1
    }
  }
  return(out)
}



f_mlta_methods <- function(X, DM, G, D, nstarts, tol, maxiter, pdGH, wfix, beta0=NULL)
{
  if (D == 0) {
    if (any(G == 1)) {
      out <- f_lca_nstarts(X, DM, G, nstarts, tol, maxiter, beta0=beta0)
    } else{
      if (length(G) == 1) {
        out <- f_lca_nstarts(X, DM, G, nstarts, tol, maxiter, beta0=beta0)
      } else{
        out <- vector("list", length(G) + 1)
        names(out) <- c(paste('G', G, sep = '='), 'BIC')
        i <- 0
        for (g in G) {
          i <- i + 1
          out[[i]] <- f_lca_nstarts(X, DM, g, nstarts, tol, maxiter, beta0=beta0)
        }
        out[[length(G) + 1]] <- tableBIC(out)
      }
    }
  } else {
    if (D > 0 && G == 1){
      if(length(D) == 1){ 
        out <- f_lta_nstarts(X, D, nstarts, tol, maxiter, pdGH)
        out$eta <- 1
      }else{
        out<-vector("list", length(D) + 1)
        names(out) <- c(paste('Dim y', D, sep = '='), 'BIC')
        i<-0
        for(diy in D){
          i <- i + 1
          out[[i]] <- f_lta_nstarts(X, diy, nstarts, tol, maxiter, pdGH)
          out[[i]]$eta <- 1
        }
        
        cat('BIC results',"\n \n")
        out[[length(D) + 1]]<-tableBIC(out)
      }
    }
    if (D > 0 && G > 1) {
      if (wfix == TRUE) {
        
        out <- f_mlta_nstarts_wfix(X, DM, G, D, nstarts, tol, maxiter, pdGH, beta0=beta0)
        class(out) <- c("mlta")
        
      } else{
        out <- f_mlta_nstarts(X, DM, G, D, nstarts, tol, maxiter, pdGH, beta0=NULL)
      }
    }
  }
  class(out) <- c("mlta")
  return(out)
}



ghLLlta <- function(X, w, b, D, pdGH) {
  b <- as.vector(b)
  N <- nrow(X)
  M <- ncol(X)
  npoints <- round(pdGH ^ (1 / D))
  ny <- npoints^D
  GaHer <- glmmML::ghq(npoints, modified = FALSE)
  ygh <- GaHer$zeros
  wgh <- GaHer$weights
  Ygh <- as.matrix(expand.grid(rep(list(ygh), D)))
  Wghm <- as.matrix(expand.grid(rep(list(wgh), D)))
  Wgh <- apply(Wghm, 1, prod) * apply(exp(Ygh ^ 2), 1, prod)
  Hy <- apply(Ygh, 1, mvtnorm::dmvnorm)
  Beta <- Hy * Wgh / sum(Hy * Wgh)
  Agh <- t(t(w) %*% t(Ygh) + b)
  pgh <- 1 / (1 + exp(-Agh))
  
  fxy <- exp(X %*% t(log(pgh)) + (1 - X) %*% t(log(1 - pgh)))
  
  llGH1 <- rowSums(rep(Beta, each = N) * fxy)
  llGH <- sum(log(llGH1))
  expected <- llGH1 * N
  
  list(expected = expected, llGH = llGH)
}


f_lta <- function(X, D, tol, maxiter, pdGH) {
  
  N <- nrow(X)
  M <- ncol(X) 
  
  # Initialization
  
  xi <- matrix(20, N, M)
  w <- matrix(rnorm(M * D), D, M)
  b <- rnorm(M)
  
  sigma_xi <- 1 / (1 + exp(-xi))
  lambda_xi <- (0.5 - sigma_xi) / (2 * xi)
  
  diff <- 1
  iter <- 0L
  ll <- -Inf
  
  while (diff > tol & iter < maxiter) {
    iter <- iter + 1
    ll_old <- ll
    
    if (D == 1) {
      # # STEP 1: Computing the Latent Posterior Statistics
      
      C <-
        1 / (1 - 2 * rowSums(sweep(lambda_xi, MARGIN = 2, w ^ 2, `*`)))
      mu <-
        C * rowSums(sweep(
          X - 0.5 + 2 * sweep(lambda_xi, MARGIN = 2, b, `*`),
          MARGIN = 2,
          w,
          `*`
        ))
      
      # # STEP 2: Optimising the Variational Parameters (xi)
      
      YY <- matrix(C + mu ^ 2, ncol = 1)
      mu <- matrix(mu, N, D)
      
      xi <-
        YY %*% w ^ 2 + mu %*% (2 * b * w) + matrix(b ^ 2,
                                                   nrow = N,
                                                   ncol = M,
                                                   byrow = TRUE)
      
      xi <- sqrt(xi)
      
      sigma_xi <- 1 / (1 + exp(-xi))
      lambda_xi <- (0.5 - sigma_xi) / (2 * xi)
      
      # STEP 3: Optimising the Model Parameters (w and b)
      
      YYh <- 2 * cbind(c(YY), mu, mu, 1)
      
      den <- t(YYh) %*% lambda_xi
      num <- rbind(c(mu), 1) %*% ((X - 0.5))
      
      wh <-
        apply(rbind(den, num), 2, function(x)
          - crossprod(solve(matrix(x[1:4], 2, 2)), x[5:6]))
      
      w <- as.matrix(t(wh[1:D, ]))
      b <- wh[D + 1, ]
      
      # Approximation of Likelihood
      
      lxi <-  (0.5 * log(C) + c(mu) ^ 2 / (2 * C) +
                 rowSums(
                   log(sigma_xi) - 0.5 * xi - lambda_xi * (xi ^ 2) +
                     sweep(lambda_xi, MARGIN = 2, b ^ 2, `*`) +
                     sweep(X - 0.5, MARGIN = 2, b, `*`)
                 ))
      
      C <- array(C, c(D, D, N))
      
      ll <- sum(lxi)
      bw <- t(rbind(b, w))
      
      # Stopping Criteria
      
      diff <- sum(abs(ll - ll_old))
    } # end if D=1
    
    if (D == 2) {
      # # STEP 1: Computing the Latent Posterior Statistics
      
      C <- apply(lambda_xi, 1, function(x)
        solve(diag(D) - 2 * crossprod(x * t(w), t(w))))
      
      mu <- apply(rbind(C, w %*% t(
        X - 0.5 + 2 * sweep(lambda_xi, MARGIN = 2, b, `*`)
      )),
      2,
      function(x)
        matrix(x[1:4], nrow = D) %*% (x[-(1:4)]))
      
      # # STEP 2: Optimising the Variational Parameters (xi)
      
      YY <- C + apply(mu, 2, tcrossprod)
      
      xi <-
        apply(YY, 2, function(x)
          rowSums((t(w) %*% matrix(x, ncol = D)) * t(w))) +
        (2 * b * t(w)) %*% mu +
        matrix(b ^ 2,
               nrow = M,
               ncol = N,
               byrow = FALSE)
      
      xi <- t(sqrt(xi))
      sigma_xi <- 1 / (1 + exp(-xi))
      lambda_xi <- (0.5 - sigma_xi) / (2 * xi)
      
      # STEP 3: Optimising the Model Parameters (w and b)
      
      YYh <-
        2 * cbind(YY[1, ], YY[2, ], mu[1, ], YY[3, ], YY[4, ], mu[2, ], mu[1, ], mu[2, ], 1)
      
      den <- t(YYh) %*% lambda_xi
      num <- rbind(mu, 1) %*% ((X - 0.5))
      wh <-
        apply(rbind(den, num), 2, function(x)
          - crossprod(solve(matrix(x[1:9], 3, 3)), x[10:12]))
      
      w <- wh[1:D, ]
      b <- wh[D + 1, ]
      
      # Approximation of Likelihood
      
      detC <- C[1, ] * C[4, ] - C[3, ] * C[2, ]
      
      lxi <- (
        0.5 * log(detC) +
          0.5 * apply(rbind(C[4, ] / detC,-C[2, ] / detC,-C[3, ] / detC, C[1, ] / detC, mu), 2,
                      function(x)
                        t((x[-(1:4)])) %*% matrix(x[1:4], nrow = D) %*% (x[-(1:4)])) +
          rowSums(
            log(sigma_xi) - 0.5 * xi - lambda_xi * (xi ^ 2) +
              sweep(lambda_xi, MARGIN = 2, b ^ 2, `*`) + sweep(X - 0.5, MARGIN = 2, b, `*`)
          )
      )
      
      C <- array(C, c(D, D, N))
      mu <- t(mu)
      
      ll <- sum(lxi)
      
      # Stopping Criteria
      
      diff <- sum(abs(ll - ll_old))
      
    }# end if D=2
    
    if (D > 2) {
      # # STEP 1: Computing the Latent Posterior Statistics
      
      C <-
        apply(lambda_xi, 1, function(x)
          solve(diag(D) - 2 * crossprod(x * t(w), t(w))))
      
      mu <-
        apply(rbind(C, w %*% t(
          X - 0.5 + 2 * sweep(lambda_xi, MARGIN = 2, b, `*`)
        )) ,
        2, function(x)
          matrix(x[1:(D * D)], nrow = D) %*% (x[-(1:(D * D))]))
      
      # # STEP 2: Optimising the Variational Parameters (xi)
      
      YY <- C + apply(mu, 2, tcrossprod)
      
      xi <-
        t(
          apply(YY, 2, function(x)
            rowSums((
              t(w) %*% matrix(x, ncol = D)
            ) * t(w))) +
            (2 * b * t(w)) %*% mu + matrix(
              b ^ 2,
              nrow = M,
              ncol = N,
              byrow = FALSE
            )
        )
      
      xi <- sqrt(xi)
      sigma_xi <- 1 / (1 + exp(-xi))
      lambda_xi <- (0.5 - sigma_xi) / (2 * xi)
      
      # # STEP 3: Optimising the Model Parameters (w and b)
      
      YYh <- sweep(buildYYh(YY, mu, D, N), MARGIN = 2, 2, `*`)
      
      den <- YYh %*% lambda_xi
      
      num <- rbind(mu, 1) %*% ((X - 0.5))
      
      wh <-
        apply(rbind(den, num), 2, function(x)
          - crossprod(solve(matrix(x[1:((D + 1) * (D + 1))], D + 1, D + 1)), x[-(1:((D + 1) * (D + 1)))]))
      
      w <- wh[1:D, ]
      b <- wh[D + 1, ]
      
      # Approximation of Likelihood
      
      detC <- apply(C, 2, function(x)
        det(matrix(x, D, D)))
      
      lxi <- (
        0.5 * log(detC) +
          0.5 * apply(rbind(C, mu), 2, function(x)
            t((x[-(1:(D * D))])) %*% solve(matrix(x[1:(D * D)], nrow = D)) %*%
              (x[-(1:(D * D))])) +
          rowSums(
            log(sigma_xi) - 0.5 * xi - lambda_xi * xi ^ 2 +
              sweep(lambda_xi, MARGIN = 2, b ^ 2, `*`) + sweep(X - 0.5, MARGIN = 2, b, `*`)
          )
      )
      
      C <- array(C, c(D, D, N))
      mu <- t(mu)
      
      ll <- sum(lxi)
      
      # Stopping Criteria
      
      diff <- sum(abs(ll - ll_old))
      
    } # end if D>2
    
  } # End of while statement
  
  # correction to the log-likelihood
  
  ghLL <- ghLLlta(X, w, b, D, pdGH)
  expected <- ghLL$expected
  LL <- ghLL$llGH
  
  LLva <- ll
  
  BICva <- -2 * LLva + (M * (D + 1) - D * (D - 1) / 2) * log(N)
  BIC <- -2 * LL + (M * (D + 1) - D * (D - 1) / 2) * log(N)
  
  b <- t(as.matrix(b))
  rownames(b) <- NULL
  colnames(b) <- colnames(b, do.NULL = FALSE, prefix = "Item ")
  rownames(b) <- ""
  rownames(w) <- NULL
  colnames(w) <- colnames(w, do.NULL = FALSE, prefix = "Item ")
  rownames(w) <- rownames(w, do.NULL = FALSE, prefix = "Dim ")
  names(LLva) <- "Log-Likelihood (variational approximation):"
  names(BICva) <- "BIC (variational approximation):"
  names(LL) <- "Log-Likelihood (G-H Quadrature correction):"
  names(BIC) <- "BIC (G-H Quadrature correction):"
  
  list(
    b = b,
    w = w,
    LLva = LLva,
    BICva = BICva,
    LL = LL,
    BIC = BIC,
    mu = mu,
    C = C,
    expected = expected
  )
}



f_lta_nstarts <- function(X, D, nstarts, tol, maxiter, pdGH)
{
  out <- f_lta(X, D, tol, maxiter, pdGH)
  if(nstarts > 1){ 
    foreach(i=2:nstarts, .packages = c("MASS","igraph"), .export = "f_lta") %dopar% {
      out1 <- f_lta(X, D, tol, maxiter, pdGH)
      if(out1$LL > out$LL) out <- out1
    }
  }
  out
}



lta <- function(X, D, nstarts = 3, tol = 0.1^2, maxiter = 250, pdGH = 21) {
  
  if(any(D == 0)) stop("D must be > 0")
  
  if(length(D) == 1){ 
    out <- f_lta_nstarts(X, D, nstarts, tol, maxiter, pdGH)
  }else{
    out<-vector("list", length(D) + 1)
    names(out) <- c(paste('Dim y', D, sep = '='), 'BIC')
    i<-0
    for(diy in D){
      i <- i + 1
      out[[i]] <- f_lta_nstarts(X, diy, nstarts, tol, maxiter, pdGH)
    }
    out[[length(D) + 1]]<-tableBIC(out)
  }
  out
}



lift <- function(x, pdGH = 21)
{
  stopifnot(inherits(x, "mlta"))
  
  z <- x$z
  w <- x$w
  b <- x$b
  
  N <- nrow(z)
  D <- nrow(w)
  M <- ncol(b)
  G <- nrow(b)
  
  if (length(dim(w)) == 2)
    w <- array(w, c(dim(w), G))
  
  npoints <- round(pdGH ^ (1 / D))
  ny <- npoints ^ D
  GaHer <- glmmML::ghq(npoints, FALSE)
  Ygh <- expand.grid(rep(list(GaHer$zeros), D))
  Ygh <- as.matrix(Ygh)
  Wgh <-
    apply(as.matrix(expand.grid(rep(
      list(GaHer$weights), D
    ))), 1, prod) * apply(exp(Ygh ^ 2), 1, prod)
  
  Hy <- apply(Ygh, 1, mvtnorm::dmvnorm)
  Beta <- Hy * Wgh / sum(Hy * Wgh)
  Agh <- matrix(NA, ny, M)
  pgh <- matrix(NA, ny, M)
  fxy <- array(0, c(N, ny, G))
  
  px1 <- rep(NA, M)
  px1x1 <- matrix(0, M, M)
  lift <- array(NA, c(M, M, G))
  
  for (g in 1:G)
  {
    if (D == 1) {
      Agh <- t(tcrossprod(w[, , g], Ygh) + b[g, ])
    } else{
      Agh <- t(tcrossprod(t(w[, , g]), Ygh) + b[g, ])
    }
    pgh <- 1 / (1 + exp(-Agh))
    px1 <- colSums(Beta * pgh)
    px1x1 <- crossprod(sqrt(Beta) * pgh)
    
    Mat <- (px1x1 / tcrossprod(px1)) * upper.tri(matrix(1, M, M))
    Mat[lower.tri(Mat)] <- NA
    diag(Mat) <- NA
    
    lift[, , g] <-  Mat
  }
  
  rownames(lift) <- rownames(lift, do.NULL = FALSE, prefix = "")
  colnames(lift) <- colnames(lift, do.NULL = FALSE, prefix = "")
  dimnames(lift)[[3]] <- paste("g =", seq(1, G) , sep = ' ')
  
  lift
}



mlta <- function(X, DM, G, D, wfix = FALSE, nstarts = 3, tol = 0.1 ^ 2, maxiter = 250, pdGH = 21, 
                 beta0 = NULL){
  if (any(G < 1)) {
    print("Specify G > 0!")
    return("Specify G > 0!")
  }
  
  if (any(D < 0)) {
    print("Specify D >= 0!")
    return("Specify D >= 0!")
  }
  
  
  if (length(D) == 1 && length(G) == 1) {
    out <- f_mlta_methods(X, DM, G, D, nstarts, tol, maxiter, pdGH, wfix, beta0 = beta0)
  } else{
    out <- vector("list", length(D) * length(G) + 3)
    names(out) <- c(t(outer(
      paste('G=', G, sep = ''),
      paste('dim y=', D, sep = ''),
      paste,
      sep = ','
    )),
    'BIC', 'LL', 'LLva')
    
    bictab <- matrix(0, length(D), length(G))
    lltab <- matrix(0, length(D), length(G))
    
    rownames(bictab) <- paste('dim y=', D, sep = '')
    colnames(bictab) <- paste('G=', G, sep = '')
    
    rownames(lltab) <- paste('dim y=', D, sep = '')
    colnames(lltab) <- paste('G=', G, sep = '')
    
    llvatab <- lltab
    
    i <- 0
    for (g in G){
      for (diy in D) {
        i <- i + 1
        
        out[[i]] <- f_mlta_methods(X, DM, g, diy, nstarts, tol, maxiter, pdGH, wfix, beta0=beta0)
        print("ciao")
        bictab[i] <- out[[i]]$BIC
        lltab[i] <- out[[i]]$LL
        
        if (diy == 0) {
          llvatab[i] <- out[[i]]$LL
        } else{
          llvatab[i] <- out[[i]]$LLva
        }
        
        out[[length(G) * length(D) + 1]] <- ResTable(bictab, restype = 'BIC')
        out[[length(G) * length(D) + 2]] <- ResTable(lltab, restype = 'll')
        out[[length(G) * length(D) + 3]] <- ResTable(llvatab, restype = 'llva')
      }
    }
    class(out) <- "mmlta"
  }
  
  out
}



f_mlta <- function(X, DM, G, D, tol, maxiter, pdGH, beta0)
{
  
  N <- nrow(X)
  M <- ncol(X) 
  J <- ncol(DM)
  
  # Initialize EM Algorithm
  
  if(!is.null(beta0)) beta = beta0 else{
    beta <- rep(0,J*(G-1))
    beta <- matrix(beta,ncol=G-1)
  }
  
  exb <- exp(DM %*% beta)
  eta <- cbind(1,exb)/(rowSums(exb)+1)  # priors
  
  
  z <- matrix(NA, nrow=N, ncol=G)
  for(i in 1:N)   z[i,] <- t(rmultinom(1, size = 1, prob = eta[i,]))
  
  
  p <- (t(z) %*% X) / colSums(z)

  
  # Initialize Variational Approximation
  
  xi <- array(20, c(N, M, G))
  sigma_xi <- 1 / (1 + exp(-xi))
  lambda_xi <- (0.5 - sigma_xi) / (2 * xi)
  
  w <- array(rnorm(M * D * G), c(D, M, G))
  b <- matrix(rnorm(M, G), G, M)
  
  C <- array(0, c(D, D, N, G))
  mu <- array(0, c(N, D, G))
  
  lxi <- matrix(0, G, N)
  
  # Iterative process
  
  v <- matrix(0, N, G)
  W <- list()
  beta_change <- rep(NA, (J)*(G-1))
  beta_change <- matrix(beta_change, ncol=(G-1), nrow=J)
  se.beta <- rep(NA, (J)*(G-1))
  se.beta <- matrix(se.beta, ncol=(G-1), nrow=J)
  ll <- -Inf
  diff <- 1
  iter <- 0
  cond <- TRUE
  
  tol <- 0.1 ^ 6
  print(c(beta))
  
  xin <- xi
  
  while (diff > tol & iter < maxiter)
  {
    iter <- iter + 1
    beta.old=beta
    ll_old <- ll
    
    if (D == 1) {
      for (g in 1:G)
      {
        w[, , g] <- as.matrix(t(w[, , g]))
        
        # # STEP 1: Computing the Latent Posterior Statistics
        
        C[, , , g] <-
          1 / (1 - 2 * rowSums(sweep(lambda_xi[, , g], MARGIN = 2, w[, , g] ^ 2, `*`)))
        mu[, , g] <-
          C[, , , g] * rowSums(sweep(
            X - 0.5 + 2 * sweep(lambda_xi[, , g], MARGIN = 2, b[g, ], `*`),
            MARGIN = 2,
            w[, , g],
            `*`
          ))
        
        mu[, , g] <- matrix(mu[, , g], N, D)
        YY <- matrix(C[, , , g] + mu[, , g] ^ 2, ncol = 1)
        
        xi[, , g] <-
          YY %*% (w[, , g] ^ 2) + mu[, , g] %*% t(2 * b[g, ] * w[, , g]) +
          matrix(b[g, ] ^ 2,
                 nrow = N,
                 ncol = M,
                 byrow = TRUE)
        
        xi[, , g] <- sqrt(xi[, , g])
        sigma_xi[, , g] <- 1 / (1 + exp(-xi[, , g]))
        lambda_xi[, , g] <- (0.5 - sigma_xi[, , g]) / (2 * xi[, , g])
        
        # STEP 3: Optimising the Model Parameters (w and b)
        
        YYh <- z[, g] * 2 * cbind(c(YY), mu[, , g], mu[, , g], 1)
        
        den <- t(YYh) %*% lambda_xi[, , g]
        num <- rbind(c(mu[, , g]), 1) %*% (z[, g] * (X - 0.5))
        wh <-
          apply(rbind(den, num), 2, function(x)
            - crossprod(solve(matrix(x[1:4], 2, 2)), x[5:6]))
        
        w[, , g] <- wh[1:D, ]
        w[, , g] <- as.matrix(t(w[, , g]))
        b[g, ] <- wh[D + 1, ]
        
        # Approximation of log(p(x¦z))
        
        lxi[g, ] <-
          0.5 * log(C[, , , g]) + c(mu[, , g]) ^ 2 / (2 * C[, , , g]) +
          rowSums(
            log(sigma_xi[, , g]) - 0.5 * xi[, , g] - lambda_xi[, , g] * xi[, , g] ^
              2 +
              sweep(lambda_xi[, , g], MARGIN = 2, b[g, ] ^ 2, `*`) +
              sweep(X - 0.5, MARGIN = 2, b[g, ], `*`)
          )
        
      } # end for (g in 1:G)
      
      # M-step 
     
      lk = sum(z*log(eta))
      it = 0; lko = lk
      XXdis = array(0,c(G,(G-1)*ncol(DM),N))
      for(i in 1:N){
        XXdis[,,i] = diag(G)[,-1]%*%(diag(G-1)%x%t(DM[i,]))
      }
      while((lk-lko>10^-6 & it<100) | it==0){
        it = it+1; lko = lk 
        sc = 0; Fi = 0
        for(i in 1:N){
          pdis = eta[i,]
          sc = sc+t(XXdis[,,i])%*%(z[i,]-pdis)
          Fi = Fi+t(XXdis[,,i])%*%(diag(pdis)-pdis%o%pdis)%*%XXdis[,,i]
        }
        
        dbe = as.vector(ginv(Fi)%*%sc)
        mdbe = max(abs(dbe))
        if(mdbe>0.5) dbe = dbe/mdbe*0.5
        be0 = c(beta)
        flag = TRUE
        while(flag){
          beta = be0+dbe
          Eta = matrix(0,N,G)
          for(i in 1:N){
            
            if(ncol(DM)==1) Eta[i,] = XXdis[,,i]*beta
            else Eta[i,] = XXdis[,,i]%*%beta
          }	
          if(max(abs(Eta))>100){
            dbe = dbe/2
            flag = TRUE	
          }else{
            flag = FALSE
          }	        	
        }
        if(iter/10 == floor(iter/10))       print(beta)
        
        beta = matrix(beta, J, G-1)    
        exb <- exp(DM %*% beta) # updfe priors
        eta <- cbind(1,exb)/(rowSums(exb)+1)
        
        lk = sum(z*log(eta))
      }
      
      
      # E-step
      v <- eta * t(exp(lxi))
      if(any(is.nan(v))) browser()
      #v[is.nan(v)] <- 0
      vsum <- apply(v, 1, sum)
      z <- v / vsum
      ll <- sum(rep(1, N) * log(vsum))
      
      # Stopping Criteria
      
      diff <- sum(abs(ll - ll_old))
      
    } # end while(diff>tol)
    
    if (D == 2) {
      for (g in 1:G)
      {
        # # STEP 1: Computing the Latent Posterior Statistics
        
        C2 <-
          apply(lambda_xi[, , g], 1, function(x)
            solve(diag(D) - 2 * crossprod(x * t(w[, , g]), t(w[, , g]))))
        C[, , , g] <- array(C2, c(D, D, N))
        
        mu[, , g] <-
          t(apply(rbind(C2, w[, , g] %*% t(
            X - 0.5 + 2 * sweep(lambda_xi[, , g], MARGIN = 2, b[g, ], `*`)
          )), 2, function(x)
            matrix(x[1:4], nrow = D) %*% (x[-(1:4)])))
        
        # # STEP 2: Optimising the Variational Parameters (xi)
        
        YY <- C2 + apply(mu[, , g], 1, tcrossprod)
        
        xi[, , g] <-
          t(
            apply(YY, 2, function(x)
              rowSums((
                t(w[, , g]) %*% matrix(x, ncol = D)
              ) * t(w[, , g]))) + (2 * b[g, ] * t(w[, , g])) %*% t(mu[, , g]) + matrix(
                b[g, ] ^ 2,
                nrow = M,
                ncol = N,
                byrow = FALSE
              )
          )
        
        # YY<-array(YY,c(D,D,Ns))
        
        xi[, , g] <- sqrt(xi[, , g])
        sigma_xi[, , g] <- 1 / (1 + exp(-xi[, , g]))
        lambda_xi[, , g] <- (0.5 - sigma_xi[, , g]) / (2 * xi[, , g])
        
        # # STEP 3: Optimising the Model Parameters (w and b)
        
        YYh <-
          z[, g] * 2 * cbind(YY[1, ], YY[2, ], mu[, 1, g], YY[3, ], YY[4, ], mu[, 2, g], mu[, 1, g], mu[, 2, g], 1)
        
        den <- t(YYh) %*% lambda_xi[, , g]
        num <- rbind(t(mu[, , g]), 1) %*% {
          z[, g] * {
            X - 0.5
          }
        }
        wh <-
          apply(rbind(den, num), 2, function(x)
            - crossprod(solve(matrix(x[1:9], 3, 3)), x[10:12]))
        
        w[, , g] <- wh[1:D, ]
        w[, , g] <- as.matrix(w[, , g])
        b[g, ] <- wh[D + 1, ]
        
        # Approximation of log(p(x¦z))
        
        detC <- C2[1, ] * C2[4, ] - C2[3, ] * C2[2, ]
        
        lxi[g, ] <-
          0.5 * log(detC) + 0.5 * apply(rbind(C2[4, ] / detC, -C2[2, ] / detC, -C2[3, ] /
                                                detC, C2[1, ] / detC, t(mu[, , g])), 2, function(x)
                                                  t((x[-{
                                                    1:4
                                                  }])) %*% matrix(x[1:4], nrow = D) %*% (x[-{
                                                    1:4
                                                  }])) + rowSums(
                                                    log(sigma_xi[, , g]) - 0.5 * xi[, , g] - lambda_xi[, , g] * {
                                                      xi[, , g] ^ 2
                                                    } + sweep(lambda_xi[, , g], MARGIN = 2, b[g, ] ^ 2, `*`) + sweep(X - 0.5, MARGIN =
                                                                                                                       2, b[g, ], `*`)
                                                  )
        
      } # end for (g in 1:G)
      
      # M-step 
      
      lk = sum(z*log(eta))
      it = 0; lko = lk
      XXdis = array(0,c(G,(G-1)*ncol(DM),N))
      for(i in 1:N){
        XXdis[,,i] = diag(G)[,-1]%*%(diag(G-1)%x%t(DM[i,]))
      }
      while((lk-lko>10^-6 & it<100) | it==0){
        it = it+1; lko = lk 
        sc = 0; Fi = 0
        for(i in 1:N){
          pdis = eta[i,]
          sc = sc+t(XXdis[,,i])%*%(z[i,]-pdis)
          Fi = Fi+t(XXdis[,,i])%*%(diag(pdis)-pdis%o%pdis)%*%XXdis[,,i]
        }
        
        dbe = as.vector(ginv(Fi)%*%sc)
        mdbe = max(abs(dbe))
        if(mdbe>0.5) dbe = dbe/mdbe*0.5
        be0 = c(beta)
        flag = TRUE
        while(flag){
          beta = be0+dbe
          Eta = matrix(0,N,G)
          for(i in 1:N){
            
            if(ncol(DM)==1) Eta[i,] = XXdis[,,i]*beta
            else Eta[i,] = XXdis[,,i]%*%beta
          }	
          if(max(abs(Eta))>100){
            dbe = dbe/2
            flag = TRUE	
          }else{
            flag = FALSE
          }	        	
        }
        if(iter/10 == floor(iter/10))       print(beta)
        
        beta = matrix(beta, J, G-1)    
        exb <- exp(DM %*% beta) # updfe priors
        eta <- cbind(1,exb)/(rowSums(exb)+1)
        
        lk = sum(z*log(eta))
      }
      
      
      # E-step
      v <- eta * t(exp(lxi))
      if(any(is.nan(v))) browser()
      #v[is.nan(v)] <- 0
      vsum <- apply(v, 1, sum)
      z <- v / vsum
      ll <- sum(rep(1, N) * log(vsum))
      
      # Stopping Criteria
      
      diff <- sum(abs(ll - ll_old))
      
    } # end while(diff>tol)
    
    if (D > 2) {
      for (g in 1:G)
      {
        # # STEP 1: Computing the Latent Posterior Statistics
        
        C2 <-
          apply(lambda_xi[, , g], 1, function(x)
            solve(diag(D) - 2 * crossprod(x * t(w[, , g]), t(w[, , g]))))
        C[, , , g] <- array(C2, c(D, D, N))
        
        mu[, , g] <-
          t(apply(rbind(C2, w[, , g] %*% t(
            X - 0.5 + 2 * sweep(lambda_xi[, , g], MARGIN = 2, b[g, ], `*`)
          )), 2, function(x)
            matrix(x[1:(D * D)], nrow = D) %*% (
              x[-(1:(D * D))])))
        
        # # STEP 2: Optimising the Variational Parameters (xi)
        
        YY <- C2 + apply(mu[, , g], 1, tcrossprod)
        
        xi[, , g] <-
          t(
            apply(YY, 2, function(x)
              rowSums((t(w[, , g]) %*% matrix(x, ncol = D)) * t(w[, , g]))) + (2 * b[g, ] * t(w[, , g])) %*% t(mu[, , g]) +
              matrix(b[g, ] ^ 2,
                     nrow = M,
                     ncol = N,
                     byrow = FALSE)
          )
        
        xi[, , g] <- sqrt(xi[, , g])
        sigma_xi[, , g] <- 1 / (1 + exp(-xi[, , g]))
        lambda_xi[, , g] <- (0.5 - sigma_xi[, , g]) / (2 * xi[, , g])
        
        # # STEP 3: Optimising the Model Parameters (w and b)
        
        YYh <-
          sweep(buildYYh(YY, t(mu[, , g]), D, N), MARGIN = 2, 2 * z[, g], `*`)
        den <- (YYh) %*% lambda_xi[, , g]
        num <- rbind(t(mu[, , g]), 1) %*% (z[, g] * (X - 0.5))
        wh <-
          apply(rbind(den, num), 2, function(x)
            - crossprod(solve(matrix(x[1:((D + 1) * (D + 1))], D + 1, D + 1)), x[-(1:((D +
                                                                                         1) * (D + 1)))]))
        
        w[, , g] <- wh[1:D, ]
        w[, , g] <- as.matrix(w[, , g])
        b[g, ] <- wh[D + 1, ]
        
        # Approximation of log(p(x¦z))
        
        detC <- apply(C2, 2, function(x)
          det(matrix(x, D, D)))
        
        lxi[g, ] <-
          0.5 * log(detC) + 0.5 * apply(rbind(C2, t(mu[, , g])), 2, function(x)
            t((x[-(1:(D * D))])) %*% solve(matrix(x[1:(D * D)], nrow = D)) %*% (x[-(1:(D * D))])) + 
          rowSums(
            log(sigma_xi[, , g]) - 0.5 * xi[, , g] - lambda_xi[, , g] * (xi[, , g] ^
                                                                           2) + sweep(lambda_xi[, , g], MARGIN = 2, b[g, ] ^ 2, `*`) + sweep(X - 0.5, MARGIN =
                                                                                                                                               2, b[g, ], `*`)
          )
      } # end for (g in 1:G)
      
      # M-step 
     
      lk = sum(z*log(eta))
      it = 0; lko = lk
      XXdis = array(0,c(G,(G-1)*ncol(DM),N))
      for(i in 1:N){
        XXdis[,,i] = diag(G)[,-1]%*%(diag(G-1)%x%t(DM[i,]))
      }
      while((lk-lko>10^-6 & it<100) | it==0){
        it = it+1; lko = lk 
        sc = 0; Fi = 0
        for(i in 1:N){
          pdis = eta[i,]
          sc = sc+t(XXdis[,,i])%*%(z[i,]-pdis)
          Fi = Fi+t(XXdis[,,i])%*%(diag(pdis)-pdis%o%pdis)%*%XXdis[,,i]
        }
        
        dbe = as.vector(ginv(Fi)%*%sc)
        mdbe = max(abs(dbe))
        if(mdbe>0.5) dbe = dbe/mdbe*0.5
        be0 = c(beta)
        flag = TRUE
        while(flag){
          beta = be0+dbe
          Eta = matrix(0,N,G)
          for(i in 1:N){
            
            if(ncol(DM)==1) Eta[i,] = XXdis[,,i]*beta
            else Eta[i,] = XXdis[,,i]%*%beta
          }	
          if(max(abs(Eta))>100){
            dbe = dbe/2
            flag = TRUE	
          }else{
            flag = FALSE
          }	        	
        }
        if(iter/10 == floor(iter/10))       print(beta)
        
        beta = matrix(beta, J, G-1)    
        exb <- exp(DM %*% beta) # updfe priors
        eta <- cbind(1,exb)/(rowSums(exb)+1)
        
        lk = sum(z*log(eta))
      }
      
      
      # E-step
      v <- eta * t(exp(lxi))
      if(any(is.nan(v))) browser()
      #v[is.nan(v)] <- 0
      vsum <- apply(v, 1, sum)
      z <- v / vsum
      ll <- sum(rep(1, N) * log(vsum))
      
      # Stopping Criteria
      
      diff <- sum(abs(ll - ll_old))
      
    } # end while(diff>tol)
    
  } # end while(diff>tol)
  
  # Correction to the log-likelihood
  
  # Gauss-Hermite Quadrature
  
  npoints <- round(pdGH ^ (1 / D))
  ny <- npoints ^ D
  GaHer <- glmmML::ghq(npoints, FALSE)
  Ygh <- expand.grid(rep(list(GaHer$zeros), D))
  Ygh <- as.matrix(Ygh)
  Wgh <- apply(as.matrix(expand.grid(rep(
    list(GaHer$weights), D
  ))), 1, prod) *
    apply(exp(Ygh ^ 2), 1, prod)
  
  Hy <- apply(Ygh, 1, mvtnorm::dmvnorm)
  Beta <- Hy * Wgh / sum(Hy * Wgh)
  
  fxy <- array(0, c(N, ny, G))
  
  for (g in 1:G)
  {
    if (D == 1) {
      Agh <- t(tcrossprod(w[, , g], Ygh) + b[g, ])
    } else{
      Agh <- t(tcrossprod(t(w[, , g]), Ygh) + b[g, ])
    }
    pgh <- 1 / (1 + exp(-Agh))
    
    fxy[, , g] <-
      exp(tcrossprod(X, log(pgh)) + tcrossprod(1 - X, log(1 - pgh)))
    fxy[is.nan(fxy[, , g])] <- 0
  }
  
  # eta <- as.vector(eta)
  
  LLva <- ll
  BICva <-
    -2 * LLva + (G * (M * (D + 1) - D * (D - 1) / 2) + J*(G - 1)) * log(N)
  
  llGH1 <- apply(rep(Beta, each = N) * fxy, c(1, 3), sum)
  LL <- sum(log(rowSums(eta * llGH1)))
  
  
  BIC <-
    -2 * LL + (G * (M * (D + 1) - D * (D - 1) / 2) + J*(G - 1)) * log(N)
  
  expected <- colSums(eta * (llGH1)) * N
  
  q <- qnorm(0.975) # CI
  u.beta <- beta + q*se.beta
  l.beta <- beta - q*se.beta
  
  rownames(b) <- NULL
  colnames(b) <- colnames(b, do.NULL = FALSE, prefix = "Item ")
  rownames(b) <- rownames(b, do.NULL = FALSE, prefix = "Group ")
  
  rownames(w) <- rownames(w, do.NULL = FALSE, prefix = "Dim ")
  colnames(w) <- colnames(w, do.NULL = FALSE, prefix = "Item ")
  dimnames(w)[[3]] <- paste("Group ", seq(1, G) , sep = '')
  
  colnames(beta) <- 2:G
  rownames(beta) <- c(colnames(DM))
  names(LLva) <- c("Log-Likelihood (variational approximation):")
  names(BICva) <- c("BIC (variational approximation):")
  names(LL) <- c("Log-Likelihood (G-H Quadrature correction):")
  names(BIC) <- c("BIC (G-H Quadrature correction):")
  
  out <- list(
    b = b,
    w = w,
    eta = eta,
    mu = mu,
    C = C,
    z = z,
    LL = LL,
    BIC = BIC,
    LLva = LLva,
    BICva = BICva,
    expected = expected,
    beta = beta, 
    se.beta = se.beta, 
    rrr=exp(beta), 
    u.beta=u.beta, 
    l.beta=l.beta
  )
  
  out
}


tableBIC <- function(out){
  lout <- length(out) - 1
  bicG <- numeric(lout)
  names(bicG) <- names(out[- (lout + 1)])
  
  for(i in 1:lout) bicG[i] <- out[[i]]$BIC
  
  resBIC <- vector('list', 2)
  names(resBIC) <- c('Model Selection', 'Table of BIC Results')
  resBIC[[1]] <- names(bicG)[bicG == min(bicG)]
  names(resBIC[[1]]) <- 'Model with lower BIC:'
  resBIC[[2]] <- bicG
  resBIC
}



print.mlta <- function(x){
  stopifnot(inherits(x, 'mlta'))
  cat("b:\n")
  print(x$b) 
  cat("\nw:\n")  
  print(x$w)
  cat("\neta:\n")
  print(x$eta)
  cat("\n")
  print(x$LL)
  print(x$BIC) 
}



print.mmlta <- function(x){
  cat("Log-Likelihood:\n")
  print(x$LL$`Table of LL (G-H Quadrature correction)`)
  cat("BIC:\n")
  print(x$BIC$`Table of BIC Results`)
  print(x$BIC$`Model Selection`)
}



## Simulation study ####

library(foreach)
library(parallel)
library(doParallel)
registerDoParallel(cores=30)
getDoParWorkers()

b.sim <- list()
w.sim <- list()
beta.sim <- list()
eta.sim <- list()
rand <- c()
# bic.sim <- array(,c(3,4,500)) # (for BIC simulation)

S <- 500
nstarts <- 10
D <- 1
N <- 5000 # 200, 500, 1000, 2000, 5000
M <- 14

# Setting with G=3
G <- 3
beta0 = matrix(c(1,-0.4,1.5,-0.9),2,2) 
load("~/w.RData")
load("~/b.RData")
b0 <- b
w0 <- w

# Setting with G=4
G <- 4
beta0 = matrix(c(1, 2, 3, -0.4, -0.9, -1.4),2,3, byrow=T)
load("~/b4.RData")
load("~/w4.RData")
b0 <- b
w0 <- w

# Setting with eta = 0.33 0.33 0.34
G=3
beta0 = matrix(c(0.46,-0.37,0.92,-0.91),2,2) 
load("~/b.RData")
load("~/w.RData")
b0 <- b
w0 <- w

# Setting with eta = 0.1 0.3 0.6
G=3
beta0 = matrix(c(1.5,-0.4,2.8,-0.9),2,2) 
load("~/b.RData")
load("~/w.RData")
b0 <- b
w0 <- w


# Setting with class separation
G=3
beta0 = matrix(c(1,-0.4,1.5,-0.9),2,2) 
load("~/bsep.RData")
load("~/w.RData")
b0 <- b
w0 <- w


# Setting without class separation
G=3
beta0 = matrix(c(1,-0.4,1.5,-0.9),2,2) 
load("~/bnsep.RData")
load("~/w.RData")
b0 <- b
w0 <- w


res <- foreach (s=1:S, .packages = c("MASS","igraph")) %dopar% {
  
  set.seed(s)
  DM = cbind(rep(1, N), rnorm(N, 1, 1))
  exb <- exp(DM %*% beta0)
  eta <- cbind(1,exb)/(rowSums(exb)+1)  
  
  z <- matrix(NA, nrow=N, ncol=G)
  for(i in 1:N){
    z[i,] <- t(rmultinom(1, size = 1, prob = eta[i,]))
  }
  
  ord <- order(apply(b0, 1, mean))
  cl = apply(z[,ord], 1, which.max)
  
  u <- rnorm(N, 0, 1)
  p <- matrix(NA, nrow=N, ncol=M)
  for (i in 1:N){
    for (m in 1:M){
      p[i,m] <- 1/(1+exp(-(b0[which.max(z[i,]),m]+w0[m]*u[i])))       }
  }
  
  
  y <- matrix(NA, nrow=N, ncol=M)
  for (i in 1:N){
    for (m in 1:M){
      y[i,m] <- rbinom(1, size = 1, prob = p[i,m])
    }
  }
  
  mod.sim <- mlta(X=y, DM=DM, G = 3, D = 1, wfix = TRUE, nstarts=nstarts)
  
  list(mod.sim,cl)
}


for(s in 1:S){
  
  # bic.sim[,,s] <- res[[s]][[1]]$BIC$`Table of BIC Results`# for BIC simulation
  
  b.sim[[s]] <- res[[s]][[1]]$b
  w.sim[[s]] <- res[[s]][[1]]$w
  beta.sim[[s]] <- res[[s]][[1]]$beta
  eta.sim[[s]] <- res[[s]][[1]]$eta
  
  ord.sim = order(apply(b.sim[[s]], 1, mean))
  c.sim <- apply(res[[s]][[1]]$z[,ord.sim],1,which.max)
  rand[s] <- adjustedRandIndex(res[[s]][[2]],c.sim)
}


summary(rand)


# beta

beta.sim2 = array(, c(2,G-1,S))
for (s in 1:S){
  print(s)
  if(!any(is.na(b.sim[[s]]))){
    ord = order(apply(b.sim[[s]], 1, mean))
    be = cbind(0, beta.sim[[s]])
    be = be[,ord]
    beta.sim2[,,s] = be[,2:G]-be[,1]
  }
}

betao = cbind(0, beta0)
ord = order(apply(b, 1, mean))
betao = betao[,ord]
print(betao[,2:G]-betao[,1])
print(apply(beta.sim2, c(1,2), mean))
print(apply(beta.sim2, c(1,2), mean, trim=0.1))
print(apply(beta.sim2, c(1,2), median))

betao=betao[,2:G]-betao[,1]
par(mar=c(5,3.3,2,2))
par(mfrow=c(3,6))
par(cex.lab=2, cex.axis=1.5)
for (h in 1:2) {
  for (k in 1:(G-1)) {
    boxplot(beta.sim2[h,k,], #xlab=bquote(bold(beta)[paste(.(h-1),.(k+1))]),
            col="lightcyan3", border=NA, frame=F, 
            las=2, ylim=c(betao[h,k]-1.5,betao[h,k]+1.5))
    grid(lty="solid")
    boxplot(beta.sim2[h,k,], #xlab=bquote(bold(beta)[paste(.(h),.(k+1))]), 
            col="lightcyan3", border="black", frame=F,
            add=TRUE, las=2, ylim=c(betao[h,k]-1.5,betao[h,k]+1.5))
    abline(h=betao[h,k], col="red3",lwd=2, lty="longdash")
    mtext(expression(paste(italic("N ="),200," ",italic("G ="),4)),side=3,outer=TRUE,padj=1.3, cex=1.3)
  }
}


## Relative bias ####

beta.sim2 = array(, c(2,G-1,S))
for (s in 1:S){
  print(s)
  if(!any(is.na(b.sim[[s]]))){
    ord = order(apply(b.sim[[s]], 1, mean))
    be = cbind(0, beta.sim[[s]])
    be = be[,ord]
    beta.sim2[,,s] = be[,2:G]-be[,1]
  }
}

betao = cbind(0, beta0)
ord = order(apply(b0, 1, mean))
betao = betao[,ord]
print(betao[,2:G]-betao[,1])
print(apply(beta.sim2, c(1,2), mean))
print(apply(beta.sim2, c(1,2), mean, trim=0.1))
print(apply(beta.sim2, c(1,2), median))
betao=betao[,2:G]-betao[,1]

for (h in 1:2) {
  for (k in 1:(G-1)) {
    print( (mean(beta.sim2[h,k,])-betao[h,k])/betao[h,k])
  }
}

# MSE ####

for(h in 1:2){
  for (k in 1:(G-1)) {
    print(mean((beta.sim2[h,k,]-betao[h,k])^2))
  }
}


## BIC ####

for(ng in 1:12){
  print(sum(apply(bic.sim[,,1:500],c(3),which.min)==ng)/500) 
}


## Covid-19 data analysis ####

G <- 2:4 

mod <- mlta(X=m, DM=DesMat, G = G, D = 1:3, wfix = FALSE, nstarts=10) 

mod.wfix <- mlta(X=m, DM=DesMat, G = G, D = 1:3, wfix = TRUE, nstarts=10) 

mod$BIC 

mod.wfix$BIC 

LTA <- lta(X=m, D=1:3) # G=1
LTA$BIC

LCA <- lca(X=m, DM=DesMat, G=G, nstarts=10) # D=0
LCA$BIC


# choice
M <- mod.wfix[[1]]

M$eta
M$z
M$b
M$w
M$beta
M$rrr

par(mar=c(5,5,5,5), xpd=FALSE)
par(mfrow=c(1,1))
plot(M$z[,1], pch = 19, col="cadetblue4",
     xlab = "Individui",
     ylab = "ProbabilitÃƒÂ  di appartenere al gruppo 1", cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2)
abline(h = 0.5, col = "red")

plot(M$z[,2], pch = 19, col="cadetblue4",
     xlab = "Individui",
     ylab = "ProbabilitÃƒÂ  di appartenere al gruppo 2", cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2)
abline(h = 0.5, col = "red")


class <- apply(M$z,1,which.max)
class
table(class)


italy$Group <- as.factor(class)

library(dplyr)
freq_table <- italy %>% group_by(Group) %>% summarise(freq = n())
italy$Group <- factor(italy$Group, levels = freq_table$Group[order(freq_table$freq, decreasing = F)])

Barplot(italy$Group, scale = c("percent"), conditional=TRUE, col="gray70",
        xlab = "Group", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, cex=1.5,
        ylab = "%", border="gray70", names.arg =c(1,2,3,4))
g <- table(italy$Group)
round(prop.table(g),digits=4)

g1 <- italy[italy$Group==1,]
i <- table(g1$illness)
round(prop.table(i),digits=4)

g2 <- italy[italy$Group==2,]
i <- table(g2$illness)
round(prop.table(i),digits=4)

par(mfrow=c(1,2))
boxplot(g1$age, main="Gruppo 1", cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, cex=2, ylim=c(18,83))
boxplot(g2$age, main="Gruppo 2", cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, cex=2, ylim=c(18,83))
summary(g1$age)
summary(g2$age)

r <- table(g1$region)
round(prop.table(r),digits=4)

r <- table(g2$region)
round(prop.table(r),digits=4)


e <- table(g1$empl)
round(prop.table(e),digits=4)

e <- table(g2$empl)
round(prop.table(e),digits=4)


gen <- table(g1$gender)
round(prop.table(gen),digits=4)

gen <- table(g2$gender)
round(prop.table(gen),digits=4)


## BOOTSTRAP ####

N <- nrow(m)
M <- ncol(m)
J <- ncol(DesMat)
S <- 1000
G <- 4

res <- foreach (s=1:S, .packages = c("MASS","igraph")) %dopar% {
  n.new <- sample(V(g)[1:N], size=N, replace=TRUE)
  m.new <- m[n.new, ]
  mod <- mlta(X=m.new, DM=DesMat, G = G, D = 1, wfix = FALSE, beta0=MOD$beta, nstarts=1)
}

res <- res[-112]
res <- res[-155]
res <- res[-618]
res <- res[-877]
res <- res[-904]
res <- res[-906]
res <- res[-995]

S=994

b1 <- matrix(NA, nrow=S , ncol=M)
b2 <- matrix(NA, nrow=S , ncol=M)
b3 <- matrix(NA, nrow=S , ncol=M)
b4 <- matrix(NA, nrow=S , ncol=M)
for (s in 1:S) {
  ord = order(apply(res[[s]]$b, 1, mean))
  bb = res[[s]]$b[ord,]
  b1[s,] <- as.vector(bb[1,])
  b2[s,] <- as.vector(bb[2,])
  b3[s,] <- as.vector(bb[3,])
  b4[s,] <- as.vector(bb[4,])
}


w1 <- matrix(NA, nrow=S , ncol=M)
w2 <- matrix(NA, nrow=S , ncol=M)
w3 <- matrix(NA, nrow=S , ncol=M)
w4 <- matrix(NA, nrow=S , ncol=M)
for (s in 1:S) {
  ord = order(apply(res[[s]]$b, 1, mean))
  ww = res[[s]]$w[,,ord]
  w1[s,] <- as.vector(ww[,1])
  w2[s,] <- as.vector(ww[,2])
  w3[s,] <- as.vector(ww[,3])
  w4[s,] <- as.vector(ww[,4])
}


beta2 <- matrix(NA, nrow=S, ncol=J)
beta3 <- matrix(NA, nrow=S, ncol=J)
beta4 <- matrix(NA, nrow=S, ncol=J)
for (s in 1:S) {
  b.sim = res[[s]]$b
  ord = order(apply(res[[s]]$b, 1, mean))
  be = cbind(rep(0,J), res[[s]]$beta)
  be = be[,ord]
  beta2[s,] <- be[,2]
  beta3[s,] <- be[,3]
  beta4[s,] <- be[,4]
}


se.b1 <- apply(b1,2,sd)
se.b2 <- apply(b2,2,sd)
se.b3 <- apply(b3,2,sd)
se.b4 <- apply(b4,2,sd)

se.w1 <- apply(abs(w1),2,sd)
se.w2 <- apply(abs(w2),2,sd)
se.w3 <- apply(abs(w3),2,sd)
se.w4 <- apply(abs(w4),2,sd)

se.beta2 <- apply(beta2, 2, sd)
se.beta3 <- apply(beta3, 2, sd)
se.beta4 <- apply(beta4, 2, sd)


z <- qnorm(0.975)

rosso <- rgb(0.7,0.13,0.13)
giallo <- rgb(1, 0.5, 0)
verde <- rgb(0,0.5,0)
blu <- rgb(0.27,0.51,0.71)

MOD = mod[[7]]
order(apply(MOD$b, 1, mean))

b_1 <- as.vector(MOD$b[1,])
u.b1 <- b_1 + z*se.b1
l.b1 <- b_1 - z*se.b1
x <- 1:14
#par(mfrow=c(1,2))
par(mar=c(5.1,5.1,4.1,2.1))
plotCI(x, b_1, ui=u.b1, li=l.b1, xlab = expression(r[k]), ylab="", pch = 19, main = "Group 1", xaxt="n",
       col=as.character(c(rosso,rosso,rosso,rosso,rosso,giallo,verde,verde,blu,blu,blu,blu,blu,giallo)),
       cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, cex=0.5, ylim=c(-5,8), lwd=2)
grid(lty="solid")
plotCI(x, b_1, ui=u.b1, li=l.b1, xlab = expression(r[k]), ylab="", pch = 19, main = "Group 1", 
       col=as.character(c(rosso,rosso,rosso,rosso,rosso,giallo,verde,verde,blu,blu,blu,blu,blu,giallo)),
       cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, cex=0.5, ylim=c(-5,8), lwd=2, add=TRUE)
axis(1, at=1:14, labels=c(1:14), las=1, cex=7)
abline(h=0, lty=2)
mtext(expression("b"),side=2,las=1,line=3, cex=2)

b_2 <- as.vector(MOD$b[4,])
u.b2 <- b_2 + z*se.b2
l.b2 <- b_2 - z*se.b2
x <- 1:14
plotCI(x, b_2, ui=u.b2, li=l.b2, xlab = expression(r[k]), ylab="", pch = 19, main = "Group 2", xaxt="n",
       col=as.character(c(rosso,rosso,rosso,rosso,rosso,giallo,verde,verde,blu,blu,blu,blu,blu,giallo)),
       cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, cex=0.5, ylim=c(-5,8), lwd=2)
grid(lty="solid")
plotCI(x, b_2, ui=u.b2, li=l.b2, xlab = expression(r[k]), ylab="", pch = 19, main = "Group 2",
       col=as.character(c(rosso,rosso,rosso,rosso,rosso,giallo,verde,verde,blu,blu,blu,blu,blu,giallo)),
       cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, cex=0.5, ylim=c(-5,8), lwd=2, add=TRUE)
axis(1, at=1:14, labels=c(1:14), las=1, cex=7)
abline(h=0, lty=2)
mtext(expression("b"),side=2,las=1,line=3, cex=2)


b_3 <- as.vector(MOD$b[2,])
u.b3 <- b_3 + z*se.b3
l.b3 <- b_3 - z*se.b3
x <- 1:14
plotCI(x, b_3, ui=u.b3, li=l.b3, xlab = expression(r[k]), ylab="", pch = 19, main = "Group 3", xaxt="n",
       col=as.character(c(rosso,rosso,rosso,rosso,rosso,giallo,verde,verde,blu,blu,blu,blu,blu,giallo)),
       cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, cex=0.5, ylim=c(-5,8), lwd=2)
grid(lty="solid")
plotCI(x, b_3, ui=u.b3, li=l.b3, xlab = expression(r[k]), ylab="", pch = 19, main = "Group 3",
       col=as.character(c(rosso,rosso,rosso,rosso,rosso,giallo,verde,verde,blu,blu,blu,blu,blu,giallo)),
       cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, cex=0.5, ylim=c(-5,8), lwd=2, add=TRUE)
axis(1, at=1:14, labels=c(1:14), las=1, cex=7)
abline(h=0, lty=2)
mtext(expression("b"),side=2,las=1,line=3, cex=2)


b_4 <- as.vector(MOD$b[3,])
u.b4 <- b_4 + z*se.b4
l.b4 <- b_4 - z*se.b4
x <- 1:14
plotCI(x, b_4, ui=u.b4, li=l.b4, xlab = expression(r[k]), ylab="", pch = 19, main = "Group 4", xaxt="n",
       col=as.character(c(rosso,rosso,rosso,rosso,rosso,giallo,verde,verde,blu,blu,blu,blu,blu,giallo)),
       cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, cex=0.5, ylim=c(-5,8), lwd=2)
grid(lty="solid")
plotCI(x, b_4, ui=u.b4, li=l.b4, xlab = expression(r[k]), ylab="", pch = 19, main = "Group 4",
       col=as.character(c(rosso,rosso,rosso,rosso,rosso,giallo,verde,verde,blu,blu,blu,blu,blu,giallo)),
       cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, cex=0.5, ylim=c(-5,8), lwd=2, add=TRUE)
axis(1, at=1:14, labels=c(1:14), las=1, cex=7)
abline(h=0, lty=2)
mtext(expression("b"),side=2,las=1,line=3, cex=2)


w_1 <- as.vector(MOD$w[,,1])
u.w1 <- w_1 + z*se.w1
l.w1 <- w_1 - z*se.w1
x <- 1:14
par(mar=c(5.1,5.1,4.1,2.1))
plotCI(x, w_1, ui=u.w1, li=l.w1, xlab = "Preventive measures", ylab = "", pch = 19, main = "Group 1",
       col=as.character(c(rosso,rosso,rosso,rosso,rosso,giallo,verde,verde,blu,blu,blu,blu,blu,giallo)),
       cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, cex=0.5, ylim=c(-1,4), lwd=2)
grid(lty="solid")
plotCI(x, w_1, ui=u.w1, li=l.w1, xlab = "Preventive measures", ylab = "", pch = 19, main = "Group 1",
       col=as.character(c(rosso,rosso,rosso,rosso,rosso,giallo,verde,verde,blu,blu,blu,blu,blu,giallo)),
       cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, cex=0.5, ylim=c(-1,4), lwd=2, add=TRUE)
abline(h=0, lty=2)
mtext(expression("w"),side=2,las=1,line=2.5, cex=2)

w_2 <- as.vector(MOD$w[,,4])
u.w2 <- w_2 + z*se.w2
l.w2 <- w_2 - z*se.w2
x <- 1:14
plotCI(x, w_2, ui=u.w2, li=l.w2, xlab = "Preventive measures", ylab = "", pch = 19, main = "Group 2",
       col=as.character(c(rosso,rosso,rosso,rosso,rosso,giallo,verde,verde,blu,blu,blu,blu,blu,giallo)),
       cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, cex=0.5, ylim=c(-1,4), lwd=2)
grid(lty="solid")
plotCI(x, w_2, ui=u.w2, li=l.w2, xlab = "Preventive measures", ylab = "", pch = 19, main = "Group 2",
       col=as.character(c(rosso,rosso,rosso,rosso,rosso,giallo,verde,verde,blu,blu,blu,blu,blu,giallo)),
       cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, cex=0.5, ylim=c(-1,4), lwd=2, add=TRUE)
abline(h=0, lty=2)
mtext(expression("w"),side=2,las=1,line=2.5, cex=2)

w_3 <- as.vector(MOD$w[,,2])
u.w3 <- w_3 + z*se.w3
l.w3 <- w_3 - z*se.w3
x <- 1:14
plotCI(x, w_3, ui=u.w3, li=l.w3, xlab = "Preventive measures", ylab = "", pch = 19, main = "Group 3",
       col=as.character(c(rosso,rosso,rosso,rosso,rosso,giallo,verde,verde,blu,blu,blu,blu,blu,giallo)),
       cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, cex=0.5, ylim=c(-1,4), lwd=2)
grid(lty="solid")
plotCI(x, w_3, ui=u.w3, li=l.w3, xlab = "Preventive measures", ylab = "", pch = 19, main = "Group 3",
       col=as.character(c(rosso,rosso,rosso,rosso,rosso,giallo,verde,verde,blu,blu,blu,blu,blu,giallo)),
       cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, cex=0.5, ylim=c(-1,4), lwd=2, add=TRUE)
abline(h=0, lty=2)
mtext(expression("w"),side=2,las=1,line=2.5, cex=2)

w_4 <- as.vector(MOD$w[,,3])
u.w4 <- w_4 + z*se.w4
l.w4 <- w_4 - z*se.w4
x <- 1:14
plotCI(x, w_4, ui=u.w4, li=l.w4, xlab = "Preventive measures", ylab = "", pch = 19, main = "Group 4", 
       col=as.character(c(rosso,rosso,rosso,rosso,rosso,giallo,verde,verde,blu,blu,blu,blu,blu,giallo)),
       cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, cex=0.5, ylim=c(-1,4), lwd=2)
grid(lty="solid")
plotCI(x, w_4, ui=u.w4, li=l.w4, xlab = "Preventive measures", ylab = "", pch = 19, main = "Group 4",
       col=as.character(c(rosso,rosso,rosso,rosso,rosso,giallo,verde,verde,blu,blu,blu,blu,blu,giallo)),
       cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, cex=0.5, ylim=c(-1,4), lwd=2, add=TRUE)
abline(h=0, lty=2)
mtext(expression("w"),side=2,las=1,line=2.5, cex=2)


beta_2 <- as.vector(MOD$beta[,3])
u.beta2 <- beta_2 + z*se.beta2
l.beta2 <- beta_2 - z*se.beta2
y <- 1:14
par(mar=c(5.1,12,4.1,0), xpd = T)
plotCI(y=y, x=beta_2, ui=u.beta2, li=l.beta2, xlab = "", ylab = "", pch = 19, col="gray40", main = "Group 2 vs 1",
       cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, cex=1, xlim=c(-2,2), lwd=2, yaxt="n", err="x", frame.plot=F)
par(mar=c(5.1,12,4.1,0), xpd = F)
grid(lty="solid")
plotCI(y=y, x=beta_2, ui=u.beta2, li=l.beta2, xlab = "", ylab = "", pch = 19, col="gray40", main = "Group 2 vs 1",
       cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, cex=1, xlim=c(-2,2), lwd=2, add=TRUE, err="x")
axis(2, at=1:14, labels=c("intercept","age","male","not empl","North","South","not ill","ill","num diseases","symptomatic",
                          "no opinion","scared","no trust","(trust) other"), las=1, cex.axis=2)
abline(v=0, lty=2)
mtext(expression(beta[2]),side=1,las=1,line=3.5, cex=1.5)
#mtext("Nodal attributes",side=1,las=1,line=10, cex=1.5)

beta_3 <- as.vector(MOD$beta[,1])
u.beta3 <- beta_3 + z*se.beta3
l.beta3 <- beta_3 - z*se.beta3
y <- 1:14
par(mar=c(5.1,6,4.1,6), xpd = T)
plotCI(y=y, x=beta_3, ui=u.beta3, li=l.beta3, xlab = "", ylab = "", pch = 19, col="gray40", main = "Group 3 vs 1",
       cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, cex=1, xlim=c(-2,2), lwd=2, yaxt="n", err="x", frame.plot=F)
par(mar=c(5.1,0,4.1,0.1), xpd = F)
grid(lty="solid")
plotCI(y=y, x=beta_3, ui=u.beta3, li=l.beta3, xlab = "", ylab = "", pch = 19, col="gray40", main = "Group 3 vs 1",
       cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, cex=1, xlim=c(-2,2), lwd=2, add=TRUE, err="x")
# axis(1, at=1:14, labels=c("intercept","age","male","not empl","north","south","no illness","illness","num illness","symptomatic",
#                           "no opinion","scared","no confidence","other"), las=2, cex.axis=1.5)
abline(v=0, lty=2)
mtext(expression(beta[3]),side=1,las=1,line=3.5, cex=1.5)
#mtext("Nodal attributes",side=1,las=1,line=10, cex=1.5)

beta_4 <- as.vector(MOD$beta[,2])
u.beta4 <- beta_4 + z*se.beta4
l.beta4 <- beta_4 - z*se.beta4
y <- 1:14
par(mar=c(5.1,0,4.1,12), xpd = T)
plotCI(y=y, x=beta_4, ui=u.beta4, li=l.beta4, xlab = "", ylab = "", pch = 19, col="gray40", main = "Group 4 vs 1",
       cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, cex=1, xlim=c(-2,2), lwd=2, yaxt="n", err="x", frame.plot=F)
par(mar=c(5.1,0,4.1,2.1), xpd = F)
grid(lty="solid")
plotCI(y=y, x=beta_4, ui=u.beta4, li=l.beta4, xlab = "", ylab = "", pch = 19, col="gray40", main = "Group 4 vs 1",
       cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, cex=1, xlim=c(-2,2), lwd=2, add=TRUE, err="x")
# axis(1, at=1:14, labels=c("intercept","age","male","not empl","north","south","no illness","illness","num illness","symptomatic",
#                           "no opinion","scared","no confidence","other"), las=2, cex.axis=1.5)
abline(v=0, lty=2)
mtext(expression(beta[4]),side=1,las=1,line=3.5, cex=1.5)
#mtext("Nodal attributes",side=1,las=1,line=10, cex=1.5)
