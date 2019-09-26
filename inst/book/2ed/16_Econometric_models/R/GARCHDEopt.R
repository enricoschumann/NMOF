rm(list=ls())
y <- read.table('DEMGBP.csv', header=FALSE)$V1
require(NMOF)   # load accompanying package

llhGARCH = function(y, params){
  q <- length(params$alpha)
  p <- length(params$beta)
  pqmax <- max(q,p)
  T <- length(y)
  e2 = c(rep(params$s2init,pqmax), (y-params$mu)**2)
  s2 = e2

  tObs <- pqmax + seq(1,T) # indices of actual observations
  for (t in (tObs)){
    s2[t] = params$alpha0 + sum(params$beta*s2[t+(-1:-p)]) + sum(params$alpha*e2[t+(-1:-q)])
  }
  LLH = -log(2*pi) * T/2 - sum(log(s2[tObs]) + e2[tObs]/s2[tObs])/2
  return(list(LLH=LLH, s2=s2[tObs]))
}

extractGARCHparams <- function(psi,order){
  ifelse(order[1]>0, P<-3+(1:order[1]),         P<-NULL)
  ifelse(order[2]>0, Q<-tail(P,1)+(1:order[2]), Q<-NULL)
  params = list( mu     = psi[1],
                 s2init = psi[2],
                 alpha0 = psi[3],
                 alpha  = psi[P],
                 beta   = psi[Q])
  return(params)
}

repairGARCHparams <- function(psi, data=data){
  psi[-1] <- abs(psi[-1]) #non-negativity for all except mu
  psi[1]  <- mean(data$y)   # fix mu with E(y)
  psi[2]  <- mean((data$y-psi[1])**2)  # fix s_0^2 with uncond var.
  return(psi)
}

GARCHorder <- c(1,1) # (p,q)
data       <- list(y=y, order = GARCHorder)

OF   <- function(psi, data){
  params <- extractGARCHparams(psi,data$order)
  LLH <- -llhGARCH(data$y,params)$LLH
  return(LLH)
}

D    <- 3+GARCHorder[1]+GARCHorder[2] # number of parameters
algo <- list (  nP = 20L,   ### population size
                nG = 1000L, ### number of generations
                F  = 0.7 ,  ### step size
                CR = 0.5 ,  ### prob of crossover
                min = rep(-1,D) , ### range for initial population
                max = rep(1,D) ,
                repair = repairGARCHparams,
                minmaxConstr = FALSE,
                printBar = TRUE )
require(NMOF)
sol <- DEopt (OF = OF , algo = algo, data = data)



#############
