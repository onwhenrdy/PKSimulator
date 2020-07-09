library(RxODE)

#################################################################
# Models
#################################################################

# one-compartment model
# 1 = KA
# 2 = KE
# 3 = F
# 4 = Q
# 5 = V2
model_1C_code <- "

  # LHS
  TVKA = THETA[1]
  KA   = TVKA * exp(ETA[1])

  TVKE = THETA[2]
  KE   = TVKE * exp(ETA[2])

  TVF = THETA[3]
  F = TVF * exp(ETA[3])

  # ODES
  d/dt(ABS)  = -KA * ABS
  d/dt(CENT) =  KA * ABS - KE * CENT
  
  f(ABS) = F
"

model_1C <- RxODE(model = model_1C_code)



#################################################################
# Model functions
#################################################################
simulate <- function(model, event_table, THETAS, ..., ETAS = NULL) {
  
  if (length(ETAS) == 0) {
    ETAS = rep(0, length(THETAS))
  }
  
  result <- RxODE::rxSolve(model, theta = THETAS, eta = ETAS, 
                           events = event_table,
                           ...)
  
  return(result)
}

