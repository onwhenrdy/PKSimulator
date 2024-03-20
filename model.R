library(RxODE)

#################################################################
# Models
#################################################################

# two-compartment model with simple Emax PD effect and
# FO or MM elimination kinetics
# 1 = KA
# 2 = KE
# 3 = F
# 4 = V1 (Central)
# 5 = Q
# 6 = V2 (Peri)
# 7 = Km (in 1/l)
# 8 = Vmax (in 1/h)
# 9 = EMAX
# 10 = C50 (in mg/l)
# 11 = HILL
model_1_code <- "

  #################### CENTRAL
  TVKA = THETA[1]
  KA   = TVKA * exp(ETA[1])

  TVKE = THETA[2]
  KE   = TVKE * exp(ETA[2])

  TVF = THETA[3]
  F = TVF * exp(ETA[3])

  TVV1 = THETA[4]
  V1 = TVV1 * exp(ETA[4])

  #################### PERI
  TVQ = THETA[5]
  Q = TVQ * exp(ETA[5])

  TVV2 = THETA[6]
  V2 = TVV2 * exp(ETA[6])

  K12 = Q / V1
  K21 = Q / V2

  ####################  MM
  TVKM = THETA[7]
  KM = TVKM * exp(ETA[7])
  
  TVVMAX = THETA[8]
  VMAX = TVVMAX * exp(ETA[8])
  
  VMM = (VMAX * CENT/V1)/(KM + CENT/V1)
  
  ####################  EMAX
  TVEMAX = THETA[9]
  EMAX = TVEMAX * exp(ETA[9])
  
  TVC50 = THETA[10]
  C50 = TVC50 * exp(ETA[10])

  TVHILL = THETA[11]
  HILL = TVHILL * exp(ETA[11])

  EFFECT = (EMAX * CENT**HILL/V1) / (C50**HILL + CENT**HILL/V1) 

  # Outputs
  CENTC = CENT / V1
  PERC = PER / V2

  # ODES
  d/dt(ABS)  = -KA * ABS
  d/dt(CENT) =  KA * ABS - (K12 + KE) * CENT + K21 * PER - VMM * CENT
  d/dt(PER) = K12 * CENT - K21 * PER 
  
  f(ABS) = F
"

model_1 <- RxODE(model = model_1_code)


# two-compartment model with IPR effects
# FO or MM elimination kinetics
# 1 = KA
# 2 = KE
# 3 = F
# 4 = V1 (Central)
# 5 = Q
# 6 = V2 (Peri)
# 7 = Km (in 1/l)
# 8 = Vmax (in 1/h)
# 9 = KIN
# 10 = KOUT
# 11 = ISMAX
# 12 = ISC50
# 13 = EMODEL
model_2_code <- "

  #################### CENTRAL
  TVKA = THETA[1]
  KA   = TVKA * exp(ETA[1])

  TVKE = THETA[2]
  KE   = TVKE * exp(ETA[2])

  TVF = THETA[3]
  F = TVF * exp(ETA[3])

  TVV1 = THETA[4]
  V1 = TVV1 * exp(ETA[4])

  #################### PERI
  TVQ = THETA[5]
  Q = TVQ * exp(ETA[5])

  TVV2 = THETA[6]
  V2 = TVV2 * exp(ETA[6])

  K12 = Q / V1
  K21 = Q / V2

  ####################  MM
  TVKM = THETA[7]
  KM = TVKM * exp(ETA[7])
  
  TVVMAX = THETA[8]
  VMAX = TVVMAX * exp(ETA[8])
  
  VMM = (VMAX * CENT/V1)/(KM + CENT/V1)
  
  # Effect
  TVKIN = THETA[9]
  KIN = TVKIN * exp(ETA[9])
  
  TVKOUT = THETA[10]
  KOUT = TVKOUT * exp(ETA[10])
  
  TVISMAX = THETA[11]
  ISMAX = TVISMAX * exp(ETA[11])
  
  TVISC50 = THETA[12]
  ISC50 = TVISC50 * exp(ETA[12])
  
  # Outputs
  CENTC = CENT / V1
  PERC = PER / V2
  
  # H1/2 Effect
  EMODEL = THETA[13]
  H1 = 0
  H2 = 0
  if (EMODEL == 1) H1 = - (ISMAX * CENTC) / (ISC50 + CENTC)
  if (EMODEL == 2) H2 = - (ISMAX * CENTC) / (ISC50 + CENTC)
  if (EMODEL == 3) H1 =  (ISMAX * CENTC) / (ISC50 + CENTC)
  if (EMODEL == 4) H2 =  (ISMAX * CENTC) / (ISC50 + CENTC)

  # ODES
  d/dt(ABS)  = -KA * ABS
  d/dt(CENT) =  KA * ABS - (K12 + KE) * CENT + K21 * PER - VMM * CENT
  d/dt(PER) = K12 * CENT - K21 * PER 
  d/dt(EFFECT) = KIN * (1 + H1) - KOUT * (1 + H2) * EFFECT
  
  f(ABS) = F
"

model_2 <- RxODE(model = model_2_code)


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

