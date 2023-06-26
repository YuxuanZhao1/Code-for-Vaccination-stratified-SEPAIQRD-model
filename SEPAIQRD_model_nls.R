## Load deSolve package
library(deSolve)
# from https://www.canadapopulation.net/ontario-population/



positive <- function(a) sapply(a, function(tmp) ifelse(tmp>0, tmp, 0))
# fit data start from 1.6, we assume at that time we do not have undocumented case
tot_population = 14051980

parameters_wo_k_i_car<- c(
  n1 = 1802257,# first day n1
  n2 = 804540,# first day n2
  n3 = 7212511,# first day n3
  n4 = 4232672,# first day n4
  v1 = 10652, 
  v2 = 9039,
  v3 = 174276,
  kappa_E = 1/1.42,
  kappa_A = 1/8.29, 
  kappa_I_R = 1/6.87, 
  kappa_I_Q = 1/1,  
  kappa_Q_R = 1/5.87, 
  
  alpha_1 = 0.003,
  alpha_2 = 0.0019,
  alpha_3 = 0.0008,
  alpha_4 = 0.0007,
  

  kappa_T_prime = 1/(8.29 - 2),  
  g = 0.45, 
  
  kappa_P = 1/2, 
  
  epsilon = 0.96, 
  # adjusted contact rate
  # group 1
  beta_11 = 2.4844 * 0.02, 
  beta_12 = 1.104594  * 0.02,
  beta_13 = 6.379842 * 0.02,
  beta_14 = 4.187418  * 0.02,
  # group 2
  beta_21 = 2.781948 * 0.02,
  beta_22 = 1.335314  * 0.02,
  beta_23 = 6.37106 * 0.02,
  beta_24 = 3.174107  * 0.02,
  
  # group 3
  beta_31 = 1.874393 * 0.02,
  beta_32 = 0.743218  * 0.02,
  beta_33 = 5.700929 * 0.02,
  beta_34 = 5.339735  * 0.02,
  # group 4
  beta_41 = 0.939218 * 0.02,
  beta_42 = 0.28268  * 0.02,
  beta_43 = 4.076515 * 0.02,
  beta_44 = 6.33165  * 0.02)


## Create an SIR function not considering the vaccination migration 
sir <- function(time, state, parameters,
                signal_g, 
                signal_n1, signal_n2, signal_n3, signal_n4,
                signal_v1, signal_v2, signal_v3) {
  gg  <- signal_g(time)
  nn1 <- signal_n1(time)
  nn2 <- signal_n2(time)
  nn3 <- signal_n3(time)
  nn4 <- signal_n4(time)
  vv1 <- signal_v1(time)
  vv2 <- signal_v2(time)
  vv3 <- signal_v3(time)
  
  parameters[-(1:length(parameters_wo_k_i_car))] = exp(parameters[-(1:length(parameters_wo_k_i_car))]) / (1 + exp(parameters[-(1:length(parameters_wo_k_i_car))]))
  with(as.list(c(state, parameters)), {
    # c(1, 25, 26, 42, 43, 54, 55, 74, 75,  151)
    
    
    if(1 <= time & time < 26){
      f_1 = f_4_p1
      f_2 = a_p1 
      f_3 = b_p1 
      f_4 = c_p1 
      
      CAR= CAR_p1
      
    }else if(26 <= time & time < 43){
      f_1 = f_4_p2
      f_2 = a_p2 
      f_3 = b_p2 
      f_4 = c_p2 
      
      CAR= CAR_p2
      
    }else if(43 <= time & time < 55){
      f_1 = f_4_p3
      f_2 = a_p3 
      f_3 = b_p3 
      f_4 = c_p3 
      
      CAR= CAR_p3
      
    }else if(55 <= time & time < 75){
      f_1 = f_4_p4
      f_2 = a_p4 
      f_3 = b_p4 
      f_4 = c_p4 
      
      CAR= CAR_p4
      
    }else{
      f_1 = f_4_p5
      f_2 = a_p5 
      f_3 = b_p5 
      f_4 = c_p5 
      CAR= CAR_p5
    }
    
    
    
      dS1 = (-S1/nn1*(beta_11*P1+beta_11*I1+0.2*beta_11*A1 + 
                           beta_11*T1+beta_11*Tprime1) -
                  S1/nn2*(beta_12*P2+beta_12*I2+0.2*beta_12*A2 + 
                            beta_12*T2+beta_12*Tprime2) -
                  S1/nn3*(beta_13*P3+beta_13*I3+0.2*beta_13*A3 + 
                            beta_13*T3+beta_13*Tprime3) -
                  S1/nn4*(beta_14*P4+beta_14*I4+0.2*beta_14*A4 +
                            beta_14*T4+beta_14*Tprime4))*gg*1 - vv1;
    
    
      dE1 = (S1/nn1*(beta_11*P1+beta_11*I1+0.2*beta_11*A1 + 
                          beta_11*T1+beta_11*Tprime1) +
                  S1/nn2*(beta_12*P2+beta_12*I2+0.2*beta_12*A2 + 
                            beta_12*T2+beta_12*Tprime2) +
                  S1/nn3*(beta_13*P3+beta_13*I3+0.2*beta_13*A3  + 
                            beta_13*T3+beta_13*Tprime3) +
                  S1/nn4*(beta_14*P4+beta_14*I4+0.2*beta_14*A4 +
                            beta_14*T4+beta_14*Tprime4))*gg*1 - 
      (1 - f_1)*kappa_E * E1 - f_1 * E1 * kappa_E;
    
    
      dA1 = f_1 * E1 * kappa_E - kappa_A *A1;
    
    
      dP1 = (1-f_1) * kappa_E * E1 - kappa_P * CAR * P1 - (1 - CAR) * kappa_P * P1;
    
    
      dTprime1 = (1 - CAR) * kappa_P * P1 - kappa_T_prime * Tprime1;
    
      dRprime1 = kappa_T_prime * Tprime1;
    
      dI1 = kappa_P * CAR * P1 - (alpha_1+ (1-alpha_1)*(1-epsilon)*kappa_I_Q + 
                                       (1-alpha_1)*epsilon*kappa_I_Q)*I1;
    
      dT1 = (1-alpha_1)*(1-epsilon)*kappa_I_Q * I1 - kappa_Q_R * T1;
    
      dR1 =  kappa_Q_R * T1+ kappa_Q_R *  Q1 ;
    
    
      dQ1 = (1-alpha_1)*(kappa_I_Q * epsilon)*I1-kappa_Q_R *  Q1;
    
    
      dD1 = alpha_1 * I1;
    
    
      dRA1 = kappa_A *A1;
    
    
    

      dS2 = (-S2/nn1*(beta_21*P1+beta_21*I1+0.2*beta_21*A1 + 
                           beta_21*T1+beta_21*Tprime1) -
                  S2/nn2*(beta_22*P2+beta_22*I2+0.2*beta_22*A2 + 
                            beta_22*T2+beta_22*Tprime2) -
                  S2/nn3*(beta_23*P3+beta_23*I3+0.2*beta_23*A3 + 
                            beta_23*T3+beta_23*Tprime3) -
                  S2/nn4*(beta_24*P4+beta_24*I4+0.2*beta_24*A4 +
                            beta_24*T4+beta_24*Tprime4))*gg*1 + vv1 - vv2;
    
    
      dE2 = (S2/nn1*(beta_21*P1+beta_21*I1+0.2*beta_21*A1 + 
                          beta_21*T1+beta_21*Tprime1) +
                  S2/nn2*(beta_22*P2+beta_22*I2+0.2*beta_22*A2 + 
                            beta_22*T2+beta_22*Tprime2) +
                  S2/nn3*(beta_23*P3+beta_23*I3+0.2*beta_23*A3  + 
                            beta_23*T3+beta_23*Tprime3) +
                  S2/nn4*(beta_24*P4+beta_24*I4+0.2*beta_24*A4 +
                            beta_24*T4+beta_24*Tprime4))*gg*1 - 
      (1 - f_2)*kappa_E * E2 - f_2 * E2 * kappa_E;
    
    
      dA2 = f_2 * E2 * kappa_E - kappa_A *A2;
    
    
      dP2 = (1-f_2) * kappa_E * E2 - kappa_P * CAR * P2 - (1 - CAR) * kappa_P * P2;
    
    
      dTprime2 = (1 - CAR) * kappa_P * P2 - kappa_T_prime * Tprime2;
    
      dRprime2 = kappa_T_prime * Tprime2;
    
      dI2 = kappa_P * CAR * P2 - (alpha_2+ (1-alpha_2)*(1-epsilon)*kappa_I_Q + 
                                       (1-alpha_2)*epsilon*kappa_I_Q)*I2;
    
      dT2 = (1-alpha_2)*(1-epsilon)*kappa_I_Q * I2 - kappa_Q_R * T2;
    
      dR2 =  kappa_Q_R * T2+ kappa_Q_R *  Q2 ;
    
    
      dQ2 = (1-alpha_2)*(kappa_I_Q * epsilon)*I2-kappa_Q_R *  Q2;
    
    
      dD2 = alpha_2 * I2;
    
    
      dRA2 = kappa_A *A2;
    
    
    
    
    
  
      dS3 = (-S3/nn1*(beta_31*P1+beta_31*I1+0.2*beta_31*A1 + 
                           beta_31*T1+beta_31*Tprime1) -
                  S3/nn2*(beta_32*P2+beta_32*I2+0.2*beta_32*A2 + 
                            beta_32*T2+beta_32*Tprime2) -
                  S3/nn3*(beta_33*P3+beta_33*I3+0.2*beta_33*A3 + 
                            beta_33*T3+beta_33*Tprime3) -
                  S3/nn4*(beta_34*P4+beta_34*I4+0.2*beta_34*A4 +
                            beta_34*T4+beta_34*Tprime4))*gg*1 + vv2 - vv3;
    
    
      dE3 = (S3/nn1*(beta_31*P1+beta_31*I1+0.2*beta_31*A1 + 
                          beta_31*T1+beta_31*Tprime1) +
                  S3/nn2*(beta_32*P2+beta_32*I2+0.2*beta_32*A2 + 
                            beta_32*T2+beta_32*Tprime2) +
                  S3/nn3*(beta_33*P3+beta_33*I3+0.2*beta_33*A3  + 
                            beta_33*T3+beta_33*Tprime3) +
                  S3/nn4*(beta_34*P4+beta_34*I4+0.2*beta_34*A4 +
                            beta_34*T4+beta_34*Tprime4))*gg*1 - 
      (1 - f_3)*kappa_E * E3 - f_3 * E3 * kappa_E;
    
    
      dA3 = f_3 * E3 * kappa_E - kappa_A *A3;
    
    
      dP3 = (1-f_3) * kappa_E * E3 - kappa_P * CAR * P3 - (1 - CAR) * kappa_P * P3;
    
    
      dTprime3 = (1 - CAR) * kappa_P * P3 - kappa_T_prime * Tprime3;
    
      dRprime3 = kappa_T_prime * Tprime3;
    
      dI3 = kappa_P * CAR * P3 - (alpha_3 + (1-alpha_3)*(1-epsilon)*kappa_I_Q + 
                                       (1-alpha_3)*epsilon*kappa_I_Q)*I3;
    
      dT3 = (1-alpha_3)*(1-epsilon)*kappa_I_Q * I3 - kappa_Q_R * T3;
    
      dR3 =  kappa_Q_R * T3+ kappa_Q_R *  Q3 ;
    
    
      dQ3 = (1-alpha_3)*(kappa_I_Q * epsilon)*I3-kappa_Q_R *  Q3;
    
    
      dD3 = alpha_3 * I3;
    
    
      dRA3 = kappa_A *A3;
    

      dS4 = (-S4/nn1*(beta_41*P1+beta_41*I1+0.2*beta_41*A1 + 
                           beta_41*T1+beta_41*Tprime1) -
                  S4/nn2*(beta_42*P2+beta_42*I2+0.2*beta_42*A2 + 
                            beta_42*T2+beta_42*Tprime2) -
                  S4/nn3*(beta_43*P3+beta_43*I3+0.2*beta_43*A3 + 
                            beta_43*T3+beta_43*Tprime3) -
                  S4/nn4*(beta_44*P4+beta_44*I4+0.2*beta_44*A4 +
                            beta_44*T4+beta_44*Tprime4))*gg*1 +  vv3;
    
    
      dE4 = (S4/nn1*(beta_41*P1+beta_41*I1+0.2*beta_41*A1 + 
                          beta_41*T1+beta_41*Tprime1) +
                  S4/nn2*(beta_42*P2+beta_42*I2+0.2*beta_42*A2 + 
                            beta_42*T2+beta_42*Tprime2) +
                  S4/nn3*(beta_43*P3+beta_43*I3+0.2*beta_43*A3  + 
                            beta_43*T3+beta_43*Tprime3) +
                  S4/nn4*(beta_44*P4+beta_44*I4+0.2*beta_44*A4 +
                            beta_44*T4+beta_44*Tprime4))*gg*1 - 
      (1 - f_4)*kappa_E * E4 - f_4 * E4 * kappa_E;
    
    
      dA4 = f_4 * E4 * kappa_E - kappa_A *A4;
    
    
      dP4 = (1-f_4) * kappa_E * E4 - kappa_P * CAR * P4 - (1 - CAR) * kappa_P * P4;
    
    
      dTprime4 = (1 - CAR) * kappa_P * P4 - kappa_T_prime * Tprime4;
    
      dRprime4 = kappa_T_prime * Tprime4;
    
      dI4 = kappa_P * CAR * P4 - (alpha_4 + (1-alpha_4)*(1-epsilon)*kappa_I_Q + 
                                       (1-alpha_4)*epsilon*kappa_I_Q)*I4;
    
      dT4 = (1-alpha_4)*(1-epsilon)*kappa_I_Q * I4 - kappa_Q_R * T4;
    
      dR4 =  kappa_Q_R * T4+ kappa_Q_R *  Q4;
    
    
      dQ4 = (1-alpha_4)*(kappa_I_Q * epsilon)*I4-kappa_Q_R *  Q4;
    
    
      dD4 = alpha_4 * I4;
    
    
      dRA4 = kappa_A *A4;
    
    return(list(c(dE1, dA1, dP1, dI1, dR1, dQ1, dD1, dRA1, dRprime1, dTprime1, dT1,
                  dE2, dA2, dP2, dI2, dR2, dQ2, dD2, dRA2, dRprime2, dTprime2, dT2,
                  dE3, dA3, dP3, dI3, dR3, dQ3, dD3, dRA3,dRprime3, dTprime3, dT3,
                  dE4, dA4, dP4, dI4, dR4, dQ4, dD4, dRA4,dRprime4, dTprime4, dT4,
                  dS1, dS2, dS3, dS4
    ), g = gg,
    n1 = nn1,
    n2 = nn2,
    n3 = nn3,
    n4 = nn4,
    v1 = vv1,
    v2 = vv2,
    v3 = vv3))
  })
}


signal_g <- approxfun(x = c(1, 25, 26, 42, 43, 54, 55, 74, 75,  151), 
                      y = c((100-55)*0.01,  (100-55)*0.01, # phase 1
                            (100-50)*0.01, (100-50)*0.01,# phase 2
                            (100-46)*0.01, (100-46)*0.01,# phase 3
                            (100-28)*0.01 , (100-28)*0.01 ,# phase 4
                            (100-20) *0.01, (100-20) *0.01 ), # phase 5
                      method = "linear", rule = 2)



# vaccine data load

vaccine_data = read.csv("vaccine_data.csv")

signal_n1 <- approxfun(x = seq(1, 151, 1), 
                       y = vaccine_data$vaccine_non, # non_vaccinated
                       method = "constant", rule = 2)

signal_n2 <- approxfun(x = seq(1,  151, 1), 
                       y = vaccine_data$vaccine_partial, 
                       method = "constant", rule = 2)

signal_n3 <- approxfun(x = seq(1,  151, 1), 
                       y = vaccine_data$vaccine_complete, 
                       method = "constant", rule = 2)


signal_n4 <- approxfun(x = seq(1,  151, 1), 
                       y = vaccine_data$vaccine_bootser, 
                       method = "constant", rule = 2)


signal_v1 <- approxfun(x = seq(1,  151, 1), 
                       y = vaccine_data$vaccine_v1_increase, 
                       method = "constant", rule = 2)


signal_v2 <- approxfun(x = seq(1,  151, 1), 
                       y = vaccine_data$vaccine_v2_increase, 
                       method = "constant", rule = 2)


signal_v3 <- approxfun(x = seq(1,  151, 1), 
                       y = vaccine_data$vaccine_v3_increase, 
                       method = "constant", rule = 2)




df_infection = read.csv("df_infection_on_day.csv")
# func: compute initial states 
# input: start date 
# output: list of initial states
n_vec = c(1802257,804540,7212511,4232672)
func_inital_states = function(current_date){
  initial_state = rep(0,44)
  
  for (i in 0:3){
    # compute E: 0.649 exposed
    initial_state[11 * i + 1] = subset(df_infection, df_infection$date== 
                                         as.Date(current_date)+5)$infect_count * 
      n_vec[i+1]/tot_population/0.649
    
    # compute A: 0.351/0.649 be asym infection
    initial_state[11 * i + 2] = subset(df_infection, df_infection$date== 
                                         as.Date(current_date)+2)$infect_count * 
      n_vec[i+1]/tot_population* 0.351/0.649
    
    
    # compute P: sym infection
    initial_state[11 * i + 3] = subset(df_infection, df_infection$date== 
                                         as.Date(current_date)+2)$infect_count * 
      n_vec[i+1]/tot_population
    
    
    # compute I: infection
    initial_state[11 * i + 4] = subset(df_infection, df_infection$date== 
                                         as.Date(current_date))$infect_count * 
      n_vec[i+1]/tot_population
    
    # compute R: recover recovery rate 0.9997
    initial_state[11 * i + 5] = subset(df_infection, df_infection$date== 
                                         as.Date(current_date)-5)$infect_count * 
      n_vec[i+1]/tot_population * 0.997
    
    
    # compute Q: qurantined
    initial_state[11 * i + 6] = subset(df_infection, df_infection$date== 
                                         as.Date(current_date)-1)$infect_count * 
      n_vec[i+1]/tot_population * 0.997 * 0.95
    
    
    # compute D: known 10315 cumulative death 
    initial_state[11 * i + 7] = 10315 * n_vec[i+1]/tot_population 
    
    # compute RA: recover  rate 0.9997 asympt0.649 
    initial_state[11 * i + 8] = subset(df_infection, df_infection$date== 
                                         as.Date(current_date)-5)$infect_count * 
      n_vec[i+1]/tot_population * 0.997 * 0.351/0.649
    
    # compute Rprime: 
    initial_state[11 * i + 9] = subset(df_infection, df_infection$date== 
                                         as.Date(current_date)-5)$infect_count * 
      n_vec[i+1]/tot_population * 0.997 
    # compute Tprime: 
    initial_state[11 * i + 10] = subset(df_infection, df_infection$date== 
                                          as.Date(current_date)-4)$infect_count * 
      n_vec[i+1]/tot_population * 0.997 
    
    # compute T: 
    initial_state[11 * i + 11] = subset(df_infection, df_infection$date== 
                                          as.Date(current_date)-1)$infect_count * 
      n_vec[i+1]/tot_population * 0.997 * 0.05
  }
  for (j in 1:4){
    initial_state[44 + j] = n_vec[j]- sum(initial_state[11 * j - 10 : 11 *j])
  }
  return(c(
    E1 = initial_state[1], 
    A1 = initial_state[2],
    P1 = initial_state[3], 
    I1 = initial_state[4], 
    R1 = initial_state[5], 
    Q1 = initial_state[6], 
    D1 = initial_state[7], 
    RA1= initial_state[8],
    Rprime1 = initial_state[9],
    Tprime1 = initial_state[10],
    T1 = initial_state[11],
    
    
    E2 = initial_state[12], 
    A2 = initial_state[13],
    P2 = initial_state[14], 
    I2 = initial_state[15], 
    R2 = initial_state[16], 
    Q2 = initial_state[17], 
    D2 = initial_state[18], 
    RA2= initial_state[19],
    Rprime2= initial_state[20],
    Tprime2 = initial_state[21],
    T2 = initial_state[22],
    
    E3 = initial_state[23], 
    A3 = initial_state[24],
    P3 = initial_state[25], 
    I3 = initial_state[26], 
    R3 = initial_state[27], 
    Q3 = initial_state[28], 
    D3 = initial_state[29], 
    RA3= initial_state[30],
    Rprime3= initial_state[31],
    Tprime3 = initial_state[32],
    T3 = initial_state[33],
    
    E4 = initial_state[34], 
    A4 = initial_state[35],
    P4 = initial_state[36], 
    I4 = initial_state[37], 
    R4 = initial_state[38], 
    Q4 = initial_state[39], 
    D4 = initial_state[40], 
    RA4= initial_state[41],
    Rprime4 = initial_state[42],
    Tprime4 = initial_state[43],
    T4 = initial_state[44],
    
    S1 = initial_state[45],
    S2 = initial_state[46],
    S3 = initial_state[47],
    S4 = initial_state[48]
  ))
}



init = func_inital_states("2022-01-06")




## Time frame phase 1 
library(lubridate)
covid = read.csv("selected_case_data.csv")

infect_data_unvac = covid$infect_unvac[1:150]
infect_data_partial = covid$infect_vac_partial[1:150]
infect_data_complete = covid$infect_vac_complete[1:150]
infect_data_booster = covid$infect_vac_booster[1:150]



loss_func = function(x){
  
  f_4_p1 = x[1]
  a_p1 = x[2]
  b_p1 = x[3]
  c_p1 = x[4]
  
  
  f_4_p2 = x[5]
  a_p2 = x[6]
  b_p2 = x[7]
  c_p2 = x[8]
  
  
  f_4_p3 = x[9]
  a_p3 = x[10]
  b_p3 = x[11]
  c_p3 = x[12]
  
  #
  f_4_p4 = x[13]
  a_p4 = x[14]
  b_p4 = x[15]
  c_p4 = x[16]
  #
  #
  f_4_p5 = x[17]
  a_p5 = x[18]
  b_p5 = x[19]
  c_p5 = x[20]
  
  CAR_p1 = x[21]
  CAR_p2 = x[22]
  CAR_p3 = x[23]
  CAR_p4 = x[24]
  CAR_p5 = x[25]
  
  
  
  # kappa_P = x[41]
  
  
  times <- seq(1,  151, by = 1)
  
  
  
  parms_updated = c(parameters_wo_k_i_car,
                    f_4_p1 = f_4_p1,
                    a_p1 = a_p1,
                    b_p1 = b_p1,
                    c_p1 = c_p1,
                    
                    f_4_p2 = f_4_p2,
                    a_p2 = a_p2,
                    b_p2 = b_p2,
                    c_p2 = c_p2,
                    
                    f_4_p3 = f_4_p3,
                    a_p3 = a_p3,
                    b_p3 = b_p3,
                    c_p3 = c_p3,
                    
                    f_4_p4 = f_4_p4,
                    a_p4 = a_p4,
                    b_p4 = b_p4,
                    c_p4 = c_p4,
                    
                    f_4_p5 = f_4_p5,
                    a_p5 = a_p5,
                    b_p5 = b_p5,
                    c_p5 = c_p5,
                    
                    CAR_p1 = CAR_p1,
                    CAR_p2 = CAR_p2,
                    CAR_p3 = CAR_p3,
                    CAR_p4 = CAR_p4,
                    CAR_p5 = CAR_p5)
  
  
  
  
  
  
  
  out <- ode(y = init, times = times, func = sir, parms = parms_updated, 
             signal_g = signal_g,
             signal_n1 = signal_n1 , signal_n2 = signal_n2 , 
             signal_n3 = signal_n3 , signal_n4 = signal_n4 ,
             signal_v1 = signal_v1, signal_v2 = signal_v2, 
             signal_v3 = signal_v3)

  out <- as.data.frame(out)

  out_I1 = diff(out$I1+out$R1 + out$Q1 +out$D1+ out$T1)
  out_I2 = diff(out$I2+out$R2 + out$Q2 +out$D2+ out$T2)
  out_I3 = diff(out$I3+out$R3 + out$Q3 +out$D3+ out$T3)
  out_I4 =  diff(out$I4+out$R4 + out$Q4 +out$D4+ out$T4)

  error = sqrt(sum((out_I1-infect_data_unvac)^2)+sum((out_I2-infect_data_partial)^2)+
                 sum((out_I3-infect_data_complete)^2)+sum((out_I4-infect_data_booster)^2))  
  
  return(error)
}

library(deSolve)
library(optimParallel)
cl <- makeCluster(6); setDefaultCluster(cl = cl)
clusterExport(cl, c("parameters_wo_k_i_car", "init", "sir", "signal_g", "signal_n1", "signal_n2", "signal_n3", "signal_n4", "signal_v1", "signal_v2", "signal_v3", "infect_data_unvac", "infect_data_partial", "infect_data_complete", "infect_data_booster"))
clusterEvalQ(cl, library("deSolve"))



x0 <- c(-3.8, -4.7,  -0.18,  -1.9 ,
        -1.0, -4.2, -0.7,  -0.66 ,
        -1.6279 , -3,  -0.41, -1.25 ,
        -1.93, -1.00,  -0.52, -1.016,
        -3.2, -0.97, -1.34 , -0.01,
        -1.5, -1.5 , -1.36, -1.31,
        -0.27)


optim_results <- optimParallel(x0, loss_func, parallel = list(loginfo = TRUE), control = list(trace =10))


times <- seq(1,  151, by = 1)

x <- optim_results$par
out <- ode(y = init, times = times, func = sir, 
           parms = c(parameters_wo_k_i_car,
                     f_4_p1 = x[1],
                     a_p1 = x[2],
                     b_p1 = x[3],
                     c_p1 = x[4],
                     
                     
                     f_4_p2 = x[5],
                     a_p2 = x[6],
                     b_p2 = x[7],
                     c_p2 = x[8],
                     
                     
                     f_4_p3 = x[9],
                     a_p3 = x[10],
                     b_p3 = x[11],
                     c_p3 = x[12],
                     
                     #
                     f_4_p4 = x[13],
                     a_p4 = x[14],
                     b_p4 = x[15],
                     c_p4 = x[16],
                     #
                     #
                     f_4_p5 = x[17],
                     a_p5 = x[18],
                     b_p5 = x[19],
                     c_p5 = x[20],
                     
                     CAR_p1 = x[21],
                     CAR_p2 = x[22],
                     CAR_p3 = x[23],
                     CAR_p4 = x[24],
                     CAR_p5 = x[25]), 
           signal_g = signal_g,
           signal_n1 = signal_n1, signal_n2 = signal_n2, 
           signal_n3 = signal_n3, signal_n4 = signal_n4,
           signal_v1 = signal_v1, signal_v2 = signal_v2, 
           signal_v3 = signal_v3)

## change to data frame

out <- as.data.frame(out)

out_I1 = diff(out$I1+out$R1 + out$Q1 +out$D1+ out$T1)
out_I2 = diff(out$I2+out$R2 + out$Q2 +out$D2+out$T2)
out_I3 = diff(out$I3+out$R3 + out$Q3 +out$D3+out$T3)
out_I4 = diff(out$I4+out$R4 + out$Q4 +out$D4+out$T4)

library(ggplot2)
df_1_phase1 = cbind(out_I1, infect_data_unvac)
df_2_phase1 = cbind(out_I2, infect_data_partial)
df_3_phase1 = cbind(out_I3, infect_data_complete)
df_4_phase1 = cbind(out_I4, infect_data_booster)

time = 1:150


## loess
par(mfrow=c(2,2))

loess_model1 = loess((positive(df_1_phase1[,1])) ~ time)
plot(time, df_1_phase1[,2], main="Unvaccinated", ylab = "Confirmed cases")
lines(predict(loess_model1),type = "l")


loess_model2 = loess((positive(df_2_phase1[,1])) ~ time)
plot(time, df_2_phase1[,2], main="Partially vaccinated", ylab = "Confirmed cases")
lines(predict(loess_model2),type = "l")


loess_model3 = loess((positive(df_3_phase1[,1])) ~ time)
plot(time, df_3_phase1[,2], main="Fully vaccinated", ylab = "Confirmed cases")
lines(predict(loess_model3),type = "l")


loess_model4 = loess((positive(df_4_phase1[,1])) ~ time)
plot(time, df_4_phase1[,2], main = "Fully vaccinated + booster", ylab = "Confirmed cases")
lines(predict(loess_model4),type = "l")

save.image(file = "lse_result.RData")

## Transform x's back to [0,1] scale
x01 <- 1 / (1 + exp(-x))

x_df <- matrix(x01[1:20], nrow = 5, ncol=4, byrow = T)
knitr::kable(x_df, digits = 4)

x01[21:25] # CAR


actual_data = read.csv("selected_case_data.csv")

times = seq(as.Date("2022-01-06"),
            as.Date("2022-06-04"),
            by = "day")

type1 = data.frame(pred = predict(loess_model1),
              actual = infect_data_unvac,
              time = times)

type1_before =  type1[1:64,]
type1_after =  type1[65:150,]


type2 = data.frame(pred = predict(loess_model2),
              actual = infect_data_partial,
              time = times)

type2_before =  type2[1:64,]
type2_after =  type2[65:150,]


type3 = data.frame(pred = predict(loess_model3),
              actual = infect_data_complete,
              time = times)
type3_before =  type3[1:64,]


type3_after =  type3[65:150,]

type4 = data.frame(pred = predict(loess_model4),
              actual = infect_data_booster,
              time = times)

type4_before =  type4[1:64,]
type4_after =  type4[65:150,]


type3_before$combine_actual = type3_before$actual + type4_before$actual

type3_before$combine_pred = type3_before$pred + type4_before$pred

type1_after$combine_actual = type1_after$actual + type2_after$actual

type1_after$combine_pred = type1_after$pred + type2_after$pred



library(zoo)
p_1 <- ggplot()+
  geom_point(data = type1_before,aes(x = time, y = actual))+
  geom_line(data = type1_before,
            aes(x = rollmean(time, 7, align = "center", fill = NA),
                y = rollmean(actual, 7, align = "center", fill = NA),
                colour = "Centered 7-day moving average"),
            linetype = "dashed", linewidth = 1.25)+
  geom_line(data = type1_before,aes(x = time, y = pred,color = "SEPAIQRD"))+
  xlab("Time")+
  ggtitle("(a) Unvaccinated")+
  theme_classic()+
  scale_color_manual("Model",limits=c("Centered 7-day moving average","SEPAIQRD"), values = c("deepskyblue1","black"))+
  theme(legend.position = "bottom",axis.title.y = element_blank(),
        plot.title = element_text(size = 12.5, face = "bold"))+
  scale_x_date(date_breaks = "14 day", date_labels =  "%m-%d ") 


p_1
rmse(type1_before$pred,type1_before$actual)


p_1


p_2 <- ggplot()+
  geom_point(data = type2_before,aes(x = time, y = actual))+
  geom_line(data = type2_before,
            aes(x = rollmean(time, 7, align = "center", fill = NA),
                y = rollmean(actual, 7, align = "center", fill = NA),
                colour = "Centered 7-day moving average"),
            linetype = "dashed", linewidth = 1.25)+
  geom_line(data = type2_before,aes(x = time, y = pred,color = "SEPAIQRD"))+
  xlab("Time")+
  ggtitle("(b) Partially Vaccinated")+
  theme_classic()+
  scale_color_manual("Model",limits=c("Centered 7-day moving average","SEPAIQRD"), values = c("deepskyblue1","black"))+
  theme(legend.position = "bottom",axis.title.y = element_blank(),
        plot.title = element_text(size = 12.5, face = "bold"))+
  scale_x_date(date_breaks = "14 day", date_labels =  "%m-%d ") 

rmse(type2_before$pred,type2_before$actual)
p_2

p_3 <- ggplot()+
  geom_point(data = type3_before,aes(x = time, y = combine_actual))+
  geom_line(data = type3_before,
            aes(x = rollmean(time, 7, align = "center", fill = NA),
                y = rollmean(combine_actual, 7, align = "center", fill = NA),
                colour = "Centered 7-day moving average"),
            linetype = "dashed", linewidth = 1.25)+
  geom_line(data = type3_before,aes(x = time, y = combine_pred,color = "SEPAIQRD"))+
  xlab("Time")+
  ggtitle("(c) Fully Vaccinated")+
  theme_classic()+
  scale_color_manual("Model",limits=c("Centered 7-day moving average","SEPAIQRD"), values = c("deepskyblue1","black"))+
  theme(legend.position = "bottom",axis.title.y = element_blank(),
        plot.title = element_text(size = 12.5, face = "bold"))+
  scale_x_date(date_breaks = "14 day", date_labels =  "%m-%d ") 


rmse(type3_before$combine_pred,type3_before$combine_actual)
p_3



p_4 <- ggplot()+
  geom_point(data = type1_after,aes(x = time, y = combine_actual))+
  geom_line(data = type1_after,
            aes(x = rollmean(time, 7, align = "center", fill = NA),
                y = rollmean(combine_actual, 7, align = "center", fill = NA),
                colour = "Centered 7-day moving average"),
            linetype = "dashed", linewidth = 1.25)+
  geom_line(data = type1_after,aes(x = time, y = combine_pred,color = "SEPAIQRD"))+
  xlab("Time")+
  ggtitle("(d) Not Fully Vaccinated")+
  theme_classic()+
  scale_color_manual("Model",limits=c("Centered 7-day moving average","SEPAIQRD"), values = c("deepskyblue1","black"))+
  theme(legend.position = "bottom",axis.title.y = element_blank(),
        plot.title = element_text(size = 12.5, face = "bold"))+
  scale_x_date(date_breaks = "14 day", date_labels =  "%m-%d ") 

p_4
rmse(type1_after$combine_pred,type1_after$combine_actual)

p_5 <- ggplot()+
  geom_point(data = type3_after,aes(x = time, y = actual))+
  geom_line(data = type3_after,
            aes(x = rollmean(time, 7, align = "center", fill = NA),
                y = rollmean(actual, 7, align = "center", fill = NA),
                colour = "Centered 7-day moving average"),
            linetype = "dashed", linewidth = 1.25)+
  geom_line(data = type3_after,aes(x = time, y = pred,color = "SEPAIQRD"))+
  xlab("Time")+
  ggtitle("(e) Completely Vaccinated")+
  theme_classic()+
  scale_color_manual("Model",limits=c("Centered 7-day moving average","SEPAIQRD"), values = c("deepskyblue1","black"))+
  theme(legend.position = "bottom",axis.title.y = element_blank(),
        plot.title = element_text(size = 12.5, face = "bold"))+
  scale_x_date(date_breaks = "14 day", date_labels =  "%m-%d ")

rmse(type3_after$pred,type3_after$actual)



p_6 <- ggplot()+
  geom_point(data = type4_after,aes(x = time, y = actual))+
  geom_line(data = type4_after,
            aes(x = rollmean(time, 7, align = "center", fill = NA),
                y = rollmean(actual, 7, align = "center", fill = NA),
                colour = "Centered 7-day moving average"),
            linetype = "dashed", linewidth = 1.25)+
  geom_line(data = type4_after,aes(x = time, y = pred,color = "SEPAIQRD"))+
  xlab("Time")+
  ggtitle("(f) Vaccinated with Booster Dose")+
  theme_classic()+
  scale_color_manual("Model",limits=c("Centered 7-day moving average","SEPAIQRD"), values = c("deepskyblue1","black"))+
  theme(legend.position = "bottom",axis.title.y = element_blank(),
        plot.title = element_text(size = 12.5, face = "bold"))+
  scale_x_date(date_breaks = "14 day", date_labels =  "%m-%d ")

rmse(type4_after$pred,type4_after$actual)

g = (p_1 | p_2 | p_3)/
  (p_4 | p_5 | p_6)  +plot_layout(guides = "collect")& theme(legend.position = "bottom")

ggsave(g, file= "lse_fit.pdf", width = 10, height =9)

### rmse

library(Metrics)
rmse(type1$pred+type2$pred+type3$pred+type4$pred,
     type1$actual+type2$actual+type3$actual+type4$actual)
