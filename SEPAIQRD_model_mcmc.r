Sys.setenv(STAN_NUM_THREADS = 2)
## population
library(rstan)
library(gridExtra)
rstan_options (auto_write = TRUE)
options (mc.cores = 4)
df_infection = read.csv("df_infection_on_day.csv")
# func: compute initial states 
# input: start date 
# output: list of initial states
tot_population = 14051980
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


# 1.9% hospitalization rate because of covid-19

init = func_inital_states("2022-01-06")

# training data 
library(lubridate)
covid = read.csv("selected_case_data.csv")

infect_data_unvac = covid$infect_unvac[1:150]
infect_data_partial = covid$infect_vac_partial[1:150]
infect_data_complete = covid$infect_vac_complete[1:150]
infect_data_booster = covid$infect_vac_booster[1:150]
# times
n_days <- length(infect_data_unvac) + 1
t <- seq(0, n_days, by = 1)
t0 = 0 
t <- t[-1]
cases_combine = rbind(infect_data_unvac[1:150],infect_data_partial[1:150],
                      infect_data_complete[1:150],infect_data_booster[1:150])

# data for Stan
data_sir <- list(n_days = n_days, y0 = init, t0 = 0,ts = t, 
                 cases = cases_combine)



model <- stan_model("SEPAIQRD_model.stan")



fit_sir_prior <- sampling(model,
                          data = data_sir, 
                          chains = 4,
                          iter = 2000,
                          seed = 0)




 load("updated_params_6_17.RData")
 load("lse_result.RData")
# 
library(rstan)
 library(bayesplot)
 library(coda)

check_hmc_diagnostics(fit_sir_prior)



mcmc_trace(fit_sir_prior, pars = c("f_1_p1","f_2_p1","f_3_p1","f_4_p1","CAR_p1",
                                    "f_1_p2","f_2_p2","f_3_p2","f_4_p2","CAR_p2",
                                    "f_1_p3","f_2_p3","f_3_p3","f_4_p3","CAR_p3",
                                    "f_1_p4","f_2_p4","f_3_p4","f_4_p4","CAR_p4",
                                    "f_1_p5","f_2_p5","f_3_p5","f_4_p5","CAR_p5"))+ 
  xlab("Post-warmup iteration")

 ggsave(trace_plot, file = "traceplot_params", width = 10, height =9)

 sir.summary <- summary(fit_sir_prior, pars = "pred_cases", probs = c(0.025, 0.5,0.975))
 post.025 <- sir.summary$summary[,4]
 post.975 <- sir.summary$summary[,6]
 post.mean <- sir.summary$summary[,1]

 case1.025 <- post.025[1:150]
 case2.025 <- post.025[151:300]
 case3.025 <- post.025[301:450]
 case4.025 <- post.025[451:600]
#
#
#
 case1.975 <- post.975[1:150]
 case2.975 <- post.975[151:300]
 case3.975 <- post.975[301:450]
 case4.975 <- post.975[451:600]
#
 case1.mean <- post.mean[1:150]
 case2.mean <- post.mean[151:300]
 case3.mean <- post.mean[301:450]
 case4.mean <- post.mean[451:600]
#
#
 times <- seq_along(case1.025)



 parm_summary = summary(fit_sir_prior, pars = c("f_1_p1","f_2_p1","f_3_p1","f_4_p1",
                                 "f_1_p2","f_2_p2","f_3_p2","f_4_p2",
                                 "f_1_p3","f_2_p3","f_3_p3","f_4_p3",
                                 "f_1_p4","f_2_p4","f_3_p4","f_4_p4",
                                 "f_1_p5","f_2_p5","f_3_p5","f_4_p5",
                                 "CAR_p1", "CAR_p2", "CAR_p3", "CAR_p4", "CAR_p5"))$summary



 expit_parm_summary = 1/(1+exp(-parm_summary))
 expit_parm_summary_mean = expit_parm_summary[,1]
 expit_parm_summary_lb = expit_parm_summary[,4]
 expit_parm_summary_ub = expit_parm_summary[,8]

 knitr::kable(data.frame(mean = expit_parm_summary_mean,
            lower_quantile = expit_parm_summary_lb,
            upper_quantile = expit_parm_summary_ub),
            "latex",booktabs = T, digits = 4)
 knitr::kable(data.frame(mean = expit_parm_summary_mean,
                         lower_quantile = expit_parm_summary_lb,
                         upper_quantile = expit_parm_summary_ub),
              "markdown",booktabs = T, digits = 4)

 knitr::kable(data.frame(mean = parm_summary[,1],
                         lower_quantile = parm_summary[,4],
                         upper_quantile = parm_summary[,8]),
              "latex", booktabs = T, digits = 4)




#



times = seq(1, 150, 1)
case1.mean.smooth = predict(loess(case1.mean ~ times))
case1.025.smooth = predict(loess(case1.025 ~ times))
case1.975.smooth = predict(loess(case1.975 ~ times))

case2.mean.smooth = predict(loess(case2.mean ~ times))
case2.025.smooth = predict(loess(case2.025 ~ times))
case2.975.smooth = predict(loess(case2.975 ~ times))



case3.mean.smooth = predict(loess(case3.mean ~ times))
case3.025.smooth = predict(loess(case3.025 ~ times))
case3.975.smooth = predict(loess(case3.975 ~ times))

case4.mean.smooth = predict(loess(case4.mean ~ times))
case4.025.smooth = predict(loess(case4.025 ~ times))
case4.975.smooth = predict(loess(case4.975 ~ times))

library(ggplot2)
library(zoo)
times = seq(as.Date("2022-01-06"),
        as.Date("2022-06-04"),
        by = "day")
df_1 = data.frame(time = times, mcmc_est = case1.mean.smooth, 
                  mcmc_lb = case1.025.smooth, mcmc_ub =case1.975.smooth, 
                  acutual = infect_data_unvac)
df_1_before = subset(df_1, df_1$time<= "2022-03-10")
df_1_after = subset(df_1, df_1$time>"2022-03-10")
df_1_before$lse_est = type1_before$pred
df_1_after$lse_est = type1_after$pred

df_2 = data.frame(time = times, mcmc_est = case2.mean.smooth, 
                  mcmc_lb = case2.025.smooth, mcmc_ub =case2.975.smooth, 
                  acutual = infect_data_partial)
df_2_before = subset(df_2, df_2$time<= "2022-03-10")
df_2_after = subset(df_2, df_2$time>"2022-03-10")
df_2_before$lse_est = type2_before$pred
df_2_after$lse_est = type2_after$pred

df_3 = data.frame(time = times, mcmc_est = case3.mean.smooth, 
                  mcmc_lb = case3.025.smooth, mcmc_ub =case3.975.smooth, 
                  acutual = infect_data_complete)
df_3_before = subset(df_3, df_3$time<= "2022-03-10")
df_3_after = subset(df_3, df_3$time>"2022-03-10")
df_3_before$lse_est = type3_before$pred
df_3_after$lse_est = type3_after$pred

df_4 = data.frame(time = times, mcmc_est = case4.mean.smooth, 
                  mcmc_lb = case4.025.smooth, mcmc_ub =case4.975.smooth, 
                  acutual = infect_data_booster)
df_4_before = subset(df_4, df_4$time<= "2022-03-10")
df_4_after = subset(df_4, df_4$time>"2022-03-10")
df_4_before$lse_est = type4_before$pred
df_4_after$lse_est = type4_after$pred


p_1 <- ggplot(data = df_1_before)+
  geom_point(aes(x = time, y = acutual))+
  geom_line(aes(x = rollmean(time, 7, align = "center", fill = NA),
                y = rollmean(acutual, 7, align = "center", fill = NA),
                colour = "Centered 7-day moving average of case counts",
                linetype = "Centered 7-day moving average of case counts",
                size = "Centered 7-day moving average of case counts"))+
  geom_line(aes(x = time, y = mcmc_est,colour = "SEPAIQRD model fit (MCMC)",
                                                linetype = "SEPAIQRD model fit (MCMC)",
                                                size = "SEPAIQRD model fit (MCMC)"))+
  geom_ribbon(aes(x = time, ymax = mcmc_ub, ymin = mcmc_lb), fill = "lightgreen", alpha = 0.2)+
  geom_line(aes(x = time, y = lse_est, 
                colour = "D-SEPAIQRD model fit (NLS)", 
                linetype = "D-SEPAIQRD model fit (NLS)",
                size =  "D-SEPAIQRD model fit (NLS)"))+
  scale_y_continuous(limits=c(0,max(df_1_before$lse_est,
                                    df_1_before$mcmc_ub,
                                    df_1_before$actual)))+
  xlab("Time")+
  ggtitle("(a) Unvaccinated")+
  theme_classic()+
  scale_colour_manual("",values = c("SEPAIQRD model fit (MCMC)"="forestgreen",
                                    "D-SEPAIQRD model fit (NLS)"= "darkorange",
                                    "Centered 7-day moving average of case counts"="red"),
                      label = c("Centered 7-day moving average of case counts",
                                "SEPAIQRD model fit (NLS)",
                                "SEPAIQRD model fit (MCMC)"))+
  scale_linetype_manual("",values = c("SEPAIQRD model fit (MCMC)"="solid",
                                      "D-SEPAIQRD model fit (NLS)"= "solid",
                                      "Centered 7-day moving average of case counts"= "11"),
                        label = c("Centered 7-day moving average of case counts",
                                  "SEPAIQRD model fit (NLS)",
                                  "SEPAIQRD model fit (MCMC)"))+
  scale_size_manual("",values = c("SEPAIQRD model fit (MCMC)"=1,
                                  "D-SEPAIQRD model fit (NLS)"= 1,
                                  "Centered 7-day moving average of case counts"=1.5),
                    label = c("Centered 7-day moving average of case counts",
                              "SEPAIQRD model fit (NLS)",
                              "SEPAIQRD model fit (MCMC)"))+
  theme(legend.position = "bottom",axis.title.y = element_blank(),
        plot.title = element_text(size = 12.5, face = "bold"),
        legend.spacing.y = unit(0.3, 'cm'),
        legend.key.size = unit(1.5, "cm"))+
  scale_x_date(date_breaks = "14 day", date_labels =  "%m-%d ")
p_1







library(Metrics)
rmse(infect_data_unvac[1:64],case1.mean.smooth[1:64])
rmse(df_1_before$lse_est, df_1_before$acutual)
# updated: 110.3696
p_2 <- ggplot(data = df_2_before)+
  geom_point(aes(x = time, y = acutual))+
  geom_line(aes(x = rollmean(time, 7, align = "center", fill = NA),
                y = rollmean(acutual, 7, align = "center", fill = NA),
                colour = "Centered 7-day moving average of case counts",
                linetype = "Centered 7-day moving average of case counts",
                size = "Centered 7-day moving average of case counts"))+
  geom_line(aes(x = time, y = mcmc_est,colour = "SEPAIQRD model fit (MCMC)",
                linetype = "SEPAIQRD model fit (MCMC)",
                size = "SEPAIQRD model fit (MCMC)"))+
  geom_ribbon(aes(x = time, ymax = mcmc_ub, ymin = mcmc_lb), fill = "lightgreen", alpha = 0.2)+
  geom_line(aes(x = time, y = lse_est, 
                colour = "D-SEPAIQRD model fit (NLS)", 
                linetype = "D-SEPAIQRD model fit (NLS)",
                size =  "D-SEPAIQRD model fit (NLS)"))+
  scale_y_continuous(limits=c(0,max(df_2_before$lse_est,
                                    df_2_before$mcmc_ub,
                                    df_2_before$actual)))+
  xlab("Time")+
  ggtitle("(b) Partially Vaccinated")+
  theme_classic()+
  scale_colour_manual("",values = c("SEPAIQRD model fit (MCMC)"="forestgreen",
                                    "D-SEPAIQRD model fit (NLS)"= "darkorange",
                                    "Centered 7-day moving average of case counts"="red"),
                      label = c("Centered 7-day moving average of case counts",
                                "SEPAIQRD model fit (NLS)",
                                "SEPAIQRD model fit (MCMC)"))+
  scale_linetype_manual("",values = c("SEPAIQRD model fit (MCMC)"="solid",
                                      "D-SEPAIQRD model fit (NLS)"= "solid",
                                      "Centered 7-day moving average of case counts"= "11"),
                        label = c("Centered 7-day moving average of case counts",
                                  "SEPAIQRD model fit (NLS)",
                                  "SEPAIQRD model fit (MCMC)"))+
  scale_size_manual("",values = c("SEPAIQRD model fit (MCMC)"=1,
                                  "D-SEPAIQRD model fit (NLS)"= 1,
                                  "Centered 7-day moving average of case counts"=1.5),
                    label = c("Centered 7-day moving average of case counts",
                              "SEPAIQRD model fit (NLS)",
                              "SEPAIQRD model fit (MCMC)"))+
  theme(legend.position = "bottom",axis.title.y = element_blank(),
        plot.title = element_text(size = 12.5, face = "bold"),
        legend.spacing.y = unit(0.3, 'cm'),
        legend.key.size = unit(1.5, "cm"))+
  scale_x_date(date_breaks = "14 day", date_labels =  "%m-%d ")
p_2
#
rmse(infect_data_partial[1:64],case2.mean.smooth[1:64])
#45.40919

df_3_before$mcmc_est_combine_34 = df_3_before$mcmc_est + df_4_before$mcmc_est 
df_3_before$lse_est_combine_34 = df_3_before$lse_est + df_4_before$lse_est 
df_3_before$mcmc_lb_combine_34 = df_3_before$mcmc_lb + df_4_before$mcmc_lb
df_3_before$mcmc_ub_combine_34 = df_3_before$mcmc_ub + df_4_before$mcmc_ub
df_3_before$acutual_combine_34 = df_3_before$acutual + df_4_before$acutual 
  
  
p_3 <- ggplot(data = df_3_before)+
  geom_point(aes(x = time, y = acutual_combine_34))+
  geom_line(aes(x = rollmean(time, 7, align = "center", fill = NA),
                y = rollmean(acutual_combine_34, 7, align = "center", fill = NA),
                colour = "Centered 7-day moving average of case counts",
                linetype = "Centered 7-day moving average of case counts",
                size = "Centered 7-day moving average of case counts"))+
  geom_line(aes(x = time, y = mcmc_est_combine_34,colour = "SEPAIQRD model fit (MCMC)",
                linetype = "SEPAIQRD model fit (MCMC)",
                size = "SEPAIQRD model fit (MCMC)"))+
  geom_ribbon(aes(x = time, ymax = mcmc_ub_combine_34, ymin = mcmc_lb_combine_34), fill = "lightgreen", alpha = 0.2)+
  geom_line(aes(x = time, y = lse_est_combine_34, 
                colour = "D-SEPAIQRD model fit (NLS)", 
                linetype = "D-SEPAIQRD model fit (NLS)",
                size =  "D-SEPAIQRD model fit (NLS)"))+
  scale_y_continuous(limits=c(0,max(df_3_before$lse_est_combine_34,
                                    df_3_before$mcmc_ub_combine_34,
                                    df_3_before$acutual_combine_34)))+
  xlab("Time")+
  ggtitle("(c) Fully Vaccinated")+
  theme_classic()+
  scale_colour_manual("",values = c("SEPAIQRD model fit (MCMC)"="forestgreen",
                                    "D-SEPAIQRD model fit (NLS)"= "darkorange",
                                    "Centered 7-day moving average of case counts"="red"),
                      label = c("Centered 7-day moving average of case counts",
                                "SEPAIQRD model fit (NLS)",
                                "SEPAIQRD model fit (MCMC)"))+
  scale_linetype_manual("",values = c("SEPAIQRD model fit (MCMC)"="solid",
                                      "D-SEPAIQRD model fit (NLS)"= "solid",
                                      "Centered 7-day moving average of case counts"= "11"),
                        label = c("Centered 7-day moving average of case counts",
                                  "SEPAIQRD model fit (NLS)",
                                  "SEPAIQRD model fit (MCMC)"))+
  scale_size_manual("",values = c("SEPAIQRD model fit (MCMC)"=1,
                                  "D-SEPAIQRD model fit (NLS)"= 1,
                                  "Centered 7-day moving average of case counts"=1.5),
                    label = c("Centered 7-day moving average of case counts",
                              "SEPAIQRD model fit (NLS)",
                              "SEPAIQRD model fit (MCMC)"))+
  theme(legend.position = "bottom",axis.title.y = element_blank(),
        plot.title = element_text(size = 12.5, face = "bold"),
        legend.spacing.y = unit(0.3, 'cm'),
        legend.key.size = unit(1.5, "cm"))+
  scale_x_date(date_breaks = "14 day", date_labels =  "%m-%d ")
p_3


rmse((infect_data_complete+infect_data_booster)[1:64],(case3.mean.smooth+case4.mean.smooth)[1:64])

# 796.4487
# updated 795.8538

df_1_after$mcmc_est_combine_12 = df_1_after$mcmc_est + df_2_after$mcmc_est 
df_1_after$lse_est_combine_12 = df_1_after$lse_est + df_2_after$lse_est 
df_1_after$mcmc_lb_combine_12 = df_1_after$mcmc_lb + df_2_after$mcmc_lb
df_1_after$mcmc_ub_combine_12 = df_1_after$mcmc_ub + df_2_after$mcmc_ub
df_1_after$acutual_combine_12 = df_1_after$acutual + df_2_after$acutual 

p_4 <- ggplot(data = df_1_after)+
  geom_point(aes(x = time, y = acutual_combine_12))+
  geom_line(aes(x = rollmean(time, 7, align = "center", fill = NA),
                y = rollmean(acutual_combine_12, 7, align = "center", fill = NA),
                colour = "Centered 7-day moving average of case counts",
                linetype = "Centered 7-day moving average of case counts",
                size = "Centered 7-day moving average of case counts"))+
  geom_line(aes(x = time, y = mcmc_est_combine_12,colour = "SEPAIQRD model fit (MCMC)",
                linetype = "SEPAIQRD model fit (MCMC)",
                size = "SEPAIQRD model fit (MCMC)"))+
  geom_ribbon(aes(x = time, ymax = mcmc_ub_combine_12, ymin = mcmc_lb_combine_12), fill = "lightgreen", alpha = 0.2)+
  geom_line(aes(x = time, y = lse_est_combine_12, 
                colour = "D-SEPAIQRD model fit (NLS)", 
                linetype = "D-SEPAIQRD model fit (NLS)",
                size =  "D-SEPAIQRD model fit (NLS)"))+
  scale_y_continuous(limits=c(0,max(df_1_after$lse_est_combine_12,
                                    df_1_after$mcmc_ub_combine_12,
                                    df_1_after$acutual_combine_12)))+
  xlab("Time")+
  ggtitle("(d) Not Fully Vaccinated")+
  theme_classic()+
  scale_colour_manual("",values = c("SEPAIQRD model fit (MCMC)"="forestgreen",
                                    "D-SEPAIQRD model fit (NLS)"= "darkorange",
                                    "Centered 7-day moving average of case counts"="red"),
                      label = c("Centered 7-day moving average of case counts",
                                "SEPAIQRD model fit (NLS)",
                                "SEPAIQRD model fit (MCMC)"))+
  scale_linetype_manual("",values = c("SEPAIQRD model fit (MCMC)"="solid",
                                      "D-SEPAIQRD model fit (NLS)"= "solid",
                                      "Centered 7-day moving average of case counts"= "11"),
                        label = c("Centered 7-day moving average of case counts",
                                  "SEPAIQRD model fit (NLS)",
                                  "SEPAIQRD model fit (MCMC)"))+
  scale_size_manual("",values = c("SEPAIQRD model fit (MCMC)"=1,
                                  "D-SEPAIQRD model fit (NLS)"= 1,
                                  "Centered 7-day moving average of case counts"=1.5),
                    label = c("Centered 7-day moving average of case counts",
                              "SEPAIQRD model fit (NLS)",
                              "SEPAIQRD model fit (MCMC)"))+
  theme(legend.position = "bottom",axis.title.y = element_blank(),
        plot.title = element_text(size = 12.5, face = "bold"),
        legend.spacing.y = unit(0.3, 'cm'),
        legend.key.size = unit(1.5, "cm"))+
  scale_x_date(date_breaks = "14 day", date_labels =  "%m-%d ")



rmse((infect_data_unvac+infect_data_partial)[65:150],
     (case1.mean.smooth+case2.mean.smooth)[65:150])
# 85.92086
# updated 85.81947
p_5 <- ggplot(data = df_3_after)+
  geom_point(aes(x = time, y = acutual))+
  geom_line(aes(x = rollmean(time, 7, align = "center", fill = NA),
                y = rollmean(acutual, 7, align = "center", fill = NA),
                colour = "Centered 7-day moving average of case counts",
                linetype = "Centered 7-day moving average of case counts",
                size = "Centered 7-day moving average of case counts"))+
  geom_line(aes(x = time, y = mcmc_est,colour = "SEPAIQRD model fit (MCMC)",
                linetype = "SEPAIQRD model fit (MCMC)",
                size = "SEPAIQRD model fit (MCMC)"))+
  geom_ribbon(aes(x = time, ymax = mcmc_ub, ymin = mcmc_lb), fill = "lightgreen", alpha = 0.2)+
  geom_line(aes(x = time, y = lse_est, 
                colour = "D-SEPAIQRD model fit (NLS)", 
                linetype = "D-SEPAIQRD model fit (NLS)",
                size =  "D-SEPAIQRD model fit (NLS)"))+
  scale_y_continuous(limits=c(0,max(df_3_after$lse_est,
                                    df_3_after$mcmc_ub,
                                    df_3_after$acutual)))+
  xlab("Time")+
  ggtitle("(e) Completely Vaccinated")+
  theme_classic()+
  scale_colour_manual("",values = c("SEPAIQRD model fit (MCMC)"="forestgreen",
                                    "D-SEPAIQRD model fit (NLS)"= "darkorange",
                                    "Centered 7-day moving average of case counts"="red"),
                      label = c("Centered 7-day moving average of case counts",
                                "SEPAIQRD model fit (NLS)",
                                "SEPAIQRD model fit (MCMC)"))+
  scale_linetype_manual("",values = c("SEPAIQRD model fit (MCMC)"="solid",
                                      "D-SEPAIQRD model fit (NLS)"= "solid",
                                      "Centered 7-day moving average of case counts"= "11"),
                        label = c("Centered 7-day moving average of case counts",
                                  "SEPAIQRD model fit (NLS)",
                                  "SEPAIQRD model fit (MCMC)"))+
  scale_size_manual("",values = c("SEPAIQRD model fit (MCMC)"=1,
                                  "D-SEPAIQRD model fit (NLS)"= 1,
                                  "Centered 7-day moving average of case counts"=1.5),
                    label = c("Centered 7-day moving average of case counts",
                              "SEPAIQRD model fit (NLS)",
                              "SEPAIQRD model fit (MCMC)"))+
  theme(legend.position = "bottom",axis.title.y = element_blank(),
        plot.title = element_text(size = 12.5, face = "bold"),
        legend.spacing.y = unit(0.3, 'cm'),
        legend.key.size = unit(1.5, "cm"))+
  scale_x_date(date_breaks = "14 day", date_labels =  "%m-%d ")




rmse(infect_data_complete[65:150],case3.mean.smooth[65:150])
# 197.1549
# updated 197.5961

p_6 <- ggplot(data = df_4_after)+
  geom_point(aes(x = time, y = acutual))+
  geom_line(aes(x = rollmean(time, 7, align = "center", fill = NA),
                y = rollmean(acutual, 7, align = "center", fill = NA),
                colour = "Centered 7-day moving average of case counts",
                linetype = "Centered 7-day moving average of case counts",
                size = "Centered 7-day moving average of case counts"))+
  geom_line(aes(x = time, y = mcmc_est,colour = "SEPAIQRD model fit (MCMC)",
                linetype = "SEPAIQRD model fit (MCMC)",
                size = "SEPAIQRD model fit (MCMC)"))+
  geom_ribbon(aes(x = time, ymax = mcmc_ub, ymin = mcmc_lb), fill = "lightgreen", alpha = 0.2)+
  geom_line(aes(x = time, y = lse_est, 
                colour = "D-SEPAIQRD model fit (NLS)", 
                linetype = "D-SEPAIQRD model fit (NLS)",
                size =  "D-SEPAIQRD model fit (NLS)"))+
  scale_y_continuous(limits=c(0,max(df_4_after$lse_est,
                                    df_4_after$mcmc_ub,
                                    df_4_after$acutual)))+
  xlab("Time")+
  ggtitle("(f) Vaccinated with Booster Dose")+
  theme_classic()+
  scale_colour_manual("",values = c("SEPAIQRD model fit (MCMC)"="forestgreen",
                                    "D-SEPAIQRD model fit (NLS)"= "darkorange",
                                    "Centered 7-day moving average of case counts"="red"),
                      label = c("Centered 7-day moving average of case counts",
                                "SEPAIQRD model fit (NLS)",
                                "SEPAIQRD model fit (MCMC)"))+
  scale_linetype_manual("",values = c("SEPAIQRD model fit (MCMC)"="solid",
                                      "D-SEPAIQRD model fit (NLS)"= "solid",
                                      "Centered 7-day moving average of case counts"= "11"),
                        label = c("Centered 7-day moving average of case counts",
                                  "SEPAIQRD model fit (NLS)",
                                  "SEPAIQRD model fit (MCMC)"))+
  scale_size_manual("",values = c("SEPAIQRD model fit (MCMC)"=1,
                                  "D-SEPAIQRD model fit (NLS)"= 1,
                                  "Centered 7-day moving average of case counts"=1.5),
                    label = c("Centered 7-day moving average of case counts",
                              "SEPAIQRD model fit (NLS)",
                              "SEPAIQRD model fit (MCMC)"))+
  theme(legend.position = "bottom",axis.title.y = element_blank(),
        plot.title = element_text(size = 12.5, face = "bold"),
        legend.spacing.y = unit(0.3, 'cm'),
        legend.key.size = unit(1.5, "cm"))+
  scale_x_date(date_breaks = "14 day", date_labels =  "%m-%d ")



# 523.4864
# updated 509.3786

rmse(infect_data_booster[65:150],case4.mean.smooth[65:150])
library(patchwork)
g = (p_1|p_2|p_3) /(p_4|p_5|p_6) +plot_layout(guides = "collect")& theme(legend.position = "bottom")

ggsave(g, file = "my_model_pred_updated_params.pdf", width = 10, height =9)
# install.packages("patchwork")
library(patchwork)
g1|g2|g3

 library(Metrics)
pred_tot = case4.mean.smooth+case3.mean.smooth+case2.mean.smooth+case1.mean.smooth
actual_tot = infect_data_booster + infect_data_complete + infect_data_partial + infect_data_unvac
 rmse(actual_tot,
    pred_tot)
 rmse(type1$pred + type2$pred + type3$pred + type4$pred, actual_tot)

expit = function(x) 1/(1+exp(-x))
library(latex2exp)
f_1_p1_prior = expit(rnorm(100000, -1,2))
f_1_p1_post = expit(cbind(as.array(fit_sir_prior, par = "f_1_p1")))
df_f1_p1 = data.frame(value = c(f_1_p1_prior, f_1_p1_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g1 = ggplot(df_f1_p1, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_{1,1}$"))+ xlim(0,1)+ ylim(0,60)+theme_classic() +
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))




f_2_p1_prior = expit(rnorm(100000, -0.5,2))
f_2_p1_post = expit(cbind(as.array(fit_sir_prior, par = "f_2_p1")))
df_f2_p1 = data.frame(value = c(f_2_p1_prior, f_2_p1_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g2 = ggplot(df_f2_p1, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_{2,1}$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))





f_3_p1_prior = expit(rnorm(100000, 1.5,2))
f_3_p1_post = expit(cbind(as.array(fit_sir_prior, par = "f_3_p1")))
df_f3_p1 = data.frame(value = c(f_3_p1_prior, f_3_p1_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g3 = ggplot(df_f3_p1, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_{3,1}$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))



f_4_p1_prior = expit(rnorm(100000, 2,2))
f_4_p1_post = expit(cbind(as.array(fit_sir_prior, par = "f_4_p1")))
df_f4_p1 = data.frame(value = c(f_4_p1_prior, f_4_p1_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g4 = ggplot(df_f4_p1, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_{4,1}$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))


library(patchwork)
g = g1|g2|g3|g4


########phase2

f_1_p2_prior = expit(rnorm(100000, -1,2))
f_1_p2_post =expit(cbind(as.array(fit_sir_prior, par = "f_1_p2")))
df_f1_p2 = data.frame(value = c(f_1_p2_prior, f_1_p2_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g11 = ggplot(df_f1_p2, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_{1,2}$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))




f_2_p2_prior = expit(rnorm(100000, -0.65,2))
f_2_p2_post = expit(cbind(as.array(fit_sir_prior, par = "f_2_p2")))
df_f2_p2 = data.frame(value = c(f_2_p2_prior, f_2_p2_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g21 = ggplot(df_f2_p2, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_{2,2}$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))





f_3_p2_prior = expit(rnorm(100000, 1.25,2))
f_3_p2_post = expit(cbind(as.array(fit_sir_prior, par = "f_3_p2")))
df_f3_p2 = data.frame(value = c(f_3_p2_prior, f_3_p2_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g31 = ggplot(df_f3_p2, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_{3,2}$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))



f_4_p2_prior = expit(rnorm(100000, 2,2))
f_4_p2_post = expit(cbind(as.array(fit_sir_prior, par = "f_4_p2")))
df_f4_p2 = data.frame(value = c(f_4_p2_prior, f_4_p2_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g41 = ggplot(df_f4_p2, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_{4,2}$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))


library(patchwork)
g = g1|g2|g3|g4





########phase3

f_1_p3_prior = expit(rnorm(100000, -1,2))
f_1_p3_post = expit(cbind(as.array(fit_sir_prior, par = "f_1_p3")))
df_f1_p3 = data.frame(value = c(f_1_p3_prior, f_1_p3_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g12 = ggplot(df_f1_p3, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_{1,3}$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))




f_2_p3_prior = expit(rnorm(100000, -0.75,2))
f_2_p3_post = expit(cbind(as.array(fit_sir_prior, par = "f_2_p3")))
df_f2_p3 = data.frame(value = c(f_2_p3_prior, f_2_p3_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g22 = ggplot(df_f2_p3, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_{2,3}$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))





f_3_p3_prior = expit(rnorm(100000, 1,2))
f_3_p3_post = expit(cbind(as.array(fit_sir_prior, par = "f_3_p3")))
df_f3_p3 = data.frame(value = c(f_3_p3_prior, f_3_p3_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g32 = ggplot(df_f3_p3, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_{3,3}$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))



f_4_p3_prior = expit(rnorm(100000, 1.5,2))
f_4_p3_post = expit(cbind(as.array(fit_sir_prior, par = "f_4_p3")))
df_f4_p3 = data.frame(value = c(f_4_p3_prior, f_4_p3_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g42 = ggplot(df_f4_p3, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_{4,3}$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))


library(patchwork)
g = g1|g2|g3|g4





########phase4

f_1_p4_prior = expit(rnorm(100000, -1,2))
f_1_p4_post = expit(cbind(as.array(fit_sir_prior, par = "f_1_p4")))
df_f1_p4 = data.frame(value = c(f_1_p4_prior, f_1_p4_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g13 = ggplot(df_f1_p4, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_{1,4}$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))




f_2_p4_prior = expit(rnorm(100000, -0.85,2))
f_2_p4_post = expit(cbind(as.array(fit_sir_prior, par = "f_2_p4")))
df_f2_p4 = data.frame(value = c(f_2_p4_prior, f_2_p4_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g23 = ggplot(df_f2_p4, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_{2,4}$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))





f_3_p4_prior = expit(rnorm(100000, 0.8,2))
f_3_p4_post = expit(cbind(as.array(fit_sir_prior, par = "f_3_p4")))
df_f3_p4 = data.frame(value = c(f_3_p4_prior, f_3_p4_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g33 = ggplot(df_f3_p4, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_{3,4}$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))



f_4_p4_prior = expit(rnorm(100000, 1.25,2))
f_4_p4_post = expit(cbind(as.array(fit_sir_prior, par = "f_4_p4")))
df_f4_p4 = data.frame(value = c(f_4_p4_prior, f_4_p4_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g43 = ggplot(df_f4_p4, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_{4,4}$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))


library(patchwork)
g = g1|g2|g3|g4




########phase5


f_1_p5_prior = expit(rnorm(100000, -1,2))
f_1_p5_post = expit(cbind(as.array(fit_sir_prior, par = "f_1_p5")))
df_f1_p5 = data.frame(value = c(f_1_p5_prior, f_1_p5_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g14 = ggplot(df_f1_p5, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_{1,5}$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))




f_2_p5_prior =expit(rnorm(100000, -0.95,2))
f_2_p5_post = expit(cbind(as.array(fit_sir_prior, par = "f_2_p5")))
df_f2_p5 = data.frame(value = c(f_2_p5_prior, f_2_p5_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g24 = ggplot(df_f2_p5, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_{2,5}$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))





f_3_p5_prior = expit(rnorm(100000, 0.75,2))
f_3_p5_post = expit(cbind(as.array(fit_sir_prior, par = "f_3_p5")))
df_f3_p5 = data.frame(value = c(f_3_p5_prior, f_3_p5_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g34 = ggplot(df_f3_p5, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_{3,5}$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))



f_4_p5_prior = expit(rnorm(100000, 1.15,2))
f_4_p5_post = expit(cbind(as.array(fit_sir_prior, par = "f_4_p5")))
df_f4_p5 = data.frame(value = c(f_4_p5_prior, f_4_p5_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g44 = ggplot(df_f4_p5, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_{4,5}$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))



library(patchwork)
g = g1|g2|g3|g4




## CAR
library(ggplot2)


car_p1_prior = expit(rnorm(100000, -1.84,1.31))
car_p1_post = expit(cbind(as.array(fit_sir_prior, par = "CAR_p1")))
df_car_p1 = data.frame(value = c(car_p1_prior, car_p1_post),
                       name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


gcarp1 = ggplot(df_car_p1, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$CAR_1$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))



car_p2_prior = expit(rnorm(100000, -1.8,1.15))
car_p2_post =expit(cbind(as.array(fit_sir_prior, par = "CAR_p2")))
df_car_p2 = data.frame(value = c(car_p2_prior, car_p2_post),
                       name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


gcarp2 = ggplot(df_car_p2, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$CAR_2$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))



car_p3_prior = expit(rnorm(100000, -1.5,1.31))
car_p3_post = expit(cbind(as.array(fit_sir_prior, par = "CAR_p3")))
df_car_p3 = data.frame(value = c(car_p3_prior, car_p3_post),
                       name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


gcarp3 = ggplot(df_car_p3, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$CAR_3$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))



car_p4_prior = expit(rnorm(100000, -1.09,1.15))
car_p4_post = expit(cbind(as.array(fit_sir_prior, par = "CAR_p4")))
df_car_p4 = data.frame(value = c(car_p4_prior, car_p4_post),
                       name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


gcarp4 = ggplot(df_car_p4, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$CAR_4$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))


car_p5_prior = expit(rnorm(100000, -0.59,1.31))
car_p5_post = expit(cbind(as.array(fit_sir_prior, par = "CAR_p5")))
df_car_p5 = data.frame(value = c(car_p5_prior, car_p5_post),
                       name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


gcarp5 = ggplot(df_car_p5, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$CAR_5$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))

orginal_scale_plot = (g1|g2|g3|g4|gcarp1)/
  (g11|g21|g31|g41|gcarp2)/
  (g12|g22|g32|g42|gcarp3)/
  (g13|g23|g33|g43|gcarp4)/
  (g14|g24|g34|g44|gcarp5) +plot_layout(guides = "collect")& theme(legend.position = "bottom")

ggsave(orginal_scale_plot, file = "my_model_para_plot_updated_params_changeindex.pdf", width = 28, height = 25)

