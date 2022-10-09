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



model <- stan_model("8.21_revised.stan")



fit_sir_prior <- sampling(model,
                          data = data_sir, 
                          chains = 4,
                          iter = 2000,
                          seed = 0)




 save.image("stan_results_8.21.RData")
 save.image(file = paste0("stan_results_8.21",as.character(Sys.time()), ".RData"))
# 
# 
# 
# # if(FALSE){
 library(rstan)
load(file = "stan_results_8.21_old_oxford.RData")

## original data

check_hmc_diagnostics(fit_sir_prior)

 # summary(fit_sir_prior)
 # #pars = c('k_1', 'k_2', 'k_3', 'k_4', 'CAR', 'CAR_f')
 # stan_dens(fit_sir_prior, pars = pars)
 # check_hmc_diagnostics(fit_sir_prior)
 traceplot(fit_sir_prior, pars = c("f_1_p1","f_2_p1","f_3_p1","f_4_p1","CAR_p1",
                                   "f_1_p2","f_2_p2","f_3_p2","f_4_p2","CAR_p2",
                                   "f_1_p3","f_2_p3","f_3_p3","f_4_p3","CAR_p3",
                                   "f_1_p4","f_2_p4","f_3_p4","f_4_p4","CAR_p4",
                                   "f_1_p5","f_2_p5","f_3_p5","f_4_p5","CAR_p5"),
           nrow = 5, ncol = 5)

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
# 
# library(ggplot2)
#  df_1 = data.frame(times, case1.mean, case1.025, case1.975, infect_data_unvac)
#  g1 = ggplot(df_1,aes(times))+
#    geom_ribbon(aes(ymax = case1.975, ymin = case1.025), fill = "grey70")+
#    geom_line(aes(y =case1.mean ))+
#    geom_point(aes(y = infect_data_unvac))+
#    xlab("Time")+ylab("Unvaccinated Cases") +theme_classic()
# g1
# #
#  df_2 = data.frame(times, case2.mean, case2.025, case2.975, infect_data_partial)
# 
#  g2 = ggplot(df_2,aes(times))+
#    geom_ribbon(aes(ymax = case2.975, ymin = case2.025), fill = "grey70")+
#    geom_line(aes(y =case2.mean ))+
#    geom_point(aes(y = infect_data_partial))+
#    xlab("Time")+ylab("Partially vaccinated Cases")+theme_classic()
# g2#
# 
#  df_3 = data.frame(times, case3.mean, case3.025, case3.975, infect_data_complete,
#                    case4.mean, case4.025, case4.975, infect_data_booster)
# 
#  g3 = ggplot(df_3,aes(times))+
#    geom_ribbon(aes(ymax = case3.975, ymin = case3.025), fill = "grey70")+
#    geom_line(aes(y =case3.mean))+
#    geom_point(aes(y = infect_data_complete))+
#    xlab("Time")+ylab("Completely vaccinated Cases")+theme_classic()
# g3
# 
# 
#  df_4 = data.frame(times, case4.mean, case4.025, case4.975, infect_data_booster)
# 
#  g4 = ggplot(df_4,aes(times))+
#    geom_ribbon(aes(ymin = case4.975, ymax = case4.025), fill = "grey70")+
#    geom_line(aes(y =case4.mean ))+
#    geom_point(aes(y = infect_data_booster))+
#    xlab("times")+ylab("Vaccinated with booster cases")+theme_classic()
# g4
# 
# #
#  install.packages("patchwork")
#  library(patchwork)
# g1|g2|g3|g4


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
times = seq(as.Date("2022-01-06"),
        as.Date("2022-06-04"),
        by = "day")
df_1 = data.frame(times, case1.mean.smooth, case1.025.smooth, case1.975.smooth, infect_data_unvac)
g1 = ggplot(df_1[1:64,],aes(times[1:64]))+
  geom_ribbon(aes(ymax = case1.975.smooth[1:64], ymin = case1.025.smooth[1:64]), fill = "grey70")+
  geom_line(aes(y =case1.mean.smooth[1:64]))+
  geom_point(aes(y = infect_data_unvac[1:64]))+
  xlab("Time")+ggtitle("Unvaccinated") +theme_classic()+
  theme(legend.position = "bottom",axis.title.y = element_blank())+
  scale_x_date(date_breaks = "14 day", date_labels =  "%m-%d ") 
g1
library(Metrics)
rmse(infect_data_unvac[1:64],case1.mean.smooth[1:64])
#110.0611
df_2 = data.frame(times, case2.mean.smooth, case2.025.smooth, case2.975.smooth, infect_data_partial)

g2 = ggplot(df_2[1:64,],aes(times)[1:64])+
  geom_ribbon(aes(ymax = case2.975.smooth[1:64], ymin = case2.025.smooth[1:64]), fill = "grey70")+
  geom_line(aes(y =case2.mean.smooth[1:64] ))+
  geom_point(aes(y = infect_data_partial[1:64]))+
  xlab("Time")+ggtitle("Partially Vaccinated")+theme_classic()+
  theme(legend.position = "bottom",axis.title.y = element_blank())+
  scale_x_date(date_breaks = "14 day", date_labels =  "%m-%d ") 
g2
#
rmse(infect_data_partial[1:64],case2.mean.smooth[1:64])
# 43.35966
df_3 = data.frame(times, case3.mean.smooth, case3.025.smooth, case3.975.smooth,
                  case4.mean.smooth, case4.025.smooth, case4.975.smooth,
                  infect_data_complete,
               infect_data_booster)

g3 = ggplot(df_3[1:64,],aes(times)[1:64])+
  geom_ribbon(aes(ymax = (case3.975.smooth+case4.975.smooth)[1:64],
                  ymin = (case3.025.smooth+case4.025.smooth)[1:64]), fill = "grey70")+
  geom_line(aes(y =(case3.mean.smooth+case4.mean.smooth)[1:64]))+
  geom_point(aes(y = (infect_data_complete+infect_data_booster)[1:64]))+
  xlab("Time")+ggtitle("Fully Vaccinated")+theme_classic()+
  theme(legend.position = "bottom",axis.title.y = element_blank())+
  scale_x_date(date_breaks = "14 day", date_labels =  "%m-%d ") 
g3
library(patchwork)
g = g1|g2|g3

g

rmse((infect_data_complete+infect_data_booster)[1:64],(case3.mean.smooth+case4.mean.smooth)[1:64])

# 796.4487



df_1 = data.frame(times, 
                  case1.mean.smooth, case1.025.smooth, case1.975.smooth, 
                  infect_data_unvac,
                  case2.mean.smooth, case2.025.smooth, case2.975.smooth, 
                  infect_data_partial)
g4 = ggplot(df_1[65:150,],aes(times))+
  geom_ribbon(aes(ymax = case1.975.smooth+case2.975.smooth, ymin = case1.025.smooth+case2.025.smooth), fill = "grey70")+
  geom_line(aes(y =case1.mean.smooth+case2.mean.smooth))+
  geom_point(aes(y = infect_data_unvac+infect_data_partial))+
  xlab("Time")+ggtitle("Not Fully Vaccinated") +theme_classic()+
  theme(legend.position = "bottom",axis.title.y = element_blank())+
  scale_x_date(date_breaks = "14 day", date_labels =  "%m-%d ") 
g4

rmse((infect_data_unvac+infect_data_partial)[65:150],
     (case1.mean.smooth+case2.mean.smooth)[65:150])
# 85.92086  
df_3 = data.frame(times, case3.mean.smooth, case3.025.smooth, case3.975.smooth, infect_data_complete)

g5 = ggplot(df_3[65:150,],aes(times))+
  geom_ribbon(aes(ymax = (case3.975.smooth),
                  ymin = (case3.025.smooth)), fill = "grey70")+
  geom_line(aes(y =case3.mean.smooth))+
  geom_point(aes(y = infect_data_complete))+
  xlab("Time")+ggtitle("Completely Vaccinated")+theme_classic()+
  theme(legend.position = "bottom",axis.title.y = element_blank())+
  scale_x_date(date_breaks = "14 day", date_labels =  "%m-%d ") 
g5
rmse(infect_data_complete[65:150],case3.mean.smooth[65:150])
# 197.1549
df_3 = data.frame(times, case4.mean.smooth, case4.025.smooth, case4.975.smooth, infect_data_booster)

g6 = ggplot(df_3[65:150,],aes(times))+
  geom_ribbon(aes(ymin = case4.975.smooth, ymax = case4.025.smooth), fill = "grey70")+
  geom_line(aes(y =case4.mean.smooth ))+
  geom_point(aes(y = infect_data_booster))+
  xlab("Time")+ggtitle("Vaccinated with Booster Dose")+theme_classic()+
  theme(legend.position = "bottom",axis.title.y = element_blank())+
  scale_x_date(date_breaks = "14 day", date_labels =  "%m-%d ") 
g6
# 523.4864
rmse(infect_data_booster[65:150],case4.mean.smooth[65:150])
g = (g1|g2|g3) /(g4|g5|g6)
ggsave(g, file = "my_model_pred.pdf", width = 10, height =9)
# install.packages("patchwork")
library(patchwork)
g1|g2|g3
#  # grid.arrange(g1,g2,g3,g4, ncol = 2, nrow = 2)
# #
# #
 library(Metrics)
pred_tot = case4.mean.smooth+case3.mean.smooth+case2.mean.smooth+case1.mean.smooth
actual_tot = infect_data_booster + infect_data_complete + infect_data_partial + infect_data_unvac
 rmse(actual_tot,
    pred_tot)
# 840.0789
## [0,1]
expit = function(x) 1/(1+exp(-x))
library(latex2exp)
f_1_p1_prior = expit(rnorm(100000, -1,2))
f_1_p1_post = expit(cbind(as.array(fit_sir_prior, par = "f_1_p1")))
df_f1_p1 = data.frame(value = c(f_1_p1_prior, f_1_p1_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g1 = ggplot(df_f1_p1, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_1^1$"))+ xlim(0,1)+ ylim(0,60)+theme_classic() +
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))




f_2_p1_prior = expit(rnorm(100000, -0.5,2))
f_2_p1_post = expit(cbind(as.array(fit_sir_prior, par = "f_2_p1")))
df_f2_p1 = data.frame(value = c(f_2_p1_prior, f_2_p1_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g2 = ggplot(df_f2_p1, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_2^1$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))





f_3_p1_prior = expit(rnorm(100000, 1.5,2))
f_3_p1_post = expit(cbind(as.array(fit_sir_prior, par = "f_3_p1")))
df_f3_p1 = data.frame(value = c(f_3_p1_prior, f_3_p1_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g3 = ggplot(df_f3_p1, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_3^1$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))



f_4_p1_prior = expit(rnorm(100000, 2,2))
f_4_p1_post = expit(cbind(as.array(fit_sir_prior, par = "f_4_p1")))
df_f4_p1 = data.frame(value = c(f_4_p1_prior, f_4_p1_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g4 = ggplot(df_f4_p1, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_4^1$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))


library(patchwork)
g = g1|g2|g3|g4
#ggsave(g, file = "f_phase_1.pdf", width = 15, height = 3)


########phase2

f_1_p2_prior = expit(rnorm(100000, -1,2))
f_1_p2_post =expit(cbind(as.array(fit_sir_prior, par = "f_1_p2")))
df_f1_p2 = data.frame(value = c(f_1_p2_prior, f_1_p2_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g11 = ggplot(df_f1_p2, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_1^2$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))




f_2_p2_prior = expit(rnorm(100000, -0.65,2))
f_2_p2_post = expit(cbind(as.array(fit_sir_prior, par = "f_2_p2")))
df_f2_p2 = data.frame(value = c(f_2_p2_prior, f_2_p2_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g21 = ggplot(df_f2_p2, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_2^2$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))





f_3_p2_prior = expit(rnorm(100000, 1.25,2))
f_3_p2_post = expit(cbind(as.array(fit_sir_prior, par = "f_3_p2")))
df_f3_p2 = data.frame(value = c(f_3_p2_prior, f_3_p2_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g31 = ggplot(df_f3_p2, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_3^2$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))



f_4_p2_prior = expit(rnorm(100000, 2,2))
f_4_p2_post = expit(cbind(as.array(fit_sir_prior, par = "f_4_p2")))
df_f4_p2 = data.frame(value = c(f_4_p2_prior, f_4_p2_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g41 = ggplot(df_f4_p2, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_4^2$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))


library(patchwork)
g = g1|g2|g3|g4

#ggsave(g, file = "f_phase_2.pdf", width = 15, height = 3)




########phase3

f_1_p3_prior = expit(rnorm(100000, -1,2))
f_1_p3_post = expit(cbind(as.array(fit_sir_prior, par = "f_1_p3")))
df_f1_p3 = data.frame(value = c(f_1_p3_prior, f_1_p3_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g12 = ggplot(df_f1_p3, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_1^3$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))




f_2_p3_prior = expit(rnorm(100000, -0.75,2))
f_2_p3_post = expit(cbind(as.array(fit_sir_prior, par = "f_2_p3")))
df_f2_p3 = data.frame(value = c(f_2_p3_prior, f_2_p3_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g22 = ggplot(df_f2_p3, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_2^3$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))





f_3_p3_prior = expit(rnorm(100000, 1,2))
f_3_p3_post = expit(cbind(as.array(fit_sir_prior, par = "f_3_p3")))
df_f3_p3 = data.frame(value = c(f_3_p3_prior, f_3_p3_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g32 = ggplot(df_f3_p3, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_3^3$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))



f_4_p3_prior = expit(rnorm(100000, 1.5,2))
f_4_p3_post = expit(cbind(as.array(fit_sir_prior, par = "f_4_p3")))
df_f4_p3 = data.frame(value = c(f_4_p3_prior, f_4_p3_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g42 = ggplot(df_f4_p3, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_4^3$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))


library(patchwork)
g = g1|g2|g3|g4

#ggsave(g, file = "f_phase_3.pdf", width = 15, height = 3)




########phase4

f_1_p4_prior = expit(rnorm(100000, -1,2))
f_1_p4_post = expit(cbind(as.array(fit_sir_prior, par = "f_1_p4")))
df_f1_p4 = data.frame(value = c(f_1_p4_prior, f_1_p4_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g13 = ggplot(df_f1_p4, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_1^4$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))




f_2_p4_prior = expit(rnorm(100000, -0.85,2))
f_2_p4_post = expit(cbind(as.array(fit_sir_prior, par = "f_2_p4")))
df_f2_p4 = data.frame(value = c(f_2_p4_prior, f_2_p4_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g23 = ggplot(df_f2_p4, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_2^4$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))





f_3_p4_prior = expit(rnorm(100000, 0.8,2))
f_3_p4_post = expit(cbind(as.array(fit_sir_prior, par = "f_3_p4")))
df_f3_p4 = data.frame(value = c(f_3_p4_prior, f_3_p4_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g33 = ggplot(df_f3_p4, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_3^4$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))



f_4_p4_prior = expit(rnorm(100000, 1.25,2))
f_4_p4_post = expit(cbind(as.array(fit_sir_prior, par = "f_4_p4")))
df_f4_p4 = data.frame(value = c(f_4_p4_prior, f_4_p4_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g43 = ggplot(df_f4_p4, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_4^4$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))


library(patchwork)
g = g1|g2|g3|g4

#ggsave(g, file = "f_phase_4.pdf", width = 15, height = 3)



########phase5


f_1_p5_prior = expit(rnorm(100000, -1,2))
f_1_p5_post = expit(cbind(as.array(fit_sir_prior, par = "f_1_p5")))
df_f1_p5 = data.frame(value = c(f_1_p5_prior, f_1_p5_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g14 = ggplot(df_f1_p5, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_1^5$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))




f_2_p5_prior =expit(rnorm(100000, -0.95,2))
f_2_p5_post = expit(cbind(as.array(fit_sir_prior, par = "f_2_p5")))
df_f2_p5 = data.frame(value = c(f_2_p5_prior, f_2_p5_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g24 = ggplot(df_f2_p5, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_2^5$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))





f_3_p5_prior = expit(rnorm(100000, 0.75,2))
f_3_p5_post = expit(cbind(as.array(fit_sir_prior, par = "f_3_p5")))
df_f3_p5 = data.frame(value = c(f_3_p5_prior, f_3_p5_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g34 = ggplot(df_f3_p5, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_3^5$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))



f_4_p5_prior = expit(rnorm(100000, 1.15,2))
f_4_p5_post = expit(cbind(as.array(fit_sir_prior, par = "f_4_p5")))
df_f4_p5 = data.frame(value = c(f_4_p5_prior, f_4_p5_post),
                      name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


g44 = ggplot(df_f4_p5, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$f_4^5$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))



library(patchwork)
g = g1|g2|g3|g4

#ggsave(g, file = "f_phase_5.pdf", width = 15, height = 3)

# orginal_scale_plot = (g1|g2|g3|g4)/
#   (g11|g21|g31|g41)/
#   (g12|g22|g32|g42)/
#   (g13|g23|g33|g43)/
#   (g14|g24|g34|g44) +plot_layout(guides = "collect")& theme(legend.position = "bottom")

# ggsave(orginal_scale_plot, file = "original_scale_f_ij_revised.pdf", width = 25, height = 25)



## CAR
library(ggplot2)


car_p1_prior = expit(rnorm(100000, -1.84,1.31))
car_p1_post = expit(cbind(as.array(fit_sir_prior, par = "CAR_p1")))
df_car_p1 = data.frame(value = c(car_p1_prior, car_p1_post),
                       name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


gcarp1 = ggplot(df_car_p1, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$CAR^1$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))



car_p2_prior = expit(rnorm(100000, -1.8,1.15))
car_p2_post =expit(cbind(as.array(fit_sir_prior, par = "CAR_p2")))
df_car_p2 = data.frame(value = c(car_p2_prior, car_p2_post),
                       name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


gcarp2 = ggplot(df_car_p2, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$CAR^2$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))



car_p3_prior = expit(rnorm(100000, -1.5,1.31))
car_p3_post = expit(cbind(as.array(fit_sir_prior, par = "CAR_p3")))
df_car_p3 = data.frame(value = c(car_p3_prior, car_p3_post),
                       name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


gcarp3 = ggplot(df_car_p3, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$CAR^3$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))



car_p4_prior = expit(rnorm(100000, -1.09,1.15))
car_p4_post = expit(cbind(as.array(fit_sir_prior, par = "CAR_p4")))
df_car_p4 = data.frame(value = c(car_p4_prior, car_p4_post),
                       name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


gcarp4 = ggplot(df_car_p4, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$CAR^4$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))


car_p5_prior = expit(rnorm(100000, -0.59,1.31))
car_p5_post = expit(cbind(as.array(fit_sir_prior, par = "CAR_p5")))
df_car_p5 = data.frame(value = c(car_p5_prior, car_p5_post),
                       name = c(rep("prior", each = 100000), rep("posterior", each = 4000)))


gcarp5 = ggplot(df_car_p5, aes(x = value, group = name, color = name)) + geom_density(aes(fill = name), alpha = 0.3)+
  xlab(TeX("$CAR^5$"))+ xlim(0,1)+ ylim(0,60)+theme_classic()+
  theme(axis.text.y = element_text(size = 22),axis.text.x = element_text(size = 22) ,legend.title=element_blank(),
        legend.text=element_text(size=25),axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 22))

orginal_scale_plot = (g1|g2|g3|g4|gcarp1)/
  (g11|g21|g31|g41|gcarp2)/
  (g12|g22|g32|g42|gcarp3)/
  (g13|g23|g33|g43|gcarp4)/
  (g14|g24|g34|g44|gcarp5) +plot_layout(guides = "collect")& theme(legend.position = "bottom")

ggsave(orginal_scale_plot, file = "my_model_para_plot.pdf", width = 28, height = 25)

