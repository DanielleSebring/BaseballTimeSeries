library(ggplot2)

## Danielle's Path
pitching <- read.csv("~/Spring 2022 5414/BaseballTimeSeries/Datasets/Pitching.csv")
## Steven's Path
pitching <- read.csv("~/VTGraduateSchool/SPRING_2022/STAT_5414/Project/BaseballTimeSeries/Datasets/Pitching.csv")

## Colors for plots
orange <- "#e87722"
maroon <- "#861f41"
gray <- "#75787b"
yellow <- "#ff9800"

## Summarize league-wide pitching statistics by year
pitch_league <- pitching %>% 
  dplyr::select(!c(playerID, stint, teamID, lgID)) %>%
  group_by(yearID) %>% 
  summarise_all(sum) %>%
  mutate(IP = IPouts/3) %>%
  ## Summarize statistics per nine innings
  mutate(H9 = 9*H/IP, HR9 = 9*HR/IP, BB9 = 9*BB/IP, SO9 = 9*SO/IP, 
         WP9 = 9*WP/IP, HBP9 = 9*HBP/IP, SAC9 = 9*(SH + SF)/IP) %>%
  ## Select the last 100 years
  filter(yearID > 1921)

## Initialize time series
so_ts <- ts(pitch_league$SO9, start = 1922, end = 2021, frequency = 1)
hr_ts <- ts(pitch_league$HR9, start = 1922, end = 2021, frequency = 1)
hit_ts <- ts(pitch_league$H9, start = 1922, end = 2021, frequency = 1)
wp_ts <- ts(pitch_league$WP9, start = 1922, end = 2021, frequency = 1)
hbp_ts <- ts(pitch_league$HBP9, start = 1922, end = 2021, frequency = 1)
sac_ts <- ts(pitch_league$SAC9[which(!is.na(pitch_league$SAC9))], 
             start = 1971, end = 2021, frequency = 1)

## Six-panel plot of all time series
par(mfrow = c(2, 3), mar = c(2.5, 4.0, 0.5, 0.5))
plot(so_ts, type = "n", ylab = "", tck = -0.02, ylim = c(2.5, 9.5), xlab = "Year", 
      yaxt = "n", xaxt = "n")
grid(lty = 1, col = "lightgrey", lwd = 1)
lines(so_ts, ylab = "", tck = -0.02, ylim = c(2.5, 9.5), xlab = "Year", 
     yaxt = "n", xaxt = "n")
axis(2, at = c(4, 6, 8, 10), las = 1, cex.axis = 1.05,
     hadj = 0.5)
axis(1, at = seq(1920, 2020, by = 20), las = 1, cex.axis = 1.05,
     padj = -0.75)
mtext("Strikeouts", side = 2, line = 1.5)

plot(hr_ts, ylab = "", tck = -0.02, ylim = c(0.35, 1.45), xlab = "Year",  
     yaxt = "n", xaxt = "n", type = "n")
grid(lty = 1, col = "lightgrey", lwd = 1)
lines(hr_ts, ylab = "", tck = -0.02, ylim = c(0.35, 1.45), xlab = "Year",  
     yaxt = "n", xaxt = "n")
axis(2, at = seq(0.4, 1.4, by = 0.2), las = 1, cex.axis = 1.05,
     hadj = 0.75)
axis(1, at = seq(1920, 2020, by = 20), las = 1, cex.axis = 1.05,
     padj = -0.75)
mtext("Home Runs", side = 2, line = 2.25)

plot(hit_ts, ylab = "", tck = -0.02, ylim = c(7.5, 11), xlab = "Year",  
     yaxt = "n", xaxt = "n", type = "n")
grid(lty = 1, col = "lightgrey", lwd = 1)
lines(hit_ts, ylab = "", tck = -0.02, ylim = c(7.5, 11), xlab = "Year",  
     yaxt = "n", xaxt = "n")
axis(2, at = seq(8, 11, by = 1), las = 1, cex.axis = 1.05,
     hadj = 0.5)
axis(1, at = seq(1920, 2020, by = 20), las = 1, cex.axis = 1.05,
     padj = -0.75)
mtext("Hits", side = 2, line = 1.5)

plot(wp_ts, ylab = "", tck = -0.02, ylim = c(0.1, 0.45), xlab = "Year",  
     yaxt = "n", xaxt = "n", type = "n")
grid(lty = 1, col = "lightgrey", lwd = 1)
lines(wp_ts, ylab = "", tck = -0.02, ylim = c(0.1, 0.45), xlab = "Year",  
     yaxt = "n", xaxt = "n")
axis(2, at = seq(0.15, 0.45, by = 0.1), las = 1, cex.axis = 1.05,
     hadj = 0.75)
axis(1, at = seq(1920, 2020, by = 20), las = 1, cex.axis = 1.05,
     padj = -0.75)
mtext("Wild Pitches", side = 2, line = 2.5)

plot(hbp_ts, ylab = "", tck = -0.02, ylim = c(0.1, 0.5), xlab = "Year",  
     yaxt = "n", xaxt = "n", type = "n")
grid(lty = 1, col = "lightgrey", lwd = 1)
lines(hbp_ts, ylab = "", tck = -0.02, ylim = c(0.1, 0.5), xlab = "Year",  
     yaxt = "n", xaxt = "n")
axis(2, at = seq(0.15, 0.45, by = 0.1), las = 1, cex.axis = 1.05,
     hadj = 0.75)
axis(1, at = seq(1920, 2020, by = 20), las = 1, cex.axis = 1.05,
     padj = -0.75)
mtext("Hit By Pitch", side = 2, line = 2.5)

plot(sac_ts, ylab = "", tck = -0.02, ylim = c(0.25, 0.85), xlab = "Year",  
     yaxt = "n", xaxt = "n", type = "n")
grid(lty = 1, col = "lightgrey", lwd = 1)
lines(sac_ts, ylab = "", tck = -0.02, ylim = c(0.25, 0.85), xlab = "Year",  
     yaxt = "n", xaxt = "n")
axis(2, at = seq(0.3, 0.8, by = 0.1), las = 1, cex.axis = 1.05,
     hadj = 0.75)
axis(1, at = seq(1980, 2020, by = 20), las = 1, cex.axis = 1.05,
     padj = -0.75)
mtext("Sacrifices", side = 2, line = 2.25)

# Fit DLM's of order 1, 2, and 3 to Strikeout Per Nine Innings time series
build_first_order <- function(parm) {
  dlmModPoly(order = 1, dV = exp(parm[1]), dW = exp(parm[2]))
}

so_order1_fit <- dlmMLE(so_ts, rep(0, 2), build_first_order)
so_order1_fit$convergence
exp(so_order1_fit$par)
so_mod_order1 <- build_first_order(so_order1_fit$par)
so_order1_filt <- dlmFilter(so_ts, so_mod_order1)
so_order1_smth <- dlmSmooth(so_order1_filt)

build_second_order <- function(parm) {
  dlmModPoly(order = 2, dV = exp(parm[1]), dW = c(exp(parm[2]), exp(parm[3])))
}

so_order2_fit <- dlmMLE(so_ts, rep(0, 3), build_second_order)
so_order2_fit$convergence
exp(so_order2_fit$par)
so_mod_order2 <- build_second_order(so_order2_fit$par)
so_order2_filt <- dlmFilter(so_ts, so_mod_order2)
so_order2_smth <- dlmSmooth(so_order2_filt)

build_third_order <- function(parm) {
  dlmModPoly(order = 3, dV = exp(parm[1]), dW = c(exp(parm[2]), exp(parm[3]),
                                                  exp(parm[4])))
}

so_order3_fit <- dlmMLE(so_ts, rep(0, 4), build_third_order)
so_order3_fit$convergence
exp(so_order3_fit$par)
so_mod_order3 <- build_third_order(so_order3_fit$par)
so_order3_filt <- dlmFilter(so_ts, so_mod_order3)
so_order3_smth <- dlmSmooth(so_order3_filt)

## Fit ARIMA models
diff_so_ts <- diff(so_ts)
par(mar = c(2.5, 5.0, 0.5, 0.5))
plot(diff_so_ts, type = "n", ylab = "", tck = -0.02, ylim = c(-0.45, 0.6), xlab = "Year", 
     yaxt = "n", xaxt = "n")
lines(diff_so_ts, ylab = "", tck = -0.02, ylim = c(-0.45, 0.65), xlab = "Year", 
      yaxt = "n", xaxt = "n")
axis(2, at = seq(-0.4, 0.6, by = 0.2), las = 1, cex.axis = 1.05,
     hadj = 0.75)
axis(1, at = seq(1920, 2020, by = 20), las = 1, cex.axis = 1.05,
     padj = -0.75)
mtext("Strikeouts (First-order difference)", side = 2, line = 3.0)

plot(diff_so_ts)
acf(diff_so_ts)
pacf(diff_so_ts)

n <- length(so_ts)
max.p <- 2
max.d <- 2
max.q <- 2
BIC.array <- array(NA,dim=c(max.p+1,max.d+1,max.q+1))
AIC.array <- array(NA,dim=c(max.p+1,max.d+1,max.q+1))
best.bic <- 1e8
x.ts = so_ts
for (p in 0:max.p) for(d in 0:max.d) for(q in 0:max.q) 
  {
    # This is a modification of a function originally from the book:
    # Cowpertwait, P.S.P., Metcalfe, A.V. (2009), Introductory Time 
    # Series with R, Springer.
    # Modified by M.A.R. Ferreira (2016, 2020).
    cat("p=",p,", d=",d,", q=",q,"\n")
    
    fit <- tryCatch(
      {  arima(x.ts, order = c(p,d,q),method="CSS-ML")
      },
      error = function(cond){
        message("Original error message:")
        message(cond)
        # Choose a return value in case of error
        return(NA)
      }
    )
    if(!is.na(fit)){ 
      number.parameters <- length(fit$coef) + 1
      BIC.array[p+1,d+1,q+1] = -2*fit$loglik + log(n)*number.parameters
      AIC.array[p+1,d+1,q+1] = -2*fit$loglik + 2*number.parameters
      
      if (BIC.array[p+1,d+1,q+1] < best.bic) 
      {
        best.bic <- BIC.array[p+1,d+1,q+1]
        best.fit <- fit
        best.model <- c(p,d,q) 
      }
    }
  }

so_arima_mod <- arima(so_ts, order = best.model, method = "CSS-ML")

## Compare BIC of each polynomial order model
so_order1_BIC <- 2 * so_order1_fit$value + length(so_order1_fit$par) * log(length(so_ts))
so_order2_BIC <- 2 * so_order2_fit$value + length(so_order2_fit$par) * log(length(so_ts))
so_order3_BIC <- 2 * so_order3_fit$value + length(so_order3_fit$par) * log(length(so_ts))
so_arima_BIC <- -2 * so_arima_mod$loglik + (length(so_arima_mod$coef) + 1) * log(length(so_ts))

# Let's perform one step ahead forecasting for the last 20 years using 
# the ARIMA(0, 1, 0) model
so_arima_fore <- rep(NA, 20)
for (i in 1:20) {
  arima_fit <- arima(so_ts[1:(length(so_ts) - 21 + i)], order = c(0, 1, 0), 
                     method = "CSS-ML")
  forecast <- predict(arima_fit, n.ahead = 1)
  so_arima_fore[i] <- forecast$pred
}

# Compare forecast error for last 20 years of each polynomial order model
## Mean absolute forecast error (MAE)
order1_mae <- mean(abs(so_order1_filt$f[81:100] - so_ts[81:100]))
order2_mae <- mean(abs(so_order2_filt$f[81:100] - so_ts[81:100]))
order3_mae <- mean(abs(so_order3_filt$f[81:100] - so_ts[81:100]))
arima_mae <- mean(abs(so_arima_fore - so_ts[81:100]))

## Mean squared forecast error (MSE)
order1_mse <- mean((so_order1_filt$f[81:100] - so_ts[81:100])^2)
order2_mse <- mean((so_order2_filt$f[81:100] - so_ts[81:100])^2)
order3_mse <- mean((so_order3_filt$f[81:100] - so_ts[81:100])^2)
arima_mse <- mean((so_arima_fore - so_ts[81:100])^2)

## Mean absolute percentage forecast error (MAPE)
order1_mape <- mean(abs(so_order1_filt$f[81:100] - so_ts[81:100]) / so_ts[81:100])
order2_mape <- mean(abs(so_order2_filt$f[81:100] - so_ts[81:100]) / so_ts[81:100])
order3_mape <- mean(abs(so_order3_filt$f[81:100] - so_ts[81:100]) / so_ts[81:100])
arima_mape <- mean(abs(so_arima_fore - so_ts[81:100]) / so_ts[81:100])

# Filtered & Smoothed Plots
## Retrieve filtered posterior mean of level and gradient
## at each time point, with 95% credible point intervals
so_level_filt <- ts(so_order2_filt$m[-(1:3),1], start = 1925, frequency = 1)
so_gradient_filt <- ts(so_order2_filt$m[-(1:3),2], start = 1925, frequency = 1)

so_level_smooth <- ts(so_order2_smth$s[-(1:3),1], start = 1925, frequency = 1)
so_gradient_smooth <- ts(so_order2_smth$s[-(1:3),2], start = 1925, frequency = 1)

## Calculating standard errors for filtered plots
so_cov_filt <- with(so_order2_filt, dlmSvd2var(U.C, D.C))
so_sd_level_filt <- rep(NA,length(so_cov_filt))
so_sd_gradient_filt <- rep(NA,length(so_cov_filt))
for(i in 1:length(so_cov_filt)) {
  so_sd_level_filt[i] <- sqrt(so_cov_filt[[i]][1,1])
  so_sd_gradient_filt[i] <- sqrt(so_cov_filt[[i]][2,2])
}

so_sd_level_filt <- ts(so_sd_level_filt[-(1:3)], start = 1925, frequency = 1)
so_sd_gradient_filt <- ts(so_sd_gradient_filt[-(1:3)], start = 1925, frequency = 1)

## Calculating standard errors for smooth plots
so_cov_smooth <- with(so_order2_smth, dlmSvd2var(U.S, D.S))
so_sd_level_smooth <- rep(NA,length(so_cov_smooth))
so_sd_gradient_smooth <- rep(NA,length(so_cov_smooth))
for(i in 1:length(so_cov_smooth)) {
  so_sd_level_smooth[i] <- sqrt(so_cov_smooth[[i]][1,1])
  so_sd_gradient_smooth[i] <- sqrt(so_cov_smooth[[i]][2,2])
}

so_sd_level_smooth <- ts(so_sd_level_smooth[-(1:3)], start = 1925, frequency = 1)
so_sd_gradient_smooth <- ts(so_sd_gradient_smooth[-(1:3)], start = 1925, frequency = 1)

# Plotting Strikeout time series and trend (filtered)
par(mfrow = c(2, 2), mar = c(2.5, 3.0, 0.5, 0.5))
so_level_ll <- so_level_filt - 1.96 * so_sd_level_filt
so_level_ul <- so_level_filt + 1.96 * so_sd_level_filt
plot(so_ts, type = "n", ylab = "", tck = -0.06, ylim = c(2.5, 9.5),
     yaxt = "n", xaxt = "n")
grid(lty = 1, col = "lightgrey", lwd = 1)
lines(so_ts, ylab = "", tck = -0.06, ylim = c(2.5, 9.5),
      yaxt = "n", xaxt = "n")
lines(so_level_filt, lty = "longdash", col = maroon)
lines(so_level_ll,lty = 3, col = orange)
lines(so_level_ul,lty = 3, col = orange)
axis(2, at = c(4, 6, 8, 10), las = 1, cex.axis = 1.05,
     hadj = 0, tck = -0.02)
axis(1, at = seq(1920, 2020, by = 20), las = 1, cex.axis = 1.05,
     padj = -0.75, tck = -0.02)
mtext("Strikeouts", side = 2, line = 1.5)

# Plotting gradient (filtered)
so_grad_ll <- so_gradient_filt - 1.96 * so_sd_gradient_filt
so_grad_ul <- so_gradient_filt + 1.96 * so_sd_gradient_filt
plot(so_gradient_filt, type = "n", ylab = "",  
     ylim = c(-0.25, 0.35), yaxt = "n", xaxt = "n")
grid(lty = 1, col = "lightgrey", lwd = 1)
lines(so_gradient_filt, ylab = "", ylim = c(-0.25, 0.35),
      yaxt = "n", xaxt = "n")
lines(so_grad_ll, lty = 2, col = orange)
lines(so_grad_ul, lty = 2, col = orange)
axis(2, at = seq(-0.1, 0.3, by = 0.2), las = 1, cex.axis = 1.05,
     hadj = 0.5, tck = -0.02)
axis(1, at = seq(1920, 2020, by = 20), las = 1, cex.axis = 1.05,
     padj = -0.75, tck = -0.02)
abline(h = mean(so_gradient_filt), col = maroon, lwd = 2)
mtext("Gradient", side = 2, line = 1.75)

# Plotting Strikeout time series and trend (smoothed)
so_level_ll <- so_level_smooth - 1.96 * so_sd_level_smooth
so_level_ul <- so_level_smooth + 1.96 * so_sd_level_smooth
plot(so_ts, type = "n", ylab = "", ylim = c(2.5, 9.5), yaxt = "n", xaxt = "n")
grid(lty = 1, col = "lightgrey", lwd = 1)
lines(so_ts, ylab = "", ylim = c(2.5, 9.5), yaxt = "n", xaxt = "n")
lines(so_level_smooth, lty = "longdash", col = maroon)
lines(so_level_ll,lty = 3, col = orange)
lines(so_level_ul,lty = 3, col = orange)
axis(2, at = c(4, 6, 8), las = 1, cex.axis = 1.05, tck = -0.02, hadj = 0)
axis(1, at = seq(1920, 2020, by = 20), las = 1, cex.axis = 1.05, tck = -0.02, 
     padj = -0.75)
mtext("Strikeouts", side = 2, line = 1.5)

# Plotting gradient (smoothed)
so_grad_ll <- so_gradient_smooth - 1.96 * so_sd_gradient_smooth
so_grad_ul <- so_gradient_smooth + 1.96 * so_sd_gradient_smooth
plot(so_gradient_smooth, type = "n", ylim = c(-0.25, 0.35), 
     yaxt = "n", xaxt = "n")
grid(lty = 1, col = "lightgrey", lwd = 1)
lines(so_gradient_smooth, ylab = "", ylim = c(-0.25, 0.35), yaxt = "n", 
      xaxt = "n")
lines(so_grad_ll, lty = 2, col = orange)
lines(so_grad_ul, lty = 2, col = orange)
axis(2, at = seq(-0.3, 0.3, by = 0.2), las = 1, cex.axis = 1.05,
     hadj = 0.5, tck = -0.02)
axis(1, at = seq(1920, 2020, by = 20), las = 1, cex.axis = 1.05,
     padj = -0.75, tck = -0.02)
abline(h = mean(so_gradient_smooth), col = maroon, lwd = 2)
mtext("Gradient", side = 2, line = 1.75)

####################
# FORECASTING #
#####################

so_fore <- dlmForecast(so_order2_filt, n = 20)
par(mfrow = c(1,1))
par(mar=c(2,3.5,1,0) + 0.1, cex=0.7)
so_fc_ll <- so_fore$f - 1.96 * sqrt(unlist(so_fore$Q))
so_fc_ul <- so_fore$f + 1.96 * sqrt(unlist(so_fore$Q))
plot(so_ts, xlim = c(1922, 2041), xlab = "", ylim = c(2.5, 16.5),
     ylab = "", type = 'o', col = gray, axes = FALSE)
lines(so_fore$f, lty = 1, col = maroon, lwd = 2)
lines(so_fc_ll, lty = 2, col = orange, lwd = 2)
lines(so_fc_ul, lty = 2, col = orange, lwd = 2)
lines(so_level_filt, lty = "longdash", col = maroon)
axis(1)
axis(2)
mtext("Strikeouts", side = 2, line = 2.25)

## NOTE: To make this block (and the next four) work, the object so_ts needs 
######## to be assigned to the actual time series of interest (e.g. home runs)
hr_fore <- dlmForecast(so_order2_filt, n = 20)
hr_level_filt <- so_level_filt
par(mfrow = c(1,1))
par(mar=c(2,3.5,1,0) + 0.1, cex=0.7)
hr_fc_ll <- hr_fore$f - 1.96 * sqrt(unlist(hr_fore$Q))
hr_fc_ul <- hr_fore$f + 1.96 * sqrt(unlist(hr_fore$Q))
plot(hr_ts, xlim = c(1922, 2041), xlab = "", ylim = c(0, 2.5),
     ylab = "", type = 'o', col = gray, axes = FALSE)
lines(hr_fore$f, lty = 1, col = maroon, lwd = 2)
lines(hr_fc_ll, lty = 2, col = orange, lwd = 2)
lines(hr_fc_ul, lty = 2, col = orange, lwd = 2)
lines(hr_level_filt, lty = "longdash", col = maroon)
axis(1)
axis(2)
mtext("Home Runs", side = 2, line = 2.25)

hits_fore <- dlmForecast(so_order2_filt, n = 20)
hits_level_filt <- so_level_filt
par(mfrow = c(1,1))
par(mar=c(2,3.5,1,0) + 0.1, cex=0.7)
hits_fc_ll <- hits_fore$f - 1.96 * sqrt(unlist(hits_fore$Q))
hits_fc_ul <- hits_fore$f + 1.96 * sqrt(unlist(hits_fore$Q))
plot(hit_ts, xlim = c(1922, 2041), ylim = c(6, 11), xlab = "",
     ylab = "", type = 'o', col = gray, axes = FALSE)
lines(hits_fore$f, lty = 1, col = maroon, lwd = 2)
lines(hits_fc_ll, lty = 2, col = orange, lwd = 2)
lines(hits_fc_ul, lty = 2, col = orange, lwd = 2)
lines(hits_level_filt, lty = "longdash", col = maroon)
axis(1)
axis(2)
mtext("Hits", side = 2, line = 2.25)

wp_fore <- dlmForecast(so_order2_filt, n = 20)
wp_level_filt <- so_level_filt
par(mfrow = c(1,1))
par(mar=c(2,3.5,1,0) + 0.1, cex=0.7)
wp_fc_ll <- wp_fore$f - 1.96 * sqrt(unlist(wp_fore$Q))
wp_fc_ul <- wp_fore$f + 1.96 * sqrt(unlist(wp_fore$Q))
plot(so_ts, xlim = c(1922, 2041), ylim = c(0.05, 0.65), xlab = "",
     ylab = "", type = 'o', col = gray, axes = FALSE)
lines(wp_fore$f, lty = 1, col = maroon, lwd = 2)
lines(wp_fc_ll, lty = 2, col = orange, lwd = 2)
lines(wp_fc_ul, lty = 2, col = orange, lwd = 2)
lines(wp_level_filt, lty = "longdash", col = maroon)
axis(1)
axis(2)
mtext("Wild Pitches", side = 2, line = 2.25)

hbp_fore <- dlmForecast(so_order2_filt, n = 20)
hbp_level_filt <- so_level_filt
par(mfrow = c(1,1))
par(mar=c(2,3.5,1,0) + 0.1, cex=0.7)
hbp_fc_ll <- hbp_fore$f - 1.96 * sqrt(unlist(hbp_fore$Q))
hbp_fc_ul <- hbp_fore$f + 1.96 * sqrt(unlist(hbp_fore$Q))
plot(so_ts, xlim = c(1922, 2041), ylim = c(0.1, 1.3), xlab = "",
     ylab = "", type = 'o', col = gray, axes = FALSE)
lines(hbp_fore$f, lty = 1, col = maroon, lwd = 2)
lines(hbp_fc_ll, lty = 2, col = orange, lwd = 2)
lines(hbp_fc_ul, lty = 2, col = orange, lwd = 2)
lines(hbp_level_filt, lty = "longdash", col = maroon)
axis(1)
axis(2)
mtext("Hit By Pitch", side = 2, line = 2.25)

sac_fore <- dlmForecast(so_order2_filt, n = 20)
sac_level_filt <- so_level_filt
par(mfrow = c(1,1))
par(mar=c(2,3.5,1,0) + 0.1, cex=0.7)
sac_fc_ll <- sac_fore$f - 1.96 * sqrt(unlist(sac_fore$Q))
sac_fc_ul <- sac_fore$f + 1.96 * sqrt(unlist(sac_fore$Q))
plot(so_ts, xlim = c(1971, 2041), ylim = c(0.0, 0.8), xlab = "",
     ylab = "", type = 'o', col = gray, axes = FALSE)
lines(sac_fore$f, lty = 1, col = maroon, lwd = 2)
lines(sac_fc_ll, lty = 2, col = orange, lwd = 2)
lines(sac_fc_ul, lty = 2, col = orange, lwd = 2)
lines(sac_level_filt, lty = "longdash", col = maroon)
axis(1)
axis(2)
mtext("Sacrifices", side = 2, line = 2.25)

####################
# DIAGNOSTICS #
#####################

# Get one-step ahead forecast errors
so_fit_res <- residuals(so_order2_filt, sd = TRUE)
so_res <- so_fit_res$res

# Plot one-step ahead forecast errors
plot(so_res, type = 'h'); abline(h = 0)

# Plot ACF and PACF of one-step ahead forecast errors
acf(so_res)
pacf(so_res)

# Plot qq-plot of one-step ahead forecast errors
qqnorm(so_res); qqline(so_res)

# Test normality with the Shapiro-Wilk normality test
# H_0: errors are normally distributed
# H_A: errors are not normally distributed
shapiro.test(so_res)

# Test autocorrelation with the Ljung-Box test
# H_0: errors are independent
# H_A: errors exhibit serial correlation
Box.test(so_res, lag=20, type="Ljung")   
