par(mfrow=c(2,2))

inflation_parameters <- parameter_estimator(Inflation,1)
pred_q <- inflation_process(Inflation[1],inflation_parameters[1], 
                            mean(Inflation),inflation_parameters[3],1,70)
plot(Inflation~data$Year,col="blue", type="l", 
     main="Real vs Fitted Inflation", xlab="Year")
lines(pred_q~data$Year,col="red")

Short_rate_parameters <- parameter_estimator(Short_rate,1)
pred_r <- Rate_process(Short_rate[1],Short_rate_parameters[1], 
                       mean(Short_rate),Short_rate_parameters[3],
                       0.494401826690166,1,70)
pred_l <- 0.0330282849263015 + 0.70155531854269*pred_r
plot(Long_rate~data$Year,col="blue", type="l", 
     main="Real vs Fitted Long-term Interest Rates", xlab="Year", 
     ylab="Interes Rate")
lines(pred_l~data$Year,col="red")

dividend_parameters <- parameter_estimator(Dividend_yield,1)
pred_d <- Dividend_process(Dividend_yield[1],dividend_parameters[1], 
                           mean(Dividend_yield),dividend_parameters[3], 
                           0.27700643,1,70)
plot(Dividend_yield~data$Year,col="blue", type="l", 
     main="Real vs Fitted Dividend Yields", xlab="Year", ylab="Dividend Yield")
lines(pred_d~data$Year,col="red")

pred_y <- Equity_process(Equity_yield[1],1,70)
plot(Equity_yield~data$Year,col="blue", type="l", 
     main="Real vs Fitted Equity Yields", xlab="Year", ylab="Equity Yield")
lines(pred_y~data$Year,col="red")

################################################################################

par(mfrow=c(2,2))

plot(Inflation[64:71]~data$Year[64:71],col="blue", type="l", 
     main="Real vs Fitted Inflation", xlab="Year", ylab="Inflation", 
     ylim=c(0,0.06))
lines(pred_q~data$Year[64:71],col="red")

plot(Long_rate[64:71]~data$Year[64:71],col="blue", type="l", 
     main="Real vs Fitted Long-term Interest Rates", xlab="Year", 
     ylab="Interes Rate", ylim=c(0.03,0.07))
lines(pred_l~data$Year[64:71],col="red")

plot(Dividend_yield[64:71]~data$Year[64:71],col="blue", type="l", 
     main="Real vs Fitted Dividend Yields", xlab="Year", ylab="Dividend Yield", 
     ylim=c(0.018,0.045))
lines(pred_d~data$Year[64:71],col="red")

plot(Equity_yield[64:71]~data$Year[64:71],col="blue", type="l", 
     main="Real vs Fitted Equity Yields", xlab="Year", ylab="Equity Yield")
lines(pred_y~data$Year[64:71],col="red")

################################################################################

par(mfrow=c(2,2))

plot(Inflation~data$Year,col="blue", type="l", 
     main="95% CI for Inflation forecast", xlab="Year", ylab="Inflation", 
     ylim=c(-0.05,0.17))
lines(pred_q$q_l~data$Year[64:71],col="red", lty=3, lwd=2)
lines(pred_q$q_u~data$Year[64:71],col="red", lty=3, lwd=2)

plot(Long_rate~data$Year,col="blue", type="l", 
     main="95% CI for Long-term Interest rate forecast", xlab="Year", 
     ylab="Inflation", ylim=c(-0.02,0.16))
lines(pred_l$r_l~data$Year[64:71],col="red", lty=3, lwd=2)
lines(pred_l$r_u~data$Year[64:71],col="red", lty=3, lwd=2)

plot(Dividend_yield~data$Year,main="95% CI Dividend Yields Forecast", 
     type="l", col="blue", xlab="Year", ylab="Dividend Yield", 
     ylim=c(-0.03,0.11))
lines(pred_d$d_l~data$Year[64:71],col="red", lty=3,lwd=2)
lines(pred_d$d_u~data$Year[64:71],col="red", lty=3,lwd=2)

plot(Equity_yield~data$Year,col="blue", type="l", 
     main="95% CI for Equity Yield forecast", xlab="Year", ylab="Equity Yield", 
     ylim=c(-0.5,0.5))
lines(pred_y$y_l~data$Year[64:71],col="red", lty=3, lwd=2)
lines(pred_y$y_u~data$Year[64:71],col="red", lty=3, lwd=2)