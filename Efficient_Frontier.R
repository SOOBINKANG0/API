source("source.R")
rm(list=ls())

#### data preparing
getSymbols(c("AMZN", "XOM", "TSLA", "NVDA"), from = "2010-01-01", to = "2019-12-31")
data_set = cbind(AMZN[,6], XOM[,6], TSLA[,6], NVDA[,6])

start_date = as.Date("2010-06-29"); end_date = as.Date("2019-12-30")
data_set = data_set[paste0(start_date, "/", end_date)]

rm(list=ls()[!ls() %in% "data_set"])
data_set = ROC(data_set, 1, "discrete"); data_set = data_set[-1,]

data_ret = apply(data_set, 2, mean)*252
data_cov = cov(data_set)*252
data_sd = apply(data_set, 2, sd)*sqrt(252)
one_vec = c(1,1,1,1)

# formula for Tangent portfolio
W_star_TP = solve(data_cov) %*% data_ret / as.numeric(data_ret %*% solve(data_cov) %*% one_vec)
tangent_ret = t(data_ret) %*% W_star_TP 
tanget_vol = sqrt(t(W_star_TP) %*% data_cov %*% W_star_TP)

## formula for Minimum variance portfolio
W_star_MVP = (solve(data_cov) %*% one_vec) / as.numeric(t(one_vec) %*% solve(data_cov) %*% one_vec)
MVP_ret = t(data_ret) %*% W_star_MVP 
MVP_vol = sqrt(t(W_star_MVP) %*% data_cov %*% W_star_MVP)

# two fund mixing to make efficient frontier
grid = seq(-10, 10, 0.01)

port_grid =  W_star_TP %*% grid + W_star_MVP %*% (1-grid)

efficient_ret = t(port_grid) %*% data_ret
efficient_vol <- sqrt(diag(t(port_grid) %*% data_cov %*% port_grid))
efficient_vol = matrix(efficient_vol, nrow = length(efficient_vol), ncol = 1)

# ##
# port_grid <- t(sapply(grid, function(g) W_star_TP * g + W_star_MVP * (1 - g)))
# 
# # 효율적 프론티어 계산
# efficient_ret <- port_grid %*% data_ret
# efficient_vol <- apply(port_grid, 1, function(w) sqrt(t(w) %*% data_cov %*% w))


# plot efficient frontier
plot(x = efficient_vol,  y = efficient_ret, lty = 2, type = "l")

##simulate random portfolio
for(i in 1:10000){
    simul = runif(4, min = -1, max = 1)
    simul = simul / sum(simul)
    
    simul_ret = t(simul) %*% data_ret
    simul_vol = sqrt(t(simul) %*% data_cov %*% simul)
    
    points(simul_vol, simul_ret, lty = 1, col = 'red')
    
}

points(x = sqrt(diag(data_cov)), y = data_ret, col = 'blue', lwd = 3, pch = 10)
