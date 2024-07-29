rm(list = ls())
library(data.table)
library(MASS)
library(ggplot2)
library(latex2exp)

source(file.path("src", "M6", "helpers", "position_baseline.R"))
source(file.path("src", "M6" ,"helpers", "compute_round.R"))


prices <- as.data.table(read.csv(file.path("data", "M6", "prices", "assets_M6.csv")))
prices[, date := as.Date(date, "%Y/%m/%d")]
prices <- dcast(prices, date ~ symbol, value.var = "price")
returns <- prices[, .(date, .SD / shift(.SD) - 1), .SDcols = colnames(prices)[-1]]

intervals <- list(
    start = seq(as.Date("2022-03-07"), by = 28, length.out = 12),
    end = seq(as.Date("2022-04-03"), by = 28, length.out = 12)
)
returns_list <- lapply(seq_along(intervals$start), function(i) {
    temp <- returns[date >= intervals$start[i] & date <= intervals$end[i], .SD, .SDcols = colnames(returns)[-1]]
    temp <- as.matrix(temp)
    temp[is.na(temp)] <- 0
    temp
})

num_shorted <- 0
num_zeros <- 0
scalings <- seq(0, 1, by = 0.001)[-1]

# -- computing effects ---------------------------------------------------------

res <- do.call(rbind, lapply(1:12, function(m) {
    returns <- returns_list[[m]]
    positions <- position_baseline(ncol(returns), 1, num_shorted = num_shorted, num_zeros = num_zeros)
    positions_scaled <- as.matrix(scalings) %*% positions
    data.table(
        month = m,
        scale = scalings,
        IR = compute_round(returns, positions_scaled),
        admissible = (scalings >= 0.25) & (scalings <= 1)
    )
}))
res[, month := as.factor(month)]
print(res[scale==0.25,mean(IR)] - res[scale==1,mean(IR)])

# -- plotting effects ----------------------------------------------------------

res[, IR := IR - .SD[scale == 1, IR], month]
optimal_scaling <- ggplot(res, aes(x = scale, y = IR, colour = month, linetype = admissible)) +
    geom_line() +
    scale_linetype_manual(values = c("dashed", "solid")) +
    ylab(TeX("($IR_{t}|\\alpha_{t}=alpha) - (IR_{t}|\\alpha_{t}=1)$")) +
    xlab(TeX("$\\alpha$")) +
    labs(color = "t")
dir.create(file.path("outputs", "plots"), showWarnings = FALSE)
ggsave(file.path("outputs", "M6", "scaling_effects.png"), optimal_scaling, width = 6, height = 5)