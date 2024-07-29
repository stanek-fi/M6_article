library(data.table)
library(ggplot2)
library(stringr)
library(patchwork)

rf <- function(x, p) {
    formatC(round(x, p), format = "f", digits = p)
}

leaderboard <- do.call(rbind, lapply(1:12, function(i) {
    temp <- as.data.table(
        read.csv(
            file.path("data", "M6", "leaderboard", paste0(i, ".csv")),
            header = FALSE,
            sep = ";",
            col.names = c("rank", "name", "rank_avg", "rps", "rank_rps", "ir", "rank_ir")
        )
    )
    temp[, id := word(name)]
    temp[, month := i]
    temp
}))
leaderboard[, eligible := .N == 12, id]


prices <- as.data.table(read.csv(file.path("data", "M6", "prices", "assets_M6.csv")))
prices[, date := as.Date(date, "%Y/%m/%d")]
intervals <- list(
    start = seq(as.Date("2022-03-07"), by = 28, length.out = 12),
    end = seq(as.Date("2022-04-03"), by = 28, length.out = 12)
)
returns <- do.call(rbind, lapply(seq_along(intervals$start), function(i) {
    temp <- prices[date >= intervals$start[i] & date <= intervals$end[i], ]
    temp <- temp[, .(return = last(price) / first(price), month = i), symbol]
    temp
}))

positions <- do.call(rbind, lapply(1:12, function(month) {
    d <- as.data.table(read.csv(file.path("data", "M6", "submissions", paste0(month, ".csv")), sep = ";"))
    d[, month := month]
    d
}))

d <- merge(positions, returns, by.x = c("ID", "month"), by.y = c("symbol", "month"), all.x = TRUE)
d[is.na(return), return := 1]
d[, decision := as.factor(Decision)]
d[, month := as.factor(month)]

leaderboard_subset <- rbind(
    leaderboard[id == "cd597d34", .(month, portfolio = "ours", ir)],
    leaderboard[id == "32cdcc24", .(month, portfolio = "M6 dummy", ir)],
    leaderboard[, .(portfolio = "average", ir = mean(ir)), month]
)
leaderboard_subset[, month := as.factor(month)]

d[, return := pmax(pmin(return, 1.2), 0.8)]
plot_returns <- ggplot(d, aes(x = month, y = ID, colour = decision, fill = return)) +
    geom_tile(linewidth = 0.5, width = 0.7, height = 0.7) +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 1, limits = c(0.8, 1.2)) +
    # scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 1) +
    scale_colour_manual(values = c("black", "gray")) +
    guides(colour = guide_legend(override.aes = list(alpha = 0))) +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
    scale_y_discrete(limits = rev) +
    ylab("asset")
# plot_returns

plot_ir <- ggplot(leaderboard_subset, aes(x = month, y = portfolio, fill = ir)) +
    geom_tile(linewidth = 0.5, width = 0.7, height = 0.7) +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, limits = c(-8, 8)) +
    geom_text(aes(label = rf(ir, 2)), angle = 90, size = 2.5)
# plot_ir

p <- plot_returns + plot_ir + plot_layout(ncol = 1, heights = c(8, 1))

ggsave(file.path("outputs", "M6", "portfolio_weights.png"), p,height = 15, width = 7)
