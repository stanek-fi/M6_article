library(data.table)
library(ggplot2)
library(ggpmisc)
library(latex2exp)
library(patchwork)

d <- do.call(rbind, lapply(1:12, function(month) {
    d <- as.data.table(read.csv(file.path("data", "M6", "submissions", paste0(month, ".csv")), sep = ";"))
    d[, month := month]
    d
}))
d[,submission:=as.factor(month)]

d[,t:=submission]
p15 <- ggplot(d, aes(x=Rank1, y=Rank5))+
    geom_point(aes(colour=t), alpha=0.3)+
    stat_poly_line(formula = y ~ x, linewidth=0.5, colour="black") +
    stat_poly_eq(use_label(c("eq", "R2")),formula = y ~ x, size=4)+
    xlab(TeX("$\\hat{P}(quintile_{t}^{(m)}=1)$"))+
    ylab(TeX("$\\hat{P}(quintile_{t}^{(m)}=5)$"))+
    coord_fixed(ratio = 1)+
    theme(legend.position="none")

p24 <- ggplot(d, aes(x=Rank2, y=Rank4))+
    geom_point(aes(colour=t), alpha=0.3)+
    stat_poly_line(formula = y ~ x, linewidth=0.5, colour="black") +
    stat_poly_eq(use_label(c("eq", "R2")),formula = y ~ x, size=4)+
    xlab(TeX("$\\hat{P}(quintile_{t}^{(m)}=2)$"))+
    ylab(TeX("$\\hat{P}(quintile_{t}^{(m)}=4)$"))+
    coord_fixed(ratio = 1)+
    theme(legend.position="bottom")

p1245 <- p15/p24

ggsave(file.path("outputs", "M6" , "quintile_predictions.png"), plot=p1245, width=6, height=13)
