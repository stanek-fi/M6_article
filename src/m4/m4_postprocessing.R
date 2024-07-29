rm(list = ls())
library(yaml)
library(data.table)
library(stringr)
library(ggplot2)
library(latex2exp)
library(scales)

selected_periods <- c("Yearly", "Quarterly", "Monthly", "Weekly")
path <- file.path("models", "m4")
results <- lapply(selected_periods, function(selected_period) {
    list(
        models = readRDS(file.path(path, paste0(selected_period, ".rds"))),
        period = selected_period
    )
})

# -- figures -------------------------------------------------------------------

for(i in seq_along(selected_periods)){
    temp <- results[[i]][["models"]]
    d <- as.data.table(temp[[length(temp)]][["fit"]][["meta_module.meta_weight"]])
    d[,lag:=as.factor(1:.N)]
    d <- melt(d, id.vars = "lag")
    d[, variable := factor(variable, levels=c("V1", "V2"), labels = c(TeX("${\\omega^{w}}[:,1]$"),TeX("${\\omega^{w}}[:,2]$")))]

    p <- ggplot(d, aes(x = lag, y =value, fill = variable, colour = variable))+
        geom_bar(stat = "identity", position = position_dodge(width = 0.4), width = 0.4, alpha=0.5, linewidth = 0) +
        geom_point(position = position_dodge(width = 0.4), size=2)+
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
            panel.grid.minor = element_blank(), legend.position="bottom", legend.title = element_blank())+
        ylab(NULL)+
        scale_fill_discrete(guide="none")+
        guides(color=guide_legend(override.aes=list(fill=c("#F8766D","#00BFC4"))))+
        scale_colour_discrete(labels = parse_format())
    
    ggsave(file.path("outputs", "m4", paste0("effects_", selected_periods[i], ".png")), p,height = 7, width = 5)
}

# -- losses_table --------------------------------------------------------------

losses_table <- do.call(rbind,lapply(results, function(obj) {
    temp <- t(t(sapply(obj$models, function(x) ifelse(is.null(x$validation), NA, x$validation))))
    data.table(
        model = factor(rownames(temp), levels = rownames(temp)),
        loss = as.numeric(temp),
        period = obj$period
    )
}))
losses_table <- dcast(losses_table, model ~ period, value.var = "loss")
names_map <- data.table(
    old = losses_table$model,
    new = c(
        "ETS",
        "auto.arima",
        "OLS (pooled)",
        "OLS (2 clusters)",
        "OLS (4 clusters)",
        "OLS (8 clusters)",
        "OLS (16 clusters)",
        "OLS (32 clusters)",
        "OLS (64 clusters)",
        "OLS (128 clusters)",
        "OLS (256 clusters)",
        "OLS (512 clusters)",
        "OLS (1024 clusters)",
        "OLS (localized via MtMs)"
    )
)
losses_table <- merge(losses_table, names_map, by.x="model", by.y = "old")
losses_table <- losses_table[,.SD, .SDcols=c("new", selected_periods)]

dir.create(file.path("outputs", "m4"), showWarnings = FALSE)
write.table(losses_table, file.path("outputs", "m4", "losses_table.csv"), row.names = FALSE, dec=".", sep = ";", na = "")

