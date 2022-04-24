library(data.table)
library(ggplot2)
library(xtable)

# ~ 50000 architecture evals in total
qdo = setDT(read.csv("results/qdo.csv"))
colnames(qdo) = as.character(1:100)
evo = setDT(read.csv("results/evo.csv"))
colnames(evo) = as.character(1:100)

qdo[, niche := factor(paste0("niche", 1:7), levels = paste0("niche", 1:7))]
evo[, niche := factor(paste0("niche", 1:7), levels = paste0("niche", 1:7))]

dat_qdo = melt(qdo, id = 101)
dat_qdo[, method := "qdo"]

dat_evo = melt(evo, id = 101)
dat_evo[, method := "evo"]

dat = rbind(dat_qdo, dat_evo)
dat[, val_loss := 100 - 100 * value]
dat$niche = factor(dat$niche, labels = paste0("[0, ", c(15, 18, 21, 24, 27, 30, 33), ")"))
dat$method = factor(dat$method, labels = c("OFA + reg. evo.", "OFA + MAP-Elites"))
dat[, budget := "Large"]

# ~ 5000 architecture evals in total
qdo_small = setDT(read.csv("results/qdo_small.csv"))
colnames(qdo_small) = as.character(1:100)
evo_small = setDT(read.csv("results/evo_small.csv"))
colnames(evo_small) = as.character(1:100)

qdo_small[, niche := factor(paste0("niche", 1:7), levels = paste0("niche", 1:7))]
evo_small[, niche := factor(paste0("niche", 1:7), levels = paste0("niche", 1:7))]

dat_qdo_small = melt(qdo_small, id = 101)
dat_qdo_small[, method := "qdo"]

dat_evo_small = melt(evo_small, id = 101)
dat_evo_small[, method := "evo"]

dat_small = rbind(dat_qdo_small, dat_evo_small)
dat_small[, val_loss := 100 - 100 * value]
dat_small$niche = factor(dat_small$niche, labels = paste0("[0, ", c(15, 18, 21, 24, 27, 30, 33), ")"))
dat_small$method = factor(dat_small$method, labels = c("OFA + reg. evo.", "OFA + MAP-Elites"))
dat_small[, budget := "Small"]

dat = rbind(dat, dat_small)
dat_agg = dat[, .(mean = mean(val_loss), se = sd(val_loss) / sqrt(.N)), by = .(niche, method, budget)]

g = ggplot(dat, aes(x = niche, y = val_loss, colour = method)) +
  geom_boxplot() +
  xlab("Niche (Latency in ms)") +
  ylab("Validaton Error") +
  labs(colour = "Method") +
  facet_wrap(~ budget) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("plots/qdo_evo.png", plot = g, width = 8, height = 4)

