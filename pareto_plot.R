library(data.table)  # 1.14.2
library(ggplot2)  # 3.3.5
library(ggimage) # 0.3.0

dat = readRDS("results/pareto_plot.rds")
extended = rbind(dat, data.table(val_err = c(85, 81, 70, 55, 31.5, 19, 12), params = c(680000, 840000, 900000, 950000, 1100000, 2300000, 2400000)))
setorderv(extended, "val_err", order = -1)
extended = extended[-c(6, 10:15, 18)]
best = rbind(extended[params < 720000][min(val_err) == val_err], extended[params < 1452460][min(val_err) == val_err], extended[min(val_err) == val_err])
#lines = data.table(x = c(min(extended)$params, c(720000, 1452460)), xend = c(720000, 1452460, max(extended$params) * 1.05), y = best$val_err, y_end = best$val_err)
images = data.table(params = c(530000, 1090000, 1950000), val_err = c(50, 50, 50), image = c("plots/cell-phone.png", "plots/laptop.png", "plots/workstation.png"), size = c(0.1, 0.3, 0.3))

g = ggplot(aes(x = params, y = val_err), data = extended) +
  geom_image(aes(x = params, y = val_err, image = image, size = I(size)), data = images, color = "darkblue", alpha = 0.3, asp = 2) +
  geom_point(size = 2, alpha = 1) +
  geom_step(direction = "hv", alpha = 0.7, linetype = "dashed") +
  geom_vline(xintercept = c(720000, 1452460), linetype = "dashed", lwd = 2, color = "darkgrey", alpha = 0.5) +
  #geom_segment(aes(x = x, xend = xend, y = y, yend = y_end), data = lines, color = "darkorange", linetype = "dashed", lwd = 2, alpha = 0.5) +
  geom_point(size = 4, color = "forestgreen", data = best) +
  xlab("Resource Usage") +
  ylab("Validation Error") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
ggsave("plots/pareto_plot.pdf", plot = g, device = "pdf", width = 6, height = 3)

