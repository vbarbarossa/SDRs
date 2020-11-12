#' multipanel

#+ fig.width=10, fig.height=16
ggplot(d1, aes(x = Q_MEAN, y = SR_tot, colour = BAS)) +
  geom_point(alpha = 0.5) +
  facet_wrap('BAS',ncol = 5) +
  theme_bw() +
  geom_line(data = cbind(d1, pred = predict(fit1)), aes(y = pred), size = 1, alpha = 0.3) +  # adding predicted line from mixed model
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.spacing = unit(2, "lines"))  # adding space between panels

