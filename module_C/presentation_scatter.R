# Run from the repository root to regenerate the image embedded on PPT slide 4.
library(data.table)
library(ggplot2)

tracts <- fread("data/tracts.csv")
tracts <- tracts[is.finite(station_m) & is.finite(nypd_rate)]
tracts[, `:=`(distance_km = station_m / 1000,
              report_log = log1p(nypd_rate))]

fig <- ggplot() +
  geom_point(data = tracts[borough != "Bronx"],
             aes(distance_km, report_log), color = "#BCCBC7",
             alpha = 0.7, size = 0.8) +
  geom_point(data = tracts[borough == "Bronx"],
             aes(distance_km, report_log), color = "#9A5A3B",
             alpha = 0.85, size = 1.05) +
  scale_x_continuous(limits = c(0, 9), breaks = seq(0, 9, 2),
                     expand = expansion(mult = 0)) +
  scale_y_continuous(limits = c(0, 9), breaks = seq(0, 9, 2),
                     expand = expansion(mult = 0)) +
  labs(title = "Rail distance vs report rate",
       x = "Nearest station (km)",
       y = "log(1 + reports / 1,000)") +
  theme_minimal(base_family = "Arial", base_size = 11) +
  theme(
    plot.background = element_rect(fill = "#F8F7F1", color = NA),
    panel.background = element_rect(fill = "#F8F7F1", color = NA),
    panel.grid.major = element_line(color = "#D9E4E1", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 15, color = "#173743", margin = margin(b = 12)),
    axis.title = element_text(size = 10, color = "#173743"),
    axis.text = element_text(size = 9, color = "#4B626A"),
    plot.margin = margin(12, 18, 8, 8)
  )

ggsave("module_C/c3_scatter_points_only.png", fig, width = 6.41, height = 4.27,
       dpi = 240, bg = "#F8F7F1")
