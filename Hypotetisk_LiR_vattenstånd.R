library(readxl)
# Packages
library(tidyverse)

# Data
water_levels <- tribble(
  ~Date,       ~`Regulated lake`, ~`Run-of-river impoundment`, ~`Natural lake`, ~LiR,
  "January",   97.5,              99.7,                        99.0,            100.0,
  "February",  95.5,              99.6,                        98.5,            100.0,
  "March",     92.0,              99.4,                        97.5,            100.0,
  "April",     88.5,              99.2,                        96.5,            100.0,
  "May",       85.0,              99.0,                        96.0,            100.0,
  "June",      90.0,              99.3,                        97.0,            100.0,
  "July",      96.0,              99.7,                        98.5,            100.0,
  "August",    100.5,             100.0,                       99.8,            100.4,
  "September", 101.0,             99.9,                        100.0,           101.0,
  "October",   100.5,             99.8,                        99.6,            100.5,
  "November",  100.0,             99.7,                        99.3,            100.0,
  "December",  99.0,              99.7,                        99.0,            100.0
)

# Put months in the correct order
water_levels$Date <- factor(
  water_levels$Date,
  levels = c("January","February","March","April","May","June",
             "July","August","September","October","November","December")
)

# Convert to long format for ggplot
water_long <- water_levels %>%
  pivot_longer(
    cols = -Date,
    names_to = "Type",
    values_to = "WaterLevel"
  )

# Graph
ggplot(water_long, aes(x = Date, y = WaterLevel, group = Type, linetype = Type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 100, linewidth = 0.5) +
  scale_y_continuous(limits = c(84, 103), breaks = seq(84, 103, by = 2)) +
  labs(
    title = "Water level response under different lake management regimes",
    subtitle = "100 = reference level",
    x = "Month",
    y = "Relative water level",
    linetype = "Regime"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

p <- ggplot(water_long, aes(x = Date, y = WaterLevel, group = Type,
                            linetype = Type, shape = Type)) +
  geom_hline(yintercept = 100, colour = "grey40", linewidth = 0.4) +
  geom_line(colour = "black", linewidth = 0.7) +
  geom_point(colour = "black", size = 2.2, stroke = 0.7, fill = "white") +
  scale_y_continuous(
    limits = c(84, 103),
    breaks = seq(84, 103, by = 2),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_linetype_manual(
    values = c("solid", "longdash", "dotted", "dotdash")
  ) +
  scale_shape_manual(
    values = c(16, 17, 15, 1)
  ) +
  labs(
    x = NULL,
    y = "Relative water level",
    linetype = NULL,
    shape = NULL
  ) +
  theme_classic(base_size = 11) +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.title.y = element_text(margin = margin(r = 8)),
    panel.border = element_blank(),
    axis.line = element_line(linewidth = 0.5, colour = "black"),
    axis.ticks = element_line(linewidth = 0.5, colour = "black"),
    axis.ticks.length = unit(0.18, "cm"),
    legend.key.width = unit(1.2, "cm"),
    legend.spacing.x = unit(0.4, "cm")
  )

p <- ggplot(water_long, aes(x = Date, y = WaterLevel, group = Type,
                            linetype = Type, shape = Type)) +
  geom_hline(yintercept = 100, colour = "grey40", linewidth = 0.4) +
  geom_line(colour = "black", linewidth = 0.7) +
  geom_point(colour = "black", size = 2.2, stroke = 0.7, fill = "white") +
  scale_y_continuous(
    limits = c(84, 103),
    breaks = seq(84, 103, by = 2),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_linetype_manual(
    values = c("solid", "longdash", "dotted", "dotdash")
  ) +
  scale_shape_manual(
    values = c(16, 17, 15, 1)
  ) +
  labs(
    x = NULL,
    y = "Relative water level",
    linetype = NULL,
    shape = NULL
  ) +
  theme_classic(base_size = 11) +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.title.y = element_text(margin = margin(r = 8)),
    panel.border = element_blank(),
    axis.line = element_line(linewidth = 0.5, colour = "black"),
    axis.ticks = element_line(linewidth = 0.5, colour = "black"),
    axis.ticks.length = unit(0.18, "cm"),
    legend.key.width = unit(1.2, "cm"),
    legend.spacing.x = unit(0.4, "cm")
  )
p

#Version 3
# Define a colorblind-friendly palette (Okabe-Ito)
colors <- c(
  "Regulated lake" = "#0072B2",              # blue
  "Run-of-river impoundment" = "#009E73",    # green
  "Natural lake" = "#D55E00",                # reddish-orange
  "LiR" = "#CC79A7"                          # purple
)

# Plot
p <- ggplot(water_long, aes(x = Date, y = WaterLevel, group = Type, color = Type)) +
  geom_hline(yintercept = 100, colour = "grey50", linewidth = 0.4) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  scale_color_manual(values = colors) +
  scale_y_continuous(
    limits = c(84, 103),
    breaks = seq(84, 103, by = 2),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  labs(
    x = NULL,
    y = "Relative water level",
    color = "Regime"
  ) +
  theme_classic(base_size = 11) +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 8)),
    axis.line = element_line(linewidth = 0.5),
    axis.ticks = element_line(linewidth = 0.5),
    axis.ticks.length = unit(0.18, "cm"),
    legend.key.width = unit(1.2, "cm")
  )
p

#Version 4
library(tidyverse)

# Emphasis styling
my_cols <- c(
  "Regulated lake" = "#B0B0B0",              # light grey
  "Run-of-river impoundment" = "#9E9E9E",    # medium grey
  "Natural lake" = "#7F8C8D",                # darker grey-blue
  "LiR" = "#D55E00"                         # strong highlight (orange)
)

my_linetypes <- c(
  "Regulated lake" = "44",    # long dashes
  "Run-of-river impoundment" = "1343",  # mixed dash-gap pattern
  "Natural lake" = "1111",    # dotted
  "LiR" = "solid"
)

my_sizes <- c(
  "Regulated lake" = 0.7,
  "Run-of-river impoundment" = 0.7,
  "Natural lake" = 0.7,
  "LiR" = 1.2
)

p <- ggplot(
  water_long,
  aes(x = Date, y = WaterLevel,
      color = Type, linetype = Type, linewidth = Type, group = Type)
) +
  geom_line() +
  scale_color_manual(values = my_cols) +
  scale_linetype_manual(values = my_linetypes) +
  scale_linewidth_manual(values = my_sizes) +
  scale_y_continuous(
    limits = c(84, 103),
    breaks = seq(84, 103, by = 2),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  labs(
    x = NULL,
    y = "Relative water level",
    color = NULL,
    linetype = NULL,
    linewidth = NULL
  ) +
  theme_classic(base_size = 11) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 8)),
    axis.line = element_line(linewidth = 0.5),
    axis.ticks = element_line(linewidth = 0.5)
  )

p


#Weeks
library(tidyverse)

water_levels <- tribble(
  ~Week, ~`Storage reservoir`, ~`Run-of-river impoundment`, ~`Natural lake`, ~LiR,
  1,    97.5,              99.7,                        99.0,            100.0,
  2,    97.3,              99.7,                        98.9,            100.0,
  3,    97.0,              99.7,                        98.9,            100.0,
  4,    96.7,              99.7,                        98.8,            100.0,
  5,    96.3,              99.6,                        98.7,            100.0,
  6,    95.9,              99.6,                        98.6,            100.0,
  7,    95.4,              99.6,                        98.5,            100.0,
  8,    94.8,              99.5,                        98.3,            100.0,
  9,    94.1,              99.5,                        98.2,            100.0,
  10,    93.4,              99.5,                        98.0,            100.0,
  11,    92.7,              99.4,                        97.8,            100.0,
  12,    92.0,              99.4,                        97.5,            100.0,
  13,    91.2,              99.4,                        97.3,            100.0,
  14,    90.4,              99.4,                        97.1,            100.0,
  15,    89.6,              99.3,                        96.9,            100.0,
  16,    88.8,              99.3,                        96.7,            100.0,
  17,    88.0,              99.2,                        96.5,            100.0,
  18,    87.2,              99.2,                        96.4,            100.0,
  19,    86.5,              99.1,                        96.3,            100.0,
  20,    85.8,              99.1,                        96.2,            100.0,
  21,    85.3,              99.0,                        96.1,            100.0,
  22,    85.0,              99.0,                        96.0,            100.0,
  23,    85.5,              99.1,                        96.1,            100.0,
  24,    86.2,              99.1,                        96.2,            100.0,
  25,    87.1,              99.2,                        96.4,            100.0,
  26,    88.2,              99.2,                        96.6,            100.0,
  27,    89.5,              99.3,                        96.9,            100.0,
  28,    91.0,              99.4,                        97.2,            100.0,
  29,    92.7,              99.5,                        97.6,            100.0,
  30,    94.4,              99.6,                        98.0,            100.0,
  31,    96.2,              99.7,                        98.4,            100.0,
  32,    97.9,              99.8,                        98.8,            100.0,
  33,    99.3,              99.9,                        99.1,            100.0,
  34,   100.2,             100.0,                        99.4,            100.2,
  35,   100.6,             100.0,                        99.6,            100.6,
  36,   100.8,             100.0,                        99.8,            100.8,
  37,   101.0,              99.9,                        99.9,            101.0,
  38,   101.0,              99.9,                       100.0,            101.0,
  39,   101.0,              99.9,                       100.0,            101.0,
  40,   100.9,              99.8,                       100.0,            100.9,
  41,   100.8,              99.8,                        99.9,            100.8,
  42,   100.7,              99.8,                        99.8,            100.7,
  43,   100.6,              99.8,                        99.7,            100.6,
  44,   100.5,              99.8,                        99.6,            100.5,
  45,   100.4,              99.7,                        99.5,            100.4,
  46,   100.2,              99.7,                        99.4,            100.2,
  47,   100.0,              99.7,                        99.3,            100.0,
  48,    99.8,              99.7,                        99.2,            100.0,
  49,    99.6,              99.7,                        99.1,            100.0,
  50,    99.4,              99.7,                        99.1,            100.0,
  51,    99.2,              99.7,                        99.0,            100.0,
  52,    99.0,              99.7,                        99.0,            100.0
)

water_long <- water_levels %>%
  pivot_longer(
    cols = -Week,
    names_to = "Type",
    values_to = "WaterLevel"
  )

my_cols <- c(
  "Regulated lake" = "#4C6272",             # dark blue-grey
  "Run-of-river impoundment" = "#7A8F9B",   # medium blue-grey
  "Natural lake" = "#4C8C4A",               # green (distinct group)
  "LiR" = "#FF8C00"                         # clearer blue (but not flashy)
)

my_linetypes <- c(
  "Regulated lake" = "dashed",
  "Run-of-river impoundment" = "dotted",
  "Natural lake" = "dotdash",
  "LiR" = "solid"
)

my_shapes <- c(
  "Regulated lake" = NA,
  "Run-of-river impoundment" = NA,
  "Natural lake" = NA,
  "LiR" = NA
)

my_sizes <- c(
  "Regulated lake" = 0.9,
  "Run-of-river impoundment" = 0.9,
  "Natural lake" = 1.0,
  "LiR" = 1.2
)

ggplot(
  water_long,
  aes(x = Week, y = WaterLevel,
      color = Type, linetype = Type, shape = Type,
      linewidth = Type, group = Type)
) +
  geom_line() +
  geom_point(size = 1.8, stroke = 0.2, na.rm = TRUE) +
  scale_color_manual(values = my_cols) +
  scale_linetype_manual(values = my_linetypes) +
  scale_shape_manual(values = my_shapes) +
  scale_linewidth_manual(values = my_sizes) +
  scale_x_continuous(breaks = seq(1, 52, by = 4)) +
  scale_y_continuous(
    limits = c(84, 103),
    breaks = seq(84, 103, by = 2),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  labs(
    x = "Week",
    y = "Relative water level",
    color = NULL,
    linetype = NULL,
    shape = NULL,
    linewidth = NULL
  ) +
  theme_classic(base_size = 11) +
  theme(
    legend.position = "top",
    axis.title.y = element_text(margin = margin(r = 8)),
    axis.line = element_line(linewidth = 0.5),
    axis.ticks = element_line(linewidth = 0.5)
  )

########Alternatives#####
water_long <- water_levels %>%
  pivot_longer(
    cols = -Week,
    names_to = "Type",
    values_to = "WaterLevel"
  )

my_cols <- c(
  "Storage reservoir"           = "#4A4A4A",
  "Run-of-river impoundment" = "#7A7A7A",
  "Natural lake"             = "#A0A0A0",
  "LiR"                      = "#C27D38"   # warm accent
)

my_linetypes <- c(
  "Storage reservoir" = "dashed",
  "Run-of-river impoundment" = "dotted",
  "Natural lake" = "dotdash",
  "LiR" = "solid"
)

my_sizes <- c(
  "Storage reservoir" = 0.9,
  "Run-of-river impoundment" = 0.9,
  "Natural lake" = 1.0,
  "LiR" = 1.2
)

p <- ggplot(
  water_long,
  aes(x = Week, y = WaterLevel,
      color = Type, linetype = Type, linewidth = Type)
) +
  geom_line() +
  scale_color_manual(values = my_cols) +
  scale_linetype_manual(values = my_linetypes) +
  scale_linewidth_manual(values = my_sizes) +
  scale_x_continuous(breaks = seq(1, 52, by = 4)) +
  scale_y_continuous(
    limits = c(84, 103),
    breaks = seq(84, 103, by = 2),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  labs(
    x = "Week",
    y = "Relative water level (100 = reference level)",
    color = NULL,
    linetype = NULL,
    linewidth = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "top",
    axis.title.y = element_text(margin = margin(r = 8)),
    axis.line = element_line(linewidth = 0.7),
    axis.ticks = element_line(linewidth = 0.6),
    legend.key.width = unit(1.6, "cm")
  )

p

ggsave(
  "water_levels.tiff",
  plot = p,
  width = 170, height = 120, units = "mm",
  dpi = 600,
  scale = 1.2,
  compression = "lzw"
)


#Version 2 - gör större skillnad i älvmagasinet####

water_levels <- tribble(
  ~Week, ~`Storage reservoir`, ~`Run-of-river impoundment`,~`Free-flowing river reach`, ~`Natural lake`, ~LiR,
  
  1,   97.5, 99.6, 98.6, 99.0, 100.0,
  2,   97.3, 99.8, 98.5, 98.9, 100.0,
  3,   97.0, 97.7, 98.3, 98.9, 100.0,
  4,   96.7, 99.5, 98.3, 98.8, 100.0,
  5,   96.3, 99.4, 98.3, 98.7, 100.0,
  6,   95.9, 97.6, 98.3, 98.6, 100.0,
  7,   95.4, 99.7, 98.4, 98.5, 100.0,
  8,   94.8, 99.5, 98.3, 98.3, 100.0,
  9,   94.1, 98.4, 98.3, 98.2, 100.0,
  10,  93.4, 99.6, 98.3, 98.0, 100.0,
  
  11,  92.7, 99.7, 98.5, 97.8, 100.0,
  12,  92.0, 99.5, 98.5, 97.5, 100.0,
  13,  91.2, 98.4, 98.8, 97.3, 100.0,
  14,  90.4, 99.6, 98.5, 97.1, 100.0,
  15,  89.6, 98.7, 98.7, 96.9, 100.0,
  16,  88.8, 99.5, 98.4, 96.7, 100.0,
  17,  88.0, 98.4, 98.4, 96.5, 100.0,
  18,  87.2, 99.6, 98.4, 96.4, 100.0,
  19,  86.5, 99.7, 98.5, 96.3, 100.0,
  20,  85.8, 98.5, 98.6, 96.2, 100.0,
  
  21,  85.3, 99.4, 98.7, 96.1, 100.0,
  22,  85.0, 99.3, 98.7, 96.0, 100.0,
  23,  85.5, 99.5, 98.8, 96.1, 100.0,
  24,  86.2, 97.6, 98.4, 96.2, 100.0,
  25,  87.1, 99.7, 98.4, 96.4, 100.0,
  26,  88.2, 97.5, 99.3, 96.6, 100.0,
  27,  89.5, 99.4, 99.2, 96.9, 100.0,
  28,  91.0, 97.6, 99.7, 97.2, 100.0,
  29,  92.7, 99.8, 99.8, 97.6, 100.0,
  30,  94.4, 97.6, 99.8, 98.0, 100.0,
  
  31,  96.2, 99.5, 99.4, 98.4, 100.0,
  32,  97.9, 97.7, 99.5, 98.8, 100.0,
  33,  99.3, 99.8, 99.0, 99.1, 100.0,
  34, 100.2, 98.9, 99.0, 99.4, 100.2,
  35, 100.6, 99.8, 98.7, 99.6, 100.6,
  36, 100.8, 99.6, 98.7, 99.8, 100.8,
  37, 101.0, 98.7, 98.2, 99.9, 101.0,
  38, 101.0, 99.9, 98.3, 100.0, 101.0,
  39, 101.0, 99.8, 98.6, 100.0, 101.0,
  40, 100.9, 98.6, 98.7, 100.0, 100.9,
  
  41, 100.8, 99.5, 98.8, 99.9, 100.8,
  42, 100.7, 98.7, 98.8, 99.8, 100.7,
  43, 100.6, 99.8, 98.9, 99.7, 100.6,
  44, 100.5, 98.6, 98.9, 99.6, 100.5,
  45, 100.4, 99.5, 98.7, 99.5, 100.4,
  46, 100.2, 98.7, 98.6, 99.4, 100.2,
  47, 100.0, 99.8, 98.6, 99.3, 100.0,
  48,  99.8, 98.6, 98.5, 99.2, 100.0,
  49,  99.6, 99.5, 98.6, 99.1, 100.0,
  50,  99.4, 98.7, 98.6, 99.1, 100.0,
  51,  99.2, 99.8, 98.6, 99.0, 100.0,
  52,  99.0, 98.6, 98.6, 99.0, 100.0
)
water_long <- water_levels %>%
  pivot_longer(
    cols = -Week,
    names_to = "Type",
    values_to = "WaterLevel"
  )

my_cols <- c(
  "Storage reservoir"           = "#4A4A4A",
  "Run-of-river impoundment" = "#7A7A7A",
  "Free-flowing river reach" = "#8aa8b4",
  "Natural lake"             = "#a6e2e5",
  "LiR"                      = "#C27D38"   # warm accent
)

my_linetypes <- c(
  "Storage reservoir" = "dashed",
  "Run-of-river impoundment" = "solid",
  "Free-flowing river reach" = "dotdash",
  "Natural lake" = "dotdash",
  "LiR" = "solid"
)

my_sizes <- c(
  "Storage reservoir" = 1,
  "Run-of-river impoundment" = 0.4,
 "Free-flowing river reach" = 1,
  "Natural lake" = 1.0,
  "LiR" = 1.2
)

p <- ggplot(
  water_long,
  aes(x = Week, y = WaterLevel,
      color = Type, linetype = Type, linewidth = Type)
) + 
  geom_line() +
  scale_color_manual(values = my_cols) +
  scale_linetype_manual(values = my_linetypes) +
  scale_linewidth_manual(values = my_sizes) +
  scale_x_continuous(breaks = seq(1, 52, by = 4)) +
  scale_y_continuous(
    limits = c(84, 103),
    breaks = seq(84, 103, by = 2),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  labs(
    x = "Week",
    y = "Relative water level (100 = reference level)",
    color = NULL,
    linetype = NULL,
    linewidth = NULL
  ) +
  theme_classic(base_size = 14) +
theme(
  legend.position = "top",
  legend.key.width = unit(1.2, "cm"),
  legend.spacing.x = unit(0.3, "cm"),
  legend.margin = margin(b = 5)
) +
guides(
  color = guide_legend(nrow = 2, byrow = TRUE),
  linetype = guide_legend(nrow = 2, byrow = TRUE),
  linewidth = guide_legend(nrow = 2, byrow = TRUE)
)

p

ggsave(
  "water_levels2.tiff",
  plot = p,
  width = 170, height = 120, units = "mm",
  dpi = 600,
  scale = 1.2,
  compression = "lzw"
)


#####Lägg till freeflowing river
water_levels <- tribble(
  ~Week, ~`Storage reservoir`, ~`Run-of-river impoundment`, ~`Unregulated lake`, ~`Unregulated river reach`, ~LiR,
  
  1,   97.5, 99.0, 98.8, 98.1, 100.0,
  2,   97.3, 99.1, 98.7, 97.8, 100.0,
  3,   97.0, 99.2, 98.6, 98.3, 100.0,
  4,   96.7, 99.1, 98.5, 98.0, 100.0,
  5,   96.3, 98.9, 98.4, 97.7, 100.0,
  6,   95.9, 99.0, 98.3, 98.1, 100.0,
  7,   95.4, 99.2, 98.2, 97.8, 100.0,
  8,   94.8, 99.1, 98.1, 98.0, 100.0,
  9,   94.1, 99.0, 98.0, 97.7, 100.0,
  10,  93.4, 99.1, 98.1, 98.1, 100.0,
  
  11,  92.7, 99.2, 98.0, 97.9, 100.0,
  12,  92.0, 99.0, 98.1, 98.2, 100.0,
  13,  91.2, 98.9, 98.2, 97.8, 100.0,
  14,  90.4, 99.0, 98.3, 98.1, 100.0,
  15,  89.6, 99.1, 98.4, 97.7, 100.0,
  16,  88.8, 99.0, 98.5, 98.0, 100.0,
  17,  88.0, 98.9, 98.6, 97.8, 100.0,
  18,  87.2, 99.0, 98.7, 98.2, 100.0,
  19,  86.5, 99.1, 98.8, 97.9, 100.0,
  20,  85.8, 99.0, 98.9, 98.1, 100.0,
  
  21,  85.3, 98.9, 99.0, 97.8, 100.0,
  22,  85.0, 98.8, 99.0, 98.0, 100.0,
  23,  85.5, 99.0, 99.1, 98.3, 100.0,
  24,  86.2, 99.1, 99.2, 98.5, 100.0,
  25, 87.1, 99.2, 99.3, 98.4, 100.0,
  26, 88.2, 99.1, 99.4, 98.7, 100.0,
  27, 89.5, 99.2, 99.5, 98.5, 100.0,
  28, 91.0, 99.3, 99.6, 98.8, 100.0,
  29, 92.7, 99.4, 99.7, 98.6, 100.0,
  30, 94.4, 99.5, 99.8, 98.9, 100.0,
  
  31, 96.2, 99.6, 99.9, 98.7, 100.0,
  32, 97.9, 99.7,100.0, 99.0, 100.0,
  33, 99.3, 99.8,100.0, 98.8, 100.0,
  34,100.2, 99.9, 99.9, 99.1, 100.2,
  35,100.6,100.0, 99.8, 98.9, 100.6,
  36,100.8,100.1, 99.7, 99.2, 100.8,
  37,101.0,100.0, 99.6, 99.0, 101.0,
  38,101.0,100.1, 99.5, 99.3, 101.0,
  39,101.0,100.0, 99.4, 99.1, 101.0,
  40,100.9, 99.9, 99.3, 99.4, 100.9,
  
  41,100.8, 99.8, 99.2, 99.2, 100.8,
  42,100.7, 99.9, 99.1, 99.5, 100.7,
  43,100.6,100.0, 99.0, 99.3, 100.6,
  44,100.5, 99.9, 98.9, 99.6, 100.5,
  45,100.4, 99.8, 98.8, 99.3, 100.4,
  46,100.2, 99.9, 98.7, 99.5, 100.2,
  47,100.0, 99.8, 98.6, 99.2, 100.0,
  48, 99.8, 99.7, 98.5, 99.0, 100.0,
  49, 99.6, 99.8, 98.4, 98.8, 100.0,
  50, 99.4, 99.7, 98.3, 99.1, 100.0,
  51, 99.2, 99.8, 98.2, 98.7, 100.0,
  52, 99.0, 99.7, 98.1, 98.9, 100.0
)

water_long <- water_levels %>%
  pivot_longer(
    cols = -Week,
    names_to = "Type",
    values_to = "WaterLevel"
  )

my_cols <- c(
  "Storage reservoir"        = "#02afffd5",
  "Run-of-river impoundment" = "#675cf8",
  "Unregulated lake"             = "#5bb982",
  "Unregulated river reach" = "#f8cd3e",
  "LiR"                      = "#ff6c3f"
)

my_linetypes <- c(
  "Storage reservoir"        = "dashed",
  "Run-of-river impoundment" = "dotted",
  "Unregulated lake"             = "dotdash",
  "Unregulated river reach" = "longdash",
  "LiR"                      = "solid"
)

my_sizes <- c(
  "Storage reservoir"        = 1.0,
  "Run-of-river impoundment" = 1.0,
  "Unregulated lake"             = 1.0,
  "Unregulated river reach" = 1.0,
  "LiR"                      = 1.2
)

p1 <- ggplot(subset(water_long, Type != "LiR"),
  aes(x = Week, y = WaterLevel,
      color = Type, linetype = Type, linewidth = Type)
) +
  aes(
    x = Week,
    y = WaterLevel,
    color = Type,
    linetype = Type,
    linewidth = Type
  ) +
  geom_line() +
  scale_color_manual(values = my_cols) +
  scale_linetype_manual(values = my_linetypes) +
  scale_linewidth_manual(values = my_sizes) +
  scale_x_continuous(breaks = seq(1, 52, by = 4)) +
  scale_y_continuous(
    limits = c(84, 103),
    breaks = seq(84, 103, by = 2),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  labs(
    x = "Week",
    y = "Relative water level (100 = reference level)",
    color = NULL,
    linetype = NULL,
    linewidth = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.key.width = unit(1.0, "cm"),
    legend.spacing.x = unit(0.2, "cm"),
    legend.margin = margin(b = 5),
    
    axis.title.y = element_text(margin = margin(r = 8)),
    axis.line = element_line(linewidth = 0.7),
    axis.ticks = element_line(linewidth = 0.6)
  )

p1

ggsave("Hydrological regimes.svg", p1,  width = 8, height = 4)

p2 <- ggplot(
  water_long, 
  aes(
    x = Week,
    y = WaterLevel,
    color = Type,
    linetype = Type,
    linewidth = Type
  )
) +
  geom_line() +
  scale_color_manual(values = my_cols) +
  scale_linetype_manual(values = my_linetypes) +
  scale_linewidth_manual(values = my_sizes) +
  scale_x_continuous(breaks = seq(1, 52, by = 4)) +
  scale_y_continuous(
    limits = c(84, 103),
    breaks = seq(84, 103, by = 2),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  labs(
    x = "Week",
    y = "Relative water level (100 = reference level)",
    color = NULL,
    linetype = NULL,
    linewidth = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.key.width = unit(1.0, "cm"),
    legend.spacing.x = unit(0.2, "cm"),
    legend.margin = margin(b = 5),
    
    axis.title.y = element_text(margin = margin(r = 8)),
    axis.line = element_line(linewidth = 0.7),
    axis.ticks = element_line(linewidth = 0.6)
  )

p2

ggsave("Hydrological regimes+LiR.svg", p2,  width = 9, height = 4)
