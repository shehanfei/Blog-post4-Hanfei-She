# 03_make_figures.R -- three figures + summary tables
library(here)
library(tidyverse)
library(scales)
library(ggrepel)
library(broom)

d <- read_csv(here("data/processed/wdi_clean.csv"), show_col_types = FALSE) |>
  mutate(income = factor(income, levels = c("Low income", "Lower middle income",
                                            "Upper middle income", "High income")))

# latest year with broad coverage
latest <- d |> count(year) |> filter(n >= 150) |> pull(year) |> max()
base_year <- 1990
writeLines(as.character(latest), here("results/tables/latest_year.txt"))

theme_set(theme_minimal(base_size = 12))
src <- "Source: World Bank WDI. GDP per capita in constant 2021 PPP dollars."

# ---- Figure 1: Preston curve in latest year ---------------------------------
d_latest <- filter(d, year == latest)
big <- d_latest |> filter(pop > 1.5e8 | country %in% c("Nigeria", "Japan", "Chad"))

fig1 <- ggplot(d_latest, aes(gdppc, lifeexp)) +
  geom_point(aes(size = pop, color = region), alpha = 0.65) +
  geom_smooth(method = "lm", formula = y ~ log(x), se = FALSE,
              color = "black", linewidth = 0.8) +
  geom_text_repel(data = big, aes(label = country), size = 3, max.overlaps = 20) +
  scale_x_log10(labels = label_dollar()) +
  scale_size_area(max_size = 14, guide = "none") +
  labs(title = paste0("Richer countries live longer, with diminishing returns (", latest, ")"),
       subtitle = "Each bubble is a country; bubble size = population; x-axis is log scale",
       x = "GDP per capita (log scale)", y = "Life expectancy at birth (years)",
       color = NULL, caption = src) +
  theme(legend.position = "bottom")
ggsave(here("results/figures/fig1_preston_latest.png"), fig1, width = 9, height = 6, dpi = 300)

# ---- Figure 2: the curve shifts up + movers ---------------------------------
two <- d |> filter(year %in% c(base_year, latest)) |>
  mutate(period = factor(year))

movers <- c("China", "India", "Nigeria", "United States", "Brazil", "Korea, Rep.", "Botswana")
arrows <- two |> filter(country %in% movers) |>
  select(country, year, gdppc, lifeexp) |>
  pivot_wider(names_from = year, values_from = c(gdppc, lifeexp))
x0 <- paste0("gdppc_", base_year);   x1 <- paste0("gdppc_", latest)
y0 <- paste0("lifeexp_", base_year); y1 <- paste0("lifeexp_", latest)

fig2 <- ggplot(two, aes(gdppc, lifeexp, color = period)) +
  geom_point(alpha = 0.35, size = 1.6) +
  geom_smooth(method = "lm", formula = y ~ log(x), se = FALSE) +
  geom_segment(data = arrows,
               aes(x = .data[[x0]], y = .data[[y0]], xend = .data[[x1]], yend = .data[[y1]]),
               inherit.aes = FALSE, color = "grey25",
               arrow = arrow(length = unit(0.15, "cm"))) +
  geom_text_repel(data = arrows, aes(x = .data[[x1]], y = .data[[y1]], label = country),
                  inherit.aes = FALSE, size = 3) +
  scale_x_log10(labels = label_dollar()) +
  scale_color_manual(values = c("#d95f02", "#1b9e77")) +
  labs(title = "Countries moved along the curve, and the curve itself shifted up",
       subtitle = paste0("Arrows: selected countries, ", base_year, " to ", latest),
       x = "GDP per capita (log scale)", y = "Life expectancy at birth (years)",
       color = NULL, caption = src) +
  theme(legend.position = "bottom")
ggsave(here("results/figures/fig2_curve_shift.png"), fig2, width = 9, height = 6, dpi = 300)

# ---- Figure 3: population-weighted life expectancy by income group -----------
grp <- d |>
  group_by(income, year) |>
  summarise(lifeexp = weighted.mean(lifeexp, pop), .groups = "drop")

fig3 <- ggplot(grp, aes(year, lifeexp, color = income)) +
  geom_line(linewidth = 1) +
  labs(title = "Life expectancy gaps between income groups have narrowed",
       subtitle = "Population-weighted mean; income groups use current World Bank classification",
       x = NULL, y = "Life expectancy at birth (years)", color = NULL,
       caption = "Source: World Bank WDI.") +
  theme(legend.position = "bottom")
ggsave(here("results/figures/fig3_income_groups.png"), fig3, width = 9, height = 5.5, dpi = 300)

# ---- Tables -----------------------------------------------------------------
fit <- two |>
  group_by(year) |>
  group_modify(~ tidy(lm(lifeexp ~ log(gdppc), data = .x))) |>
  filter(term == "log(gdppc)") |>
  transmute(year, slope = estimate, se = std.error,
            per_doubling = estimate * log(2)) |>
  ungroup()
write_csv(fit, here("results/tables/preston_fit.csv"))

gaps <- grp |> filter(year %in% c(base_year, latest)) |>
  pivot_wider(names_from = income, values_from = lifeexp) |>
  mutate(gap_high_low = `High income` - `Low income`)
write_csv(gaps, here("results/tables/income_group_gaps.csv"))
