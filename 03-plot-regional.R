library(tidyverse)

load("datos/datos-procesados.Rdata")
Sys.setlocale("LC_TIME", "es_PE.utf8")

plot_date <- Sys.Date()

# Regional ----------------------------------------------------------------

regional_daily <- regional %>%
  ungroup() %>%
  arrange(
    region, date
  ) %>%
  group_by(region, date) %>%
  summarise(
    epiyear = lubridate::epiyear(date),
    epiweek = lubridate::epiweek(date),
    sunday = lubridate::floor_date(
      date,
      "weeks",
      week_start = 7), # epiweeks comienzan en Domingo
    saturday = sunday + 6,
    deaths = sum(deaths, na.rm = TRUE),
    pos = sum(pos, na.rm = TRUE),
    neg = sum(neg, na.rm = TRUE),
    tot =  pos + neg
  ) %>%
  ungroup() %>%
  group_by(region) %>%
  mutate(
    deaths_diff = deaths - lag(deaths),
    pos_diff = pos - lag(pos),
    neg_diff = neg - lag(neg),
    tot_diff = tot - lag(tot),
    letal_diff = deaths_diff / pos_diff,
    posit_diff = pos_diff / tot_diff
  )

p_reg <- ggplot(
  regional_daily %>%
    filter(abs(posit_diff) < .6),
  aes(x = date, y = posit_diff)
) +
  geom_smooth(method = "gam") +
  geom_boxplot(
    aes(x = saturday,
        y = posit_diff,
        group = paste0(epiyear, "-", epiweek)),
    fill = NA,
    outlier.shape = NA
  ) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 0.1),
                     limits = c(0, NA)) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme_bw(18) +
  theme(
    axis.text = element_text(family = "Inconsolata"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.caption = element_text(family = "Inconsolata")
  ) +
  labs(
    y = "Positividad (Pruebas positivas / Total de pruebas)",
    x = "",
    title = "COVID-19 Perú: Cambio en la positividad a nivel regional",
    subtitle = "Usando todos los tipos de pruebas (PR, PCR y AG), y datos publicados en Comunicados, Sala Situacional y Reportes de DGE/MINSA",
    caption = glue::glue("Truncado a positivad diaria < 60% // Curva: Modelo GAM de positividad diaria // Boxplot: Distribución por semana epidemiológica\n@jmcastagnetto, Jesus M. Castagnetto ({plot_date})")
  ) +
  facet_wrap(~region, scales = "free_y")

ggsave(
  p_reg,
  filename = glue::glue("{plot_date}_cambio_positividad_regional.png"),
  width = 18,
  height = 18
)


d_reg <- ggplot(
  regional_daily,
  aes(x = date, y = letal_diff)
) +
#  geom_point() +
  geom_smooth(method = "gam") +
  geom_boxplot(
    aes(x = saturday,
        y = letal_diff,
        group = paste0(epiyear, "-", epiweek)),
    fill = NA,
    outlier.shape = NA
  ) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 0.1),
                     limits = c(0, .15)) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme_bw(18) +
  theme(
    axis.text = element_text(family = "Inconsolata"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.caption = element_text(family = "Inconsolata")
  ) +
  labs(
    y = "Letalidad (Fallecidos / Pruebas positivas)",
    x = "",
    title = "COVID-19 Perú: Cambio en la letalidad a nivel regional",
    subtitle = "Usando todos los tipos de pruebas (PR, PCR y AG), y datos publicados en Comunicados, Sala Situacional y Reportes de DGE/MINSA",
    caption = glue::glue("Curva: Modelo GAM de letalidad diaria // Boxplot: Distribución por semana epidemiológica\n@jmcastagnetto, Jesus M. Castagnetto ({plot_date})")
  ) +
  facet_wrap(~region)

ggsave(
  d_reg,
  filename = glue::glue("{plot_date}_cambio_letalidad_regional.png"),
  width = 18,
  height = 18
)

