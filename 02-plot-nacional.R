library(tidyverse)

load("datos/datos-procesados.Rdata")
Sys.setlocale("LC_TIME", "es_PE.utf8")
plot_date <- Sys.Date()

# Nacional ----------------------------------------------------------------

nacional_daily <- nacional %>%
  mutate(
    epiyear = lubridate::epiyear(date),
    epiweek = lubridate::epiweek(date),
    sunday = lubridate::floor_date(
      date,
      "weeks",
      week_start = 7), # epiweeks comienzan en Domingo
    saturday = sunday + 6,
    tot = pos + neg,
    deaths_diff = deaths - lag(deaths),
    pos_diff = pos - lag(pos),
    neg_diff = neg - lag(neg),
    tot_diff = tot - lag(tot),
    letal_diff = deaths_diff / pos_diff,
    posit_diff = pos_diff / tot_diff
  )

p_nac <- ggplot(
  nacional_daily,
  aes(x = date, y = posit_diff)
) +
  geom_smooth(method = "gam") +
  geom_boxplot(
    aes(x = saturday,
        y = posit_diff,
        group = paste0(epiyear, "-", epiweek)),
    fill = NA,
    outlier.colour = "red",
    outlier.size = .5
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
    title = "COVID-19 Perú: Cambio en la positividad a nivel nacional",
    subtitle = "Usando todos los tipos de pruebas (PR, PCR y AG), y datos publicados en Comunicados, Sala Situacional y Reportes de DGE/MINSA",
    caption = glue::glue("Curva: Modelo GAM de positividad diaria // Boxplot: Distribución por semana epidemiológica\n@jmcastagnetto, Jesus M. Castagnetto ({plot_date})")
  )
p_nac

ggsave(
  p_nac,
  filename = glue::glue("{plot_date}_cambio_positividad_nacional.png"),
  width = 16,
  height = 9
)

d_nac <- ggplot(
  nacional_daily,
  aes(x = date, y = letal_diff)
) +
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
    title = "COVID-19 Perú: Cambio en la letalidad a nivel nacional",
    subtitle = "Usando todos los tipos de pruebas (PR, PCR y AG), y datos publicados en Comunicados, Sala Situacional y Reportes de DGE/MINSA",
    caption = glue::glue("Curva: Modelo GAM de letalidad diaria // Boxplot: Distribución por semana epidemiológica\n@jmcastagnetto, Jesus M. Castagnetto ({plot_date})")
  )
d_nac

ggsave(
  d_nac,
  filename = glue::glue("{plot_date}_cambio_letalidad_nacional.png"),
  width = 16,
  height = 9
)

