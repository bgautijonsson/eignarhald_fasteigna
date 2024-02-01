library(tidyverse)
library(metill)
library(jsonlite)
library(ggh4x)
library(ggtext)
library(glue)
library(patchwork)
theme_set(theme_metill())

colour_einst_ein <- "#1b9e77"
colour_einst_fleir <- "#d95f02"
colour_log_ein <- "#7570b3"
colour_log_fleir <- "#e7298a"

url <- "https://talnaefni.fasteignaskra.is/talnaefni/v1/eignarhaldibuda"


caption <- str_c(
  "Mynd eftir @bggjonsson hjá metill.is byggð á gögnum fasteignaskrár um eignarhald fasteigna\n",
  "Gögn og kóði: https://github.com/bgautijonsson/eignarhald_fasteigna"
)

p <- read_csv("data/data.csv") |> 
  filter(tegund == "fjoldi") |> 
  mutate(
    total = sum(value),
    .by = date
  ) |> 
  mutate(
    diff = c(NA, diff(value)),
    diff_total = c(NA, diff(total)),
    .by = c(fjoldi, eigandi)
  ) |> 
  mutate(
    p_diff = diff / diff_total
  ) |> 
  drop_na() |> 
  mutate(
    group = str_c(eigandi, "\n",fjoldi),
    group2 = group,
    colour = case_when(
      (fjoldi == "Sem eiga eina íbúð") & (eigandi == "Einstaklingar") ~ colour_einst_ein,
      (fjoldi != "Sem eiga eina íbúð") & (eigandi == "Einstaklingar") ~ colour_einst_fleir,
      (fjoldi == "Sem eiga eina íbúð") & (eigandi != "Einstaklingar") ~ colour_log_ein,
      (fjoldi != "Sem eiga eina íbúð") & (eigandi != "Einstaklingar") ~ colour_log_fleir
    )
  ) |> 
  filter(date >= 1996) |>
  mutate(
    label = glue(
      str_c(
        "Í kjölfar fjármálahrunsins fækkaði<br>",
        "<span style='color:{colour_einst_ein};'>einstaklingum sem áttu eina íbúð</span>",  
        " en<br>",
        "<span style='color:{colour_einst_fleir};'>einstaklingum sem áttu fleiri en eina íbúð</span><br>",
        "fjölgaði."
      )
    ),
    label = if_else(
      (fjoldi == "Sem eiga fleiri en eina íbúð") & (eigandi == "Einstaklingar") & (date == 2011),
      label,
      NA_character_
    ),
    y_lower = if_else(
      fjoldi == "Sem eiga eina íbúð",
      -1.1,
      -0.6
    ),
    y_upper = if_else(
      fjoldi == "Sem eiga eina íbúð",
      1.4,
      1.05
    )
  ) |> 
  group_by(group2) |> 
  group_map(
    \(x,...) {
      p <- x |> 
        ggplot(aes(date, p_diff)) +
        geom_col(
          aes(fill = colour),
          alpha = 0.7
        ) +
        geom_segment(
          data = tibble(x = 1996 - 0.4, xend = 2024 + 0.4, y = 0, yend = 0),
          aes(x = x, xend = xend, y = y, yend = yend),
          lty = 1,
          alpha = 0.4
        ) +
        geom_richtext(
          aes(x = 2010.7, y = 1.405, label = label),
          hjust = 0,
          vjust = 0.5,
          fill = NA,
          label.colour = NA,
          family = "Lato",
          fontface = "bold"
        ) +
        scale_x_continuous(
          breaks = seq(1996, 2024, by = 2),
          guide = guide_axis_truncated(),
          expand = expansion(0.02)
        ) +
        scale_y_continuous(
          limits = c(-2, 2),
          breaks = seq(0, 1, by = 0.25),
          expand = expansion(c(0, 0.01)),
          labels = label_hlutf(),
          guide = guide_axis_truncated(
            trunc_lower = 0, trunc_upper = 1
          )
        ) +
        scale_fill_identity() +
        coord_cartesian(ylim = c(unique(x$y_lower), unique(x$y_upper)), clip = "off") +
        theme(
          legend.position = "none",
          panel.spacing.x = unit(0.4, "cm"),
          panel.spacing.y = unit(0.05, "cm"),
          panel.background = element_blank()
        ) +
        labs(
          x = NULL,
          y = NULL,
          subtitle = unique(x$group)
        )
      
      if ("Sem eiga eina íbúð" %in% x$fjoldi) {
        p <- p + theme(
          axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank()
        )
      }
      p
    }
  ) |> 
  wrap_plots(
    design = "
    AC
    BD
    ",
    heights = c(2.5, 1.65)
  ) +
  plot_annotation(
    title = "Tilfærslur á fasteignum milli mismunandi eignarhaldshópa",
    subtitle = str_c(
      "Reiknað sem árleg breyting í eignarhaldi hvers undirhóps deild með árlegri breytingu í heildarfjölda íbúða",
      " | ",
      "Ef eignum fækkar í undirhóp eitthvert árið\ngetur samanlagt hlutfall allra hópanna verið hærra en 100%"
    ),
    caption = caption
  )


ggsave(
  plot = p,
  filename = "figures/plot.png",
  width = 8, height = 0.7 * 8, scale = 1.7
)
