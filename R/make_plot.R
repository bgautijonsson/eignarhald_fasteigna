library(tidyverse)
library(metill)
library(ggh4x)
library(ggtext)
library(glue)
library(patchwork)
theme_set(theme_metill())

colour_einst_ein <- "#1b9e77"
colour_einst_fleir <- "#d95f02"
colour_log_ein <- "#7570b3"
colour_log_fleir <- "#e7298a"


caption <- str_c(
  "Mynd eftir @bggjonsson hjá metill.is byggð á gögnum fasteignaskrár um eignarhald fasteigna\n",
  "Gögn og kóði: https://github.com/bgautijonsson/eignarhald_fasteigna"
)

p <- read_csv("data/data.csv") |> 
  filter(tegund == "fjoldi", date < 2024) |> 
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
    diff_total2 = diff_total + sum(pmax(-diff, 0)),
    .by = date
  ) |> 
  mutate(
    p_diff = diff / diff_total2
  ) |> 
  drop_na() |> 
  mutate(
    group = str_c(eigandi, "\n", str_to_lower(fjoldi)),
    group2 = group,
    colour = case_when(
      (fjoldi == "Sem eiga eina íbúð") & (eigandi == "Einstaklingar") ~ colour_einst_ein,
      (fjoldi != "Sem eiga eina íbúð") & (eigandi == "Einstaklingar") ~ colour_einst_fleir,
      (fjoldi == "Sem eiga eina íbúð") & (eigandi != "Einstaklingar") ~ colour_log_ein,
      (fjoldi != "Sem eiga eina íbúð") & (eigandi != "Einstaklingar") ~ colour_log_fleir
    )
  ) |> 
  filter(date >= 1995) |>
  mutate(
    label = case_when(
      (fjoldi == "Sem eiga fleiri en eina íbúð") & (eigandi == "Einstaklingar") ~ glue(
        str_c(
          "Árin 2009 og 2010 fækkaði íbúðum í eigu<br>",
          "<span style='color:{colour_einst_ein};'>einstaklinga sem áttu eina íbúð</span>",  
          " en<br>fjölgaði meðal ",
          "<span style='color:{colour_einst_fleir};'>einstaklinga sem áttu<br>fleiri en eina íbúð</span><br>"
        )
      ),
      (fjoldi != "Sem eiga fleiri en eina íbúð") & (eigandi == "Einstaklingar") ~ glue(
        str_c(
          "Almennt eru flestar eignir keyptar af<br>",
          "<span style='color:{colour_einst_ein};'>einstaklingum sem eiga eina íbúð</span>"
        )
      ),
      (fjoldi == "Sem eiga fleiri en eina íbúð") & (eigandi != "Einstaklingar") ~ glue(
        str_c(
          "Á árunum eftir fjármálahrunið keyptu<br>",
          "<span style='color:{colour_log_fleir};'>lögaðilar sem eiga fleiri en eina íbúð</span><br>",
          "fleiri íbúðir en annars"
        )
      ),
      TRUE ~ NA_character_
    ),
    label = if_else(
      (date == 2011),
      label,
      NA_character_
    ),
    y_lower = if_else(
      fjoldi == "Sem eiga eina íbúð",
      -0.1,
      -0.33
    ),
    y_upper = if_else(
      fjoldi == "Sem eiga eina íbúð",
      1,
      1
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
          data = tibble(x = 1995 - 0.4, xend = 2023 + 0.4, y = 0, yend = 0),
          aes(x = x, xend = xend, y = y, yend = yend),
          lty = 1,
          alpha = 1,
          linewidth = 0.4
        ) +
        geom_richtext(
          data = ~ filter(.x, (fjoldi != "Sem eiga fleiri en eina íbúð") & (eigandi == "Einstaklingar")),
          aes(x = 2023.5, y = 0.8, label = label),
          hjust = 1,
          vjust = 0.5,
          fill = NA,
          label.colour = NA,
          family = "Lato",
          fontface = "bold"
        ) +
        geom_richtext(
          data = ~ filter(.x, (fjoldi == "Sem eiga fleiri en eina íbúð") & (eigandi == "Einstaklingar")),
          aes(x = 2010.7, y = 0.66, label = label),
          hjust = 0,
          vjust = 0.5,
          fill = NA,
          label.colour = NA,
          family = "Lato",
          fontface = "bold"
        ) +
        geom_richtext(
          data = ~ filter(.x, (fjoldi == "Sem eiga fleiri en eina íbúð") & (eigandi != "Einstaklingar")),
          aes(x = 2021.2, y = 0.7, label = label),
          hjust = 1,
          vjust = 0.5,
          fill = NA,
          label.colour = NA,
          family = "Lato",
          fontface = "bold"
        ) +
        scale_x_continuous(
          breaks = seq(1995, 2023, by = 2),
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
          panel.background = element_blank(),
          plot.background = element_blank()
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
    heights = c(1, 1)
  ) +
  plot_annotation(
    title = "Hver kaupa nýjar íbúðir og gamlar?",
    subtitle = str_c(
      "Reiknað sem árleg breyting í eignarhaldi hvers undirhóps deilt með árlegri breytingu í heildarfjölda nýrra íbúða og þeim sem seljast á milli hópanna"
    ),
    caption = caption
  )

p


ggsave(
  plot = p,
  filename = "figures/plot.png",
  width = 8, height = 0.7 * 8, scale = 1.7
)
