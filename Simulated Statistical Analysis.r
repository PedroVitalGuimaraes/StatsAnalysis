## =========================
## SECTION 1: Packages
## =========================

library(tidyverse)
library(afex)
library(emmeans)
library(rstatix)
library(janitor)
library(effectsize)

dir.create("output", showWarnings = FALSE)

## =========================
## SECTION 2: Import and reshape data
## =========================

dados_wide <- read_csv("insertsimulateddb.csv") %>% #dbSimulated
  clean_names() %>%
  mutate(id = factor(row_number()),
         tipi_extroversao_grau = factor(tipi_extroversao_grau, levels = c("L", "H")))

## Reshape to long format, keeping only the B_/M_ columns (the two Dinamica levels: Background, Mozart).

dados_long <- dados_wide %>%
  select(id, tipi_extroversao_grau, starts_with("b_acc_fonte"), starts_with("m_acc_fonte")) %>%
  pivot_longer(
    cols = c(starts_with("b_acc_fonte"), starts_with("m_acc_fonte")),
    names_to = c("dinamica", "contexto"),
    names_pattern = "([bm])_acc_fonte_(.*)",
    values_to = "score"
  ) %>%
  mutate(
    dinamica = factor(dinamica, levels = c("b", "m"), labels = c("Background", "Mozart")),
    contexto = factor(str_to_title(contexto),
                       levels = c("Casa", "Escritorio", "Musica", "Silencio"))
  )

## afex requires complete cases across all within-subject cells
dados_long <- dados_long %>%
  group_by(id) %>%
  filter(!any(is.na(score))) %>%
  ungroup()

cat("Subjects retained after removing missing cells:",
    n_distinct(dados_long$id), "of", nrow(dados_wide), "\n")

## =========================
## SECTION 3: Repeated Measures ANOVA
## =========================

rm_anova <- aov_ez(
  id          = "id",
  dv          = "score",
  data        = dados_long,
  within      = c("dinamica", "contexto"),
  between     = "tipi_extroversao_grau",
  type        = 3,
  anova_table = list(es = "pes", correction = "GG")  # partial eta sq + Greenhouse-Geisser
)

print(rm_anova)             # ANOVA table (matches JASP Within/Between Subjects Effects)
summary(rm_anova)           # includes Mauchly's sphericity test + corrected p-values

## =========================
## SECTION 4: Post hoc comparisons (Contexto x Extraversao)
## =========================

emm_full <- emmeans(rm_anova, ~ tipi_extroversao_grau * contexto)
posthoc_full <- pairs(emm_full, adjust = "bonferroni")
print(posthoc_full)

## =========================
## SECTION 5: Simple effects - independent t-tests by Contexto
## (Student's + Mann-Whitney, with assumption checks)
## =========================

contextos <- c("casa", "escritorio", "musica", "silencio")
resultados <- list()
shapiro_resultados <- list()
levene_resultados <- list()

for (ctx in contextos) {

  var <- paste0("bm_fonte_", ctx)
  label <- paste0("BM_fonte_", str_to_title(ctx))

  dados_ctx <- dados_wide %>%
    select(tipi_extroversao_grau, score = all_of(var)) %>%
    drop_na()

  ## ---- Assumption checks ----
  shapiro <- dados_ctx %>%
    group_by(tipi_extroversao_grau) %>%
    shapiro_test(score) %>%
    mutate(measure = label)
  shapiro_resultados[[ctx]] <- shapiro

  levene <- dados_ctx %>% levene_test(score ~ tipi_extroversao_grau) %>%
    mutate(measure = label)
  levene_resultados[[ctx]] <- levene

  ## ---- Student's t-test ----
  t_res <- t.test(score ~ tipi_extroversao_grau, data = dados_ctx, var.equal = TRUE)
  d_res <- cohens_d(score ~ tipi_extroversao_grau, data = dados_ctx)

  ## ---- Mann-Whitney U (Wilcoxon rank-sum) ----
  mw_res <- wilcox.test(score ~ tipi_extroversao_grau, data = dados_ctx,
                         exact = FALSE, correct = TRUE)
  rb_res <- rank_biserial(score ~ tipi_extroversao_grau, data = dados_ctx)

  resultados[[ctx]] <- tibble(
    measure         = label,
    test            = c("Student", "Mann-Whitney"),
    statistic       = c(t_res$statistic, mw_res$statistic),
    df              = c(t_res$parameter, NA),
    p               = c(t_res$p.value, mw_res$p.value),
    effect_size     = c(d_res$Cohens_d, rb_res$rank_biserial_r),
    se_effect_size  = c(d_res$SE, rb_res$SE)
  )
}

resultados_df <- bind_rows(resultados)
print(resultados_df)

## ---- Assumption checks tables ----
shapiro_df <- bind_rows(shapiro_resultados)
print(shapiro_df)

levene_df <- bind_rows(levene_resultados)
print(levene_df)

## ---- Descriptives table (mean, SD, mean rank) ----
descritivas <- map_dfr(contextos, function(ctx) {
  var <- paste0("bm_fonte_", ctx)
  label <- paste0("BM_fonte_", str_to_title(ctx))
  dados_wide %>%
    select(tipi_extroversao_grau, score = all_of(var)) %>%
    drop_na() %>%
    mutate(rank = rank(score)) %>%
    group_by(tipi_extroversao_grau) %>%
    summarise(
      measure   = label,
      n         = n(),
      mean      = mean(score),
      sd        = sd(score),
      se        = sd / sqrt(n),
      mean_rank = mean(rank),
      .groups   = "drop"
    )
})

print(descritivas)

## =========================
## SECTION 6: Descriptives plot (Contexto x Extraversao, faceted by Dinamica)
## =========================

summary_df <- dados_long %>%
  group_by(dinamica, contexto, tipi_extroversao_grau) %>%
  summarise(
    mean = mean(score, na.rm = TRUE),
    sem  = sd(score, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

dodge_width <- 0.3

interaction_plot <- ggplot() +
  geom_jitter(
    data = dados_long,
    aes(x = contexto, y = score, color = tipi_extroversao_grau),
    position = position_jitterdodge(jitter.width = 0.1, dodge.width = dodge_width),
    alpha = 0.15, size = 1.6, show.legend = FALSE
  ) +
  geom_line(
    data = summary_df,
    aes(x = contexto, y = mean, color = tipi_extroversao_grau,
        group = tipi_extroversao_grau),
    position = position_dodge(width = dodge_width),
    linewidth = 0.8, alpha = 0.8
  ) +
  geom_errorbar(
    data = summary_df,
    aes(x = contexto, ymin = mean - sem, ymax = mean + sem,
        color = tipi_extroversao_grau, group = tipi_extroversao_grau),
    position = position_dodge(width = dodge_width),
    width = 0.15, linewidth = 0.8
  ) +
  geom_point(
    data = summary_df,
    aes(x = contexto, y = mean, color = tipi_extroversao_grau,
        shape = tipi_extroversao_grau, fill = tipi_extroversao_grau,
        group = tipi_extroversao_grau),
    position = position_dodge(width = dodge_width),
    size = 3.2, stroke = 1.2
  ) +
  facet_wrap(~ dinamica) +
  scale_color_manual(values = c("H" = "#D95F02", "L" = "#2C7FB8"),
                      labels = c("High", "Low"), name = "Extraversion") +
  scale_fill_manual(values = c("H" = "white", "L" = "#2C7FB8"),
                     labels = c("High", "Low"), name = "Extraversion") +
  scale_shape_manual(values = c("H" = 21, "L" = 22),
                      labels = c("High", "Low"), name = "Extraversion") +
  labs(
    title = "Source Memory by Context and Extraversion",
    x = "Context",
    y = "Accuracy"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "italic", size = 11, hjust = 0),
    strip.background = element_blank(),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14)
  )

ggsave("output/interaction_plot_contexto_extroversao.png",
       interaction_plot, width = 10, height = 5, dpi = 300)

print(interaction_plot)
