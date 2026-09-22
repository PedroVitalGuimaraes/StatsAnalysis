# Source Memory, Context, Extraversion

Statistical analysis in R examining how **context** (Home, Office, Music,
Silence) and **trait extraversion** jointly influence **source memory
accuracy**, using a mixed repeated-measures design.\ 
Originally analysed in JASP; this repository reproduces the full analysis pipeline in reproducible
R code.

## Design

| Factor | Type | Levels |
|---|---|---|
| Dinamica | Within-subjects | Background, Mozart |
| Contexto | Within-subjects | Casa, Escritorio, Musica, Silencio |
| TIPI_Extroversao_Grau | Between-subjects | High (H), Low (L) |

- **N = 40** participants (20 High extraversion, 20 Low extraversion)
- Dependent variable: source memory accuracy
- Extraversion measured via the Ten-Item Personality Inventory (TIPI)

## About the data

This analysis was done with a simulated database rather than the original
participant data, to avoid any privacy or data-sharing concerns. 
The showcase of the statistical analysis stands.

## Requirements

- R >= 4.2
- Packages: `tidyverse`, `afex`, `emmeans`, `rstatix`, `janitor`,
  `effectsize`

## Steps

1. Packages
2. Import and reshape data
3. Repeated Measures ANOVA
4. Post hoc comparisons (Contexto x Extraversion)
5. Simple effects - independent t-tests by Contexto
6. Descriptives plot (Contexto x Extraversion, faceted by Dinamica)

## References

Guimarães, P. V. (2022). ***O papel moderador da extroversão no efeito da música na memória episódica***. [Dissertação de Mestrado, Universidade do Porto]. Repositório Aberto da Universidade do Porto.
[doi:10.34626/ethm-n129](https://doi.org/10.34626/ethm-n129)
