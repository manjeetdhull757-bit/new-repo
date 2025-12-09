library(tibble)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(ggpubr)
library(scales)
library(emmeans)
library(multcomp)
library(car)
library(purrr)

#------------------------ FULL HARDCODED DATA -----------------------
gcms_dat <- tibble::tribble(
  ~Plant_ID, ~Treatment, ~Timepoint, ~Pathogen, ~SA, ~ABA,
  2, 'No AM Fungi', 'Before Pathogen', 'Absent', 0.5744, 7.7886,
  18, 'Community I', 'Before Pathogen', 'Absent', 0.7554, 5.3684,
  19, 'Community I', 'Before Pathogen', 'Absent', 1.178, 1.2318,
  20, 'Community I', 'Before Pathogen', 'Absent', 1.0779, 8.4755,
  35, 'Community II', 'Before Pathogen', 'Absent', 0.7524, 6.125,
  36, 'Community II', 'Before Pathogen', 'Absent', 1.0239, 7.4327,
  37, 'Community II', 'Before Pathogen', 'Absent', 0.6287, 7.2678,
  53, 'No AM Fungi', 'Before Pathogen', 'Absent', 0.8935, 5.1753,
  70, 'Community I', 'Before Pathogen', 'Absent', 1.1734, 4.4895,
  71, 'Community I', 'Before Pathogen', 'Absent', 0.9131, 6.4071,
  86, 'Community II', 'Before Pathogen', 'Absent', 0.6501, 4.5731,
  6, 'No AM Fungi', '1 week after', 'Absent', 0.53398, 17.461,
  7, 'No AM Fungi', '1 week after', 'Absent', 0.48498, 5.796,
  8, 'No AM Fungi', '1 week after', 'Absent', 0.6156, 5.747,
  9, 'No AM Fungi', '1 week after', 'Absent', 1.075, 4.451,
  12, 'No AM Fungi', '1 week after', 'Absent', 1.192, 6.927,
  13, 'No AM Fungi', '1 week after', 'Absent', 0.7137, 5.384,
  14, 'No AM Fungi', '1 week after', 'Absent', 0.7659, 8.649,
  16, 'No AM Fungi', '1 week after', 'Absent', 0.665, 4.419,
  17, 'No AM Fungi', '1 week after', 'Absent', 1.3811, 6.308,
  21, 'Community I', '1 week after', 'Absent', 0.821, 3.83,
  22, 'Community I', '1 week after', 'Absent', 0.671, 3.843,
  23, 'Community I', '1 week after', 'Absent', 0.63036, 5.045,
  25, 'Community I', '1 week after', 'Absent', 2.3786, 6.927,
  27, 'Community I', '1 week after', 'Absent', 1.3581, 4.76,
  31, 'Community I', '1 week after', 'Absent', 0.5993, 6.934,
  33, 'Community I', '1 week after', 'Absent', 0.7496, 8.288,
  38, 'Community II', '1 week after', 'Absent', 1.4953, 6.004,
  39, 'Community II', '1 week after', 'Absent', 1.3229, 7.087,
  40, 'Community II', '1 week after', 'Absent', 1.9322, 6.122,
  41, 'Community II', '1 week after', 'Absent', 0.49125, 5.739,
  44, 'Community II', '1 week after', 'Absent', 0.66661, 9.898,
  46, 'Community II', '1 week after', 'Absent', 0.40698, 8.094,
  47, 'Community II', '1 week after', 'Absent', 1.5634, 4.063,
  49, 'Community II', '1 week after', 'Absent', 0.6417, 3.843,
  50, 'Community II', '1 week after', 'Absent', 1.1443, 7.916,
  56, 'No AM Fungi', '1 week after', 'Present', 1.4302, 7.382,
  57, 'No AM Fungi', '1 week after', 'Present', 1.1162, 12.519,
  59, 'No AM Fungi', '1 week after', 'Present', 1.0066, 0.7743,
  63, 'No AM Fungi', '1 week after', 'Present', 0.33166, 10.46,
  64, 'No AM Fungi', '1 week after', 'Present', 1.2751, 15.12,
  65, 'No AM Fungi', '1 week after', 'Present', 0.9202, 16.81,
  67, 'No AM Fungi', '1 week after', 'Present', 1.4498, 1.338,
  68, 'No AM Fungi', '1 week after', 'Present', 0.9603, 4.917,
  75, 'Community I', '1 week after', 'Present', 0.25893, 7.284,
  77, 'Community I', '1 week after', 'Present', 1.0561, 6.067,
  79, 'Community I', '1 week after', 'Present', 0.3569, 7.862,
  80, 'Community I', '1 week after', 'Present', 0.7034, 3.829,
  82, 'Community I', '1 week after', 'Present', 1.2913, 6.186,
  83, 'Community I', '1 week after', 'Present', 0.7645, 14.94,
  84, 'Community I', '1 week after', 'Present', 1.3546, 16.23,
  85, 'Community I', '1 week after', 'Present', 1.3515, 5.397,
  89, 'Community II', '1 week after', 'Present', 1.1647, 14.04,
  92, 'Community II', '1 week after', 'Present', 1.7163, 9.859,
  94, 'Community II', '1 week after', 'Present', 0.5015, 8.831,
  95, 'Community II', '1 week after', 'Present', 0.9382, 15.76,
  99, 'Community II', '1 week after', 'Present', 1.7493, 8.011,
  100, 'Community II', '1 week after', 'Present', 1.2055, 18.41,
  101, 'Community II', '1 week after', 'Present', 1.0448, 2.982,
  102, 'Community II', '1 week after', 'Present', 0.7627, 8.284,
  5, 'No AM Fungi', '2 week after', 'Absent', 0.42279, 5.337,
  10, 'No AM Fungi', '2 week after', 'Absent', 1.1316, 6.232,
  11, 'No AM Fungi', '2 week after', 'Absent', 1.0431, 4.526,
  15, 'No AM Fungi', '2 week after', 'Absent', 1.3301, 4.935,
  26, 'Community I', '2 week after', 'Absent', 1.9078, 5.074,
  30, 'Community I', '2 week after', 'Absent', 0.77549, 11.56,
  32, 'Community I', '2 week after', 'Absent', 0.70654, 10.70,
  34, 'Community I', '2 week after', 'Absent', 0.8388, 9.574,
  43, 'Community II', '2 week after', 'Absent', 0.58034, 21.89,
  45, 'Community II', '2 week after', 'Absent', 0.54978, 3.875,
  48, 'Community II', '2 week after', 'Absent', 1.578, 7.248,
  51, 'Community II', '2 week after', 'Absent', 1.5884, 6.427,
  58, 'No AM Fungi', '2 week after', 'Present', 1.5943, 23.41,
  66, 'No AM Fungi', '2 week after', 'Present', 1.0237, 19.18,
  72, 'Community I', '2 week after', 'Present', 0.30997, 8.377,
  76, 'Community I', '2 week after', 'Present', 1.16, 7.746,
  78, 'Community I', '2 week after', 'Present', 1.4211, 6.043,
  81, 'Community I', '2 week after', 'Present', 1.3178, 11.44,
  93, 'Community II', '2 week after', 'Present', 1.5346, 4.246,
  98, 'Community II', '2 week after', 'Present', 0.4512, 21.48
)

gcms_dat <- gcms_dat %>%
  mutate(
    Treatment = factor(Treatment, levels = c("No AM Fungi","Community I","Community II")),
    Pathogen  = factor(Pathogen,  levels = c("Absent","Present")),
    Timepoint = factor(Timepoint, levels = c("Before Pathogen","1 week after","2 week after"))
  )

long_gcms <- gcms_dat %>%
  pivot_longer(cols = c(SA, ABA), names_to = "Hormone", values_to = "Concentration") %>%
  mutate(Hormone = factor(Hormone, levels = c("SA", "ABA"), labels = c("Salicylic Acid", "Abscisic Acid"))) %>%
  # ADD FillGroup variable to match phosphorus plot colors
  mutate(FillGroup = ifelse(Timepoint == "Before Pathogen", "Baseline", as.character(Pathogen)))

#-------------------- CLD letters helper -------------------
get_cld_letters <- function(dat, timepoint, hormone) {
  sub <- dat %>% filter(Hormone == hormone, Timepoint == timepoint)
  if (n_distinct(sub$Treatment) > 1 & n_distinct(sub$Pathogen) > 1) {
    m <- lm(Concentration ~ Treatment * Pathogen, data = sub)
    em <- emmeans(m, ~ Treatment * Pathogen)
    cl <- as.data.frame(cld(em, Letters = letters, type = "response"))
    cl$Timepoint <- timepoint
    cl$Hormone   <- hormone
    cl
  } else {
    expand.grid(
      Treatment = unique(sub$Treatment),
      Pathogen  = unique(sub$Pathogen),
      .group    = "a"
    ) %>% mutate(Timepoint = timepoint, Hormone = hormone)
  }
}

combos <- expand.grid(
  timepoint = levels(long_gcms$Timepoint),
  hormone   = levels(long_gcms$Hormone)
)

cld_all <- purrr::map_dfr(
  seq_len(nrow(combos)),
  ~get_cld_letters(long_gcms, combos[.x, "timepoint"], combos[.x, "hormone"])
) %>% rename(Signif = .group)

cld_all <- cld_all %>%
  left_join(
    long_gcms %>%
      group_by(Treatment, Pathogen, Timepoint, Hormone) %>%
      summarise(y_pos = max(Concentration, na.rm = TRUE)*1.17, .groups = "drop"),
    by = c("Treatment","Pathogen","Timepoint","Hormone")
  )

# UPDATED COLOR PALETTE to match phosphorus plot
fill_vals <- c(Baseline = "#BDBDBD", Absent = "#B8E186", Present = "chocolate4")
base_sz    <- 13
axis_sz    <- 11
strip_sz   <- 11
title_sz   <- 14
lab_sz     <- 11
cld_sz     <- 2.5

time_labels <- c(
  "Before Pathogen" = "Before pathogen inoculation",
  "1 week after"    = "1 week post-inoculation",
  "2 week after"    = "2 weeks post-inoculation"
)

#-------------------- Updated plotting function --------------------
plot_gcms <- function(data, hormone, title, ylab) {
  d <- data %>% filter(Hormone == hormone)
  cld_df <- cld_all %>% filter(Hormone == hormone)
  
  p <- ggplot(d, aes(x=Treatment, y=Concentration, fill=FillGroup)) +  # Changed to FillGroup
    geom_violin(aes(group=interaction(Treatment,Pathogen)), 
                position=position_dodge(width=0.80), width=0.58, alpha=0.19, trim=TRUE, scale="width",
                colour="black", linewidth=0.46, adjust=1.1) +
    geom_boxplot(aes(group=interaction(Treatment,Pathogen)), 
                 position=position_dodge(0.8), width=0.31, outlier.shape=NA, alpha=0.85,
                 colour="black", linewidth=0.7) +
    # CORRECT MEAN DIAMONDS: explicit grouping for Pathogen with same dodge
    stat_summary(
      fun=mean, 
      geom="point", 
      shape=23, 
      size=2.2, 
      fill="white", 
      colour="black", 
      stroke=0.38,
      aes(group=Pathogen),  # Explicit group!
      position=position_dodge(width=0.8), 
      show.legend=FALSE
    ) +
    geom_jitter(
      aes(shape=Pathogen, group=interaction(Treatment, Pathogen)),
      position=position_jitterdodge(jitter.width=0.09, dodge.width=0.8), size=1.4,
      alpha=0.43, stroke=0.18, colour="black", show.legend=FALSE
    ) +
    # FIXED: Add inherit.aes=FALSE to geom_text
    geom_text(
      data=cld_df, aes(x=Treatment, y=y_pos, label=Signif, group=Pathogen),
      inherit.aes=FALSE,  # THIS IS THE KEY FIX
      position=position_dodge(width=0.8), family="Times", fontface="bold", size=7,
      color="black", vjust=0.15
    ) +
    facet_wrap(~factor(Timepoint, levels=levels(d$Timepoint), labels=time_labels), nrow=1) +
    scale_fill_manual(values=fill_vals, breaks=c("Absent","Present"), name="Pathogen", 
                      guide=guide_legend(override.aes=list(alpha=1,size=4))) +  # Updated to use fill_vals
    scale_shape_manual(values=c(21, 22), name="Pathogen") +
    scale_y_continuous(expand = expansion(mult = c(0.04, 0.22))) +
    labs(title=title, x="", y=ylab,
         caption="") +
    theme_pubr(base_size=base_sz, base_family="Times", border=TRUE) +
    theme(
      plot.title         = NULL,
      axis.text.x        = element_text(angle=0, hjust=0.5, size=18, family="Times"),
      axis.text.y        = element_text(size=20, family="Times"),
      axis.title         = element_text(size=22, family="Times", margin=margin(r=8)),
      strip.text         = element_text(size=20, family="Times", face="bold"),
      legend.position    = "bottom",
      legend.title       = element_text(face="bold", family="Times", size=22),
      legend.text        = element_text(size=22, family="Times"),
      panel.grid.major.y = element_line(colour="#EEEEEE", linetype="dotted"),
      panel.grid.major.x = element_blank(),
      plot.caption       = element_text(size=9, colour="grey20", family="Times"),
      plot.margin        = margin(t=10, r=12, b=8, l=8)
    )
  return(p)
}

p_SA  <- plot_gcms(long_gcms, "Salicylic Acid", "", "Salicylic Acid (mg/L)")
p_ABA <- plot_gcms(long_gcms, "Abscisic Acid", "", "Abscisic Acid (mg/L)")

ggsave("GCMS_SA_Publication.png", p_SA, width=12, height=6.2, dpi=400)
ggsave("GCMS_ABA_Publication.png", p_ABA, width=12, height=6.2, dpi=400)

print(p_SA)
print(p_ABA)
