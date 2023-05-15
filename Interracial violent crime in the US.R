library(tidyverse)
library(scales)

# based on table 13 in https://bjs.ojp.gov/content/pub/pdf/cv21.pdf
victimization_data <- tribble(
  ~offender_group, ~victim_group, ~N,
  "White", "White", 1495440,
  "White", "Black", 69850,
  "White", "Hispanic", 202640,
  "Black", "White", 480030,
  "Black", "Black", 358360,
  "Black", "Hispanic", 135630,
  "Hispanic", "White", 197180,
  "Hispanic", "Black", 65330,
  "Hispanic", "Hispanic", 194830
)
victimization_data$offender_group <- factor(
  victimization_data$offender_group,
  levels = c("White", "Black", "Hispanic")
)
victimization_data$victim_group <- factor(
  victimization_data$victim_group,
  levels = c("Hispanic", "Black", "White")
)

ggplot(data = victimization_data, aes(x = offender_group, y = N, fill = victim_group)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(
    aes(label = scales::comma(N)),
    position = position_stack(vjust = 0.5)
  ) +
  theme_bw() +
  ggtitle(
    "Racial/ethnic patterns of victimization in the US (2021)"
  ) +
  xlab("Race/ethnicity of offender") +
  ylab("Number of victimizations") +
  scale_y_continuous(label = comma) +
  scale_fill_brewer(
    name = "Race/ethnicity of victim",
    palette = "Set3"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    legend.title = element_text(face = "bold")
  ) +
  labs(caption = "Source : Bureau of Justice Statistics (National Crime Victimization Survey) - Chart by Philippe Lemoine (@phl43)")

ggsave(
  "Racial-ethnic patterns of victimization in the US (2021).png",
  width = 12,
  height = 6
)

# obtained from https://data.census.gov/table?t=Race+and+Ethnicity&g=010XX00US$1400000&y=2020&tid=DECENNIALPL2020.P2
# (note 1: the file is too big to put it on GitHub, but you can download it from the Census Bureau's website)
# (note 2: I'm assuming that, for people who identified with 2 races, black > white > everything else and I put anyone who
# identified with more than 2 races in the "other" category, which is based on a combination of how I expect victims
# to perceive multiracial offenders and convenience)
neighborhood_census_data <- read_csv("data/DECENNIALPL2020.P2-Data.csv") %>%
  slice(-1) %>%
  mutate(
    census_tract = GEO_ID,
    total = as.integer(P2_001N),
    nonhispanic_white = as.integer(P2_005N) + as.integer(P2_014N) + as.integer(P2_015N) + as.integer(P2_016N) + as.integer(P2_017N),
    nonhispanic_black = as.integer(P2_006N) + as.integer(P2_013N) + as.integer(P2_018N) + as.integer(P2_019N) + as.integer(P2_020N) + as.integer(P2_021N),
    hispanic = as.integer(P2_002N),
    other = total - nonhispanic_white - nonhispanic_black - hispanic
  ) %>%
  select(
    census_tract,
    total,
    nonhispanic_white,
    nonhispanic_black,
    hispanic,
    other
  )

national_census_data <- neighborhood_census_data %>%
  summarize(
    total = sum(total),
    nonhispanic_white = sum(nonhispanic_white),
    nonhispanic_black = sum(nonhispanic_black),
    hispanic = sum(hispanic),
    other = sum(other)
  )

expected_victimization1 <-tribble(
  ~offender_group, ~victim_group, ~N,
  "White", "White", round(sum(victimization_data$N) * national_census_data$nonhispanic_white / (national_census_data$total - national_census_data$other) * national_census_data$nonhispanic_white / (national_census_data$total - national_census_data$other)),
  "White", "Black", round(sum(victimization_data$N) * national_census_data$nonhispanic_white / (national_census_data$total - national_census_data$other) * national_census_data$nonhispanic_black / (national_census_data$total - national_census_data$other)),
  "White", "Hispanic", round(sum(victimization_data$N) * national_census_data$nonhispanic_white / (national_census_data$total - national_census_data$other) * national_census_data$hispanic / (national_census_data$total - national_census_data$other)),
  "Black", "White", round(sum(victimization_data$N) * national_census_data$nonhispanic_black / (national_census_data$total - national_census_data$other) * national_census_data$nonhispanic_white / (national_census_data$total - national_census_data$other)),
  "Black", "Black", round(sum(victimization_data$N) * national_census_data$nonhispanic_black / (national_census_data$total - national_census_data$other) * national_census_data$nonhispanic_black / (national_census_data$total - national_census_data$other)),
  "Black", "Hispanic", round(sum(victimization_data$N) * national_census_data$nonhispanic_black / (national_census_data$total - national_census_data$other) * national_census_data$hispanic / (national_census_data$total - national_census_data$other)),
  "Hispanic", "White", round(sum(victimization_data$N) * national_census_data$hispanic / (national_census_data$total - national_census_data$other) * national_census_data$nonhispanic_white / (national_census_data$total - national_census_data$other)),
  "Hispanic", "Black", round(sum(victimization_data$N) * national_census_data$hispanic / (national_census_data$total - national_census_data$other) * national_census_data$nonhispanic_black / (national_census_data$total - national_census_data$other)),
  "Hispanic", "Hispanic", round(sum(victimization_data$N) * national_census_data$hispanic / (national_census_data$total - national_census_data$other) * national_census_data$hispanic / (national_census_data$total - national_census_data$other))
)
expected_victimization1$offender_group <- factor(
  victimization_data$offender_group,
  levels = c("White", "Black", "Hispanic")
)
expected_victimization1$victim_group <- factor(
  victimization_data$victim_group,
  levels = c("Hispanic", "Black", "White")
)

ggplot(data = expected_victimization1, aes(x = offender_group, y = N, fill = victim_group)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(
    aes(label = scales::comma(N)),
    position = position_stack(vjust = 0.5)
  ) +
  theme_bw() +
  ggtitle(
    "Expected racial/ethnic patterns of victimization in the US if offending were random at the national level",
    subtitle = "(keeping only the total number of violent crimes fixed)"
  ) +
  xlab("Race/ethnicity of offender") +
  ylab("Number of victimizations") +
  scale_y_continuous(label = comma) +
  scale_fill_brewer(
    name = "Race/ethnicity of victim",
    palette = "Set3"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    legend.title = element_text(face = "bold")
  ) +
  labs(caption = "Source : Bureau of Justice Statistics (National Crime Victimization Survey) - Chart by Philippe Lemoine (@phl43)")
  
ggsave(
  "Expected racial-ethnic patterns of victimization in the US - Model 1.png",
  width = 12,
  height = 6
)

expected_victimization2 <-tribble(
  ~offender_group, ~victim_group, ~N,
  "White", "White", round(sum(victimization_data$N[victimization_data$offender_group == "White"]) * national_census_data$nonhispanic_white / (national_census_data$total - national_census_data$other)),
  "White", "Black", round(sum(victimization_data$N[victimization_data$offender_group == "White"]) * national_census_data$nonhispanic_black / (national_census_data$total - national_census_data$other)),
  "White", "Hispanic", round(sum(victimization_data$N[victimization_data$offender_group == "White"]) * national_census_data$hispanic / (national_census_data$total - national_census_data$other)),
  "Black", "White", round(sum(victimization_data$N[victimization_data$offender_group == "Black"]) * national_census_data$nonhispanic_white / (national_census_data$total - national_census_data$other)),
  "Black", "Black", round(sum(victimization_data$N[victimization_data$offender_group == "Black"]) * national_census_data$nonhispanic_black / (national_census_data$total - national_census_data$other)),
  "Black", "Hispanic", round(sum(victimization_data$N[victimization_data$offender_group == "Black"]) * national_census_data$hispanic / (national_census_data$total - national_census_data$other)),
  "Hispanic", "White", round(sum(victimization_data$N[victimization_data$offender_group == "Hispanic"]) * national_census_data$nonhispanic_white / (national_census_data$total - national_census_data$other)),
  "Hispanic", "Black", round(sum(victimization_data$N[victimization_data$offender_group == "Hispanic"]) * national_census_data$nonhispanic_black / (national_census_data$total - national_census_data$other)),
  "Hispanic", "Hispanic", round(sum(victimization_data$N[victimization_data$offender_group == "Hispanic"]) * national_census_data$hispanic / (national_census_data$total - national_census_data$other))
)
expected_victimization2$offender_group <- factor(
  victimization_data$offender_group,
  levels = c("White", "Black", "Hispanic")
)
expected_victimization2$victim_group <- factor(
  victimization_data$victim_group,
  levels = c("Hispanic", "Black", "White")
)

ggplot(data = expected_victimization2, aes(x = offender_group, y = N, fill = victim_group)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(
    aes(label = scales::comma(N)),
    position = position_stack(vjust = 0.5)
  ) +
  theme_bw() +
  ggtitle(
    "Expected racial/ethnic patterns of victimization in the US if offending were random at the national level",
    subtitle = "(keeping both the total number of violent crimes and the offending rates by race/ethnicity fixed)"
  ) +
  xlab("Race/ethnicity of offender") +
  ylab("Number of victimizations") +
  scale_y_continuous(label = comma) +
  scale_fill_brewer(
    name = "Race/ethnicity of victim",
    palette = "Set3"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    legend.title = element_text(face = "bold")
  ) +
  labs(caption = "Source : Bureau of Justice Statistics (National Crime Victimization Survey) - Chart by Philippe Lemoine (@phl43)")

ggsave(
  "Expected racial-ethnic patterns of victimization in the US - Model 2.png",
  width = 12,
  height = 6
)

exposure_by_group1 <- neighborhood_census_data %>%
  mutate(
    percent_total_nonhispanic_white = nonhispanic_white / national_census_data$nonhispanic_white,
    percent_total_nonhispanic_black = nonhispanic_black / national_census_data$nonhispanic_black,
    percent_total_hispanic = hispanic / national_census_data$hispanic
  ) %>%
  summarize(
    white_exposure_white = weighted.mean(nonhispanic_white / (total - other), percent_total_nonhispanic_white),
    white_exposure_black = weighted.mean(nonhispanic_black / (total - other), percent_total_nonhispanic_white),
    white_exposure_hispanic = weighted.mean(hispanic / (total - other), percent_total_nonhispanic_white),
    black_exposure_white = weighted.mean(nonhispanic_white / (total - other), percent_total_nonhispanic_black),
    black_exposure_black = weighted.mean(nonhispanic_black / (total - other), percent_total_nonhispanic_black),
    black_exposure_hispanic = weighted.mean(hispanic / (total - other), percent_total_nonhispanic_black),
    hispanic_exposure_white = weighted.mean(nonhispanic_white / (total - other), percent_total_hispanic),
    hispanic_exposure_black = weighted.mean(nonhispanic_black / (total - other), percent_total_hispanic),
    hispanic_exposure_hispanic = weighted.mean(hispanic / (total - other), percent_total_hispanic)
  )

exposure_by_group2 <- tribble(
  ~group, ~exposure_group, ~proportion,
  "White", "White", exposure_by_group1$white_exposure_white,
  "White", "Black", exposure_by_group1$white_exposure_black,
  "White", "Hispanic", exposure_by_group1$white_exposure_hispanic,
  "Black", "White", exposure_by_group1$black_exposure_white,
  "Black", "Black", exposure_by_group1$black_exposure_black,
  "Black", "Hispanic", exposure_by_group1$black_exposure_hispanic,
  "Hispanic", "White", exposure_by_group1$hispanic_exposure_white,
  "Hispanic", "Black", exposure_by_group1$hispanic_exposure_black,
  "Hispanic", "Hispanic", exposure_by_group1$hispanic_exposure_hispanic
)
exposure_by_group2$group <- factor(
  exposure_by_group2$group,
  levels = c("White", "Black", "Hispanic")
)
exposure_by_group2$exposure_group <- factor(
  exposure_by_group2$exposure_group,
  levels = c("Hispanic", "Black", "White")
)

ggplot(data = exposure_by_group2, aes(x = group, y = proportion, fill = exposure_group)) +
  geom_bar(position = "fill", stat = "identity", color = "black") +
  geom_text(
    aes(label = scales::percent(proportion, accuracy = 0.1)),
    #size = 5,
    position = position_stack(vjust = 0.5)
  ) +
  theme_bw() +
  ggtitle(
    "Average racial/ethnic makeup of neighborhoods by race/ethnicity in the US (2020)"
  ) +
  xlab("Group") +
  ylab("Neighborhood composition") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(
    name = "Group",
    palette = "Set3"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    legend.title = element_text(face = "bold")
  ) +
  labs(caption = "Source : Census Bureau (Census Redistricting Data) - Chart by Philippe Lemoine (@phl43)")

ggsave(
  "Average Racial-ethnic makeup of neighborhoods by race-ethnicity in the US (2020).png",
  width = 12,
  height = 6
)

expected_victimization3 <-tribble(
  ~offender_group, ~victim_group, ~N,
  "White", "White", round(sum(victimization_data$N[victimization_data$offender_group == "White"]) * exposure_by_group1$white_exposure_white),
  "White", "Black", round(sum(victimization_data$N[victimization_data$offender_group == "White"]) * exposure_by_group1$white_exposure_black),
  "White", "Hispanic", round(sum(victimization_data$N[victimization_data$offender_group == "White"]) * exposure_by_group1$white_exposure_hispanic),
  "Black", "White", round(sum(victimization_data$N[victimization_data$offender_group == "Black"]) * exposure_by_group1$black_exposure_white),
  "Black", "Black", round(sum(victimization_data$N[victimization_data$offender_group == "Black"]) * exposure_by_group1$black_exposure_black),
  "Black", "Hispanic", round(sum(victimization_data$N[victimization_data$offender_group == "Black"]) * exposure_by_group1$black_exposure_hispanic),
  "Hispanic", "White", round(sum(victimization_data$N[victimization_data$offender_group == "Hispanic"]) * exposure_by_group1$hispanic_exposure_white),
  "Hispanic", "Black", round(sum(victimization_data$N[victimization_data$offender_group == "Hispanic"]) * exposure_by_group1$hispanic_exposure_black),
  "Hispanic", "Hispanic", round(sum(victimization_data$N[victimization_data$offender_group == "Hispanic"]) * exposure_by_group1$hispanic_exposure_hispanic)
)
expected_victimization3$offender_group <- factor(
  victimization_data$offender_group,
  levels = c("White", "Black", "Hispanic")
)
expected_victimization3$victim_group <- factor(
  victimization_data$victim_group,
  levels = c("Hispanic", "Black", "White")
)

ggplot(data = expected_victimization3, aes(x = offender_group, y = N, fill = victim_group)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(
    aes(label = scales::comma(N)),
    position = position_stack(vjust = 0.5)
  ) +
  theme_bw() +
  ggtitle(
    "Expected racial/ethnic patterns of victimization in the US if offending were random at the neighborhood level",
    subtitle = "(keeping both the total number of violent crimes and the offending rates by race/ethnicity fixed)"
  ) +
  xlab("Race/ethnicity of offender") +
  ylab("Number of victimizations") +
  scale_y_continuous(label = comma) +
  scale_fill_brewer(
    name = "Race/ethnicity of victim",
    palette = "Set3"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    legend.title = element_text(face = "bold")
  ) +
  labs(caption = "Source : Bureau of Justice Statistics (National Crime Victimization Survey) - Chart by Philippe Lemoine (@phl43)")

ggsave(
  "Expected racial-ethnic patterns of victimization in the US - Model 3.png",
  width = 12,
  height = 6
)

expected_victimization4 <-tribble(
  ~offender_group, ~victim_group, ~N,
  "White", "White", round(sum(victimization_data$N) * national_census_data$nonhispanic_white / (national_census_data$total - national_census_data$other) * exposure_by_group1$white_exposure_white),
  "White", "Black", round(sum(victimization_data$N) * national_census_data$nonhispanic_white / (national_census_data$total - national_census_data$other) * exposure_by_group1$white_exposure_black),
  "White", "Hispanic", round(sum(victimization_data$N) * national_census_data$nonhispanic_white / (national_census_data$total - national_census_data$other) * exposure_by_group1$white_exposure_hispanic),
  "Black", "White", round(sum(victimization_data$N) * national_census_data$nonhispanic_black / (national_census_data$total - national_census_data$other) * exposure_by_group1$black_exposure_white),
  "Black", "Black", round(sum(victimization_data$N) * national_census_data$nonhispanic_black / (national_census_data$total - national_census_data$other) * exposure_by_group1$black_exposure_black),
  "Black", "Hispanic", round(sum(victimization_data$N) * national_census_data$nonhispanic_black / (national_census_data$total - national_census_data$other) * exposure_by_group1$black_exposure_hispanic),
  "Hispanic", "White", round(sum(victimization_data$N) * national_census_data$hispanic / (national_census_data$total - national_census_data$other) * exposure_by_group1$hispanic_exposure_white),
  "Hispanic", "Black", round(sum(victimization_data$N) * national_census_data$hispanic / (national_census_data$total - national_census_data$other) * exposure_by_group1$hispanic_exposure_black),
  "Hispanic", "Hispanic", round(sum(victimization_data$N) * national_census_data$hispanic / (national_census_data$total - national_census_data$other) * exposure_by_group1$hispanic_exposure_hispanic)
)
expected_victimization4$offender_group <- factor(
  victimization_data$offender_group,
  levels = c("White", "Black", "Hispanic")
)
expected_victimization4$victim_group <- factor(
  victimization_data$victim_group,
  levels = c("Hispanic", "Black", "White")
)

ggplot(data = expected_victimization4, aes(x = offender_group, y = N, fill = victim_group)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(
    aes(label = scales::comma(N)),
    position = position_stack(vjust = 0.5)
  ) +
  theme_bw() +
  ggtitle(
    "Expected racial/ethnic patterns of victimization in the US if offending were random at the neighborhood level",
    subtitle = "(keeping only the total number of crimes fixed)"
  ) +
  xlab("Race/ethnicity of offender") +
  ylab("Number of victimizations") +
  scale_y_continuous(label = comma) +
  scale_fill_brewer(
    name = "Race/ethnicity of victim",
    palette = "Set3"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    legend.title = element_text(face = "bold")
  ) +
  labs(caption = "Source : Bureau of Justice Statistics (National Crime Victimization Survey) - Chart by Philippe Lemoine (@phl43)")

ggsave(
  "Expected racial-ethnic patterns of victimization in the US - Model 4.png",
  width = 12,
  height = 6
)
