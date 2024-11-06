#necessary libraries
library(tidyverse)
library(scales)
library(dplyr)
library(janitor)
library(forcats)

# Exercise 1
# Loading the data
case_data <- read_csv("data/scdb-case.csv")
vote_data <- read_csv("data/scdb-vote.csv")

# Filtering the data (1 is voted with majority or plurality and 2 is dissent)
filtered_data <- vote_data %>%
  filter(vote %in% 1:2)

# Group and summarize
one_vote_margin_data <- filtered_data %>%
  group_by(caseId, term) %>%
  summarise(
    voted_with_majority_or_plurality = sum(vote == 1),
    dissent = sum(vote == 2)
  ) %>%
  ungroup() %>%
  mutate(one_vote_margin = abs(voted_with_majority_or_plurality - dissent) == 1)

# Calculate the percentage of one-vote margin cases for each term
percentage_data <- one_vote_margin_data %>%
  group_by(term) %>%
  summarise(
    total_cases = n_distinct(caseId),
    one_vote_margin_cases = sum(one_vote_margin)
  ) %>%
  mutate(percentage_one_vote_margin = (one_vote_margin_cases / total_cases) * 100)

# Visualization
ggplot(percentage_data, aes(x = term, y = percentage_one_vote_margin)) +
  geom_line(color = "orange") +
  labs(
    x = "Term (Year)",
    y = "Percentage of One-Vote Margin Cases",
    title = "Change in Percentage of One-Vote Margin Cases Over Time"
  ) +
  theme_light()

# Exercise 2
# Filtering specified issue areas
filtered_cases <- case_data %>%
  filter(issueArea %in% c(1, 2, 8, 12))

# Selecting current active judges
selected_justices <- c("JGRoberts", "CThomas", "SAAlito", "SSotomayor", "EKagan", "NMGorsuch", "BMKavanaugh", "ACBarrett")
justices_data <- vote_data %>%
  filter(justiceName %in% selected_justices) %>%
  semi_join(filtered_cases, by = "caseId") %>%
  drop_na()

# Ordering in seniority
seniority_data <- justices_data %>%
  group_by(justiceName) %>%
  summarise(first_term = min(term)) %>%
  rename(judge_name = justiceName) %>%
  mutate(seniority = 2023 - first_term) %>%
  select(judge_name, first_term, seniority)

justices_summary <- justices_data %>%
  group_by(justiceName) %>%
  summarise(
    total_cases = n(),
    conservative_votes = sum(direction == 1),
    percentage_conservative = (conservative_votes / total_cases) * 100
  )

combined_data_ex2 <- seniority_data %>%
  left_join(justices_summary, by = c("judge_name" = "justiceName")) %>%
  arrange(desc(seniority))

combined_data_ex2$judge_name <- factor(
  combined_data_ex2$judge_name,
  levels = seniority_data$judge_name[order(desc(seniority_data$seniority))]
)

# Set "JGRoberts" as the first category as Chief Justice
combined_data_ex2$judge_name <- fct_relevel(combined_data_ex2$judge_name, "JGRoberts")

# Create a bar plot with the updated order
ggplot(combined_data_ex2, aes(x = judge_name, y = percentage_conservative, fill = judge_name)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Conservative Voting Percentage in Specific Issue Areas",
    subtitle = "Issue areas: criminal procedure, civil rights, economic activity, and federal taxation",
    x = "Justice Name (in order of seniority) ->",
    y = "Percentage of Conservative Votes"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")

# Exercise 3
# Filter only orally argued decisions
filtered_ex3 <- case_data %>%
  filter(!is.na(dateDecision) & decisionType %in% c(1, 6, 7)) %>%
  mutate(
    dateDecision = as.Date(dateDecision, format = "%m/%d/%Y"),
    decision_month = month(dateDecision, label = TRUE)
  )

# Count the number of decisions announced in each calendar month
decisions_per_month <- filtered_ex3 %>%
  group_by(term, decision_month) %>%
  summarise(decisions = n(), .groups = 'drop') %>%
  complete(term, decision_month = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"), fill = list(decisions = 0)) %>%
  filter(!term %in% c(1802, 1811))

decisions_per_month$decision_month <- factor(decisions_per_month$decision_month, levels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"), ordered = TRUE)

# Visualization
ggplot(decisions_per_month, aes(x = decision_month, y = decisions)) +
  geom_bar(stat = "identity", fill = "pink") +
  labs(
    title = "Number of Decisions Announced in Calendar Months",
    subtitle = "(Decided after oral arguments)",
    x = "Decision Month",
    y = "Number of Decisions"
  ) +
  theme_light()

# Exercise 4
# Filter cases where the Court declared something unconstitutional
unconstitution <- case_data %>%
  filter(declarationUncon %in% c(2, 3, 4))

# Filter vote data for the above selected cases
vote_data_filtered <- vote_data %>%
  filter(caseId %in% unconstitution$caseId) %>%
  filter(!is.na(vote))

# Group by justice and calculate agreement and total votes
justice_agreement <- vote_data_filtered %>%
  group_by(justice) %>%
  summarise(
    total_votes = n(),
    agree_votes = sum(vote == 1),
    agree_percentage = (agree_votes / total_votes) * 100
  )

# Exclude justices with less than 30 votes
justice_agreement_filtered <- justice_agreement %>%
  filter(total_votes >= 30)

# Determine the top 10 and bottom 10 justices based on agreement percentage
top_10_justices <- justice_agreement_filtered %>%
  arrange(desc(agree_percentage)) %>%
  head(10)

bottom_10_justices <- justice_agreement_filtered %>%
  arrange(agree_percentage) %>%
  head(10)

# Create a combined visualization
combined_plot <- rbind(
  top_10_justices %>% mutate(position = "Top 10"),
  bottom_10_justices %>% mutate(position = "Bottom 10")
)

ggplot(combined_plot, aes(x = reorder(justice, agree_percentage), y = agree_percentage, fill = position)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Top 10 and Bottom 10 Justices by Agreement Percentage on Unconstitutionality",
    x = "Justice ID",
    y = "Agreement Percentage",
    fill = "Justice Group"
  ) +
  theme_light() +
  theme(legend.position = "top")
