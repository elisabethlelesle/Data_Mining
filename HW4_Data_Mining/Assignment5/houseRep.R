#R.version.string
#install.packages("pbkrtest")
#install.packages("car")
# Load required packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(car)
library(stats)
library(e1071)
library(class)
library(MASS)
library(nnet)
library(arules) # if needed for association, though may not be central here

# Load the dataset
house <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-11-07/house.csv')

# Examine structure and summary
str(house)
summary(house)
head(house)

# Filter only general elections (stage = "gen") to focus on final electoral outcomes
gen_house <- house %>%
  filter(stage == "GEN", !is.na(candidatevotes), !is.na(totalvotes)) 
gen_house <- gen_house %>% filter(candidatevotes > 0)

# Check the distribution of years and parties
table(gen_house$year)
table(gen_house$party)


winners <- gen_house %>%
  group_by(year, state, district) %>%
  summarise(candidate = candidate[which.max(candidatevotes)],
            candidatevotes_sum = max(candidatevotes),
            top_party = party[which.max(candidatevotes)],
            totalvotes = first(totalvotes),
            runoff = any(runoff, na.rm=TRUE),
            special = any(special, na.rm=TRUE),
            .groups = "drop")


# Check number of winners identified
nrow(winners)
head(winners)


# Count seats won by party each year
party_year <- winners %>%
  group_by(year, top_party) %>%
  summarise(seats = n()) %>%
  ungroup()

ggplot(party_year, aes(x=year, y=seats, color=top_party, group=top_party)) +
  geom_line() + geom_point() +
  labs(title="Number of Seats Won by Party Over Time", x="Year", y="Number of Seats")

# Distribution of total votes over time
#ggplot(winners, aes(x=year, y=totalvotes)) +
#  geom_boxplot() + 
#  labs(title="Distribution of Total Votes Cast Over Years", x="Year", y="Total Votes")


major_two <- winners %>%
  filter(top_party %in% c("DEMOCRAT", "REPUBLICAN"))

ggplot(major_two, aes(x=year, fill=top_party)) +
  geom_bar(position="fill") +
  labs(title="Share of Districts Won by Democrats vs. Republicans", x="Year", y="Proportion")

presidents_df <- data.frame(
  Start_Year = c(1977, 1981, 1989, 1993, 2001, 2009, 2017, 2021),
  End_Year   = c(1981, 1989, 1993, 2001, 2009, 2017, 2021, 2023),
  president  = c("CARTER", "REAGAN", "H. W. BUSH", "CLINTON", "W. BUSH", "OBAMA", "TRUMP", "BIDEN"),
  prez_party = c("Democrat","Republican","Republican","Democrat","Republican","Democrat","Republican","Democrat")
)

ggplot(major_two, aes(x=year, fill=top_party)) +
  geom_bar(position="fill") +
  labs(title="Share of Districts Won by Democrats vs. Republicans", x="Year", y="Proportion") +
  # Add a segment above the bars to show presidential terms
  geom_segment(data=presidents_df, 
               aes(x=Start_Year, xend=End_Year, y=1.05, yend=1.05, color=prez_party), 
               size=3, inherit.aes = FALSE, show.legend = FALSE) +
  # Add text labels for presidents
  geom_text(data=presidents_df, 
            aes(x=(Start_Year + End_Year)/2, y=1.1, label=president, color=prez_party), 
            size=3.5, fontface="bold", hjust=0.5, inherit.aes = FALSE, show.legend = FALSE) +
  # Map party colors for presidents
  scale_color_manual(values=c("Democrat"="#64B6EC", "Republican"="#FF8972")) +
  # Expand limits so we have room above 1.0 for the segments and text
  expand_limits(y=1.2) +
  # Allow drawing outside plot area if needed
  coord_cartesian(clip="off") +
  theme_minimal() +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title = element_text(face="bold"),
    plot.margin = unit(c(1, 4, 1, 1), "lines") # Add extra right margin if needed
  )


library(timeDate)
skewness(winners$candidatevotes_sum)
kurtosis(winners$candidatevotes_sum)


winners_numeric <- winners %>%
  mutate(runoff_num = as.numeric(runoff),
         special_num = as.numeric(special))

#cor(winners_numeric[, c("year","totalvotes", "runoff_num","special_num")], use="pairwise.complete.obs")


binary_data <- major_two %>%
  mutate(dem_win = ifelse(top_party=="democratic", 1, 0))

# Simple logistic regression: Predicting Democratic win based on year, total votes, special and runoff.
LR_model <- glm(dem_win ~ year + totalvotes + special + runoff, data=binary_data, family=binomial(link="logit"))
summary(LR_model)

# Predictions and accuracy
pred_prob <- predict(LR_model, type="response")
pred_class <- ifelse(pred_prob>0.5, 1, 0)
conf_mat <- table(binary_data$dem_win, pred_class)
conf_mat
accuracy <- sum(diag(conf_mat))/sum(conf_mat)
accuracy


