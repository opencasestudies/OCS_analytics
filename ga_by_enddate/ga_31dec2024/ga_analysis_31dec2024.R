# =======================================
# Title: GA Report - Data By December 31, 2024
# Description: Performs analysis on Google Analytics data by December 31, 2024
#              This script can be reused each time this analysis is reran.
# Author: Qier Meng
# Date: Jan 26, 2024
# =======================================

library(tidyverse)
library(googleAnalyticsR)
library(fuzzyjoin)

# update the following items each time running this script

analy_enddate <- "2024-12-31"
output_folder <- "OCS_analytics/ga_by_enddate/ga_31dec2024"
ocs_property_id <- 256923228

case_study_codelist <- data.frame(
  topic = c("Obesity",
            "Air Pollution",
            "Vaping",
            "Opiods",
            "RTC Wrangling",
            "RTC Analysis",
            "Youth Disconnection",
            "Mental Health",
            "School Shootings",
            "CO2 Emissions",
            "Diet"),
  path = c("ocs-bp-rural-and-urban-obesity", 
           "ocs-bp-air-pollution",
           "ocs-bp-vaping-case-study",
           "ocs-bp-opioid-rural-urban",
           "ocs-bp-RTC-wrangling", 
           "ocs-bp-RTC-analysis", 
           "ocs-bp-youth-disconnection",
           "ocs-bp-youth-mental-health", 
           "ocs-bp-school-shootings-dashboard",
           "ocs-bp-co2-emissions",
           "ocs-bp-diet"),
  title = c("Exploring global patterns of obesity across rural and urban regions",
            "Predicting Annual Air Pollution",
            "Vaping Behaviors in American Youth",
            "Opioids in United States",
            "Influence of Multicollinearity on Measured Impact of Right-to-Carry Gun Laws Part 1", 
            "Influence of Multicollinearity on Measured Impact of Right-to-Carry Gun Laws",
            "Disparities in Youth Disconnection",
            "Mental Health of American Youth",
            "School Shootings in the United States",
            "Exploring CO2 emissions across time",
            "Exploring global patterns of dietary behaviors associated with health risk")
)

# -----------------------------
# Plot sessions - overall
# -----------------------------

# get GA sessions
df_sessions <- ga_data(
  propertyId = ocs_property_id,
  date_range = c("2021-01-01", analy_enddate),
  metrics = "sessions",
  dimensions = "date", 
  limit = -1
)

# calculate cumulative sessions by day
df_sessions_cumulative <- df_sessions %>%
  arrange(date) %>%
  mutate(cumulativeSessions = cumsum(sessions))

# mark the last day of each year and the last day on the plot
annotation_points <- df_sessions_cumulative %>%
  mutate(year = format(date, "%Y")) %>%
  group_by(year) %>%
  filter(date == max(date[date <= as.Date(paste0(year, "-12-31"))])) %>%
  ungroup() %>%
  mutate(
    label = paste(format(date, "%b %d, %Y"), ":", cumulativeSessions)  # format the labels
  )

# make ths plot
plot_session_overall <- ggplot(df_sessions_cumulative, aes(x = date, y = cumulativeSessions)) +
  geom_line(color = "#F8766D") +
  geom_point(data = annotation_points,
             aes(x = date, y = cumulativeSessions),
             color = "#F8766D",
             size = 2) +
  geom_text(data = annotation_points,
            aes(x = date, y = cumulativeSessions, label = label),
            hjust = 1.1,
            vjust = 0,
            color = "black",
            size = 3) +
  labs(title = "Cumulative Sessions (Daily) Over Time - All Case Studies",
       subtitle = paste0("Jan 1, 2021 - ", format(as.Date(analy_enddate), "%b %d, %Y")),
       x = "Date",
       y = "Cumulative Sessions",
       caption = str_wrap(
         paste0("Statistics sourced from Google Analytics, employing 'sessions' as metrics and 'date' as dimensions. ", 
                "Note that statistics using alternative analytical parameters may vary, and number of sessions reported through the Google Analytics interface tend to be higher. ", 
                "Furthermore, these statistics may also underestimate the total engagement, ", 
                "as historical data from interactive case studies previously hosted on RStudio Connect may not be included due to the host no longer being available."),
         width = 158)
       ) +
         theme_minimal() +
  theme(plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption.position = "plot",
        plot.caption = element_text(face = "italic", hjust = 0, size = 8),
        plot.background = element_rect(fill = "white"))

# save the plot
ggsave(file.path(output_folder, "plot_session_overall.png"),
       plot = plot_session_overall,
       width = 8, height = 5)

# -----------------------------
# Plot sessions - by case study
# Data here needs to be examined before being used for generating plots
# -----------------------------

# identify case studies either by name or url
df_topics <- ga_data(ocs_property_id,
                     date_range = c("2021-01-01", analy_enddate),
                     metrics = "sessions",
                     dimensions = c("pagePath", "pageTitle"),
                     limit = -1)

# Assign topic based on either URL or page title (join df_topics with codelist)
# Join condition 1: if the URL contains the path in the codelist
# Join condition 2: if the page title contains the title in the codelist (not case sensitive)

# define functions to do the matching (vectorized to be used in the later fuzzy join)
match_path <- Vectorize(function(x, y) grepl(y, x))
match_title <- Vectorize(function(x, y) grepl(y, x, ignore.case = TRUE))

# first join: match based on path
joined_path <- df_topics %>%
  fuzzy_left_join(case_study_codelist, 
                  by = c("pagePath" = "path"),
                  match_fun = list(match_path)) %>%
  select(pagePath, pageTitle, topic_path = topic)

# second join: match based on title
joined_title <- df_topics %>%
  fuzzy_left_join(case_study_codelist, 
                  by = c("pageTitle" = "title"),
                  match_fun = list(match_title)) %>%
  select(pagePath, pageTitle, topic_title = topic)

# RTC part 1 was matched twice because part 2 is it's sub string, deal with this
joined_title <- joined_title %>%
  fuzzy_left_join(case_study_codelist %>% filter(str_detect(title, "Part 1")) %>% rename(title1 = title),
                  by = c("pageTitle" = "title1"),
                  match_fun = list(match_title)) %>%
  filter(is.na(topic) | topic_title == topic) %>%
  select(pagePath, pageTitle, topic_title)

# combine the results
df_topics_final <- df_topics %>%
  left_join(joined_path, by = c("pagePath", "pageTitle")) %>%
  left_join(joined_title, by = c("pagePath", "pageTitle")) %>%
  # get topic based on either path or title
  mutate(topic = coalesce(topic_path, topic_title)) %>%
  select(-topic_path, -topic_title)

# check - make sure the final data is the same as the starting one
# library(diffdf)
# diffdf(df_topics, df_topics_final %>% select(pagePath, pageTitle, sessions))
# should have no difference

# count sessions by topics
df_sessions_by_topics <- df_topics_final %>%
  filter(!is.na(topic)) %>%
  group_by(topic) %>%
  summarise(total_sessions = sum(sessions),
            .groups = "drop") %>%
  arrange(desc(total_sessions))

# calculate the maximum x-axis limit with padding
max_sessions <- max(df_sessions_by_topics$total_sessions)
x_axis_limit <- max_sessions * 1.2  # add 20% padding

# create the horizontal bar chart
plot_session_bycasestudy <- ggplot(df_sessions_by_topics, aes(x = total_sessions, y = reorder(topic, total_sessions))) +
  geom_bar(stat = "identity", fill = "steelblue") +  # Create bars
  geom_text(aes(label = total_sessions), hjust = -0.2, size = 4) +  # add counts to the right
  labs(
    x = "Total Sessions", 
    y = "", 
    title = "Total Sessions by Case Study",
    subtitle = paste0("Jan 1, 2021 - ", format(as.Date(analy_enddate), "%b %d, %Y")),
    caption = str_wrap(
      paste0("Counts are obtained from sessions where the URL includes a unique identifier for each case study ('ocs-bp-[casestudytitle]'), or the page title contains the case study topic in English. ", 
             "Note that these counts may be underestimated due to potential omissions: ", 
             "historical data from interactive case studies previously hosted on RStudio Connect may not be included since the host is no longer available, ", 
             "and page titles translated into other languages may not be counted."),
      width = 158)
  ) +
  xlim(0, x_axis_limit) +  # set x-axis limits with padding
  theme_minimal() + 
  theme(axis.text.y = element_text(size = 10, color = "black"),  # adjust y-axis text size
        plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption.position = "plot",
        plot.caption = element_text(face = "italic", hjust = 0, size = 8),
        plot.background = element_rect(fill = "white"))

# save the plot
ggsave(file.path(output_folder, "plot_session_bycasestudy.png"),
       plot = plot_session_bycasestudy,
       width = 8, height = 5)
