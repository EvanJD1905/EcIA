#EcIA Results

library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)

North_Inverts<- read_excel("~/Desktop/Masters/EIA/Arran fieldcourse.xlsx", sheet="North side invert transects")                  #inv
South_Inverts<- read_excel("~/Desktop/Masters/EIA/Arran fieldcourse.xlsx", sheet="South side invert transects")                  #inv
Moth<- read_excel("~/Desktop/Masters/EIA/Arran fieldcourse.xlsx", sheet="N+S Moth traps")                  #inv
Stream<- read_excel("~/Desktop/Masters/EIA/Arran fieldcourse.xlsx", sheet="N+S Stream inverts")                  #inv
Bog<- read_excel("~/Desktop/Masters/EIA/Arran fieldcourse.xlsx", sheet="BOG")                  #inv
Verts<- read_excel("~/Desktop/Masters/EIA/Arran fieldcourse.xlsx", sheet="Vertebrates")          #V
Incidentals <- read_excel("~/Desktop/Masters/EIA/Arran fieldcourse.xlsx", sheet="Incidentals")     #V
Tech <- read_excel("~/Desktop/Masters/EIA/Arran fieldcourse.xlsx", sheet="Vertebrates (tech)")    #V


# Comparison between Invertebrate Orders at both sites

N_North_Inverts <- North_Inverts %>%
  group_by(order) %>%
  summarize(totalCount = sum(individualCount), .groups = "drop")

N_South_Inverts <- South_Inverts %>%
  group_by(order) %>%
  summarize(totalCount = sum(individualCount), .groups = "drop")

N_North_Inverts$Site <- "Site A"
N_South_Inverts$Site <- "Site B"

combined_data <- rbind(N_North_Inverts, N_South_Inverts)

combined_data <- combined_data %>%
  complete(order, Site, fill = list(totalCount = 0))
combined_data <- combined_data %>%
  mutate(barWidth = ifelse(totalCount == 0, 0.5, 1))

#Plotting results
ggplot(combined_data, aes(x = order, y = totalCount, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Invertebrate Counts by Order and Site",
       x = "Order",
       y = "Total Count") +
  scale_fill_manual(values = c("Site B" = "deepskyblue2", "Site A" = "firebrick2")) +
  theme_minimal()

ggsave("Invertebrates_SiteComparison.JPEG", width = 10, height= 6, dpi=300)



#Comparison between Vertebrates at both sites

N_Verts <- Verts %>%
  group_by(order, site) %>%
  summarize(orderCount = n(), .groups = "drop")

N_Incidentals <- Incidentals %>%
  group_by(order, site) %>%
  summarize(orderCount = n(), .groups = "drop")

N_Tech <- Tech %>%
  group_by(order, site) %>%
  summarize(orderCount = n(), .groups = "drop")

combined_data <- bind_rows(N_Verts, N_Incidentals, N_Tech)

combined_data <- combined_data %>%
  mutate(site = case_when(
    site == "North" ~ "Site A",
    site == "South" ~ "Site B",
  ))

combined_data <- combined_data %>%
  complete(order, site, fill = list(orderCount = 0))
combined_data <- combined_data %>%
  mutate(barWidth = ifelse(orderCount == 0, 0.5, 1))

#Plotting results

ggplot(combined_data, aes(x = order, y = orderCount, fill = site)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Vertebrate Species Counts by Order and Region",
       x = "Order",
       y = "Total Species Count") +
  scale_fill_manual(values = c("Site B" = "deepskyblue2", "Site A" = "firebrick2")) +
  theme_minimal()





