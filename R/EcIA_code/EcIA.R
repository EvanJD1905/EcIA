#Shannon Index Calculation + Order calculations
library(vegan)
library(readxl)
library(dplyr)
library(ggplot2)
library (stringr)

#"~/Desktop/Masters/EIA/Arran fieldcourse.xlsx" was taken directly from shared excel document and unaltered.
North_Inverts<- read_excel("~/Desktop/Masters/EIA/Arran fieldcourse.xlsx", sheet="North side invert transects")                  #inv
South_Inverts<- read_excel("~/Desktop/Masters/EIA/Arran fieldcourse.xlsx", sheet="South side invert transects")                  #inv
Moth<- read_excel("~/Desktop/Masters/EIA/Arran fieldcourse.xlsx", sheet="N+S Moth traps")                  #inv
Stream<- read_excel("~/Desktop/Masters/EIA/Arran fieldcourse.xlsx", sheet="N+S Stream inverts")                  #inv
Bog<- read_excel("~/Desktop/Masters/EIA/Arran fieldcourse.xlsx", sheet="BOG")               #inv
Verts<- read_excel("~/Desktop/Masters/EIA/Arran fieldcourse.xlsx", sheet="Vertebrates")            #V
Incidentals <- read_excel("~/Desktop/Masters/EIA/Arran fieldcourse.xlsx", sheet="Incidentals")     #V
Tech <- read_excel("~/Desktop/Masters/EIA/Arran fieldcourse.xlsx", sheet="Vertebrates (tech)")   #V

#Combined Data
N_North_Inverts <- North_Inverts %>%
  group_by(order, site) %>%
  summarize(orderCount = n(), .groups = "drop")

N_South_Inverts <- South_Inverts %>%
  group_by(order, site) %>%
  summarize(orderCount = n(), .groups = "drop")

N_Moth <- Moth %>%
  group_by(order, site) %>%
  summarize(orderCount = n(), .groups = "drop")

N_Stream <- Stream %>%
  group_by(order, site) %>%
  summarize(orderCount = n(), .groups = "drop")

N_Bog <- Bog %>%
  group_by(order, site) %>%
  summarize(orderCount = n(), .groups = "drop")

N_Verts <- Verts %>%
  group_by(order, site) %>%
  summarize(orderCount = n(), .groups = "drop")

N_Incidentals <- Incidentals %>%
  group_by(order, site) %>%
  summarize(orderCount = n(), .groups = "drop")

N_Tech <- Tech %>%
  group_by(order, site) %>%
  summarize(orderCount = n(), .groups = "drop")


combined_data <- bind_rows(N_North_Inverts, N_South_Inverts, N_Moth, N_Stream, N_Bog, N_Verts, N_Incidentals, N_Tech)
print(combined_data)

SiteAmoth_data <- combined_data %>%
  filter(site == "North") %>%
  select(order, orderCount)

SiteBmoth_data <- combined_data %>%
  filter(site == "South") %>%
  select(order, orderCount)


Site_A <- north_data %>%
  group_by(order) %>%
  summarize(totalCount = sum(orderCount)) %>%
  pull(totalCount)

Site_B <- south_data %>%
  group_by(order) %>%
  summarize(totalCount = sum(orderCount)) %>%
  pull(totalCount)


shannon_A <- diversity(Site_A, index = "shannon")
shannon_B <- diversity(Site_B, index = "shannon")

print(shannon_A)
print(shannon_B)


#Invertebrate hillside surveys

SiteAinverts_data <- N_North_Inverts %>%
  filter(site == "North") %>%
  select(order, orderCount)

SiteBinverts_data <- N_South_Inverts %>%
  filter(site == "South") %>%
  select(order, orderCount)

Site_A_inverts <- SiteAinverts_data %>%
  group_by(order) %>%
  summarize(totalCount = sum(orderCount)) %>%
  pull(totalCount)

Site_B_inverts <- SiteBinverts_data %>%
  group_by(order) %>%
  summarize(totalCount = sum(orderCount)) %>%
  pull(totalCount)

shannon_inverts_A <- diversity(Site_A_inverts, index = "shannon")
shannon_inverts_B <- diversity(Site_B_inverts, index = "shannon")

print(shannon_inverts_A)
print(shannon_inverts_B)


#Moth Traps

SiteAmoth_data <- N_Moth %>%
  filter(site == "North") %>%
  select(order, orderCount)

SiteBmoth_data <- N_Moth %>%
  filter(site == "South") %>%
  select(order, orderCount)

Site_A_moth <- SiteAmoth_data %>%
  group_by(order) %>%
  summarize(totalCount = sum(orderCount)) %>%
  pull(totalCount)

Site_B_moth <- SiteBmoth_data %>%
  group_by(order) %>%
  summarize(totalCount = sum(orderCount)) %>%
  pull(totalCount)

shannon_moth_A <- diversity(Site_A_moth, index = "shannon")
shannon_moth_B <- diversity(Site_B_moth, index = "shannon")

print(shannon_moth_A)
print(shannon_moth_B)


#Stream

SiteAstream_data <- N_Stream %>%
  filter(site == "North") %>%
  select(order, orderCount)

SiteBstream_data <- N_Stream %>%
  filter(site == "South") %>%
  select(order, orderCount)

Site_A_stream <- SiteAstream_data %>%
  group_by(order) %>%
  summarize(totalCount = sum(orderCount)) %>%
  pull(totalCount)

Site_B_stream <- SiteBstream_data %>%
  group_by(order) %>%
  summarize(totalCount = sum(orderCount)) %>%
  pull(totalCount)

shannon_stream_A <- diversity(Site_A_stream, index = "shannon")
shannon_stream_B <- diversity(Site_B_stream, index = "shannon")

print(shannon_stream_A)
print(shannon_stream_B)


#Vertebrates (combined, main vertebrates, incidentals and tech surveys)

N_Verts <- Verts %>%
  group_by(scientificName, site) %>%
  summarize(speciesCount = n(), .groups = "drop")
  
N_Incidentals <- Incidentals %>%
  group_by(scientificName, site) %>%
  summarize(speciesCount = n(), .groups = "drop")

N_Tech <- Tech %>%
  group_by(scientificName, site) %>%
  summarize(speciesCount = n(), .groups = "drop")

combined_verts_data <- bind_rows(N_Verts, N_Incidentals, N_Tech)

SiteAverts_data <- combined_verts_data %>%
  filter(site == "North") %>%
  select(scientificName, speciesCount)

SiteBverts_data <- combined_verts_data %>%
  filter(site == "South") %>%
  select(scientificName, speciesCount)

Site_A_verts <- SiteAverts_data %>%
  group_by(scientificName) %>%
  summarize(totalCount = sum(speciesCount)) %>%
  pull(totalCount)

Site_B_verts <- SiteBverts_data %>%
  group_by(scientificName) %>%
  summarize(totalCount = sum(speciesCount)) %>%
  pull(totalCount)

shannon_verts_A <- diversity(Site_A_verts, index = "shannon")
shannon_verts_B <- diversity(Site_B_verts, index = "shannon")

print(shannon_verts_A)
print(shannon_verts_B)


#Combining collected shannon index results into one chart 

shannon_data <- data.frame(
  order = c("Combined survey data", "Combined survey data", 
            "Hillside invertebrate surveys", "Hillside invertebrate surveys", 
            "Freshwater kick-sampling surveys", "Freshwater kick-sampling surveys", 
            "Moth trap surveys", "Moth trap surveys", 
            "Combined vertebrate surveys*", "Combined vertebrate surveys*"),
  Shannon_Index = c(shannon_A, shannon_B, 
                    shannon_inverts_A, shannon_inverts_B, 
                    shannon_stream_A, shannon_stream_B, 
                    shannon_moth_A, shannon_moth_B, 
                    shannon_verts_A, shannon_verts_B),
  Site = rep(c("Site A", "Site B"), length.out = 10) #Alternate allocation oF 'Site A' and 'Site B' correctly for colouring on chart  
)

shannon_data$order <- factor(shannon_data$order, 
                             levels = c("Combined survey data", "Hillside invertebrate surveys", 
                                        "Freshwater kick-sampling surveys", "Moth trap surveys", 
                                        "Combined vertebrate surveys*"))

(shannon_data)


  #Making a bar chart from these combined results

ggplot(shannon_data, aes(x = order, y = Shannon_Index, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge") +  
  labs(title = "Shannon Index by Survey Grouping and Site",
       x = "Survey Grouping",
       y = "Shannon Index") +
  scale_fill_manual(values = c("Site A" = "firebrick2", "Site B" = "deepskyblue2")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
ggsave("ShannonScores.JPEG", width = 10, height= 6, dpi=300)

  

#Working out the total number of orders
combined_data <- bind_rows(N_North_Inverts, N_South_Inverts, N_Moth, N_Stream, N_Bog, N_Verts, N_Incidentals, N_Tech)
orderss <- length(unique(combined_data$order))
print(orderss)

combinedv <-  bind_rows(N_Verts, N_Incidentals, N_Tech)
ordersv <- length(unique(combinedv$order))
print(ordersv)

combinedi <- bind_rows(N_North_Inverts, N_South_Inverts, N_Moth, N_Stream, N_Bog)
ordersi <- length(unique(combinedi$order))
print(ordersi)




# Total orders per site
site_orders <- combined_data %>%
  group_by(site) %>%
  summarize(total_orders = n_distinct(order))

print(site_orders)

total_orders_list <- combined_data %>%
  summarize(orders_list = unique(order))
total_orders_list

# Unique orders per site
site_orders_list <- combined_data %>%
  group_by(site) %>%
  summarize(orders_list = list(unique(order)))

write.csv(site_orders_list$orders_list, "orders_list.csv", row.names = FALSE)



