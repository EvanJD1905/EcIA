library(readxl)
library(dplyr)

#Reading the Scottish Biodiversity List Excel
file1 <- "~/Desktop/Masters/EIA/Scottish Biodiversity List.xls"
sheet1 <- "Terrestrial Species"
data1 <- read_excel(file1, sheet = sheet1)

#"~/Desktop/Masters/EIA/Arran fieldcourse.xlsx" was taken directly from shared excel document and unaltered.
North_Inverts<- read_excel("~/Desktop/Masters/EIA/Arran fieldcourse.xlsx", sheet="North side invert transects")                  #inv
South_Inverts<- read_excel("~/Desktop/Masters/EIA/Arran fieldcourse.xlsx", sheet="South side invert transects")                  #inv
Moth<- read_excel("~/Desktop/Masters/EIA/Arran fieldcourse.xlsx", sheet="N+S Moth traps")                  #inv
Stream<- read_excel("~/Desktop/Masters/EIA/Arran fieldcourse.xlsx", sheet="N+S Stream inverts")                  #inv
Bog<- read_excel("~/Desktop/Masters/EIA/Arran fieldcourse.xlsx", sheet="BOG")            
Verts<- read_excel("~/Desktop/Masters/EIA/Arran fieldcourse.xlsx", sheet="Vertebrates")            #V
Incidentals <- read_excel("~/Desktop/Masters/EIA/Arran fieldcourse.xlsx", sheet="Incidentals")     #V
Tech <- read_excel("~/Desktop/Masters/EIA/Arran fieldcourse.xlsx", sheet="Vertebrates (tech)")   #V


N_North_Inverts <- North_Inverts %>%
  group_by(scientificName, site) %>%
  summarize(speciesCount = n(), .groups = "drop")

N_South_Inverts <- South_Inverts %>%
  group_by(scientificName, site) %>%
  summarize(speciesCount = n(), .groups = "drop")

N_Moth <- Moth %>%
  group_by(scientificName, site) %>%
  summarize(speciesrCount = n(), .groups = "drop")

N_Stream <- Stream %>%
  group_by(scientificName, site) %>%
  summarize(speciesCount = n(), .groups = "drop")

N_Bog <- Bog %>%
  group_by(scientificName, site) %>%
  summarize(speciesCount = n(), .groups = "drop")

N_Verts <- Verts %>%
  group_by(scientificName, site) %>%
  summarize(speciesCount = n(), .groups = "drop")

N_Incidentals <- Incidentals %>%
  group_by(scientificName, site) %>%
  summarize(speciesCount = n(), .groups = "drop")

N_Tech <- Tech %>%
  group_by(scientificName, site) %>%
  summarize(speciesCount = n(), .groups = "drop")

combined_data <- bind_rows(N_North_Inverts, N_South_Inverts, N_Moth, N_Stream, N_Bog, N_Verts, N_Incidentals, N_Tech)
print(combined_data)

# Extracting the scientific names from both datasets
names1 <- data1 %>% pull(`Scientific Name`)
names2 <- combined_data %>% pull(scientificName)

# Finding the matches between the lists
Matches <- intersect(names1, names2)

print(Matches)



