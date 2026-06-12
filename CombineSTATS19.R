setwd("Annual Data")
library(dplyr) # Package for data transformation

# Load in STATS-19 data files

s19 = read.csv("Road Casualty Statistics 2019.csv")
s20 = read.csv("Road Casualty Statistics 2020.csv")
s21 = read.csv("Road Casualty Statistics 2021.csv")
s22 = read.csv("Road Casualty Statistics 2022.csv")
s23 = read.csv("Road Casualty Statistics 2023.csv")

# 2024 onwards changes "accident" to "collision" in variable names, we rename for backwards compatibility
s24 = read.csv("Road Casualty Statistics 2024.csv") |> 
  rename(accident_year = "collision_year",
         accident_severity = "collision_severity")
s25 = read.csv("Road Casualty Statistics 2025.csv") |> 
  rename(accident_year = "collision_year",
         accident_severity = "collision_severity")

# Identify the columns we wish to extract
vars = c("accident_year", "accident_severity", "number_of_casualties", 
         "local_authority_ons_district", "first_road_class", "road_type", 
         "speed_limit", "junction_detail", "urban_or_rural_area")

# A dictionary for converting LA codes to their actual place names
LA_df = read.csv("LACodes.csv")

# Converting STATS-19 entries to what they represent

roadtype_df = data.frame(RoadType = c(1, 2, 3, 6, 7, 9),
                         Type = c("Roundabout", "One-way Street", "Dual Carriageway",
                                  "Single Carriageway",
                                  "Slip Road", "Unknown"))
road_class_df = data.frame(
    RoadClass = 1 : 6,
    Class = c("Motorway", "A(M)", "A", "B", "C", "Unclassified")
  )

junctype_df = data.frame(
  JunctionType = c(-1, 0, 1, 2, 3, 5, 6, 7, 8, 9, 13, 16, 17, 18, 19, 99),
  JunctionDetail = c("Unknown", "Not at or within 20 metres of junction", "Roundabout", "Mini roundabout",
              "T or staggered junction", "Slip road", "Crossroads", "Junction more than four arms",
              "Using private drive or entrance", "Other junction", "T or staggered junction", "Crossroads",
              "Junction more than four arms", "Using private drive or entrance", "Other junction", "Unknown") 
)

# Extract the relevant columns from each year data file, and row bind together
all_stats19 = bind_rows(
  s19 |> select(all_of(vars)),
  s20 |> select(all_of(vars)),
  s21 |> select(all_of(vars)),
  s22 |> select(all_of(vars)),
  s23 |> select(all_of(vars)),
  s24 |> select(all_of(vars)),
  s25 |> select(all_of(vars))
) |> # Rename variables to be shorter/easier to read
  rename(Year = "accident_year", Severity = "accident_severity", Code = "local_authority_ons_district",
         Casualties = "number_of_casualties", RoadClass = "first_road_class", RoadType = "road_type", 
         SpeedLimit = "speed_limit", JunctionType = "junction_detail", Urban = "urban_or_rural_area") |> 
  # Add the LA place name and remove the Code after
  left_join(LA_df, by = "Code") |> 
  select(-Code) |> 
  # Add the road class definition and remove the numerical code after
  left_join(road_class_df, by = "RoadClass") |> 
  select(-RoadClass) |> 
  # Add the road type definition and remove the numerical code after
  left_join(roadtype_df, by = "RoadType") |> 
  select(-RoadType) |> 
  # Add the junction detail definition and remove the numerical code after
  left_join(junctype_df, by = "JunctionType") |> 
  select(-JunctionType) |> 
  # Reorder rows and columns for easy reading (doesn't affect the data itself)
  relocate(LA, Year, Severity, Casualties, Class, Type, SpeedLimit, JunctionDetail, Urban) |> 
  arrange(LA, Year)

# Convert Urban/Rural label to words
all_stats19$Urban = case_when(all_stats19$Urban == 1 ~ "Urban",
                              all_stats19$Urban == 2 ~ "Rural",
                              !(all_stats19$Urban %in% c(1, 2)) ~ "Unknown")

write.csv(all_stats19, "Raw STATS-19.csv", row.names = F)
