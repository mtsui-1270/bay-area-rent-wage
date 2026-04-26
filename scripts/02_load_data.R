library(tidyverse)

# opens a file picker window
rent_raw <- read_csv(file.choose())
glimpse(rent_raw)

rent_raw %>% 
  filter(str_detect(RegionName, "San Francisco|Oakland|San Jose"))

# Filter and clean Bay Area rent data
rent_clean <- rent_raw %>%
  filter(str_detect(RegionName, "San Francisco|San Jose")) %>%
  select(RegionName, starts_with("20")) %>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "date",
    values_to = "rent"
  ) %>%
  mutate(date = as.Date(date))

# Check it looks right
glimpse(rent_clean)




dir.create("output/figures", recursive = TRUE)

ggplot(rent_clean, aes(x = date, y = rent, color = RegionName)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = "Bay Area Rent Trends (2015–2025)",
    subtitle = "Zillow Observed Rent Index",
    x = "Year",
    y = "Monthly Rent ($)",
    color = "Region"
  ) 
  theme_minimal()

ggsave(
  filename = "output/figures/bay_area_rent_trends.png",
  width = 10,
  height = 6,
  dpi = 300
)

bls_raw <- read_excel(file.choose())
glimpse(bls_raw)


# filter to San Francisco
bls_raw %>%
  filter(str_detect(AREA_TITLE, "San Francisco"))
glimpse(bls_raw)

colnames(bls_raw)

bls_clean <- bls_raw %>%
  filter(str_detect(AREA_TITLE, "San Francisco")) %>%
  filter(OCC_TITLE %in% c(
    "All Occupations",
    "Software Developers",
    "Registered Nurses",
    "Teachers, Postsecondary",
    "Retail Salespersons",
    "Food Preparation Workers"
  )) %>%
  select(AREA_TITLE, OCC_TITLE, A_MEDIAN, H_MEDIAN)

glimpse(bls_clean)
print(bls_clean)

bls_clean <- bls_clean %>%
  mutate(
    A_MEDIAN = as.numeric(A_MEDIAN),
    monthly_wage = A_MEDIAN / 12
  )

print(bls_clean)


# Get the most recent SF rent value
sf_rent_latest <- rent_clean %>%
  filter(str_detect(RegionName, "San Francisco")) %>%
  filter(date == max(date)) %>%
  pull(rent)

# Plot wages vs rent
ggplot(bls_clean, aes(x = reorder(OCC_TITLE, monthly_wage), y = monthly_wage)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = sf_rent_latest, color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = 1, y = sf_rent_latest + 200, 
           label = "Avg SF Rent", color = "red", hjust = 0) +
  scale_y_continuous(labels = scales::dollar) +
  coord_flip() +
  labs(
    title = "Monthly Wages vs. SF Rent (2024)",
    subtitle = "Red line = average San Francisco monthly rent",
    x = "Occupation",
    y = "Monthly ($)"
  ) +
  theme_minimal()



ggsave(
  filename = "output/figures/wages_vs_rent.png",
  width = 10,
  height = 6,
  dpi = 300
)
system("git --version")

system('git config --global user.name "Mariah Tsui"')
system('git config --global user.email "mariah.tsui@gmail.com"')
Sys.getenv("USERPROFILE")
system('git -C "C:/Users/maria/OneDrive/Documents/projects/bay-area-rent-wage" init')

writeLines(c(
  ".Rhistory",
  ".RData",
  ".Rproj.user"
), "C:/Users/maria/OneDrive/Documents/projects/bay-area-rent-wage/.gitignore")

system('git -C "C:/Users/maria/OneDrive/Documents/projects/bay-area-rent-wage" add .')
system('git -C "C:/Users/maria/OneDrive/Documents/projects/bay-area-rent-wage" commit -m "Initial commit: Bay Area rent vs wage analysis"')

