library(tidyverse)

file_path <- '/Users/jacknugent/Downloads/Copy of r_biotech salary and company survey - 2024.csv'# put ya own file path here

data <- readr::read_csv(file_path)

data <- data |>#rename stuff to nicer names, they were horrible horrible before
  janitor::clean_names() |>
  rename(
    annual_base_salary = compensation_annual_base_salary_pay,
    years_experience = years_of_experience, 
    location = where_are_you_located)
names(data) #prints names so you can do your own analyses

top_two_locations <- data |>
  count(location, sort = TRUE) |>
  slice_head(n = 2) |> #take first two
  pull(location)

top_x_industries <- data|> #compares top x industries in wages across years experience etc
  count(biotech_sub_industry, sort = TRUE) |>
  slice_head(n = 2) |> #take first two
  pull(biotech_sub_industry)

#should be easy to add whatever you want to this graph to analyse whatever you want

data_filtered <- data |>
#  filter(location %in% top_two_locations) |> #feel free to adjust thes
  filter(biotech_sub_industry %in% top_x_industries)|>
  filter(years_experience < 25) #reduces the kind of garbage we get after 25 years from sparse data

p <- ggplot(data_filtered,aes(x = as.factor(years_experience),y = annual_base_salary,fill = biotech_sub_industry)) +
  geom_boxplot() +
  geom_smooth(aes(group = 1),se = TRUE,color='black') + #trendline thing 
  coord_cartesian(ylim = c(0, 350000)) +#makes the graph neater
  scale_y_continuous(labels = scales::dollar_format(scale = 1),
                     breaks = scales::pretty_breaks(n = 20)) #gives us more accurate numbers to look at and compare to the graph

print(p)