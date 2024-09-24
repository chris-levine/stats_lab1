install.packages("readr")
install.packages("countrycode")

library("readr")
library("ggplot2")
library("dplyr")
library("countrycode")

# creating dataframes 

population <- read_csv("/Users/christopherlevine/Desktop/Stats_Inference/pop.csv")
life_expectancy <- read_csv("/Users/christopherlevine/Desktop/Stats_Inference/lex.csv")
daily_income <- read_csv("/Users/christopherlevine/Desktop/Stats_Inference/mincpcap_cppp.csv")
babies_per_woman <- read_csv("/Users/christopherlevine/Desktop/Stats_Inference/children_per_woman_total_fertility.csv")
child_mortality <- read_csv("/Users/christopherlevine/Desktop/Stats_Inference/child_mortality_0_5_year_olds_dying_per_1000_born.csv")
c02_emmissions <- read_csv("/Users/christopherlevine/Desktop/Stats_Inference/co2_pcap_cons.csv")
gdp_per_capita <- read_csv("/Users/christopherlevine/Desktop/Stats_Inference/gdppercapita_us_inflation_adjusted.csv")
total_health_spending <- read_csv("/Users/christopherlevine/Desktop/Stats_Inference/total_health_spending_per_person_us.csv")
pop_density <- read_csv("/Users/christopherlevine/Desktop/Stats_Inference/population_density_per_square_km.csv")
water_source <- read_csv("/Users/christopherlevine/Desktop/Stats_Inference/at_least_basic_water_source_overall_access_percent.csv")
murders <- read_csv("/Users/christopherlevine/Desktop/Stats_Inference/murder_total_deaths.csv")

# filtering data 

population_2010 <- population %>%
  select(country, "2010") %>%
  rename(population = "2010")

life_expectancy_2010 <- life_expectancy %>%
  select(country, "2010") %>%
  rename(life_expectancy = "2010")

daily_income_2010 <- daily_income %>%
  select(country, "2010") %>%
  rename(daily_income = "2010")

babies_per_woman_2010 <- babies_per_woman %>%
  select(country, "2010") %>%
  rename(babies_per_woman = "2010")

child_mortality_2010 <- child_mortality %>%
  select(country, "2010") %>%
  rename(child_mortality = "2010")

c02_emmissions_2010 <- c02_emmissions %>%
  select(country, "2010") %>%
  rename(c02_emmissions = "2010")

gdp_per_capity_2010 <- gdp_per_capita %>%
  select(country, "2010") %>%
  rename(gdp_per_capita = "2010")

total_health_spending_2010 <- total_health_spending %>%
  select(country, "2010") %>%
  rename(total_health_spending = "2010")

pop_density_2010 <- pop_density %>%
  select(country, "2010") %>%
  rename(pop_density = "2010")

water_source_2010 <- water_source %>%
  select(country, "2010") %>%
  rename(water_source = "2010")

murders_2010 <- murders %>%
  select(country, "2010") %>%
  rename(murders = "2010")

# merging the dataframes

merged_df <- merge(population_2010, life_expectancy_2010, by = "country")
merged_df <- merge(merged_df, daily_income_2010, by = "country")
merged_df <- merge(merged_df, babies_per_woman_2010, by = "country")
merged_df <- merge(merged_df, child_mortality_2010, by = "country")
merged_df <- merge(merged_df, c02_emmissions_2010, by = "country")
merged_df <- merge(merged_df, gdp_per_capity_2010, by = "country")
merged_df <- merge(merged_df, total_health_spending_2010, by = "country")
merged_df <- merge(merged_df, pop_density_2010, by = "country")
merged_df <- merge(merged_df, water_source_2010, by = "country")
final_df <- merge(merged_df, murders_2010, by = "country")

View(final_df)

# removing rows with missing information
final_df[final_df == ""] <- NA
final_df[final_df == "NA"] <- NA

cleaned_df <- final_df %>%
  filter(complete.cases(.))

sum(is.na(cleaned_df))

cleaned_df <- final_df %>%
  filter_all(all_vars(!is.na(.)))

View(cleaned_df)

# adding a row with country code
cleaned_df$continent <- countrycode(sourcevar = cleaned_df[, "country"],
                            origin = "country.name",
                            destination = "continent")

View(cleaned_df)

# adding code to fix size character denotation
text.to.num <- function(x){
  xx = as.character(x)
  res=rep(0, length(x))
  for(i in 1:length(x)){
    if(is.na(xx[i])){
      res[i]=NA
      next
    }
    if (grepl("M", xx[i], ignore.case = TRUE)) {
      res[i]=as.numeric(gsub("M", "", xx[i], ignore.case = TRUE)) * 1e6
    } else if (grepl("k", xx[i], ignore.case = TRUE)) {
      res[i]=as.numeric(gsub("k", "", xx[i], ignore.case = TRUE)) * 1e3
    } else {
      res[i]=as.numeric(xx[i])
    }
  }
  res
}

# applying it to population column
cleaned_df$population <- text.to.num(cleaned_df$population)

View(cleaned_df)

# creating a scatter plot for life expectancy vs. child mortality
ggplot(cleaned_df, aes(x = child_mortality, y = life_expectancy)) + 
  geom_jitter(width = 3, height = 3) +
  labs(title = "Life Expectancy vs Child Mortality",
       x = "Child Mortality",
       y = "Life Expectancy")
                                                                                
# creating a scatter plot with color and size parameters

ggplot(cleaned_df, aes(x = child_mortality, y = life_expectancy,color = continent, size = population)) +
                       geom_jitter(width = 3, height = 3, alpha= 0.75) +
                       labs(title = "Life Expectancy vs Child Mortality by Continent",
                       x = "Child Mortality",
                       y = "Life Expectancy",
                       size = "Population",
                       color = "Continent")

                       

                                                                                
                                                                                
                                                                            