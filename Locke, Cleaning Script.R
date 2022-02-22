# Cleaning Script, Data Wrangling
# Bethany Locke

# Identifying all CSV files in folder, storing all files in list, combining data sets into one data set

trends <- list.files(path = "Raw Data", 
                     pattern = "trends_up_to_", full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows

# Reading in the Scorecard and IDName CSV files

Scorecard <- read.csv(file = 'Most+Recent+Cohorts+(Scorecard+Elements).csv')
IDName <- read.csv(file = 'id_name_link.csv')

# Removing duplicate university and/or college names (schname)

IDName <- IDName %>%
  group_by(schname) %>%
  mutate(N = n()) %>%
  filter(N == 1)

# Converting column names in Scorecard to match lowercase 
names(Scorecard) <- tolower(names(Scorecard))

# Combining IDName and trends into IDTRends by the school's name

IDTrends <- merge(x = IDName, y = trends, by = "schname")

# Filtering the Scorecard to display colleges/universities that predominantly grant Bachelor degrees

filter(Scorecard, preddeg == 3)
Scorecard <- Scorecard %>% select(unitid, opeid, preddeg, md_earn_wne_p10.reported.earnings) %>%
  filter(preddeg == 3)

# Merging the previous Scorecard with IDTrends as IDEarningsColleges before creating the binary

IDEarningsColleges <- merge(x = IDTrends, y = Scorecard, by = 'opeid')

# Creating the binary for high-earnings and low-earnings colleges/universities that predominantly grant Bachelor degrees. $60,000 is the identified median reported earnings that would set the binary.

IDEarningsColleges$earningshigh <- ifelse(IDEarningsColleges$md_earn_wne_p10.reported.earnings >= 60000, "1", "0")

# Standardizing IDEarningsColleges as TrendsBachelorMonthGrouped by month, opeid, and index and displaying the dates correctly using lubridate.

TrendsBachelorMonthGrouped <- IDEarningsColleges %>%
  mutate(date = as.Date(str_sub(monthorweek, 1, 10))) %>%
  group_by(schname, keyword) %>%
  mutate(index_std = (index - mean(index,na.rm = TRUE))/sd(index, na.rm = TRUE)) %>%
  group_by(month=floor_date(date, "month"), opeid) %>%
  summarize(index = mean(index, na.rm = TRUE))

# Joining Scorecard and IDName as BachelorIDData

BachelorIDData <- left_join(IDName, Scorecard, by = 'opeid')

# Officially cleaned data before dropping NA values

CleanOfficial <- left_join(TrendsBachelorMonthGrouped, BachelorIDData, by = 'opeid')

# Only looking at distinct columns from IDEarningsColleges
IDEarningsSort <- IDEarningsColleges %>% select(earningshigh, opeid)
DistinctIDEarningsSort <- distinct(IDEarningsSort, opeid, earningshigh)

# Dropping all NA values
CleanReal <- left_join(CleanOfficial, DistinctIDEarningsSort)
CleanReal <- CleanReal %>% drop_na()

# Exporting as a CSV to read into regression R markdown

write.csv(CleanReal,"C:/Users/betha/OneDrive/Econometrics/Data Exploration/CleanReal.csv", row.names = FALSE)

