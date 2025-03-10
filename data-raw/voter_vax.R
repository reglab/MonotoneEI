## code to prepare voter_vax, the voting and vaccination dataset

# Election data
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ
election <- read.csv("data-raw/countypres_2000-2020.csv")
election <- election[election$year == 2020,]
election <- election %>%
  dplyr::group_by(county_name) %>%
  dplyr::filter(party == "REPUBLICAN") %>%
  dplyr::filter(mode == "TOTAL") %>%
  dplyr::ungroup() %>%
  dplyr::select(c("county_fips", "candidatevotes", "totalvotes"))
election$candidatevotes = election$candidatevotes/election$totalvotes

# Vaccine data
# https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh/data_preview
vaccine <- read.csv("data-raw/COVID-19_Vaccinations_in_the_United_States_County.csv")
vaccine <- vaccine %>%
  dplyr::select(c("FIPS", "Series_Complete_Pop_Pct"))

voter_vax <- merge(election, vaccine, by.x = "county_fips", by.y = "FIPS")
colnames(voter_vax) = c("county_fips", "prop_republican", "total_votes", "prop_covid_vax")

voter_vax <- voter_vax[rowSums(is.na(voter_vax)) == 0, ]
voter_vax$prop_covid_vax  = voter_vax$prop_covid_vax/100

voter_vax <- voter_vax %>%
  dplyr::select(prop_republican, prop_covid_vax, total_votes, county_fips)
usethis::use_data(voter_vax, overwrite = TRUE)
