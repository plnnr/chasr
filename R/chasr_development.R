##### ToC #####
## Table of contents for CHAS data tables
# T1:   Income x race (x housing problems x tenure)
# T2:   Income x race (x severe housing problems x tenure)
# T3:   Income x housing problem severity, ordered (x tenure)
# T4:   Household type x household size x housing problems (x tenure)
# T5:   Income x elderly occupant x housing problems (x tenure)
# T6:   DISCONTINUED (Income x disability x housing problems)
# T7:   Income x cost burden x household type (x tenure)
# T8:   Income x cost burden x facilities (x tenure)
# T9:   Cost burden x race (x tenure)
# T10:  Income x persons per room x family type (x tenure)
# T11:  Detailed income x housing problems (x tenure)
# T12:  Income x year built x cost burden (x tenure)
# T13:  Income x year built x children (x tenure)
# T14A: Home price x bedrooms x facilities (VACANT FOR SALE)
# T14B: Home price x bedrooms x facilities (VACANT FOR RENT)
# T15A: Income x home value x bedrooms x facilities (owner with mortgage)
# T15B: Income x home value x bedrooms x facilities (owner without mortgage)
# T15C: Income x home value x bedrooms x facilities (renter)
# T16:  Income x household type x housing problems (x tenure)
# T17A: Asking price x units in structure (FOR SALE)
# T17B: Rent x units in structure (RENTALS)
# T18A: Income x home value x units in structure (owner with a mortgage)
# T18B: Income x home value x units in structure (owner without a mortage)
# T18C: Income x rent (cost burden) x units in structure (renter)

##### Utility functions #####

library(tidyverse)
library(scales)
library(rio)
library(purrr)

# Function to cast your CHAS data from wide to long format (NEEDS WORK TO INCORPORATE 2009-2012 data -- see end of document for beta version)
cast_chas_long <- function(chas_table, header_vars = 7) {
  # header_vars is the number of variables that serve as headers before the actual data starts
  ## If you used the cleanup_chas_*() functions, then the default value is 7
  require(tidyr)
  fields_to_melt <- names(chas_table[,as.numeric(header_vars+1):length(names(chas_table))])
  melted <- gather(chas_table, fields_to_melt, key = 'variable', value = 'values')
  melted <- separate(data = melted, col = variable, into = c('table', 'variable'))
  melted <- separate(data = melted, col = variable, into = c('var_type', 'var_num'), sep = 3)
  melted$var_num <- paste0('v',melted$var_num)
  melted <- spread(melted, var_type, values)
  melted$moesq <- melted$moe * melted$moe
  return(melted)
}


# Function to cleanup tract-level CHAS data
cleanup_chas_tract <- function(chas_tract_data, year = 2015) {
  refcols <- c('source', 'sumlevel', 'geoid', 'geoid2', 'name', 'st', 'cnty_plc')
  chas_tract_data <- chas_tract_data %>% select(-tract)
  names(chas_tract_data)[names(chas_tract_data) == 'cnty'] <- 'cnty_plc'
  chas_tract_data$geoid2 <- substr(chas_tract_data$geoid, 8, 18)
  chas_tract_data <- chas_tract_data[, c(refcols, setdiff(names(chas_tract_data), refcols))]
  return(chas_tract_data)
}

# Function to cleanup state-level CHAS data
cleanup_chas_st <- function(chas_st_data, year = 2015) {
  refcols <- c('source', 'sumlevel', 'geoid', 'geoid2', 'name', 'st', 'cnty_plc')
  chas_st_data$cnty_plc <- ''
  chas_st_data$geoid2 <- substr(chas_st_data$geoid, 8, 10)
  chas_st_data <- chas_st_data[, c(refcols, setdiff(names(chas_st_data), refcols))]
  return(chas_st_data)
}

# Function to cleanup place-level CHAS data
cleanup_chas_plc <- function(chas_plc_data, year = 2015) {
  refcols <- c('source', 'sumlevel', 'geoid', 'geoid2', 'name', 'st', 'cnty_plc')
  names(chas_plc_data)[names(chas_plc_data) == 'place'] <- 'cnty_plc'
  chas_plc_data$geoid2 <- substr(chas_plc_data$geoid, 8, 14)
  chas_plc_data <- chas_plc_data[, c(refcols, setdiff(names(chas_plc_data), refcols))]
  return(chas_plc_data)
}

# Function to cleanup county-level CHAS data
cleanup_chas_cnty <- function(chas_cnty_data, year = 2015) {
  refcols <- c('source', 'sumlevel', 'geoid', 'geoid2', 'name', 'st', 'cnty_plc')
  names(chas_cnty_data)[names(chas_cnty_data) == 'cnty'] <- 'cnty_plc'
  chas_cnty_data$geoid2 <- substr(chas_cnty_data$geoid, 8, 13)
  chas_cnty_data <- chas_cnty_data[, c(refcols, setdiff(names(chas_cnty_data), refcols))]
  return(chas_cnty_data)
}

# Function to cast cleaned up long-format chas data to wide format
cast_chas_wide <- function(long_cleaned_chas_table) {
  long_cleaned_chas_table <- long_cleaned_chas_table %>%
    gather(variable, value, -(source:var_num)) %>%
    unite(temp, var_num, variable) %>%
    spread(temp, value)
  return(long_cleaned_chas_table)
}

##### GEOID lists for interesting geographies #####
msa_geoid2_vector <- c('41005', '41009', '41051', '41067', '41071', '53011', '53059')
orwa_place_vector <- c('4159000', '4123850', '4164900', '4131250', '4105350', '4134100', '4105800', 
                     '4147000', '4173650', '4101650', '4140550', '4155200', '4145000', '4174950', 
                     '4180150', '4148650', '4182800', '4126200', '4183750', '4105950', '4152100', 
                     '4153900', '4167100', '4141650', '4110750', '4172950', '4112150', '4174850', 
                     '4132050', '4129000', '4164600', '4153988', '4165250', '4124250', '4117800', 
                     '4163010', '4112050', '4115550', '4180900', '4134900', '4160900', '4165500', 
                     '5304475', '5309480', '5336710', '5358410', '5374060', '5376405', '5379625')
peer_city_list <- c('0666000', '0667000', '4805000', '1836003', '0820000', '4752006', '2743000', '2758000', '4967000')
sw_corridor_list <- c('41051005800', '41051005900', '41051006001', '41051006002', '41051006200', '41051006402', '41051006403', 
                      '41051006404', '41051006501', '41051006502', '41051006602', '41005020302', '41005020304', '41005020401', 
                      '41067030600', '41067030700', '41067030801', '41067030806', '41067030900', '41067031912', '41067032005')
icura_list <- c('41051002401' , '41051002402' , '41051002501' , '41051002502' , '41051003100' , '41051003200' , '41051003301' , 
                '41051003302' , '41051003401' , '41051003402' , '41051003501' , '41051003502' , '41051003601' , '41051003602' , 
                '41051003701' , '41051003702' , '41051003801' , '41051003802' , '41051003803' , '41051003901' , '41051003902' , 
                '41051002303' , '41051002203')
swcpdx_list <- c('41051005800', '41051005900', '41051006001', '41051006002', '41051006200', '41051006402', '41051006403', 
                 '41051006404', '41051006501', '41051006502', '41051006602')
rossi_fips1 <- c('41051007800', '41051007900', '41051009502')
rossi_fips2 <- c('41051008001', '41051008002', '41051009501', '41051009400', '41051007800', '41051007900', '41051009502')

place_vector <- c(peer_city_list, orwa_place_vector)

pp_areas_df <- import("N:/work/district_planning/Economic Development/NICK/Resources/Shapefiles/PHBHousingReportStudyAreas/PHBHousingReportStudyAreas.dbf")
div_122_fips_list <- select(filter(pp_areas_df, Analysis_A == "122nd-Division"), FIPS)[,]
hawthorne_fips_list <- select(filter(pp_areas_df, Analysis_A == "Belmont-Hawthorne-Division"), FIPS)[,]
wilkes_fips_list <- select(filter(pp_areas_df, Analysis_A == "Centennial-Glenfair-Wilkes"), FIPS)[,]
central_city_fips_list <- select(filter(pp_areas_df, Analysis_A == "Central City"), FIPS)[,]
forest_park_fips_list <- select(filter(pp_areas_df, Analysis_A == "Forest Park-Northwest Hills"), FIPS)[,]
gateway_fips_list <- select(filter(pp_areas_df, Analysis_A == "Gateway"), FIPS)[,]
hayden_fips_list <- select(filter(pp_areas_df, Analysis_A == "Hayden Island-Bridgeton"), FIPS)[,]
hillsdale_fips_list <- select(filter(pp_areas_df, Analysis_A == "Hillsdale-Multnomah-Barbur"), FIPS)[,]
hollywood_fips_list <- select(filter(pp_areas_df, Analysis_A == "Hollywood"), FIPS)[,]
interstate_fips_list <- select(filter(pp_areas_df, Analysis_A == "Interstate Corridor"), FIPS)[,]
lents_foster_fips_list <- select(filter(pp_areas_df, Analysis_A == "Lents-Foster"), FIPS)[,]
mlk_alberta_fips_list <- select(filter(pp_areas_df, Analysis_A == "MLK-Alberta"), FIPS)[,]
montavilla_fips_list <- select(filter(pp_areas_df, Analysis_A == "Montavilla"), FIPS)[,]
northwest_fips_list <- select(filter(pp_areas_df, Analysis_A == "Northwest"), FIPS)[,]
parkrose_fips_list <- select(filter(pp_areas_df, Analysis_A == "Parkrose-Argay"), FIPS)[,]
pleasant_valley_fips_list <- select(filter(pp_areas_df, Analysis_A == "Pleasant Valley"), FIPS)[,]
raleigh_hills_fips_list <- select(filter(pp_areas_df, Analysis_A == "Raleigh Hills"), FIPS)[,]
roseway_cully_fips_list <- select(filter(pp_areas_df, Analysis_A == "Roseway-Cully"), FIPS)[,]
sellwood_fips_list <- select(filter(pp_areas_df, Analysis_A == "Sellwood-Moreland-Brooklyn"), FIPS)[,]
south_portland_fips_list <- select(filter(pp_areas_df, Analysis_A == "South Portland-Marquam Hill"), FIPS)[,]
st_johns_fips_list <- select(filter(pp_areas_df, Analysis_A == "St. Johns"), FIPS)[,]
tryon_fips_list <- select(filter(pp_areas_df, Analysis_A == "Tryon Creek-Riverdale"), FIPS)[,]
west_portland_fips_list <- select(filter(pp_areas_df, Analysis_A == "West Portland"), FIPS)[,]
woodstock_fips_list <- select(filter(pp_areas_df, Analysis_A == "Woodstock"), FIPS)[,]

##### Load tables #####

# Load in Table 1 for various geographic entities / summary levels
chas.tct.2015.t1 <- read_csv('C:/Users/nkobel/Desktop/Large_files/CHAS/data/tract/2011thru2015-140-csv/140/Table1.csv')
chas.cnty.2015.t1 <- read_csv('C:/Users/nkobel/Desktop/Large_files/CHAS/data/county/2011thru2015-050-csv/050/Table1.csv')
chas.st.2015.t1 <- read_csv('C:/Users/nkobel/Desktop/Large_files/CHAS/data/state/2011thru2015-040-csv/040/Table1.csv')
chas.plc.2015.t1 <- read_csv('C:/Users/nkobel/Desktop/Large_files/CHAS/data/place/2011thru2015-160-csv/160/Table1.csv')

#chas.tct.2012.t1 <- read_csv('C:/Users/nkobel/Desktop/Large_files/CHAS/data/tract/2008thru2012-080-csv/Table1.csv')

# Load in Table 8 for various geographic entities / summary levels
chas.tct.2015.t8 <- read_csv('C:/Users/nkobel/Desktop/Large_files/CHAS/data/tract/2011thru2015-140-csv/140/Table8.csv')
chas.cnty.2015.t8 <- read_csv('C:/Users/nkobel/Desktop/Large_files/CHAS/data/county/2011thru2015-050-csv/050/Table8.csv')
chas.st.2015.t8 <- read_csv('C:/Users/nkobel/Desktop/Large_files/CHAS/data/state/2011thru2015-040-csv/040/Table8.csv')
chas.plc.2015.t8 <- read_csv('C:/Users/nkobel/Desktop/Large_files/CHAS/data/place/2011thru2015-160-csv/160/Table8.csv')

chas.tct.2010.t8 <- read_csv('C:/Users/nkobel/Desktop/Large_files/CHAS/data/tract/2006thru2010-080-csv/Table8.csv')
chas.cnty.2010.t8 <- read_csv('C:/Users/nkobel/Desktop/Large_files/CHAS/data/county/2006thru2010-050-csv/Table8.csv')
chas.st.2010.t8 <- read_csv('C:/Users/nkobel/Desktop/Large_files/CHAS/data/state/2006thru2010-040-csv/Table8.csv')
chas.plc.2010.t8 <- read_csv('C:/Users/nkobel/Desktop/Large_files/CHAS/data/place/2006thru2010-160-csv/Table8.csv')

# Load in Table 11 for various geographic entities / summary levels
chas.tct.2015.t11 <- read_csv('C:/Users/nkobel/Desktop/Large_files/CHAS/data/tract/2011thru2015-140-csv/140/Table11.csv')
chas.cnty.2015.t11 <- read_csv('C:/Users/nkobel/Desktop/Large_files/CHAS/data/county/2011thru2015-050-csv/050/Table11.csv')
chas.st.2015.t11 <- read_csv('C:/Users/nkobel/Desktop/Large_files/CHAS/data/state/2011thru2015-040-csv/040/Table11.csv')
chas.plc.2015.t11 <- read_csv('C:/Users/nkobel/Desktop/Large_files/CHAS/data/place/2011thru2015-160-csv/160/Table11.csv')

# Load in Table 14B for various geographic entities / summary levels
chas.tct.2015.t14b <- read_csv('C:/Users/nkobel/Desktop/Large_files/CHAS/data/tract/2011thru2015-140-csv/140/Table14B.csv')
chas.cnty.2015.t14b <- read_csv('C:/Users/nkobel/Desktop/Large_files/CHAS/data/county/2011thru2015-050-csv/050/Table14B.csv')
chas.st.2015.t14b <- read_csv('C:/Users/nkobel/Desktop/Large_files/CHAS/data/state/2011thru2015-040-csv/040/Table14B.csv')
chas.plc.2015.t14b <- read_csv('C:/Users/nkobel/Desktop/Large_files/CHAS/data/place/2011thru2015-160-csv/160/Table14B.csv')

# Load in Table 15C for various geographic entities / summary levels
chas.tct.2015.t15c <- read_csv('C:/Users/nkobel/Desktop/Large_files/CHAS/data/tract/2011thru2015-140-csv/140/Table15C.csv')
chas.cnty.2015.t15c <- read_csv('C:/Users/nkobel/Desktop/Large_files/CHAS/data/county/2011thru2015-050-csv/050/Table15C.csv')
chas.st.2015.t15c <- read_csv('C:/Users/nkobel/Desktop/Large_files/CHAS/data/state/2011thru2015-040-csv/040/Table15C.csv')
chas.plc.2015.t15c <- read_csv('C:/Users/nkobel/Desktop/Large_files/CHAS/data/place/2011thru2015-160-csv/160/Table15C.csv')

# chas.tct.2014.t15c <-cleanup_chas_tract(read_csv('C:/Users/nkobel/Desktop/Large_files/CHAS/data/tract/2010thru2014-140-csv/Table15C.csv'))
# chas.tct.2014.t14b <-cleanup_chas_tract(read_csv('C:/Users/nkobel/Desktop/Large_files/CHAS/data/tract/2010thru2014-140-csv/Table14B.csv'))
# chas.plc.2014.t15c <-cleanup_chas_plc(read_csv('C:/Users/nkobel/Desktop/Large_files/CHAS/data/place/2010thru2014-160-csv/Table15C.csv'))
# chas.plc.2014.t14b <-cleanup_chas_plc(read_csv('C:/Users/nkobel/Desktop/Large_files/CHAS/data/place/2010thru2014-160-csv/Table14B.csv'))

chas.tct.2015.t18c <- read_csv('C:/Users/nkobel/Desktop/Large_files/CHAS/data/tract/2011thru2015-140-csv/140/Table18C.csv')
chas.cnty.2015.t18c <- read_csv('C:/Users/nkobel/Desktop/Large_files/CHAS/data/county/2011thru2015-050-csv/050/Table18C.csv')
chas.st.2015.t18c <- read_csv('C:/Users/nkobel/Desktop/Large_files/CHAS/data/state/2011thru2015-040-csv/040/Table18C.csv')
chas.plc.2015.t18c <- read_csv('C:/Users/nkobel/Desktop/Large_files/CHAS/data/place/2011thru2015-160-csv/160/Table18C.csv')

##### Clean tables #####

# Clean up pre-loaded geographies so that they are in a consistent format
chas.tct.2015.t1 <- cleanup_chas_tract(chas.tct.2015.t1)
chas.plc.2015.t1 <- cleanup_chas_plc(chas.plc.2015.t1)
chas.cnty.2015.t1 <- cleanup_chas_cnty(chas.cnty.2015.t1)
chas.st.2015.t1 <- cleanup_chas_st(chas.st.2015.t1)

chas.tct.2015.t8 <- cleanup_chas_tract(chas.tct.2015.t8)
chas.plc.2015.t8 <- cleanup_chas_plc(chas.plc.2015.t8)
chas.cnty.2015.t8 <- cleanup_chas_cnty(chas.cnty.2015.t8)
chas.st.2015.t8 <- cleanup_chas_st(chas.st.2015.t8)

chas.tct.2015.t11 <- cleanup_chas_tract(chas.tct.2015.t11)
chas.plc.2015.t11 <- cleanup_chas_plc(chas.plc.2015.t11)
chas.cnty.2015.t11 <- cleanup_chas_cnty(chas.cnty.2015.t11)
chas.st.2015.t11 <- cleanup_chas_st(chas.st.2015.t11)

chas.tct.2015.t14b <- cleanup_chas_tract(chas.tct.2015.t14b)
chas.plc.2015.t14b <- cleanup_chas_plc(chas.plc.2015.t14b)
chas.cnty.2015.t14b <- cleanup_chas_cnty(chas.cnty.2015.t14b)
chas.st.2015.t14b <- cleanup_chas_st(chas.st.2015.t14b)

chas.tct.2015.t15c <- cleanup_chas_tract(chas.tct.2015.t15c)
chas.plc.2015.t15c <- cleanup_chas_plc(chas.plc.2015.t15c)
chas.cnty.2015.t15c <- cleanup_chas_cnty(chas.cnty.2015.t15c)
chas.st.2015.t15c <- cleanup_chas_st(chas.st.2015.t15c)

chas.tct.2015.t18c <- cleanup_chas_tract(chas.tct.2015.t18c)
chas.plc.2015.t18c <- cleanup_chas_plc(chas.plc.2015.t18c)
chas.cnty.2015.t18c <- cleanup_chas_cnty(chas.cnty.2015.t18c)
chas.st.2015.t18c <- cleanup_chas_st(chas.st.2015.t18c)

# Bind all geographies together into a single file (by year by table)
chas.2015.t1 <- rbind(chas.tct.2015.t1, chas.plc.2015.t1, chas.cnty.2015.t1, chas.st.2015.t1)
chas.2015.t8 <- rbind(chas.tct.2015.t8, chas.plc.2015.t8, chas.cnty.2015.t8, chas.st.2015.t8)
chas.2015.t11 <- rbind(chas.tct.2015.t11, chas.plc.2015.t11, chas.cnty.2015.t11, chas.st.2015.t11)
chas.2015.t14b <- rbind(chas.tct.2015.t14b, chas.plc.2015.t14b, chas.cnty.2015.t14b, chas.st.2015.t14b)
chas.2015.t15c <- rbind(chas.tct.2015.t15c, chas.plc.2015.t15c, chas.cnty.2015.t15c, chas.st.2015.t15c)

chas.2015.t18c <- rbind(chas.tct.2015.t18c, chas.plc.2015.t18c ,chas.cnty.2015.t18c, chas.st.2015.t18c)


##### Compile long-cast data tables for metro area #####
# Example of casting data into long format for MSA geographies and peer cities
chas.msa.15.t1 <- cast_chas_long(chas.2015.t1 %>% filter( (geoid2 %in% msa_geoid2_vector) | 
                                                  (geoid2 %in% orwa_place_vector) | 
                                                  (geoid2 %in% c('41', '53')) |
                                                  (geoid2 %in% peer_city_list) ), 7)

chas.msa.15.t11 <- cast_chas_long(chas.2015.t11 %>% filter( (geoid2 %in% msa_geoid2_vector) | 
                                                             (geoid2 %in% orwa_place_vector) | 
                                                             (geoid2 %in% c('41', '53')) |
                                                             (geoid2 %in% peer_city_list) ), 7)

c('41005', '41009', '41051', '41067', '41071', '53011', '53059')

chas.msa.15.t8 <- chas.2015.t8 %>%
  filter(geoid2 %in% msa_geoid2_vector | 
           geoid2 %in% orwa_place_vector | 
           geoid2 %in% c('41', '53') |
           geoid2 %in% peer_city_list |
           (cnty_plc %in% c('005', '009', '051', '067', '071') & st == '41') |
           (cnty_plc %in% c('011', '059') & st == '53'))



##### Testing 2010 file #####

chas.tct.2010.t8$name <- ''
chas.tct.2010.t8$st <- substr(chas.tct.2010.t8$geoid, 8, 9)
chas.tct.2010.t8$cnty_plc <- substr(chas.tct.2010.t8$geoid, 10, 12)
chas.tct.2010.t8$geoid2 <- paste0(substr(chas.tct.2010.t8$geoid, 8, 12), substr(chas.tct.2010.t8$geoid, 23, 28))
refcols_vintage <- c('source', 'geoid', 'geoid2', 'name', 'st', 'cnty_plc')
chas.tct.2010.t8 <- chas.tct.2010.t8[, c(refcols_vintage, setdiff(names(chas.tct.2010.t8), refcols_vintage))]
chas.tct.2010.t8_header <- names(chas.tct.2010.t8)[1:7]

chas.msa.tct.10.t8 <- chas.tct.2010.t8 %>%
  filter(cnty_plc %in% c('005', '009', '051', '067', '071') & st == '41' |
           (cnty_plc %in% c('011', '059') & st == '53')) %>%
  mutate(li_cb_rent = T8_est73 + T8_est76 + T8_est86 + T8_est89 + T8_est99 + T8_est102) %>%
  group_by(geoid2) %>%
  summarize(li_cb_rent = sum(li_cb_rent))

write_csv(chas.msa.tct.10.t8, "C:/Users/nkobel/Desktop/t8.tract.msa.2010.csv")


chas.pdx.plc.10.t8 <- chas.plc.2010.t8 %>%
  filter(place == '59000', st == '41') %>%
  mutate(li_cb_rent = T8_est73 + T8_est76 + T8_est86 + T8_est89 + T8_est99 + T8_est102) %>%
  select(li_cb_rent)

chas.msa.tct.10.t8.long <- cast_chas_long(chas.msa.tct.10.t8, header_vars = 6)



test <- chas.msa.tct.10.t8.long %>%
  group_by(geoid2, var_num) %>%
  summarize(est = sum(est),
            moe = sqrt(sum(moesq)))

write_csv(test, "C:/Users/nkobel/Desktop/t8.tract.msa.2010.long.csv")


##### for() loop to run function over vector of tracts #####

msa_tracts <- chas.tct.2015.t8 %>%
  mutate(stcnty = paste0(st,cnty_plc)) %>%
  filter(stcnty %in% msa_geoid2_vector) %>%
  select(geoid2) %>%
  data.frame(.)

df_for_appending <- data.frame()

for (i in seq_along(msa_tracts$geoid2)){
  li_data <- get_li_cb_simple(msa_tracts[[1]][i], chas.tct.2015.t8)
  li_data$FIPS <- msa_tracts[[1]][i]
  df_for_appending <- rbind(df_for_appending, li_data)
}

###

df_for_appending <- data.frame()
for (i in seq_along(msa_tracts$geoid2)){
  li_data <- get_li_cb_sfr(msa_tracts[[1]][i], chas.tct.2015.t18c)
  li_data$FIPS <- msa_tracts[[1]][i]
  df_for_appending <- rbind(df_for_appending, li_data)
}
df_for_appending <- df_for_appending %>% select(FIPS, 1:as.numeric(length(df_for_appending)-1))

write.csv(df_for_appending, "N:/work/district_planning/Economic Development/Nick/_quick-tasks/get_li_cb_sfr_2015.csv", row.names = F)



# Create vector for a county
multco_tct_vector <- chas.tct.2015.t11 %>%
  filter(cnty_plc == '051', st == '41') %>%
  select(geoid2) %>% data.frame(.)

# Create empty dataframe to append data to 
df_for_appending <- data.frame()

# Perform function over vector of FIPS codes
for (i in seq_along(multco_tct_vector$geoid2)){
    li_data <- get_li_cb_simple(multco_tct_vector[[1]][i], chas.tct.2015.t8)
  li_data$FIPS <- multco_tct_vector[[1]][i]
  df_for_appending <- rbind(df_for_appending, li_data)
}

# Re-order columns
df_for_appending <- df_for_appending %>% select(FIPS, 1:as.numeric(length(df_for_appending)-1))

write.csv(df_for_appending, "N:/work/district_planning/Economic Development/Nick/_quick-tasks/li_cb_rent_tract_2015.csv", row.names = F)

## Perform same function on reference geography
pdx_t8 <- cast_chas_long(chas.plc.2015.t8 %>% filter(geoid2 == '4159000') )
write.csv(pdx_t8, "N:/work/district_planning/Economic Development/Nick/_quick-tasks/li_cb_rent_pdx_2015.csv")



get_li_cb_simple('4159000', chas.plc.2015.t8)

orplace <- chas.plc.2015.t8 %>% filter(st == '41') %>% select(geoid2) %>% data.frame(.)
df_for_appending <- data.frame()
for (i in seq_along(orplace$geoid2)){
  li_data <- get_li_cb_simple(orplace[[1]][i], chas.plc.2015.t8)
  li_data$FIPS <- orplace[[1]][i]
  df_for_appending <- rbind(df_for_appending, li_data)
}
df_for_appending <- df_for_appending %>% select(FIPS, 1:as.numeric(length(df_for_appending)-1))
write.csv(df_for_appending, "N:/work/district_planning/Economic Development/Nick/_quick-tasks/li_cb_rent_or_places_2015v2.csv", row.names = F)


##### Get simple share of HH that are low-income #####

get_simple_li <- function(geoid2_vector, chas_t11) {
  # census places must prepend the state fips (2-dig), e.g. '4159000' wrapped in quotes
  chas_t11 <- cast_chas_long(chas_t11 %>% filter(geoid2 %in% geoid2_vector), 7)
  chas_t11 <- cast_chas_wide(chas_t11)
  chas_t11 <- chas_t11 %>%
    summarize(total_hh = sum(v1_est),
              total_000_to_020_own = sum(v4_est) + sum(v18_est) + sum(v32_est),
              total_000_to_020_rent = sum(v47_est) + sum(v61_est) + sum(v75_est),
              total_021_to_030_own = sum(v5_est) + sum(v19_est) + sum(v33_est),
              total_021_to_030_rent = sum(v48_est) + sum(v62_est) + sum(v76_est),
              total_031_to_040_own = sum(v6_est) + sum(v20_est) + sum(v34_est),
              total_031_to_040_rent = sum(v49_est) + sum(v63_est) + sum(v77_est),
              total_041_to_050_own = sum(v7_est) + sum(v21_est) + sum(v35_est),
              total_041_to_050_rent = sum(v50_est) + sum(v64_est) + sum(v78_est),
              total_051_to_060_own = sum(v8_est) + sum(v22_est) + sum(v36_est),
              total_051_to_060_rent = sum(v51_est) + sum(v65_est) + sum(v79_est),
              total_061_to_065_own = sum(v9_est) + sum(v23_est) + sum(v37_est),
              total_061_to_065_rent = sum(v52_est) + sum(v66_est) + sum(v80_est),
              total_066_to_080_own = sum(v10_est) + sum(v24_est) + sum(v38_est),
              total_066_to_080_rent = sum(v53_est) + sum(v67_est) + sum(v81_est),
              total_081_to_095_own = sum(v11_est) + sum(v25_est) + sum(v39_est),
              total_081_to_095_rent = sum(v54_est) + sum(v68_est) + sum(v82_est),
              total_096_to_100_own = sum(v12_est) + sum(v26_est) + sum(v40_est),
              total_096_to_100_rent = sum(v55_est) + sum(v69_est) + sum(v83_est),
              total_101_to_115_own = sum(v13_est) + sum(v27_est) + sum(v41_est),
              total_101_to_115_rent = sum(v56_est) + sum(v70_est) + sum(v84_est),
              total_116_to_120_own = sum(v14_est) + sum(v28_est) + sum(v42_est),
              total_116_to_120_rent = sum(v57_est) + sum(v71_est) + sum(v85_est),
              total_121_to_140_own = sum(v15_est) + sum(v29_est) + sum(v43_est),
              total_121_to_140_rent = sum(v58_est) + sum(v72_est) + sum(v86_est),
              total_141_to_999_own = sum(v16_est) + sum(v30_est) + sum(v44_est),
              total_141_to_999_rent = sum(v59_est) + sum(v73_est) + sum(v87_est),
              total_000_to_020 = total_000_to_020_own + total_000_to_020_rent,
              total_021_to_030 = total_021_to_030_own + total_021_to_030_rent,
              total_031_to_040 = total_031_to_040_own + total_031_to_040_rent,
              total_041_to_050 = total_041_to_050_own + total_041_to_050_rent,
              total_051_to_060 = total_051_to_060_own + total_051_to_060_rent,
              total_061_to_065 = total_061_to_065_own + total_061_to_065_rent,
              total_066_to_080 = total_066_to_080_own + total_066_to_080_rent,
              total_081_to_095 = total_081_to_095_own + total_081_to_095_rent,
              total_096_to_100 = total_096_to_100_own + total_096_to_100_rent,
              total_101_to_115 = total_101_to_115_own + total_101_to_115_rent,
              total_116_to_120 = total_116_to_120_own + total_116_to_120_rent,
              total_121_to_140 = total_121_to_140_own + total_121_to_140_rent,
              total_141_to_999 = total_141_to_999_own + total_141_to_999_rent,
              share_000_to_020 = total_000_to_020 / total_hh,
              share_021_to_030 = total_021_to_030 / total_hh,
              share_031_to_040 = total_031_to_040 / total_hh,
              share_041_to_050 = total_041_to_050 / total_hh,
              share_051_to_060 = total_051_to_060 / total_hh,
              share_061_to_065 = total_061_to_065 / total_hh,
              share_066_to_080 = total_066_to_080 / total_hh,
              share_081_to_095 = total_081_to_095 / total_hh,
              share_096_to_100 = total_096_to_100 / total_hh,
              share_101_to_115 = total_101_to_115 / total_hh,
              share_116_to_120 = total_116_to_120 / total_hh,
              share_121_to_140 = total_121_to_140 / total_hh,
              share_141_to_999 = total_141_to_999 / total_hh,
              total_li = total_000_to_020 + total_021_to_030 + total_031_to_040 + total_041_to_050 + 
                total_051_to_060 + total_061_to_065 + total_066_to_080,
              share_li = total_li / total_hh,
              total_000_to_030 = total_000_to_020 + total_021_to_030,
              total_031_to_060 = total_031_to_040 + total_041_to_050 + total_051_to_060,
              total_061_to_080 = total_061_to_065 + total_066_to_080,
              total_081_to_100 = total_081_to_095 + total_096_to_100,
              total_101_to_120 = total_101_to_115 + total_116_to_120,
              total_121_to_999 = total_121_to_140 + total_141_to_999,
              share_000_to_030 = share_000_to_020 + share_021_to_030,
              share_031_to_060 = share_031_to_040 + share_041_to_050 + share_051_to_060,
              share_061_to_080 = share_061_to_065 + share_066_to_080,
              share_081_to_100 = share_081_to_095 + share_096_to_100,
              share_101_to_120 = share_101_to_115 + share_116_to_120,
              share_121_to_999 = share_121_to_140 + share_141_to_999
    )
  return(chas_t11 %>% select(total_hh, total_li, share_li))
}

get_simple_li('4159000', chas.plc.2015.t11)



get_li_cb_simple(ldist_cc_fips_list, chas.tct.2015.t8)
get_simple_li(ldist_cc_fips_list, chas.tct.2015.t11)

get_li_cb_simple('4159000', chas.plc.2015.t8)
get_simple_li('4159000', chas.plc.2015.t11)




get_li_cb_simple('4159000', chas.plc.2015.t8)
get_li_cb_simple(msa_geoid2_vector, chas.cnty.2015.t8)


##### Get low-income cost-burdened renters in SFR ####
get_li_cb_sfr <- function(geoid2_vector, chas_t18c) {
  chas_t18c <- cast_chas_long(chas_t18c %>% filter(geoid2 %in% geoid2_vector), 7)
  chas_t18c <- cast_chas_wide(chas_t18c)
  chas_t18c %>%
    summarize(total_rent = sum(v1_est),
              rent_sfr_li_cb = sum(v10_est) + sum(v16_est) + sum(v17_est) + sum(v22_est) + sum(v23_est) + sum(v24_est))
}

get_li_cb_sfr(rossi_fips2, chas.tct.2015.t18c)

##### Get cost-burdened renters ##### 
get_li_cb_simple <- function(geoid2_vector, chas_t8) {
  # census places must prepend the state fips (2-dig), e.g. '4159000' wrapped in quotes
  chas_t8 <- cast_chas_long(chas_t8 %>% filter(geoid2 %in% geoid2_vector), 7)
  chas_t8 <- cast_chas_wide(chas_t8)
  chas_t8 <- chas_t8 %>%
    mutate(total_cb = v7_est + v10_est + v20_est + v23_est + v33_est + v36_est + v46_est + 
             v49_est + v59_est + v62_est + v73_est + v76_est + v86_est + v89_est + v99_est + 
             v102_est + v112_est + v115_est + v125_est + v128_est,
           cb_own = v7_est + v10_est + v20_est + v23_est + v33_est + v36_est + v46_est + 
             v49_est + v59_est + v62_est,
           cb_rent = v73_est + v76_est + v86_est + v89_est + v99_est + v102_est + v112_est + 
             v115_est + v125_est + v128_est,
           li_cb_total = v7_est + v10_est + v20_est + v23_est + v33_est + v36_est + v73_est + 
             v76_est + v86_est + v89_est + v99_est + v102_est,
           li_cb_own = v7_est + v10_est + v20_est + v23_est + v33_est + v36_est,
           li_cb_rent = v73_est + v76_est + v86_est + v89_est + v99_est + v102_est,
           total_scb = v10_est + v23_est + v36_est + v49_est + v62_est + v76_est + v89_est + 
             v102_est + v115_est + v128_est,
           scb_own = v10_est + v23_est + v36_est + v49_est + v62_est,
           scb_rent = v76_est + v89_est + v102_est + v115_est + v128_est,
           li_scb_total = v10_est + v23_est + v36_est,
           li_scb_own = v76_est + v89_est + v102_est)
  chas_t8 %>%
    summarize(total_hh = sum(v1_est),
              total_own = sum(v2_est),
              total_rent = sum(v68_est),
              total_cb = sum(total_cb),
              cb_own = sum(cb_own),
              cb_rent = sum(cb_rent),
              li_cb_total = sum(li_cb_total),
              li_cb_own = sum(li_cb_own),
              li_cb_rent = sum(li_cb_rent),
              total_scb = sum(total_scb),
              scb_own = sum(scb_own),
              scb_rent = sum(scb_rent),
              li_scb_total = sum(li_scb_total),
              li_scb_own = sum(li_scb_own)
    )
}

get_li_cb_simple(sw_corridor_list, chas.tct.2015.t8)
get_li_cb_simple('4159000', chas.plc.2015.t8)

orplace <- chas.plc.2015.t8 %>% filter(st == '41') %>% select(geoid2) %>% data.frame(.)

cb_renter_by_city <- map(place_vector, get_li_cb_simple(chas_t8 = chas.plc.2015.t8))


##### Get detailed income distribution by tenure ##### 
get_detailed_income_distribution <- function(geoid2_vector, chas_t11) {
  # census places must prepend the state fips (2-dig), e.g. '4159000' wrapped in quotes
  chas_t11 <- cast_chas_long(chas_t11 %>% filter(geoid2 %in% geoid2_vector), 7)
  chas_t11 <- cast_chas_wide(chas_t11)
  chas_t11 %>%
    summarize(total_hh = sum(v1_est),
              total_000_to_020_own = sum(v4_est) + sum(v18_est) + sum(v32_est),
              total_000_to_020_rent = sum(v47_est) + sum(v61_est) + sum(v75_est),
              total_021_to_030_own = sum(v5_est) + sum(v19_est) + sum(v33_est),
              total_021_to_030_rent = sum(v48_est) + sum(v62_est) + sum(v76_est),
              total_031_to_040_own = sum(v6_est) + sum(v20_est) + sum(v34_est),
              total_031_to_040_rent = sum(v49_est) + sum(v63_est) + sum(v77_est),
              total_041_to_050_own = sum(v7_est) + sum(v21_est) + sum(v35_est),
              total_041_to_050_rent = sum(v50_est) + sum(v64_est) + sum(v78_est),
              total_051_to_060_own = sum(v8_est) + sum(v22_est) + sum(v36_est),
              total_051_to_060_rent = sum(v51_est) + sum(v65_est) + sum(v79_est),
              total_061_to_065_own = sum(v9_est) + sum(v23_est) + sum(v37_est),
              total_061_to_065_rent = sum(v52_est) + sum(v66_est) + sum(v80_est),
              total_066_to_080_own = sum(v10_est) + sum(v24_est) + sum(v38_est),
              total_066_to_080_rent = sum(v53_est) + sum(v67_est) + sum(v81_est),
              total_081_to_095_own = sum(v11_est) + sum(v25_est) + sum(v39_est),
              total_081_to_095_rent = sum(v54_est) + sum(v68_est) + sum(v82_est),
              total_096_to_100_own = sum(v12_est) + sum(v26_est) + sum(v40_est),
              total_096_to_100_rent = sum(v55_est) + sum(v69_est) + sum(v83_est),
              total_101_to_115_own = sum(v13_est) + sum(v27_est) + sum(v41_est),
              total_101_to_115_rent = sum(v56_est) + sum(v70_est) + sum(v84_est),
              total_116_to_120_own = sum(v14_est) + sum(v28_est) + sum(v42_est),
              total_116_to_120_rent = sum(v57_est) + sum(v71_est) + sum(v85_est),
              total_121_to_140_own = sum(v15_est) + sum(v29_est) + sum(v43_est),
              total_121_to_140_rent = sum(v58_est) + sum(v72_est) + sum(v86_est),
              total_141_to_999_own = sum(v16_est) + sum(v30_est) + sum(v44_est),
              total_141_to_999_rent = sum(v59_est) + sum(v73_est) + sum(v87_est),
              total_000_to_020 = total_000_to_020_own + total_000_to_020_rent,
              total_021_to_030 = total_021_to_030_own + total_021_to_030_rent,
              total_031_to_040 = total_031_to_040_own + total_031_to_040_rent,
              total_041_to_050 = total_041_to_050_own + total_041_to_050_rent,
              total_051_to_060 = total_051_to_060_own + total_051_to_060_rent,
              total_061_to_065 = total_061_to_065_own + total_061_to_065_rent,
              total_066_to_080 = total_066_to_080_own + total_066_to_080_rent,
              total_081_to_095 = total_081_to_095_own + total_081_to_095_rent,
              total_096_to_100 = total_096_to_100_own + total_096_to_100_rent,
              total_101_to_115 = total_101_to_115_own + total_101_to_115_rent,
              total_116_to_120 = total_116_to_120_own + total_116_to_120_rent,
              total_121_to_140 = total_121_to_140_own + total_121_to_140_rent,
              total_141_to_999 = total_141_to_999_own + total_141_to_999_rent,
              share_000_to_020 = total_000_to_020 / total_hh,
              share_021_to_030 = total_021_to_030 / total_hh,
              share_031_to_040 = total_031_to_040 / total_hh,
              share_041_to_050 = total_041_to_050 / total_hh,
              share_051_to_060 = total_051_to_060 / total_hh,
              share_061_to_065 = total_061_to_065 / total_hh,
              share_066_to_080 = total_066_to_080 / total_hh,
              share_081_to_095 = total_081_to_095 / total_hh,
              share_096_to_100 = total_096_to_100 / total_hh,
              share_101_to_115 = total_101_to_115 / total_hh,
              share_116_to_120 = total_116_to_120 / total_hh,
              share_121_to_140 = total_121_to_140 / total_hh,
              share_141_to_999 = total_141_to_999 / total_hh,
              total_li = total_000_to_020 + total_021_to_030 + total_031_to_040 + total_041_to_050 + 
                total_051_to_060 + total_061_to_065 + total_066_to_080,
              share_li = total_li / total_hh,
              total_000_to_030 = total_000_to_020 + total_021_to_030,
              total_031_to_060 = total_031_to_040 + total_041_to_050 + total_051_to_060,
              total_061_to_080 = total_061_to_065 + total_066_to_080,
              total_081_to_100 = total_081_to_095 + total_096_to_100,
              total_101_to_120 = total_101_to_115 + total_116_to_120,
              total_121_to_999 = total_121_to_140 + total_141_to_999,
              share_000_to_030 = share_000_to_020 + share_021_to_030,
              share_031_to_060 = share_031_to_040 + share_041_to_050 + share_051_to_060,
              share_061_to_080 = share_061_to_065 + share_066_to_080,
              share_081_to_100 = share_081_to_095 + share_096_to_100,
              share_101_to_120 = share_101_to_115 + share_116_to_120,
              share_121_to_999 = share_121_to_140 + share_141_to_999
    )
}

glimpse(get_detailed_income_distribution('4159000', chas.plc.2015.t11))
glimpse(get_simple_li(sw_corridor_list, chas.tct.2015.t11))

##### Simple function to get low-income households by tenure ##### 
get_low_income_tenure <- function(geoid2_vector, chas_t1) {
  # census places must prepend the state fips (2-dig), e.g. '4159000' wrapped in quotes
  chas_t1 <- cast_chas_long(chas_t1 %>% filter(geoid2 %in% geoid2_vector), 7)
  chas_t1 <- cast_chas_wide(chas_t1)
  chas_t1 %>%
    summarize(total_hh = sum(v1_est),
              total_hh_own = sum(v2_est),
              total_hh_rent = sum(v126_est),
              total_li_own = sum(v4_est) + sum(v12_est) + sum(v20_est) + sum(v45_est) + sum(v53_est) + sum(v61_est) + sum(v86_est) + sum(v94_est) + sum(v102_est),
              total_li_rent = sum(v128_est) + sum(v136_est) + sum(v144_est) + sum(v169_est) + sum(v177_est) + sum(v185_est) + sum(v210_est) + sum(v218_est) + sum(v226_est),
              total_li = total_li_own + total_li_rent,
              share_li = total_li / total_hh,
              share_own = total_hh_own / total_hh,
              share_rent = total_hh_rent / total_hh,
              share_li_that_own = total_li_own / total_li,
              share_li_that_rent = total_li_rent / total_li,
              share_hh_that_li_own = total_li_own / total_hh,
              share_hh_that_li_rent = total_li_rent / total_hh)
}

# Examples for MSA total, SW Corridor and Portland
glimpse(get_low_income_tenure(msa_geoid2_vector, chas.cnty.2015.t1))
glimpse(get_low_income_tenure('4159000', chas.plc.2015.t1))
glimpse(get_low_income_tenure(sw_corridor_list, chas.tct.2015.t1))



##### Perform rental housing gap analysis with printed graphs ##### 
get_rental_housing_gap <- function(geoid2_vector, gtitle, chas_t14b, chas_t15c) {
  results <- c()
  
  chas_t15c <- cast_chas_long(chas_t15c %>% filter(geoid2 %in% geoid2_vector), 7)
  chas_t15c <- cast_chas_wide(chas_t15c)
  chas_t14b <- cast_chas_long(chas_t14b %>% filter(geoid2 %in% geoid2_vector), 7)
  chas_t14b <- cast_chas_wide(chas_t14b)
  
  chas_t14b <- chas_t14b %>%
    mutate(vacant_total = v1_est,
           vacant_complete = v3_est,
           vacant_available_000_to_030 = v4_est,
           vacant_available_031_to_050 = v8_est,
           vacant_available_051_to_080 = v12_est,
           vacant_available_081_to_999 = v16_est)
  
  chas_t14b <- chas_t14b %>%
    summarize(vacant_total = sum(vacant_total),
              vacant_complete = sum(vacant_complete),
              vacant_available_000_to_030 = sum(vacant_available_000_to_030),
              vacant_available_031_to_050 = sum(vacant_available_031_to_050),
              vacant_available_051_to_080 = sum(vacant_available_051_to_080),
              vacant_available_081_to_999 = sum(vacant_available_081_to_999))
  
  chas_t15c <- chas_t15c %>%
    mutate(matched_units_000_to_030 = v5_est,
           matched_units_031_to_050 = v30_est,
           matched_units_051_to_080 = v55_est,
           matched_units_081_to_999 = v80_est + v84_est,
           hh_000_to_030_renting_up = v26_est + v47_est + v68_est,
           hh_031_to_050_renting_up = v51_est + v72_est,
           hh_051_to_080_renting_up = v76_est,
           hh_081_to_999_renting_up = 0,
           hh_000_to_030_renting_down = 0,
           hh_031_to_050_renting_down = v9_est,
           hh_051_to_080_renting_down = v13_est + v34_est,
           hh_081_to_999_renting_down = v17_est + v21_est + v38_est + v42_est + v59_est + v63_est,
           units_000_to_030_rented_to_higher_income = v9_est + v13_est + v17_est + v21_est,
           units_031_to_050_rented_to_higher_income = v34_est + v38_est + v42_est,
           units_051_to_080_rented_to_higher_income = v59_est + v63_est,
           units_081_to_999_rented_to_higher_income = 0,
           units_000_to_030_rented_to_lower_income = 0,
           units_031_to_050_rented_to_lower_income = v26_est,
           units_051_to_080_rented_to_lower_income = v47_est + v51_est,
           units_081_to_999_rented_to_lower_income = v68_est + v72_est + v76_est)
  
  chas_t15c <- chas_t15c %>%
    summarize(matched_units_000_to_030 = sum(matched_units_000_to_030),
              matched_units_031_to_050 = sum(matched_units_031_to_050),
              matched_units_051_to_080 = sum(matched_units_051_to_080),
              matched_units_081_to_999 = sum(matched_units_081_to_999),
              hh_000_to_030_renting_up = sum(hh_000_to_030_renting_up),
              hh_031_to_050_renting_up = sum(hh_031_to_050_renting_up),
              hh_051_to_080_renting_up = sum(hh_051_to_080_renting_up),
              hh_081_to_999_renting_up = 0,
              hh_000_to_030_renting_down = 0,
              hh_031_to_050_renting_down = sum(hh_031_to_050_renting_down),
              hh_051_to_080_renting_down = sum(hh_051_to_080_renting_down),
              hh_081_to_999_renting_down = sum(hh_081_to_999_renting_down),
              units_000_to_030_rented_to_higher_income = sum(units_000_to_030_rented_to_higher_income),
              units_031_to_050_rented_to_higher_income = sum(units_031_to_050_rented_to_higher_income),
              units_051_to_080_rented_to_higher_income = sum(units_051_to_080_rented_to_higher_income),
              units_081_to_999_rented_to_higher_income = 0,
              units_000_to_030_rented_to_lower_income = 0,
              units_031_to_050_rented_to_lower_income = sum(units_031_to_050_rented_to_lower_income),
              units_051_to_080_rented_to_lower_income = sum(units_051_to_080_rented_to_lower_income),
              units_081_to_999_rented_to_lower_income = sum(units_081_to_999_rented_to_lower_income))
    
  
  units_at_000_to_030 <- chas_t15c$matched_units_000_to_030 + chas_t14b$vacant_available_000_to_030 +
    chas_t15c$units_000_to_030_rented_to_higher_income + chas_t15c$units_000_to_030_rented_to_lower_income
  hh_at_000_to_030 <- chas_t15c$matched_units_000_to_030 + chas_t15c$hh_000_to_030_renting_up + 
    chas_t15c$hh_000_to_030_renting_down
  gross_gap_or_surplus_in_units_000_to_030 <- units_at_000_to_030 - hh_at_000_to_030
  effective_gap_or_surplus_in_units_000_to_030 <- gross_gap_or_surplus_in_units_000_to_030 - chas_t15c$units_000_to_030_rented_to_higher_income
  
  units_at_031_to_050 <- chas_t15c$matched_units_031_to_050 + chas_t14b$vacant_available_031_to_050 +
    chas_t15c$units_031_to_050_rented_to_higher_income + chas_t15c$units_031_to_050_rented_to_lower_income
  hh_at_031_to_050 <- chas_t15c$matched_units_031_to_050 + chas_t15c$hh_031_to_050_renting_up + 
    chas_t15c$hh_031_to_050_renting_down
  gross_gap_or_surplus_in_units_031_to_050 <- units_at_031_to_050 - hh_at_031_to_050
  effective_gap_or_surplus_in_units_031_to_050 <- gross_gap_or_surplus_in_units_031_to_050 - chas_t15c$units_031_to_050_rented_to_higher_income
  
  units_at_051_to_080 <- chas_t15c$matched_units_051_to_080 + chas_t14b$vacant_available_051_to_080 +
    chas_t15c$units_051_to_080_rented_to_higher_income + chas_t15c$units_051_to_080_rented_to_lower_income
  hh_at_051_to_080 <- chas_t15c$matched_units_051_to_080 + chas_t15c$hh_051_to_080_renting_up + 
    chas_t15c$hh_051_to_080_renting_down
  gross_gap_or_surplus_in_units_051_to_080 <- units_at_051_to_080 - hh_at_051_to_080
  effective_gap_or_surplus_in_units_051_to_080 <- gross_gap_or_surplus_in_units_051_to_080 - chas_t15c$units_051_to_080_rented_to_higher_income
  
  units_at_081_to_999 <- chas_t15c$matched_units_081_to_999 + chas_t14b$vacant_available_081_to_999 +
    chas_t15c$units_081_to_999_rented_to_higher_income + chas_t15c$units_081_to_999_rented_to_lower_income
  hh_at_081_to_999 <- chas_t15c$matched_units_081_to_999 + chas_t15c$hh_081_to_999_renting_up + 
    chas_t15c$hh_081_to_999_renting_down
  gross_gap_or_surplus_in_units_081_to_999 <- units_at_081_to_999 - hh_at_081_to_999
  effective_gap_or_surplus_in_units_081_to_999 <- gross_gap_or_surplus_in_units_081_to_999 - chas_t15c$units_081_to_999_rented_to_higher_income
  
  
  gap_analysis = data.frame("income_range" = c("0% to 30% MFI", "31% to 50% MFI", "51% to 80% MFI", "81% MFI or higher"), 
                            "total_households" = c(hh_at_000_to_030, hh_at_031_to_050, hh_at_051_to_080, hh_at_081_to_999),
                            "total_units" = c(units_at_000_to_030, units_at_031_to_050, units_at_051_to_080, units_at_081_to_999),
                            "matched_units_to_hh" = c(chas_t15c$matched_units_000_to_030, chas_t15c$matched_units_031_to_050, chas_t15c$matched_units_051_to_080, chas_t15c$matched_units_081_to_999),
                            "paying_too_much" = c(chas_t15c$hh_000_to_030_renting_up, chas_t15c$hh_031_to_050_renting_up, chas_t15c$hh_051_to_080_renting_up, chas_t15c$hh_081_to_999_renting_up),
                            "getting_a_deal" = c(chas_t15c$units_000_to_030_rented_to_higher_income, chas_t15c$units_031_to_050_rented_to_higher_income, chas_t15c$units_051_to_080_rented_to_higher_income, chas_t15c$units_081_to_999_rented_to_higher_income),
                            "vacant_units" = c(chas_t14b$vacant_available_000_to_030, chas_t14b$vacant_available_031_to_050, chas_t14b$vacant_available_051_to_080, chas_t14b$vacant_available_081_to_999),
                            "gross_existing_gap" = c(gross_gap_or_surplus_in_units_000_to_030, gross_gap_or_surplus_in_units_031_to_050, gross_gap_or_surplus_in_units_051_to_080, gross_gap_or_surplus_in_units_081_to_999),
                            "gap_in_available_units" = c(effective_gap_or_surplus_in_units_000_to_030, effective_gap_or_surplus_in_units_031_to_050, effective_gap_or_surplus_in_units_051_to_080, effective_gap_or_surplus_in_units_081_to_999))
  
  gap_analysis_plot_data <- data.frame("income_range" = c("0% to 30% MFI", "0% to 30% MFI", "31% to 50% MFI", "31% to 50% MFI", "51% to 80% MFI", "51% to 80% MFI"), 
                                       "control" = c("Households", "Units", "Households", "Units", "Households", "Units"),
                                       "total" = c(hh_at_000_to_030, units_at_000_to_030, hh_at_031_to_050, units_at_031_to_050, hh_at_051_to_080, units_at_051_to_080),
                                       "matched_units_to_hh" = c(chas_t15c$matched_units_000_to_030, chas_t15c$matched_units_000_to_030, chas_t15c$matched_units_031_to_050, chas_t15c$matched_units_031_to_050, chas_t15c$matched_units_051_to_080, chas_t15c$matched_units_051_to_080),
                                       "paying_too_much" = c(chas_t15c$hh_000_to_030_renting_up, chas_t15c$units_000_to_030_rented_to_higher_income, chas_t15c$hh_031_to_050_renting_up, chas_t15c$units_031_to_050_rented_to_higher_income, chas_t15c$hh_051_to_080_renting_up, chas_t15c$units_051_to_080_rented_to_higher_income),
                                       "vacant_units" = c(0, chas_t14b$vacant_available_000_to_030, 0, chas_t14b$vacant_available_031_to_050, 0, chas_t14b$vacant_available_051_to_080),
                                       "getting_a_deal" = c(chas_t15c$hh_000_to_030_renting_down, chas_t15c$units_000_to_030_rented_to_lower_income, chas_t15c$hh_031_to_050_renting_down, chas_t15c$units_031_to_050_rented_to_lower_income, chas_t15c$hh_051_to_080_renting_down, chas_t15c$units_051_to_080_rented_to_lower_income),
                                       "existing_gap" = c(0, as.numeric(gross_gap_or_surplus_in_units_000_to_030*-1), 0, as.numeric(gross_gap_or_surplus_in_units_031_to_050*-1), 0, as.numeric(gross_gap_or_surplus_in_units_051_to_080*-1)))
  
  gap_analysis_plot_data <- gather(gap_analysis_plot_data, c(matched_units_to_hh, paying_too_much, vacant_units, getting_a_deal, existing_gap), key = 'variable', value = 'value')
  gap_analysis_plot_data$variable <- factor(gap_analysis_plot_data$variable, ordered = T, levels = c('existing_gap', 'getting_a_deal', 'vacant_units', 'paying_too_much', 'matched_units_to_hh'))
  gap_analysis_plot_data <- gap_analysis_plot_data %>%
    mutate(share = value / total)
  
  group.colors <- c(getting_a_deal = '#D5D1D1', paying_too_much = '#D6B261', matched_units_to_hh = '#726868', vacant_units = '#B88F35', existing_gap = '#FFFFFF')
  
  p <- gap_analysis_plot_data %>% ggplot(aes(x = control, y = value, fill = variable)) +
    geom_col() +
    facet_wrap(~income_range) + 
    labs(title = 'RENTAL AFFORDABILITY GAP', subtitle = gtitle, x = "", y = "Number of renter households or units\n") +
    scale_y_continuous(label = comma) +
    scale_fill_manual(values = group.colors,
                      name = "",
                      breaks = c('existing_gap', 'getting_a_deal', 'vacant_units', 'paying_too_much', 'matched_units_to_hh'),
                      labels = c('Existing gap', 'Renting down\n(getting a deal)', 'Vacant units', 'Renting up\n(paying too much)', 'Matched units'))
  
  print(p)
  return(gap_analysis)
  
}


gap_analysis <- get_rental_housing_gap('4159000', 'City of Portland, 2015', chas.plc.2015.t14b, chas.plc.2015.t15c)

get_rental_housing_gap(ldist_east_fips_list, 'East Portland Liaison District, 2015', chas.tct.2015.t14b, chas.tct.2015.t15c)
get_rental_housing_gap(ldist_cc_fips_list, 'Central City Liaison District, 2015', chas.tct.2015.t14b, chas.tct.2015.t15c)
get_rental_housing_gap(ldist_west_fips_list, 'West Liaison District, 2015', chas.tct.2015.t14b, chas.tct.2015.t15c)
get_rental_housing_gap(ldist_ne_fips_list, 'Northeast Portland Liaison District, 2015', chas.tct.2015.t14b, chas.tct.2015.t15c)
get_rental_housing_gap(ldist_se_fips_list, 'Southeast Portland Liaison District, 2015', chas.tct.2015.t14b, chas.tct.2015.t15c)
get_rental_housing_gap(ldist_north_fips_list, 'North Portland Liaison District, 2015', chas.tct.2015.t14b, chas.tct.2015.t15c)

get_rental_housing_gap(sw_corridor_list, 'SW Corridor, 2015', chas.tct.2015.t14b, chas.tct.2015.t15c)

gap_analysis <- get_rental_housing_gap('0666000', 'City of San Diego, 2015', chas.plc.2015.t14b, chas.plc.2015.t15c)
gap_analysis <- get_rental_housing_gap('0667000', 'City of San Francisco, 2015', chas.plc.2015.t14b, chas.plc.2015.t15c)
gap_analysis <- get_rental_housing_gap('0820000', 'City of Denver, 2015', chas.plc.2015.t14b, chas.plc.2015.t15c)
gap_analysis <- get_rental_housing_gap('1836003', 'City of Indianapolis, 2015', chas.plc.2015.t14b, chas.plc.2015.t15c)
gap_analysis <- get_rental_housing_gap(c('2743000', '2758000'), 'Cities of Minneapolis and St. Paul, 2015', chas.plc.2015.t14b, chas.plc.2015.t15c)
gap_analysis <- get_rental_housing_gap('4752006', 'City of Nashville, 2015', chas.plc.2015.t14b, chas.plc.2015.t15c)
gap_analysis <- get_rental_housing_gap('4805000', 'City of Austin, 2015', chas.plc.2015.t14b, chas.plc.2015.t15c)
gap_analysis <- get_rental_housing_gap('4967000', 'Salt Lake City, 2015', chas.plc.2015.t14b, chas.plc.2015.t15c)
gap_analysis <- get_rental_housing_gap('4105800', 'City of Bend, 2015', chas.plc.2015.t14b, chas.plc.2015.t15c)
gap_analysis <- get_rental_housing_gap('5374060', 'City of Vancouver, 2015', chas.plc.2015.t14b, chas.plc.2015.t15c)
gap_analysis <- get_rental_housing_gap('4123850', 'City of Eugene, 2015', chas.plc.2015.t14b, chas.plc.2015.t15c)
gap_analysis <- get_rental_housing_gap('4164900', 'City of Salem, 2015', chas.plc.2015.t14b, chas.plc.2015.t15c)
gap_analysis <- get_rental_housing_gap('4131250', 'City of Gresham, 2015', chas.plc.2015.t14b, chas.plc.2015.t15c)
gap_analysis <- get_rental_housing_gap('4105350', 'City of Beaverton, 2015', chas.plc.2015.t14b, chas.plc.2015.t15c)
gap_analysis <- get_rental_housing_gap('4134100', 'City of Hillsboro, 2015', chas.plc.2015.t14b, chas.plc.2015.t15c)
gap_analysis <- get_rental_housing_gap('4173650', 'City of Tigard, 2015', chas.plc.2015.t14b, chas.plc.2015.t15c)

gap_analysis <- get_rental_housing_gap('41', 'Oregon, 2015', chas.st.2015.t14b, chas.st.2015.t15c)
gap_analysis <- get_rental_housing_gap('51', 'Washington, 2015', chas.st.2015.t14b, chas.st.2015.t15c)







##### More complex function to get low-income households by race by tenure ##### 
get_income_race_tenure <- function(geoid2_vector, chas_t1) {
  # census places must prepend the state fips (2-dig), e.g. '4159000' wrapped in quotes
  chas_t1 <- cast_chas_long(chas_t1 %>% filter(geoid2 %in% geoid2_vector), 7)
  chas_t1 <- cast_chas_wide(chas_t1)
  chas_t1 <- chas_t1 %>%
    summarize(total_hh = sum(v1_est),
              total_white = sum(v5_est) + sum(v13_est) + sum(v21_est) + sum(v29_est) + sum(v37_est) + 
                sum(v46_est) + sum(v54_est) + sum(v62_est) + sum(v70_est) + sum(v78_est) + sum(v87_est) + 
                sum(v95_est) + sum(v103_est) + sum(v111_est) + sum(v119_est) + sum(v129_est) + sum(v137_est) + 
                sum(v145_est) + sum(v153_est) + sum(v161_est) + sum(v170_est) + sum(v178_est) + sum(v186_est) + 
                sum(v194_est) + sum(v202_est) + sum(v211_est) + sum(v219_est) + sum(v227_est) + sum(v235_est) + sum(v243_est),
              total_aian = sum(v8_est) + sum(v16_est) + sum(v24_est) + sum(v32_est) + sum(v40_est) + sum(v49_est) + 
                sum(v57_est) + sum(v65_est) + sum(v73_est) + sum(v81_est) + sum(v90_est) + sum(v98_est) + 
                sum(v106_est) + sum(v114_est) + sum(v122_est) + sum(v132_est) + sum(v140_est) + sum(v148_est) + 
                sum(v156_est) + sum(v164_est) + sum(v173_est) + sum(v181_est) + sum(v189_est) + sum(v197_est) + 
                sum(v205_est) + sum(v214_est) + sum(v222_est) + sum(v230_est) + sum(v238_est) + sum(v246_est),
              total_asian = sum(v7_est) + sum(v15_est) + sum(v23_est) + sum(v31_est) + sum(v39_est) + sum(v48_est) + 
                sum(v56_est) + sum(v64_est) + sum(v72_est) + sum(v80_est) + sum(v89_est) + sum(v97_est) + 
                sum(v105_est) + sum(v113_est) + sum(v121_est) + sum(v131_est) + sum(v139_est) + sum(v147_est) + 
                sum(v155_est) + sum(v163_est) + sum(v172_est) + sum(v180_est) + sum(v188_est) + sum(v196_est) + 
                sum(v204_est) + sum(v213_est) + sum(v221_est) + sum(v229_est) + sum(v237_est) + sum(v245_est),
              total_black = sum(v6_est) + sum(v14_est) + sum(v22_est) + sum(v30_est) + sum(v38_est) + sum(v47_est) + 
                sum(v55_est) + sum(v63_est) + sum(v71_est) + sum(v79_est) + sum(v88_est) + sum(v96_est) + 
                sum(v104_est) + sum(v112_est) + sum(v120_est) + sum(v130_est) + sum(v138_est) + sum(v146_est) + 
                sum(v154_est) + sum(v162_est) + sum(v171_est) + sum(v179_est) + sum(v187_est) + sum(v195_est) + 
                sum(v203_est) + sum(v212_est) + sum(v220_est) + sum(v228_est) + sum(v236_est) + sum(v244_est),
              total_hispanic = sum(v10_est) + sum(v18_est) + sum(v26_est) + sum(v34_est) + sum(v42_est) + sum(v51_est) + 
                sum(v59_est) + sum(v67_est) + sum(v75_est) + sum(v83_est) + sum(v92_est) + sum(v100_est) + 
                sum(v108_est) + sum(v116_est) + sum(v124_est) + sum(v134_est) + sum(v142_est) + sum(v150_est) + 
                sum(v158_est) + sum(v166_est) + sum(v175_est) + sum(v183_est) + sum(v191_est) + sum(v199_est) + 
                sum(v207_est) + sum(v216_est) + sum(v224_est) + sum(v232_est) + sum(v240_est) + sum(v248_est),
              total_nhpi = sum(v9_est) + sum(v17_est) + sum(v25_est) + sum(v33_est) + sum(v41_est) + sum(v50_est) + 
                sum(v58_est) + sum(v66_est) + sum(v74_est) + sum(v82_est) + sum(v91_est) + sum(v99_est) + 
                sum(v107_est) + sum(v115_est) + sum(v123_est) + sum(v133_est) + sum(v141_est) + sum(v149_est) + 
                sum(v157_est) + sum(v165_est) + sum(v174_est) + sum(v182_est) + sum(v190_est) + sum(v198_est) + 
                sum(v206_est) + sum(v215_est) + sum(v223_est) + sum(v231_est) + sum(v239_est) + sum(v247_est),
              total_other = sum(v11_est) + sum(v19_est) + sum(v27_est) + sum(v35_est) + sum(v43_est) + sum(v52_est) + 
                sum(v60_est) + sum(v68_est) + sum(v76_est) + sum(v84_est) + sum(v93_est) + sum(v101_est) + 
                sum(v109_est) + sum(v117_est) + sum(v125_est) + sum(v135_est) + sum(v143_est) + sum(v151_est) + 
                sum(v159_est) + sum(v167_est) + sum(v176_est) + sum(v184_est) + sum(v192_est) + sum(v200_est) + 
                sum(v208_est) + sum(v217_est) + sum(v225_est) + sum(v233_est) + sum(v241_est) + sum(v249_est),
              total_poc = total_aian + total_asian + total_black + total_nhpi + total_other + total_hispanic,
              li_white_owner = sum(v5_est) + sum(v13_est) + sum(v21_est) + sum(v46_est) + sum(v54_est) + 
                sum(v62_est) + sum(v87_est) + sum(v95_est) + sum(v103_est),
              li_white_renter = sum(v129_est) + sum(v137_est) + sum(v145_est) + sum(v170_est) + sum(v178_est) + 
                sum(v186_est) + sum(v211_est) + sum(v219_est) + sum(v227_est),
              li_white = li_white_owner + li_white_renter,
              li_aian_owner = sum(v8_est) + sum(v16_est) + sum(v24_est) + sum(v49_est) + sum(v57_est) + 
                sum(v65_est) + sum(v90_est) + sum(v98_est) + sum(v106_est),
              li_aian_renter = sum(v132_est) + sum(v140_est) + sum(v148_est) + sum(v173_est) + sum(v181_est) + 
                sum(v189_est) + sum(v214_est) + sum(v222_est) + sum(v230_est),
              li_aian = li_aian_owner + li_aian_renter,
              li_asian_owner = sum(v7_est) + sum(v15_est) + sum(v23_est) + sum(v48_est) + sum(v56_est) + 
                sum(v64_est) + sum(v89_est) + sum(v97_est) + sum(v105_est),
              li_asian_renter = sum(v131_est) + sum(v139_est) + sum(v147_est) + sum(v172_est) + sum(v180_est) + 
                sum(v188_est) + sum(v213_est) + sum(v221_est) + sum(v229_est),
              li_asian = li_asian_owner + li_asian_renter,
              li_black_owner = sum(v6_est) + sum(v14_est) + sum(v22_est) + sum(v47_est) + sum(v55_est) + 
                sum(v63_est) + sum(v88_est) + sum(v96_est) + sum(v104_est),
              li_black_renter = sum(v130_est) + sum(v138_est) + sum(v146_est) + sum(v171_est) + sum(v179_est) + 
                sum(v187_est) + sum(v212_est) + sum(v220_est) + sum(v228_est),
              li_black = li_black_owner + li_black_renter,
              li_hispanic_owner = sum(v10_est) + sum(v18_est) + sum(v26_est) + sum(v51_est) + sum(v59_est) + 
                sum(v67_est) + sum(v92_est) + sum(v100_est) + sum(v108_est),
              li_hispanic_renter = sum(v134_est) + sum(v142_est) + sum(v150_est) + sum(v175_est) + sum(v183_est) + 
                sum(v191_est) + sum(v216_est) + sum(v224_est) + sum(v232_est),
              li_hispanic = li_hispanic_owner + li_hispanic_renter,
              li_nhpi_owner = sum(v9_est) + sum(v17_est) + sum(v25_est) + sum(v50_est) + sum(v58_est) + 
                sum(v66_est) + sum(v91_est) + sum(v99_est) + sum(v107_est),
              li_nhpi_renter = sum(v133_est) + sum(v141_est) + sum(v149_est) + sum(v174_est) + sum(v182_est) + 
                sum(v190_est) + sum(v215_est) + sum(v223_est) + sum(v231_est),
              li_nhpi = li_nhpi_owner + li_nhpi_renter,
              li_other_owner = sum(v11_est) + sum(v19_est) + sum(v27_est) + sum(v52_est) + sum(v60_est) + 
                sum(v68_est) + sum(v93_est) + sum(v101_est) + sum(v109_est),
              li_other_renter = sum(v135_est) + sum(v143_est) + sum(v151_est) + sum(v176_est) + sum(v184_est) + 
                sum(v192_est) + sum(v217_est) + sum(v225_est) + sum(v233_est),
              li_other = li_other_owner + li_other_renter,
              li_poc_owner = li_aian_owner + li_asian_owner + li_black_owner + li_hispanic_owner + li_nhpi_owner + li_other_owner,
              li_poc_renter = li_aian_renter + li_asian_renter + li_black_renter + li_hispanic_renter + li_nhpi_renter + li_other_renter,
              li_poc = li_poc_owner + li_poc_renter,
              li_owner = li_white_owner + li_poc_owner,
              li_renter = li_white_renter + li_poc_renter,
              li_total = li_owner + li_renter,
              share_hh_that_li = li_total / total_hh,
              share_hh_that_li_owner = li_owner / total_hh,
              share_hh_that_li_renter = li_renter / total_hh,
              share_poc_that_li = li_poc / total_poc,
              share_poc_that_li_renter = li_poc_renter / total_poc,
              share_white_that_li = li_white / total_white,
              share_white_that_li_renter = li_white_renter / total_white,
              share_hh_that_poc = total_poc / total_hh,
              share_hh_that_li_poc = li_poc / total_hh,
              share_hh_that_li_poc_renter = li_poc_renter / total_hh,
              ratio_li_poc_vs_white = share_poc_that_li / share_white_that_li,
              ratio_li_renter_poc_vs_white = share_poc_that_li_renter / share_white_that_li_renter,
              total_hh_moe = sqrt(sum(v1_moesq)),
              total_white_moe = sqrt( sum(v5_moesq) + sum(v13_moesq) + sum(v21_moesq) + sum(v29_moesq) + sum(v37_moesq) + 
                                        sum(v46_moesq) + sum(v54_moesq) + sum(v62_moesq) + sum(v70_moesq) + sum(v78_moesq) + 
                                        sum(v87_moesq) + sum(v95_moesq) + sum(v103_moesq) + sum(v111_moesq) + sum(v119_moesq) + 
                                        sum(v129_moesq) + sum(v137_moesq) + sum(v145_moesq) + sum(v153_moesq) + sum(v161_moesq) + 
                                        sum(v170_moesq) + sum(v178_moesq) + sum(v186_moesq) + sum(v194_moesq) + sum(v202_moesq) + 
                                        sum(v211_moesq) + sum(v219_moesq) + sum(v227_moesq) + sum(v235_moesq) + sum(v243_moesq) ),
              total_aian_moe = sqrt( sum(v8_moesq) + sum(v16_moesq) + sum(v24_moesq) + sum(v32_moesq) + 
                                       sum(v40_moesq) + sum(v49_moesq) + sum(v57_moesq) + sum(v65_moesq) + 
                                       sum(v73_moesq) + sum(v81_moesq) + sum(v90_moesq) + sum(v98_moesq) + 
                                       sum(v106_moesq) + sum(v114_moesq) + sum(v122_moesq) + sum(v132_moesq) + 
                                       sum(v140_moesq) + sum(v148_moesq) + sum(v156_moesq) + sum(v164_moesq) + 
                                       sum(v173_moesq) + sum(v181_moesq) + sum(v189_moesq) + sum(v197_moesq) + 
                                       sum(v205_moesq) + sum(v214_moesq) + sum(v222_moesq) + sum(v230_moesq) + 
                                       sum(v238_moesq) + sum(v246_moesq) ),
              total_asian_moe = sqrt( sum(v7_moesq) + sum(v15_moesq) + sum(v23_moesq) + sum(v31_moesq) + 
                                        sum(v39_moesq) + sum(v48_moesq) + sum(v56_moesq) + sum(v64_moesq) + 
                                        sum(v72_moesq) + sum(v80_moesq) + sum(v89_moesq) + sum(v97_moesq) + 
                                        sum(v105_moesq) + sum(v113_moesq) + sum(v121_moesq) + sum(v131_moesq) + 
                                        sum(v139_moesq) + sum(v147_moesq) + sum(v155_moesq) + sum(v163_moesq) + 
                                        sum(v172_moesq) + sum(v180_moesq) + sum(v188_moesq) + sum(v196_moesq) + 
                                        sum(v204_moesq) + sum(v213_moesq) + sum(v221_moesq) + sum(v229_moesq) + 
                                        sum(v237_moesq) + sum(v245_moesq) ),
              total_black_moe = sqrt( sum(v6_moesq) + sum(v14_moesq) + sum(v22_moesq) + sum(v30_moesq) + 
                                        sum(v38_moesq) + sum(v47_moesq) + sum(v55_moesq) + sum(v63_moesq) + 
                                        sum(v71_moesq) + sum(v79_moesq) + sum(v88_moesq) + sum(v96_moesq) + 
                                        sum(v104_moesq) + sum(v112_moesq) + sum(v120_moesq) + sum(v130_moesq) + 
                                        sum(v138_moesq) + sum(v146_moesq) + sum(v154_moesq) + sum(v162_moesq) + 
                                        sum(v171_moesq) + sum(v179_moesq) + sum(v187_moesq) + sum(v195_moesq) + 
                                        sum(v203_moesq) + sum(v212_moesq) + sum(v220_moesq) + sum(v228_moesq) + 
                                        sum(v236_moesq) + sum(v244_moesq) ),
              total_hispanic_moe = sqrt( sum(v10_moesq) + sum(v18_moesq) + sum(v26_moesq) + sum(v34_moesq) + 
                                           sum(v42_moesq) + sum(v51_moesq) + sum(v59_moesq) + sum(v67_moesq) + 
                                           sum(v75_moesq) + sum(v83_moesq) + sum(v92_moesq) + sum(v100_moesq) + 
                                           sum(v108_moesq) + sum(v116_moesq) + sum(v124_moesq) + sum(v134_moesq) + 
                                           sum(v142_moesq) + sum(v150_moesq) + sum(v158_moesq) + sum(v166_moesq) + 
                                           sum(v175_moesq) + sum(v183_moesq) + sum(v191_moesq) + sum(v199_moesq) + 
                                           sum(v207_moesq) + sum(v216_moesq) + sum(v224_moesq) + sum(v232_moesq) + 
                                           sum(v240_moesq) + sum(v248_moesq) ),
              total_nhpi_moe = sqrt( sum(v9_moesq) + sum(v17_moesq) + sum(v25_moesq) + sum(v33_moesq) + sum(v41_moesq) + 
                                       sum(v50_moesq) + sum(v58_moesq) + sum(v66_moesq) + sum(v74_moesq) + sum(v82_moesq) + 
                                       sum(v91_moesq) + sum(v99_moesq) + sum(v107_moesq) + sum(v115_moesq) + sum(v123_moesq) + 
                                       sum(v133_moesq) + sum(v141_moesq) + sum(v149_moesq) + sum(v157_moesq) + sum(v165_moesq) + 
                                       sum(v174_moesq) + sum(v182_moesq) + sum(v190_moesq) + sum(v198_moesq) + sum(v206_moesq) + 
                                       sum(v215_moesq) + sum(v223_moesq) + sum(v231_moesq) + sum(v239_moesq) + sum(v247_moesq) ),
              total_other_moe = sqrt( sum(v11_moesq) + sum(v19_moesq) + sum(v27_moesq) + sum(v35_moesq) + sum(v43_moesq) + 
                                        sum(v52_moesq) + sum(v60_moesq) + sum(v68_moesq) + sum(v76_moesq) + 
                                        sum(v84_moesq) + sum(v93_moesq) + sum(v101_moesq) + sum(v109_moesq) + 
                                        sum(v117_moesq) + sum(v125_moesq) + sum(v135_moesq) + sum(v143_moesq) + 
                                        sum(v151_moesq) + sum(v159_moesq) + sum(v167_moesq) + sum(v176_moesq) + 
                                        sum(v184_moesq) + sum(v192_moesq) + sum(v200_moesq) + sum(v208_moesq) + 
                                        sum(v217_moesq) + sum(v225_moesq) + sum(v233_moesq) + sum(v241_moesq) + 
                                        sum(v249_moesq) ),
              total_poc_moe = sqrt(total_aian_moe^2 + total_asian_moe^2 + total_black_moe^2 + total_nhpi_moe^2 + total_other_moe^2 + total_hispanic_moe^2) )
  return(chas_t1)
}

# Examples for MSA total, SW Corridor and Portland
glimpse(get_income_race_tenure(msa_geoid2_vector, chas.cnty.2015.t1))
glimpse(get_income_race_tenure(sw_corridor_list, chas.tct.2015.t1))
glimpse(get_income_race_tenure('4159000', chas.plc.2015.t1))
glimpse(get_income_race_tenure(icura_list, chas.tct.2015.t1))

p <- print(paste0("Roseway-Cully contains ", round((get_income_race_tenure(roseway_cully_fips_list, chas.tct.2015.t1)[[5]] / get_income_race_tenure('4159000', chas.plc.2015.t1)[[5]] * 100), 1 )," percent of all Black households."))

##### BETA: Cleanup tract-level data for 2009-2012 data ##### 
cleanup_chas_tract <- function(chas_tract_data, year = 2015) {
  refcols <- c('source', 'sumlevel', 'geoid', 'geoid2', 'name', 'st', 'cnty_plc')
  refcols_vintage <- c('source', 'geoid', 'geoid2', 'name', 'st', 'cnty_plc')
  if (year >= 2013) {
    chas_tract_data <- chas_tract_data %>% select(-tract)
    names(chas_tract_data)[names(chas_tract_data) == 'cnty'] <- 'cnty_plc'
    chas_tract_data$geoid2 <- substr(chas_tract_data$geoid, 8, 18)
    chas_tract_data <- chas_tract_data[, c(refcols, setdiff(names(chas_tract_data), refcols))]
    return(chas_tract_data)
  } else if (year %in% c(2009, 2010, 2011, 2012)) {
    chas_tract_data$name <- ''
    chas_tract_data$st <- substr(chas_tract_data$geoid, 8, 9)
    chas_tract_data$cnty_plc <- substr(chas_tract_data$geoid, 10, 12)
    chas_tract_data$geoid2 <- paste0(substr(chas_tract_data$geoid, 8, 12), substr(chas_tract_data$geoid, 23, 28))
    chas_tract_data <- chas_tract_data[, c(refcols_vintage, setdiff(names(chas_tract_data), refcols))]
    chas_header <- names(chas_tract_data)[1:6]
    tct_moes <- cbind(chas_header, chas_tract_data[, grepl("moe", names(chas_tract_data))])
    #tct_ests <- cbind(chas_header, chas_tract_data[, grepl("est", names(chas_tract_data))])
    # cast_chas_long(chas_tract_data)
    #chas_tract_data <- chas_tract_data %>%
    #  group_by(source, sumlevel, geoid2, name, st, cnty_plc) %>%
    #  summarize_at(vars(names(chas_tract_data[,8:length(names(chas_tract_data))])), funs(sum))
    tct_moes <- tct_moes %>%
      group_by(source, sumlevel, geoid2, name, st, cnty_plc) %>%
      summarize_at(vars(names(tct_moes[,8:length(names(tct_moes))])), funs(sqr))
    return(tct_moes)
    } else return(warning("Error: Please select a valid year (2009 through 2015). CHAS 2000 under development."))
}



##### Random bits of code ##### 
test2 <- cleanup_chas_tract(chas.tct.2012.t1, year = 2012)
glimpse(test2)


test <- chas.msa.15.t1 %>%
  filter(geoid2 %in% sw_corridor_list) %>%
  group_by(var_num) %>%
  summarize(sumest = sum(est),
            summoe = kw_moe_sum(moe, est))

test

kw_moe_sum <- function(moe, estimate = NULL) {
  if (!is.null(estimate)) {
    # ID those MOE values with 0 estimates
    zeros <- estimate == 0
    # Reduce the vector and keep the first one
    onezero <- unique(moe[zeros])
    # Combine with the non-zeros
    forcalc <- c(onezero, moe[!zeros])
  } else if (is.null(estimate)) {
    warning("You have not specified the estimates associated with the margins of error.  In the event that your calculation involves multiple zero estimates, this will unnaturally inflate the derived margin of error.", call. = FALSE)
    forcalc <- moe
  }
  squared <- map_dbl(forcalc, function(x) x^2)
  result <- sqrt(sum(squared))
  return(result)
}
