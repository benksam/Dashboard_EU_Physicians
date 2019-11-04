
### ---- PS 0: Population -----
### Europe population
tot_pop = read.csv("1_RawData/tot_pop.csv")

# 2017 for PCA1: Description of number of doctors per country!!! 
pop17 = tot_pop %>% filter(wstatus == "POP") %>% 
  filter(sex == "T") %>% 
  filter(age == "Y_GE25") %>% 
  filter(deg_urb == "TOTAL") %>% 
  filter(time == 2017)

pop17 = pop17 %>% 
  select(geo, pop = values)

# 2015 Total Population Over 25 years
pop15 = tot_pop %>% filter(wstatus == "POP") %>% 
  filter(sex == "T") %>% 
  filter(age == "Y_GE25") %>% 
  filter(deg_urb == "TOTAL") %>% 
  filter(time == 2015)


### ---- PS 1: GDP per cap -----
gdp_cap = read_csv("1_RawData/gdp_cap.csv")

gdp_cap17 = gdp_cap %>% filter(unit == "CLV10_EUR_HAB") %>% 
  filter(time == 2017)

gdp_cap15 = gdp_cap %>% filter(unit == "CLV10_EUR_HAB") %>% 
  filter(time == 2015)


### ---- PS 2: NUTS0 data -----
nuts0g = st_read("1_RawData/EU_Nuts0.shp")


### ---- 1. Europe Physicians Based on long for PCA1: 2017 data per inhabitants -----
spec_doct = read_csv("1_RawData/special_doctors.csv")

spec_doct17 = spec_doct %>% filter(unit == "P_HTHAB") %>% 
  filter(time == 2017)

special17 = spec_doct17 %>% pull(med_spec) %>% unique()

spec_doct17rc = spec_doct17 %>% 
  mutate(med_spec = as.factor(med_spec),
         geo = as.factor(geo),
         title = fct_recode(med_spec,
                            "Gynaecologists and obstetricians" = "GYN",          
                            "Psychiatrists" = "PSY",                             
                            "Medical group of specialists" = "MED",              
                            "Surgical group of specialists" = "SURG",            
                            "Other specialists not elsewhere classified" = "OTH",
                            "Medical doctors not further defined" = "NSP",       
                            "Generalist medical practitioners" = "GEN",          
                            "General practitioners" = "GEN_PRAC",                
                            "Other generalist medical practitioners" = "GEN_OTH",
                            "Specialist medical practitioners" = "SPEC",         
                            "General paediatricians" = "PAED"    ),
         title = as.factor(title))

### Unique specialities
special17rc = spec_doct17rc %>% pull(title) %>% unique()


### GIS data
spec_doct17rc_g =  nuts0g %>% select(geo, NUTS_NAME) %>% 
  left_join(spec_doct17rc %>% 
              select(geo, title, values), 
            by = c("geo")) %>% 
  filter(!(is.na(title))) %>% 
  filter(!(is.na(values))) %>% 
  rename(Country = geo,
         Country_Name = NUTS_NAME,
         Title = title)

df_spec_doct17rc_g = spec_doct17rc_g %>% 
  st_drop_geometry() %>%
  mutate(Country = as.factor(Country))


### ----- PS 3: Relabelling from FULL DOCTORS (from Visit data): recode labels of factor!!! ------
### Merged and with duplicates
Doctor_Specialities = read.csv("1_RawData/Doctor_Specialities.csv")
Doctor_Specialities = distinct(Doctor_Specialities)


spec_doct0815 = spec_doct %>% 
  filter(unit == "P_HTHAB") %>%
  filter(time > 2007 & time < 2016)

# From raw data
special0815 = spec_doct0815 %>% pull(med_spec) %>% unique 


Doctor_Specialities0815 = Doctor_Specialities %>% 
  filter(Acronym %in% special0815)


### Preparing and shaping data for easy recoding
Doctor_Specialities0815 = Doctor_Specialities0815 %>% 
  unite(united, Speciality, Acronym, sep = ' =  ', remove = FALSE)

# From raw data
# Doctor_Specialities0815 = Doctor_Specialities %>% 
#   filter(Acronym %in% special0815)

spec_doct_recoded = spec_doct0815 %>% 
  filter(unit == "P_HTHAB") %>% 
  mutate(geo = as.factor(geo),
         med_spec = as.factor(med_spec)) %>% 
  mutate(spec = fct_recode(med_spec, 
                           "Gynaecologists and obstetricians" = "GYN",          
                           "Psychiatrists" = "PSY"                    ,         
                           "Medical group of specialists" = "MED"      ,        
                           "Internal medicine" = "MED_INT"              ,       
                           "Cardiology" = "MED_CAR"                      ,      
                           "Endocrinology" = "MED_END"                    ,     
                           "Gastroenterology" = "MED_GAS"                  ,    
                           "Respiratory medicine" = "MED_RES"               ,   
                           "Oncology" = "MED_ONC"                            ,  
                           "Immunology" = "MED_IMM"                           , 
                           "Neurology" = "MED_NEU"                             ,
                           "Otorhinolaryngology" = "MED_ORL"                   ,
                           "Radiology" = "MED_RAD"                             ,
                           "Microbiology-bacteriology" = "MED_MIC"             ,
                           "Haematology" = "MED_HAE"                           ,
                           "Dermatology" = "MED_DER"                           ,
                           "Pathology" = "MED_PAT"                             ,
                           "Occupational medicine" = "MED_OCC"                 ,
                           "Surgical group of specialists" = "SURG"            ,
                           "General surgery" = "SURG_GEN"                      ,
                           "Neurological surgery" = "SURG_NEU"                 ,
                           "Plastic surgery" = "SURG_PLA"                      ,
                           "Ophthalmology" = "SURG_OPH"                        ,
                           "Orthopaedics" = "SURG_ORT"                         ,
                           "Thoracic surgery" = "SURG_THO"                     ,
                           "Vascular surgery" = "SURG_VAS"                     ,
                           "Anaesthesiology and intensive care" = "SURG_ANE"   ,
                           "Urology" = "SURG_URO"                          ,    
                           "Accident and emergency medicine" = "SURG_EME"   ,   
                           "Other specialists not elsewhere classified" = "OTH",
                           "Medical doctors not further defined" = "NSP"       ,
                           "Generalist medical practitioners" = "GEN"          ,
                           "General practitioners" = "GEN_PRAC"                ,
                           "Other generalist medical practitioners" = "GEN_OTH",
                           "Specialist medical practitioners" = "SPEC"         ,
                           "General paediatricians" = "PAED" ))

### Unique relabelled levels!
spec_TS = spec_doct_recoded %>% pull(spec) %>% unique


###### - 2. GrowthRates from Time Series Data 13 - 15 (PCA2) ----

### Long for plots
spec_doct_rc_gr = spec_doct_recoded %>% 
  filter(unit == "P_HTHAB") %>% 
  filter(time %in% c(2013:2015)) %>% 
  spread(time, values) %>% 
  mutate(GrowthRate = 100*(`2015`/`2013`-1)/2)

###### ------ PS 4: Comparing 2008 and 2015 Point Chart ---- 
### Use variables from PCA1 (2017) and PCA2 (gr 1315)
### Select relevant variables for comparing 2008 and 2015! 
#### (Based on erlier PCA)

spec_doct08_15 = spec_doct_recoded %>% 
  filter(time == 2008 | time == 2015) %>% 
  mutate(time = as.factor(time))


### Check lapply for looped plots
spec_08_15 = spec_doct08_15 %>% pull(spec) %>% unique()



### Merging with GIS data _ Specialities
spec_doct_rc_gr_g =  nuts0g %>% select(geo, NUTS_NAME) %>% 
  left_join(spec_doct_rc_gr %>% 
              select(geo, spec, GrowthRate), 
            by = c("geo")) %>% 
  filter(!(is.na(spec))) %>% 
  filter(!(is.na(GrowthRate))) %>% 
  rename(Country = geo,
         Country_Name = NUTS_NAME,
         Title = spec)

df_spec_doct_rc_gr_g = spec_doct_rc_gr_g %>% 
  st_drop_geometry() %>% 
  mutate(Title = as.factor(Title)) %>% 
  mutate(Country = as.factor(Country)) 



### ----- 3. Visits Totals - Based on long for PCA3:   ------
visit12_doct_1 = read_csv("1_RawData/visit12_doct_1.csv")


### Raw Long Data - TOTAL aggregates
visit12_doct1_tot = visit12_doct_1 %>% 
  filter(isced11 == "TOTAL") %>% 
  filter(sex == "T") %>% 
  filter(age == "TOTAL") %>% 
  select(-unit, -time, -isced11,-sex, -age)


#### FLEX part
# relevelling levels (frequency logic)

visit12_doct1_tot = visit12_doct1_tot %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(frequenc = fct_relevel(frequenc,
                                "NVR", "1-2", "3-5", "6-9" , "GE10")) %>% 
  mutate(med_spec = fct_relevel(med_spec,
                                "GEN", "SURG_GEN", "MED_DDS"))

### recoding factors
visit12_doct1_tot = visit12_doct1_tot %>% 
  rename(Title = med_spec,
         Frequency = frequenc) %>% 
  mutate(Title = fct_recode(Title,
                            "Generalists" = "GEN",
                            "Dentists" = "MED_DDS",
                            "Surgeons" = "SURG_GEN")) %>% 
  mutate(Frequency = fct_recode(Frequency,
                                "10+" = "GE10",
                                "0" = "NVR")) 

### United factors
visit12_doct1_tot = visit12_doct1_tot %>%
  unite(Physician_Visits, Frequency, Title, sep = " visits to ", remove = FALSE)


visit12_doct1_tot = visit12_doct1_tot %>% 
  mutate(Physician_Visits = fct_relevel(Physician_Visits,
                                        "0 visits to Generalists",
                                        "1-2 visits to Generalists",
                                        "3-5 visits to Generalists",
                                        "6-9 visits to Generalists",
                                        "10+ visits to Generalists",
                                        "0 visits to Surgeons",
                                        "1-2 visits to Surgeons",
                                        "3-5 visits to Surgeons",
                                        "6-9 visits to Surgeons",
                                        "10+ visits to Surgeons",
                                        "0 visits to Dentists",
                                        "1-2 visits to Dentists",
                                        "3-5 visits to Dentists",
                                        "6-9 visits to Dentists",
                                        "10+ visits to Dentists"
  ))

### Selecting only Countries - No EU28 ...
countries_vis_tot = visit12_doct1_tot %>% 
  distinct(geo) %>% 
  pull()

countries_vis_tot = countries_vis_tot[c(1,5:35)]

### EU countries ALONE
visit12_doct1_tot_country = visit12_doct1_tot %>% 
  filter(geo %in% countries_vis_tot) %>% 
  mutate(geo = droplevels(geo))


visit12_doct1_tot_gcountry = nuts0g %>% 
  select(geo, NUTS_NAME) %>% 
  left_join(visit12_doct1_tot_country %>% 
              filter(geo != "EU28"), by = "geo") %>% 
  filter(!(is.na(values)))

visit12_doct1_tot_gcountry = visit12_doct1_tot_gcountry %>% 
  rename(Country = geo,
         Country_Name = NUTS_NAME,
         Percentage = values) 



### ----- 4. Visits Freq/Edu/Age Based on long for PCA4:   ------


### Raw Long Data - Freq, Education and Age
visit12_doct1_age_edu = visit12_doct_1 %>% 
  
  filter(med_spec == "GEN") %>% 
  filter(sex == "T") %>% 
  filter(frequenc %in% c("NVR", "GE10")) %>% 
  filter(isced11 != "TOTAL") %>% 
  filter(age %in% c("Y25-34","Y35-49", "Y50-64")) %>% 

  select(-unit, -time, -med_spec, -sex)


#### FLEX part

### For .sidebar DT
visit12_doct1_age_edu_country = visit12_doct1_age_edu %>% 
  filter(!(geo %in% c("EU27", "EA19", "EA18")))   # "EU28" KEPT!!!!

visit12_doct1_age_edu_country = visit12_doct1_age_edu_country %>% 
  mutate_if(is.character, as.factor)


# Recoding and Uniting
visit12_doct1_age_edu_country = visit12_doct1_age_edu_country %>% 
  mutate(frequenc = fct_recode(frequenc,
                               "10+ visits" = "GE10",
                               "0 visits" = "NVR")) %>% 
  unite(Frequency_Edu, frequenc, isced11, sep = " for ", remove = FALSE) %>% 
  unite(Frequency_Edu_Age, Frequency_Edu, age, sep = " and for ", remove = FALSE) %>% 
  select(geo, Frequency_Edu_Age, frequenc, isced11, age, values) %>% 
  mutate(Frequency_Edu_Age = as.factor(Frequency_Edu_Age))


### Distinct combinations for Input-Selection
frequency_edu_age = visit12_doct1_age_edu_country %>% 
  distinct(Frequency_Edu_Age) %>% 
  pull()

### Input-Selection reordered logically!
frequency_edu_age = frequency_edu_age[c(10:18,1:9)]

### Merging GIS data
visit12_doct1_age_edu_gcountry = nuts0g %>% 
  select(geo, NUTS_NAME) %>%
  left_join(visit12_doct1_age_edu_country, by = "geo") %>% 
  filter(!(is.na(values)))


visit12_doct1_age_edu_gcountry = visit12_doct1_age_edu_gcountry %>% 
  rename(Country = geo,
         Country_Name = NUTS_NAME,
         Percentage = values) 



### ----- 5. Visits Freq/Inc/Employment Based on long for PCA4:   ------

visit12_doct_2 = read_csv("1_RawData/visit12_doct_2.csv")


### Selection FREQ, EMPLOI, SALAIRE
doct_2_sel = visit12_doct_2 %>% 
  filter(med_spec == "GEN") %>% 
  filter(frequenc %in% c("NVR", "GE10")) %>% 
  filter(wstatus %in% c("EMP", "UNE")) %>% 
  filter(deg_urb %in% c("TOTAL")) %>%
  filter(quantile %in% c("QU1", "QU3", "QU4")) %>%

  select(-med_spec, -time, -unit, -deg_urb)


#### ----- For plotting point graphs ------

### New data filtering
### 1. GE10 visiting by employment and quantile
visit12_doct1_ge10_emp_quant = visit12_doct_2 %>% 
  filter(deg_urb == "TOTAL") %>% 
  filter(med_spec == "GEN") %>% 
  filter(frequenc %in% c("GE10")) %>% # "NVR", 

  filter(quantile %in% c("QU1", "QU3", "QU4")) %>% 
  filter(wstatus %in% c("EMP", "UNE")) %>% 
  
  
  select(-unit, -time, -deg_urb, -med_spec, -frequenc)



# => Taking away missing trios in UNEMPLOYED!!!

geo_ge10_unemp = visit12_doct1_ge10_emp_quant  %>% 
  # select(geo, time, values, spec) %>% 
  arrange(geo) %>% 
  filter(wstatus == "UNE") %>% 
  filter(!(is.na(values))) %>%  
  count(geo) %>% 
  filter(n<3) %>% 
  pull(geo)




### New data filtering
### 2. NVR visiting by employment and quantile
visit12_doct1_nvr_emp_quant = visit12_doct_2 %>% 
  filter(deg_urb == "TOTAL") %>% 
  filter(med_spec == "GEN") %>% 
  filter(frequenc %in% c("NVR")) %>% # "NVR", 
  
  filter(quantile %in% c("QU1", "QU3", "QU4")) %>% 
  filter(wstatus %in% c("EMP", "UNE")) %>% 
  
  
  select(-unit, -time, -deg_urb, -med_spec, -frequenc)




# => Taking away missing trios in UNEMPLOYED!!!

geo_nvr_unemp = visit12_doct1_nvr_emp_quant  %>% 
  # select(geo, time, values, spec) %>% 
  arrange(geo) %>% 
  filter(wstatus == "UNE") %>% 
  filter(!(is.na(values))) %>%  
  count(geo) %>% 
  filter(n<3) %>% 
  pull(geo)






### ----- Flex PC5 part: Selecting countries: + EU28 for summary stat! ------
country_em_inc = doct_2_sel %>% pull(geo) %>% unique
country_em_inc = country_em_inc[c(1,5:35)]

doct_2_sel_country = doct_2_sel %>% 
  filter(geo %in% country_em_inc)


### Distinct combinations 

doct_2_sel_country = doct_2_sel_country %>% 
  mutate(frequenc = fct_recode(frequenc,
                               "10+ visits" = "GE10",
                               "0 visits" = "NVR")) %>% 
  mutate(wstatus = fct_recode(wstatus,
                              "Employed" = "EMP",
                              "Unemployed" = "UNE")) %>% 
  
  unite(Frequency_EM, frequenc, wstatus, sep = " for ", remove = FALSE) %>% 
  unite(Frequency_Employ_Income, Frequency_EM, quantile, sep = " with income ", remove = FALSE) %>% 
  select(geo, Frequency_Employ_Income, frequenc, wstatus, quantile, values) %>% 
  mutate(Frequency_Employ_Income = as.factor(Frequency_Employ_Income)) %>% 
  mutate(geo = as.factor(geo))


frequency_em_inc = doct_2_sel_country %>% 
  distinct(Frequency_Employ_Income) %>% 
  pull() %>% 
  as.character()

frequency_em_inc = frequency_em_inc[c(7:12,1:6)]


# GiS data
doct_2_sel_gcountry = nuts0g %>% 
  select(geo, NUTS_NAME) %>% 
  left_join(doct_2_sel_country %>% 
              filter(geo != "EU28"), by = "geo") %>% 
  filter(!(is.na(values)))

doct_2_sel_gcountry = doct_2_sel_gcountry %>% 
  rename(Country = geo,
         Country_Name = NUTS_NAME,
         Percentage = values) 






