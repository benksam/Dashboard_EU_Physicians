
####### ------- FLEX1: Summary tables -------

### Create a function for summary statistics

df = list(spec_doct17rc_g, spec_doct_rc_gr_g)
df_names = c("spec_doct17rc_g", "spec_doct_rc_gr_g")

fun_stat = function(data){
  data %>% 
    st_drop_geometry() %>% 
    group_by(Title) %>% 
    nest() %>% 
    mutate(Min = map_dbl(data, ~min(.x[[3]], na.rm = TRUE)),
           Median = map_dbl(data, ~median(.x[[3]], na.rm = TRUE)),
           Mean = map_dbl(data, ~mean(.x[[3]], na.rm = TRUE)),
           Max = map_dbl(data, ~max(.x[[3]], na.rm = TRUE)),
           Std_Deviation = map_dbl(data, ~sd(.x[[3]], na.rm = TRUE))) %>% 
    select(-data)
}


df_stats = map(df, fun_stat) %>% 
  set_names(df_names)


Europe_Physicians_Sum = df_stats[[1]]
# GrowthRates_Physicians_Sum = df_stats[[2]] # Pbm with Inf and NanA
GrowthRates_Physicians_Sum = read_csv("1_RawData/GrowthRates_Physicians_Sum.csv")



####### ------- FLEX2: Extra tables -------

### Difference 2008 - 2015  (extension to Growth Rates of Europe Physicians)
### Differentials

spec_doct08_15_stats = spec_doct08_15 %>% 
  select(-unit) %>% 
  spread(time, values) %>% 
  mutate(Difference = `2015` - `2008`) %>% 
  group_by(spec) %>% 
  nest() %>% 
  mutate(Min08 = map_dbl(data, ~min(.x[[3]], na.rm = TRUE)),
         Median08 = map_dbl(data, ~median(.x[[3]], na.rm = TRUE)),
         Mean08 = map_dbl(data, ~mean(.x[[3]], na.rm = TRUE)),
         Max08 = map_dbl(data, ~max(.x[[3]], na.rm = TRUE)),
         Std_Deviation08 = map_dbl(data, ~sd(.x[[3]], na.rm = TRUE))
  ) %>% 
  mutate(Min15 = map_dbl(data, ~min(.x[[4]], na.rm = TRUE)),
         Median15 = map_dbl(data, ~median(.x[[4]], na.rm = TRUE)),
         Mean15 = map_dbl(data, ~mean(.x[[4]], na.rm = TRUE)),
         Max15 = map_dbl(data, ~max(.x[[4]], na.rm = TRUE)),
         Std_Deviation15 = map_dbl(data, ~sd(.x[[4]], na.rm = TRUE))
  ) %>% 
  mutate(Min_Dif0815 = map_dbl(data, ~min(.x[[5]], na.rm = TRUE)),
         Median_Dif0815 = map_dbl(data, ~median(.x[[5]], na.rm = TRUE)),
         Mean_Dif0815 = map_dbl(data, ~mean(.x[[5]], na.rm = TRUE)),
         Max_Dif0815 = map_dbl(data, ~max(.x[[5]], na.rm = TRUE)),
         Std_Deviation_Dif0815 = map_dbl(data, ~sd(.x[[5]], na.rm = TRUE))
  ) %>% 
  select(-data)


spec_doct08_15_stats_08 = spec_doct08_15_stats %>% 
  select(1:6) %>% 
  rename(Title = spec,
         Min = Min08,
         Median = Median08,
         Mean = Mean08,
         Max = Max08,
         Std_Deviation = Std_Deviation08)


spec_doct08_15_stats_15 = spec_doct08_15_stats %>% 
  select(1,7:11) %>% 
  rename(Title = spec,
         Min = Min15,
         Median = Median15,
         Mean = Mean15,
         Max = Max15,
         Std_Deviation = Std_Deviation15)



spec_doct08_15_stats_Dif0815 = spec_doct08_15_stats %>% 
  select(1,12:16) %>% 
  rename(Title = spec,
         Min = Min_Dif0815,
         Median = Median_Dif0815,
         Mean = Mean_Dif0815,
         Max = Max_Dif0815,
         Std_Deviation = Std_Deviation_Dif0815)




