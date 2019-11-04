

######## ----- 1. PCA1: Europe Physicians -----

# has missing data!!!
PCA_spec_doct = read_csv("1_RawData/PCA_spec_doct.csv")


rownames(PCA_spec_doct) <- PCA_spec_doct[,1] %>% pull()
PCA_spec_doct[,1] = NULL


### NO TIBBLES!!!
df1 = as.data.frame(PCA_spec_doct)

nb1 <- estim_ncpPCA(df1, scale=TRUE) ## Estime le nb de dimensions
nb1

comp1 <- imputePCA(df1, ncp=2, scale=TRUE) ## Compl?te le tableau

# Adding Countries to imputed table
spec_doct_imp = comp1$completeObs 

### PCA1
res1.pca <- PCA(spec_doct_imp,
               quanti.sup = 11:12)


### If Necessary: For Plotting Top Rankings - Wide
data_res1.pca = res1.pca$call$X %>% 
  rownames_to_column(var = "Country") 

# For Plotting Cluster Plots
res1.hcpc <- HCPC(res1.pca, 
                   nb.clust=8, 
                   kk = Inf, 
                   graph=F, 
                   consol=F)


######## ----- 2. PCA2: Growth Rates - Europe Physicians -----


PCA_spec_doct_gr = read.csv("1_rawData/PCA_spec_doct_gr.csv")

PCA_spec_doct_gr_gdp =  nuts0g %>% select(geo, NUTS_NAME) %>% 
  left_join(PCA_spec_doct_gr %>% 
              # cbind(geo) %>% 
              select(geo, GDP_cap), 
            by = c("geo")) %>% 
  filter(!(is.na(GDP_cap))) %>% 
  rename(Country = geo,
         Country_Name = NUTS_NAME)


### No values for them nearly for proper imputation
PCA_spec_doct_gr = PCA_spec_doct_gr %>% 
  filter(!(geo %in% c("AL", "CY", "CZ")))


rownames(PCA_spec_doct_gr) <- PCA_spec_doct_gr[,1]
PCA_spec_doct_gr[,1] = NULL

### NO TIBBLES!!!
df2 = as.data.frame(PCA_spec_doct_gr)

nb2 <- estim_ncpPCA(df2, scale=TRUE) ## Estime le nb de dimensions
nb2

comp2 <- imputePCA(df2, ncp=2, scale=TRUE) ## Compl?te le tableau

# Adding Countries to imputed table
spec_doct_gr_imp = comp2$completeObs 


### Visits 2 dataset
res2.pca <- PCA(spec_doct_gr_imp,

               quanti.sup = 31:32)


### If needed: For Plotting Top Rankings - Wide
data_res2.pca = res2.pca$call$X %>% 
  rownames_to_column(var = "Country") 


res2.hcpc <- HCPC(res2.pca, 
                   nb.clust=9, 
                   kk = Inf, 
                   graph=F, 
                   consol=F)



######## ----- 3. PCA3: PCA Visit:  Doctor / Frequency! -----

### Has missing data!!!
PCA_visit_med_freq = read_csv("1_RawData/PCA_visit_med_freq.csv")

rownames(PCA_visit_med_freq) = PCA_visit_med_freq[,1] %>% pull()
PCA_visit_med_freq[,1] = NULL


### NO TIBBLES!!!
df3 = as.data.frame(PCA_visit_med_freq)

nb3 <- estim_ncpPCA(df3, scale=TRUE) ## Estime le nb de dimensions
nb3

comp3 <- imputePCA(df3, ncp=2, scale=TRUE) ## Compl?te le tableau

# Adding Countries to imputed table
med_freq_imp = comp3$completeObs 


res3.pca <- PCA(med_freq_imp,
               quanti.sup = 16:17,
               ind.sup = 1:4)


### If needed: For Plotting Top Rankings - Wide
data_res3.pca = res3.pca$call$X %>% 
  rownames_to_column(var = "Country") 


res3.hcpc <- HCPC(res3.pca, 
                 nb.clust=6, 
                 kk = Inf, 
                 graph=F, 
                 consol=F)


######## ----- 4. PCA4: PCA Visit:  Frequency / EDUC / AGE! -----


### Has missing data!!!
PCA_visit_freq_edu_age = read_csv("1_RawData/PCA_visit_freq_edu_age.csv")

rownames(PCA_visit_freq_edu_age) = PCA_visit_freq_edu_age[,1] %>% pull()
PCA_visit_freq_edu_age[,1] = NULL


### NO TIBBLES!!!
df4 = as.data.frame(PCA_visit_freq_edu_age)

nb4 <- estim_ncpPCA(df4, scale=TRUE) ## Estime le nb de dimensions
nb4

comp4 <- imputePCA(df4, ncp=2, scale=TRUE) ## Compl?te le tableau

# Adding Countries to imputed table
freq_edu_age_imp = comp4$completeObs 


### Visits 2 dataset
res4.pca <- PCA(freq_edu_age_imp,
               ind.sup = 1:4,
               quanti.sup = 19:20)


### If needed: For Plotting Top Rankings - Wide
data_res4.pca = res4.pca$call$X %>% 
  rownames_to_column(var = "Country") 



res4.hcpc <- HCPC(res4.pca, 
                  nb.clust=6, 
                  kk = Inf, 
                  graph=F, 
                  consol=F)


######## ----- 5. PCA5: PCA Visit:  Frequency / Income / Employment -----


### Has missing data!!!
PCA_visit_wstatus_inc_freq = read_csv("1_RawData/PCA_visit_wstatus_inc_freq.csv")
glimpse(PCA_visit_wstatus_inc_freq)

rownames(PCA_visit_wstatus_inc_freq) <- PCA_visit_wstatus_inc_freq[,1] %>% pull()
PCA_visit_wstatus_inc_freq[,1] = NULL


### NO TIBBLES!!!
df5 = as.data.frame(PCA_visit_wstatus_inc_freq)

nb5 <- estim_ncpPCA(df5, scale=TRUE) ## Estime le nb de dimensions
nb5

comp5 <- imputePCA(df5, ncp=2, scale=TRUE) ## Compl?te le tableau

# Adding Countries to imputed table
wstatus_inc_freq_imp = comp5$completeObs


### Visits 2 dataset
res5.pca <- PCA(wstatus_inc_freq_imp,
               ind.sup = 1:4,
               quanti.sup = 13:14)


### If needed: For Plotting Top Rankings - Wide
data_res5.pca = res5.pca$call$X %>% 
  rownames_to_column(var = "Country") 


res5.hcpc <- HCPC(res5.pca, 
                  nb.clust=6, 
                  kk = Inf, 
                  graph=F, 
                  consol=F)















