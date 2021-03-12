
# Group predictors ----
# Read in raw data
national_incomplete = read.csv("national_incomplete.csv")

# Attach level labels
varlabs = read.csv("list of var levels.csv", header = FALSE)

national_incomplete = left_join(national_incomplete, varlabs, by = c("var" = "V1", "level" = "V2")) %>% 
  rename("var_label" = "V4", "level_label" = "V5")

# Change some labels as they are too long for graph
national_incomplete = national_incomplete %>% 
  mutate(var_label = str_replace(var_label, "Mother highest qualification", "Mother high qual")) %>% 
  mutate(level_label = str_replace(level_label, "Tertiary below Bachelor", "Tertiary")) %>% 
  mutate(level_label = str_replace(level_label, "No qualifications", "No quals")) 

# Attach ref values
refvalues = national_incomplete %>% 
  filter(V3 == "ref") %>% 
  select(var, total, vht_incomplete, nurse_incomplete, sdqt_incomplete) %>% 
  rename(ref_total = total, ref_vht = vht_incomplete, ref_nurse = nurse_incomplete, ref_sdqt = sdqt_incomplete)

# Transfer the reference category values back to the main file
national_incomplete_ref = left_join(national_incomplete, refvalues, by = c("var" = "var"))
as.character(national_incomplete_ref$V3)

# Calculate RR for each level except ref level
national_incomplete_ref = national_incomplete_ref %>% 
  mutate(rr_vht = ifelse(V3 == "", (vht_incomplete/total)/(ref_vht/ref_total),NA)) %>% 
  mutate(rr_nurse = ifelse(V3 == "",(nurse_incomplete/total)/(ref_nurse/ref_total),NA)) %>% 
  mutate(rr_sdqt = ifelse(V3 == "",(sdqt_incomplete/total)/(ref_sdqt/ref_total),NA)) 

# Calculate the RRs and rank them
# calculate 95% CI for RR
# formula is
# ln(RR) +- 1.96*(sqrt((((n1-x1)/x1)/n1) + (((n2-x2)/x2)/n2)))

national_incomplete_ref_ci = national_incomplete_ref %>% 
  mutate(ci_vht = 1.96*(sqrt((((total-vht_incomplete)/vht_incomplete)/total) + (((ref_total-ref_vht)/ref_vht)/ref_total)))) %>% 
  mutate(vht_ci_ll = exp(log(rr_vht)-ci_vht)) %>% 
  mutate(vht_ci_ul = exp(log(rr_vht)+ci_vht)) %>% 
  mutate(ci_nurse = 1.96*(sqrt((((total-nurse_incomplete)/nurse_incomplete)/total) + (((ref_total-ref_nurse)/ref_nurse)/ref_total)))) %>% 
  mutate(nurse_ci_ll = exp(log(rr_nurse)-ci_nurse)) %>% 
  mutate(nurse_ci_ul = exp(log(rr_nurse)+ci_nurse)) %>% 
  mutate(ci_sdqt = 1.96*(sqrt((((total-sdqt_incomplete)/sdqt_incomplete)/total) + (((ref_total-ref_sdqt)/ref_sdqt)/ref_total)))) %>% 
  mutate(sdqt_ci_ll = exp(log(rr_sdqt)-ci_sdqt)) %>% 
  mutate(sdqt_ci_ul = exp(log(rr_sdqt)+ci_sdqt)) 

# Exclude birth weight group and number of days in hospital before 4th birthday
national_incomplete_ref_ci = national_incomplete_ref_ci %>% 
  filter(var != "bw_grp" & var != "los_days_grp")


# Rank the predictors based on RR
# VHT
top10_pred_vht_nation = national_incomplete_ref_ci %>% 
  filter(rr_vht != "NA") %>% 
  select(var_label, level_label, rr_vht) %>% 
  group_by(var_label) %>% 
  filter(rr_vht == max(rr_vht)) %>%
  arrange(desc(rr_vht), var_label, level_label)

top10_pred_vht_nation[1:10,]

# Nurse
top10_pred_nurse_nation = national_incomplete_ref_ci %>% 
  filter(rr_nurse != "NA") %>% 
  select(var_label, level_label, rr_nurse) %>% 
  group_by(var_label) %>% 
  filter(rr_nurse == max(rr_nurse)) %>%
  arrange(desc(rr_nurse), var_label, level_label)

top10_pred_nurse_nation[1:10,]

# SDQT
top10_pred_sdqt_nation = national_incomplete_ref_ci %>% 
  filter(rr_sdqt != "NA") %>% 
  select(var_label, level_label, rr_sdqt) %>% 
  group_by(var_label) %>% 
  filter(rr_sdqt == max(rr_sdqt)) %>%
  arrange(desc(rr_sdqt), var_label, level_label)

top10_pred_sdqt_nation[1:10,]



### Order the predictors by their RR, by module, national level -----

order_pred = national_incomplete_ref_ci %>% 
  filter(V3 == "") %>% 
  select(var_label, level_label, rr_vht, rr_nurse, rr_sdqt)

# new var combining var and level
order_pred$varlevel = paste(order_pred$var_label, order_pred$level_label, sep = ": ")

varlevel = as.factor(order_pred$varlevel)
varlevel_text = levels(varlevel)
varlevel_text[c(39, 33:36, 15:17, 2, 26:27, 
                41, 31:32, 20:23, 11:14, 8:9, 40, 37, 10, 6, 1, 7, 4, 18:19, 
                28:30, 24:25, 5, 38, 3, 42:43)]

order_pred$varlevelnew = factor(order_pred$varlevel, levels = 
                                  varlevel_text[c(39, 33:36, 15:17, 2, 26:27, 
                                                  41, 31:32, 20:23, 11:14, 8:9, 40, 37, 10, 6, 1, 7, 4, 18:19, 
                                                  28:30, 24:25, 5, 38, 3, 42:43)])

order_pred$batch = factor(order_pred$varlevelnew, levels = 
                            varlevel_text[c(39, 33:36, 15:17, 2, 26:27, 
                                            41, 31:32, 20:23, 11:14, 8:9, 40, 37, 10, 6, 1, 7, 4, 18:19,
                                            28:30, 24:25, 5, 38, 3, 42:43)],
                          label = c(rep("Family Socioeconomic Position", 11), rep("Family & Child Characteristics", 22),
                                    rep("Health & Health System", 10)))


### VHT
order_pred %>% 
  filter(rr_vht != "NA" & batch == "Family Socioeconomic Position") %>%  
  group_by(var_label) %>% 
  filter(rr_vht == max(rr_vht)) %>%
  arrange(desc(rr_vht), var_label, level_label)
# N motor; Mother high qual; NZDep; Rented home; Benefit

order_pred %>% 
  filter(rr_vht != "NA" & batch == "Family & Child Characteristics") %>%  
  group_by(var_label) %>% 
  filter(rr_vht == max(rr_vht)) %>%
  arrange(desc(rr_vht), var_label, level_label)
# Mother age; Household size; Father on birth record; N change to age4; Maori;
# Pacific; N sibling; Mother english; Sex; Urbanicity; Mother born in NZ;
# Child born in NZ; Asian; European

order_pred %>% 
  filter(rr_vht != "NA" & batch == "Health & Health System") %>%  
  group_by(var_label) %>% 
  filter(rr_vht == max(rr_vht)) %>%
  arrange(desc(rr_vht), var_label, level_label)
# N quarters GP; Received disability support; Birthweight; Weeks gestation;
# ED visit; N inpatient

### Nurse  
order_pred %>% 
  filter(rr_nurse != "NA" & batch == "Family Socioeconomic Position") %>%  
  group_by(var_label) %>% 
  filter(rr_nurse == max(rr_nurse)) %>%
  arrange(desc(rr_nurse), var_label, level_label)
# N motor; Mother high qual; Rented home; Benefit; NZDep

order_pred %>% 
  filter(rr_nurse != "NA" & batch == "Family & Child Characteristics") %>%  
  group_by(var_label) %>% 
  filter(rr_nurse == max(rr_nurse)) %>%
  arrange(desc(rr_nurse), var_label, level_label)
# Household size; Mother age; N change age4; Father on birth record; Maori;
# N sibling; Pacific; Sex; Mother english; Urbanicity; Mother born NZ;
# Child born NZ; European; Asian

order_pred %>% 
  filter(rr_nurse != "NA" & batch == "Health & Health System Use") %>%  
  group_by(var_label) %>% 
  filter(rr_nurse == max(rr_nurse)) %>%
  arrange(desc(rr_nurse), var_label, level_label)
# Received disability; N quarter GP; ED visit; Birthweight; Weeks gestation;
# N inpatient;

### SDQT  
order_pred %>% 
  filter(rr_sdqt != "NA" & batch == "Family Socioeconomic Position") %>%  
  group_by(var_label) %>% 
  filter(rr_sdqt == max(rr_sdqt)) %>%
  arrange(desc(rr_sdqt), var_label, level_label)
# NZDep; N motor; Mother high qual; Benefit; Rented home

order_pred %>% 
  filter(rr_sdqt != "NA" & batch == "Family & Child Characteristics") %>%  
  group_by(var_label) %>% 
  filter(rr_sdqt == max(rr_sdqt)) %>%
  arrange(desc(rr_sdqt), var_label, level_label)
# Pacific; Household size; Father on birth record; Mother age; Urbanicity;
# Mother english; N change age4; Maori; N sibling; Asian; Sex; Child born NZ;
# Mother born NZ; European; 

order_pred %>% 
  filter(rr_sdqt != "NA" & batch == "Health & Health System Use") %>%  
  group_by(var_label) %>% 
  filter(rr_sdqt == max(rr_sdqt)) %>%
  arrange(desc(rr_sdqt), var_label, level_label)
# Received disability; N quarter GP; ED visit; Weeks gestation; Birthweight;
# N inpatient;

# Check top 10 predictors across DHBs
# Exclude birth weight group and number of days in hospital before 4th birthday
dhb_incomplete_ref_ci = dhb_incomplete_ref_ci %>% 
  filter(var != "bw_grp" & var != "los_days_grp")

# Rank the predictors base on RR
ranked_pred_vht_dhb = dhb_incomplete_ref_ci %>% 
  filter(DHB2015_name == "Auckland" & rr_vht != "NA") %>% 
  select(var_label, level_label, rr_vht) %>% 
  group_by(var_label) %>% 
  filter(rr_vht == max(rr_vht)) %>%
  arrange(desc(rr_vht), var_label, level_label)

## Top 10 VHT predictors acorss DHBs
fint_top10_dhb_vht = function(dhb){
  ranked_pred_vht_dhb = dhb_incomplete_ref_ci %>% 
    filter(DHB2015_name == dhb & rr_vht != "NA") %>% 
    select(var_label, level_label, rr_vht) %>% 
    group_by(var_label) %>% 
    filter(rr_vht == max(rr_vht)) %>%
    arrange(desc(rr_vht), var_label, level_label)
  
  ranked_pred_vht_dhb[1:10,]
}

fint_top10_dhb_vht("Auckland")

# For all 20 DHBs
DHB = unique(dhb_incomplete$DHB2015_name)
top10_vht_dhb = lapply(as.factor(DHB), fint_top10_dhb_vht)

# List top 10 from all DHBs
top10_vht_freq = NULL
for (i in 1:20){
  top10_vht_freq = c(top10_vht_freq, top10_vht_dhb[[i]]$var_label)
}

# Get a frequency table and export
arrange(data.frame(table(top10_vht_freq)), desc(Freq))[1:10,]
write.csv(top10_vht_dhb, "top10_vht_dhb.csv")

## Top 10 Nurse predictors across DHBs
fint_top10_dhb_nurse = function(dhb){
  ranked_pred_nurse_dhb = dhb_incomplete_ref_ci %>% 
    filter(DHB2015_name == dhb & rr_nurse != "NA") %>% 
    select(var_label, level_label, rr_nurse) %>% 
    group_by(var_label) %>% 
    filter(rr_nurse == max(rr_nurse)) %>%
    arrange(desc(rr_nurse), var_label, level_label)
  
  ranked_pred_nurse_dhb[1:10,]
}

fint_top10_dhb_nurse("Auckland")

# For all 20 DHBs
DHB = unique(dhb_incomplete$DHB2015_name)
top10_nurse_dhb = lapply(as.factor(DHB), fint_top10_dhb_nurse)

# List top 10 from all DHBs
top10_nurse_freq = NULL
for (i in 1:20){
  top10_nurse_freq = c(top10_nurse_freq, top10_nurse_dhb[[i]]$var_label)
}

# Get a frequency table and export
arrange(data.frame(table(top10_nurse_freq)), desc(Freq))[1:10,]
write.csv(top10_nurse_dhb, "top10_nurse_freq.csv")


## Top 10 SDQT predictors across DHBs
fint_top10_dhb_sdqt = function(dhb){
  ranked_pred_sdqt_dhb = dhb_incomplete_ref_ci %>% 
    filter(DHB2015_name == dhb & rr_sdqt != "NA") %>% 
    select(var_label, level_label, rr_sdqt) %>% 
    group_by(var_label) %>% 
    filter(rr_sdqt == max(rr_sdqt)) %>%
    arrange(desc(rr_sdqt), var_label, level_label)
  
  ranked_pred_sdqt_dhb[1:10,]
}

# For all 20 DHBs
DHB = unique(dhb_incomplete$DHB2015_name)
top10_sdqt_dhb = lapply(as.factor(DHB), fint_top10_dhb_sdqt)

# List top 10 from all DHBs
top10_sdqt_freq = NULL
for (i in 1:20){
  top10_sdqt_freq = c(top10_sdqt_freq, top10_sdqt_dhb[[i]]$var_label)
}

# Get a frequency table and export
arrange(data.frame(table(top10_sdqt_freq)), desc(Freq))[1:10,]
write.csv(top10_sdqt_dhb, "top10_sdqt_freq.csv")


## Top 10 VHT predictors at TA level for each DHB---------
# Read in raw data at TA/WARD level
ta_incomplete = read.csv("grp_taward_final.csv")
ta_incomplete$grp_ta_ward_name = as.character(ta_incomplete$grp_ta_ward_name)
dhb_map$WARD2013_name = as.character(dhb_map$WARD2013_name)

# Attach DHB names
ta_incomplete = left_join(ta_incomplete, dhb_map, by = c("grp_ta_ward_name" = "WARD2013_name"))

# Attach level labels
ta_incomplete = left_join(ta_incomplete, varlabs, by = c("var" = "V1", "level" = "V2")) %>% 
  rename("var_label" = "V4", "level_label" = "V5")

# Change some labels as they are too long for graph
ta_incomplete = ta_incomplete %>% 
  mutate(var_label = str_replace(var_label, "mother_highest_qual", "Mother highest qual")) %>% 
  mutate(level_label = str_replace(level_label, "Tertiary below Bachelor", "Tertiary")) %>% 
  mutate(level_label = str_replace(level_label, "No qualifications", "No quals")) 

# Attach ref values
refvalues = ta_incomplete %>% 
  filter(V3 == "ref") %>% 
  select(var, grp_ta_ward_name, total, vht_incomplete, nurse_incomplete, sdqt_incomplete) %>% 
  rename(ref_total = total, ref_vht = vht_incomplete, ref_nurse = nurse_incomplete, ref_sdqt = sdqt_incomplete)

# Transfer the reference category values back to the main file
ta_incomplete_ref = left_join(ta_incomplete, refvalues, by = c("var" = "var", "grp_ta_ward_name" = "grp_ta_ward_name"))

# Calculate RR for each level except ref level
# Note: numerical variables are factors here, need to convert them into characters first then to numberic
ta_incomplete_ref = ta_incomplete_ref %>%
  mutate(total = as.numeric(as.character(total)), 
         vht_incomplete = as.numeric(as.character(vht_incomplete)),
         nurse_incomplete = as.numeric(as.character(nurse_incomplete)),
         sdqt_incomplete = as.numeric(as.character(sdqt_incomplete)),
         ref_vht = as.numeric(as.character(ref_vht)),
         ref_nurse = as.numeric(as.character(ref_nurse)),
         ref_sdqt = as.numeric(as.character(ref_sdqt)),
         ref_total = as.numeric(as.character(ref_total))) %>% 
  mutate(rr_vht = ifelse(V3 == "", (vht_incomplete/total)/(ref_vht/ref_total),NA)) %>% 
  mutate(rr_nurse = ifelse(V3 == "",(nurse_incomplete/total)/(ref_nurse/ref_total),NA)) %>% 
  mutate(rr_sdqt = ifelse(V3 == "",(sdqt_incomplete/total)/(ref_sdqt/ref_total),NA)) 

ta_incomplete_ref_ci = ta_incomplete_ref %>% 
  mutate(ci_vht = 1.96*(sqrt((((total-vht_incomplete)/vht_incomplete)/total) + (((ref_total-ref_vht)/ref_vht)/ref_total)))) %>% 
  mutate(vht_ci_ll = exp(log(rr_vht)-ci_vht)) %>% 
  mutate(vht_ci_ul = exp(log(rr_vht)+ci_vht)) %>% 
  mutate(ci_nurse = 1.96*(sqrt((((total-nurse_incomplete)/nurse_incomplete)/total) + (((ref_total-ref_nurse)/ref_nurse)/ref_total)))) %>% 
  mutate(nurse_ci_ll = exp(log(rr_nurse)-ci_nurse)) %>% 
  mutate(nurse_ci_ul = exp(log(rr_nurse)+ci_nurse)) %>% 
  mutate(ci_sdqt = 1.96*(sqrt((((total-sdqt_incomplete)/sdqt_incomplete)/total) + (((ref_total-ref_sdqt)/ref_sdqt)/ref_total)))) %>% 
  mutate(sdqt_ci_ll = exp(log(rr_sdqt)-ci_sdqt)) %>% 
  mutate(sdqt_ci_ul = exp(log(rr_sdqt)+ci_sdqt)) 


# Exclude birth weight group and number of days in hospital before 4th birthday
ta_incomplete_ref_ci = ta_incomplete_ref_ci %>% 
  filter(var != "bw_grp" & var != "los_days_grp")

# Rank the predictors base on RR - at TA/WARD level
ranked_pred_vht_ta = ta_incomplete_ref_ci %>% 
  filter(grp_ta_ward_name == "Albany Ward" & rr_vht != "NA") %>% 
  select(var_label, level_label, rr_vht, DHB_name) %>% 
  group_by(var_label) %>% 
  filter(rr_vht == max(rr_vht)) %>%
  arrange(desc(rr_vht), var_label, level_label)

## Top 10 VHT predictors across DHBs
find_top10_ta_vht = function(ta){
  ranked_pred_vht_ta = ta_incomplete_ref_ci %>% 
    filter(grp_ta_ward_name == ta & rr_vht != "NA") %>% 
    select(var_label, level_label, rr_vht, DHB_name, grp_ta_ward_name) %>% 
    group_by(var_label) %>% 
    filter(rr_vht == max(rr_vht)) %>%
    arrange(desc(rr_vht), var_label, level_label)
  
  ranked_pred_vht_ta[1:10,]
}

find_top10_ta_vht("Albany Ward")

# Excluding the 71th TA which is Chatham Islands Territory
ta = unique(ta_incomplete$grp_ta_ward_name)[1:70]
top10_vht_ta = lapply(as.factor(ta), find_top10_ta_vht)
write.csv(top10_vht_ta, "top10_vht_ta.csv")

# List top 10 vht predictors at TA/WARD level
top10_vht_ta_df = NULL
for (i in 1:70){
  top10_vht_ta_df = rbind(top10_vht_ta_df, data.frame(top10_vht_ta[[i]]))
}

# Find top 10 most frequent predictors at DHB level
ta_DHB_top10_vht = function(dhb){
  one_DHB = top10_vht_ta_df %>% 
    filter(DHB_name == dhb)
  
  colname = as.character(dhb)
  
  # For all ta/wards together (not separated by DHB)
  data = arrange(data.frame(table(one_DHB$var_label)), desc(Freq))[1:10,]
  names(data)[1] = colname
  data
}

ta_DHB_top10_vht("Auckland")
a = as.factor(unique(ta_incomplete$DHB_name))
DHB_ta = lapply(a, ta_DHB_top10_vht)
write.csv(DHB_ta, "DHB_ta.csv")


findmaxrr = function(dhb){
  dhbdata = allplotdata %>% 
    filter(DHB2015_name == dhb)
  
  print(max(dhbdata$rr_vht, na.rm = TRUE))
  print("vht")
  print(max(dhbdata$rr_nurse, na.rm = TRUE))
  print("nurse")
  print(max(dhbdata$rr_sdqt, na.rm = TRUE))
  print("sdqt")
  print(max(dhbdata$vht_ci_ul, na.rm = TRUE))
  print(min(dhbdata$vht_ci_ul, na.rm = TRUE))
}  

DHB = unique(allplotdata$DHB2015_name)
findmaxrr("Auckland") #3.5
findmaxrr("Bay of Plenty") #5.5
findmaxrr("Canterbury") #4.5
findmaxrr("Capital and Coast") #4.5
findmaxrr("Counties Manukau") #3.5
findmaxrr("Hawkes Bay") #9
findmaxrr("Hutt Valley") #5.5
findmaxrr("Lakes") #7
findmaxrr("MidCentral") #5
findmaxrr("Nelson Marlborough") #5.5
findmaxrr("Northland") #3.5
findmaxrr("South Canterbury") #13
findmaxrr("Southern") #5
findmaxrr("Tairawhiti") #8
findmaxrr("Taranaki") #6
findmaxrr("Waikato") #9.5
findmaxrr("Wairarapa") #8.5
findmaxrr("Waitemata") #4
findmaxrr("West Coast") #16
findmaxrr("Whanganui") #6

