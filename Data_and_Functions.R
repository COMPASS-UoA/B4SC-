
# Loading packages
library(data.table)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(forestplot)
library(lattice)
library(grid)

# Read in DHB-map concordance table
dhb_map = read.csv("dhb_map.csv")
# Read in level values
varlabs = read.csv("list of var levels.csv", header = FALSE)

# Read in file with completion counts by module (national and TA level) ------------------------------
com_all_year = read.csv("completion_all_year.csv")
com_all_year_ta = read.csv("completion_all_year_ta.csv")

# Calculate national level completion rate
# Convert counts from integer to numeric by as.numeric
com_all_year = com_all_year %>% 
  mutate(vht_rate = as.numeric(vht_complete)/as.numeric(total),
         nurse_rate = as.numeric(nurse_complete)/as.numeric(total), 
         sdqt_rate = as.numeric(sdqt_complete)/as.numeric(total))

# Map TA to DHB
com_all_year_ta = left_join(com_all_year_ta, dhb_map, by = c("grp_ta_ward_name" = "WARD2013_name"))
table(com_all_year_ta$grp_ta_ward_name[is.na(com_all_year_ta$DHB_name)])

# Calculate completion rate
com_all_year_ta = com_all_year_ta %>% 
  mutate(vht_rate = as.numeric(vht_complete)/as.numeric(total),
         nurse_rate = as.numeric(nurse_complete)/as.numeric(total),
         sdqt_rate = as.numeric(sdqt_complete)/as.numeric(total))

# For each DHB, attach one row for national average
data1 = com_all_year %>% select(YEAR, vht_rate, nurse_rate, sdqt_rate)
data2 = data.frame(DHB_name = unique(dhb_map$DHB_name))
data3 = merge(data1, data2)
data3$grp_ta_ward_name = "National average"

com_all_year_final = bind_rows(com_all_year_ta, data3) %>% 
  mutate(vht_rate_perc = round(vht_rate*100,2)) %>% 
  mutate(nurse_rate_perc = round(nurse_rate*100,2)) %>% 
  mutate(sdqt_rate_perc = round(sdqt_rate*100,2))


# Attendance by module and year ----

# List of SWA colour schemes
# #ce5b30 #26567f #2a2a3e #2c86b4 #E8731B #ce5b30 #007e87 #db9824 #366a59 #6b9b5f
# #F7B735 primary yellow
# #3a9cae primary teal
# #C6C8C9 light gray

col = c("#ff0000", "#6b9b5f", "#2a2a3e", "#2c86b4", "#E8731B", "#007e87",
         "#3a9cea", "#db9824", "#366a59", "#26567f")

# Wrap into function - VHT
myplot_vht = function(dhb){
  # Create data set to be plotted
  toplot = com_all_year_final %>% filter(DHB_name==dhb) 
  
  # Find out which is national average
  ta_text = levels(factor(toplot$grp_ta_ward_name))
  index = which(ta_text=="National average")
  toplot$grp_ta_ward_name = factor(toplot$grp_ta_ward_name, levels = c(ta_text[index], ta_text[-index]))
  
  ggplot(data = toplot, aes(x = YEAR, y = vht_rate_perc, color = grp_ta_ward_name, 
                            size = grp_ta_ward_name, linetype = grp_ta_ward_name,
                            shape = grp_ta_ward_name)) +
    scale_x_continuous(breaks = seq(min(toplot$YEAR), max(toplot$YEAR), by = 1),
                       labels = c("11/12", "12/13", "13/14", "14/15", "15/16", "16/17", "17/18", "18/19")) + 
    xlab("") +
    ylab("") + 
    labs(title = "VHT Attendance Rates (%)", subtitle = paste("")) +
    scale_y_continuous(limits = c(50, 100),
                       breaks = seq(50, 100, by = 10)) + 
    geom_line() + geom_point(size = 3.5) +
    scale_color_manual(values = col) +
    scale_linetype_manual(values = c("twodash", rep(c("solid"), length.out = length(ta_text)-1))) +
    scale_size_manual(values = c(1.2, rep(1, length(ta_text)-1))) +
    scale_shape_manual(values = c(20, 15:18, 0:2, 5, 6)) +
    theme_classic() +
    theme(legend.position="bottom", legend.title = element_blank(),
          legend.text = element_text(size = 12),
          panel.grid.major.y = element_line(color = "gray", linetype = 2),
          title = element_text(size = 16),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15))
}

#png("AttendanceRateVHT.png", width = 8.66, height = 6.3, units = "in", res = 300)
myplot_vht("Auckland")
#dev.off()

# Wrap into function - Nurse
myplot_nurse = function(dhb){
  # Create data set to be plotted
  toplot = com_all_year_final %>% filter(DHB_name==dhb) 
  
  # Find out which is national average
  ta_text = levels(factor(toplot$grp_ta_ward_name))
  index = which(ta_text=="National average")
  toplot$grp_ta_ward_name = factor(toplot$grp_ta_ward_name, levels = c(ta_text[index], ta_text[-index]))
  
  ggplot(data = toplot, aes(x = YEAR, y = nurse_rate_perc, color = grp_ta_ward_name, 
                            size = grp_ta_ward_name, linetype = grp_ta_ward_name,
                            shape = grp_ta_ward_name)) +
    scale_x_continuous(breaks = seq(min(toplot$YEAR), max(toplot$YEAR), by = 1),
                       labels = c("11/12", "12/13", "13/14", "14/15", "15/16", "16/17", "17/18", "18/19")) + 
    xlab("") +
    ylab("") + 
    labs(title = "Nurse Attendance Rates (%)", subtitle = paste("")) +
    scale_y_continuous(limits = c(50, 100),
                       breaks = seq(50, 100, by = 10)) + 
    geom_line() + geom_point(size = 3.5) +
    scale_color_manual(values = col) +
    scale_linetype_manual(values = c("twodash", rep(c("solid"), length.out = length(ta_text)-1))) +
    scale_size_manual(values = c(1.2, rep(1, length(ta_text)-1))) +
    scale_shape_manual(values = c(20, 15:18, 0:2, 5, 6)) +
    theme_classic() +
    theme(legend.position="bottom", legend.title = element_blank(),
          legend.text = element_text(size = 12),
          panel.grid.major.y = element_line(color = "gray", linetype = 2),
          title = element_text(size = 16),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15))
}

#png("AttendanceRateNURSE.png", width = 8.66, height = 6.3, units = "in", res = 300)
myplot_nurse("Auckland")
#dev.off()


# Wrap into function - SDQT
myplot_sdqt = function(dhb){
  # Create data set to be plotted
  toplot = com_all_year_final %>% filter(DHB_name==dhb) 
  
  # Find out which is national average
  ta_text = levels(factor(toplot$grp_ta_ward_name))
  index = which(ta_text=="National average")
  toplot$grp_ta_ward_name = factor(toplot$grp_ta_ward_name, levels = c(ta_text[index], ta_text[-index]))
  
  ggplot(data = toplot, aes(x = YEAR, y = sdqt_rate_perc, color = grp_ta_ward_name, 
                            size = grp_ta_ward_name, linetype = grp_ta_ward_name,
                            shape = grp_ta_ward_name)) +
    scale_x_continuous(breaks = seq(min(toplot$YEAR), max(toplot$YEAR), by = 1),
                       labels = c("11/12", "12/13", "13/14", "14/15", "15/16", "16/17", "17/18", "18/19")) + 
    xlab("") +
    ylab("") + 
    labs(title = "SDQ-Teacher Attendance Rates (%)", subtitle = paste("")) +
    scale_y_continuous(limits = c(0, 100),
                       breaks = seq(0, 100, by = 10)) + 
    geom_line() + geom_point(size = 3.5) +
    scale_color_manual(values = col) +
    scale_linetype_manual(values = c("twodash", rep(c("solid"), length.out = length(ta_text)-1))) +
    scale_size_manual(values = c(1.2, rep(1, length(ta_text)-1))) +
    scale_shape_manual(values = c(20, 15:18, 0:2, 5, 6)) +
    theme_classic() +
    theme(legend.position="bottom", legend.title = element_blank(),
          legend.text = element_text(size = 12),
          panel.grid.major.y = element_line(color = "gray", linetype = 2),
          title = element_text(size = 16),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15)) 
}

#png("AttendanceRateSDQT.png", width = 8.66, height = 6.3, units = "in", res = 300)
myplot_sdqt("Auckland")
#dev.off()

# Read in data with LB counts
com_all_year_lb = read.csv("completion_all_year_LB.csv")

c = NULL

for (i in 2011:2018){
  a = com_all_year_lb[(com_all_year_lb$CB2018_name == "Great Barrier and Waiheke" | 
                         com_all_year_lb$CB2018_name == "Waitemata") & com_all_year_lb$YEAR == i,]
  
  b = data.frame(CB2018_name = "Great Barrier, Waiheke and Waitemata", total = sum(a$total), vht_complete = sum(a$vht_complete),
                 nurse_complete = sum(a$nurse_complete), sdqt_complete = sum(a$sdqt_complete),
                 YEAR = a$YEAR[1], DHB_name = a$DHB_name[1])
  
  c = rbind(c, b)
}

com_all_year_lb = rbind(com_all_year_lb, c)

com_all_year_lb = com_all_year_lb %>% 
  mutate(vht_rate = as.numeric(vht_complete)/as.numeric(total),
         nurse_rate = as.numeric(nurse_complete)/as.numeric(total),
         sdqt_rate = as.numeric(sdqt_complete)/as.numeric(total)) %>% 
  filter(DHB_name != "" & CB2018_name != "Great Barrier and Waiheke" & CB2018_name != "Waitemata" 
         & CB2018_name != "Onewhero-Tuakau")

# For each DHB, attach one row for national average
data1 = com_all_year %>% select(YEAR, vht_rate, nurse_rate, sdqt_rate)
data2 = data.frame(DHB_name = unique(dhb_map$DHB_name))
data3 = merge(data1, data2)
data3$CB2018_name = "National average"

com_all_year_lb_final = bind_rows(com_all_year_lb, data3) %>% 
  mutate(vht_rate_perc = round(vht_rate*100,2)) %>% 
  mutate(nurse_rate_perc = round(nurse_rate*100,2)) %>% 
  mutate(sdqt_rate_perc = round(sdqt_rate*100,2))

# Wrap into function - VHT
myplot_vht_lb = function(dhb){
  # Create data set to be plotted
  toplot = com_all_year_lb_final %>% filter(DHB_name==dhb) 
  
  if (dhb == "Waitemata"){lowerlimit = 40}
  else (lowerlimit = 50)
  
  # Find out which is national average
  ta_text = levels(factor(toplot$CB2018_name))
  index = which(ta_text=="National average")
  toplot$CB2018_name = factor(toplot$CB2018_name, levels = c(ta_text[index], ta_text[-index]))
  
  ggplot(data = toplot, aes(x = YEAR, y = vht_rate_perc, color = CB2018_name, 
                            size = CB2018_name, linetype = CB2018_name,
                            shape = CB2018_name)) +
    scale_x_continuous(breaks = seq(min(toplot$YEAR), max(toplot$YEAR), by = 1),
                       labels = c("11/12", "12/13", "13/14", "14/15", "15/16", "16/17", "17/18", "18/19")) + 
    xlab("") +
    ylab("") + 
    labs(title = "VHT Attendance Rates (%)", subtitle = paste("")) +
    scale_y_continuous(limits = c(lowerlimit, 100),
                       breaks = seq(lowerlimit, 100, by = 10)) + 
    geom_line() + geom_point(size = 3.5) +
    scale_color_manual(values = col) +
    scale_linetype_manual(values = c("twodash", rep(c("solid"), length.out = length(ta_text)-1))) +
    scale_size_manual(values = c(1.2, rep(1, length(ta_text)-1))) +
    scale_shape_manual(values = c(20, 15:18, 0:2, 5, 6)) +
    theme_classic() +
    theme(legend.position="bottom", legend.title = element_blank(),
          legend.text = element_text(size = 12),
          panel.grid.major.y = element_line(color = "gray", linetype = 2),
          title = element_text(size = 16),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15))
}

myplot_vht_lb("Auckland")
#myplot_vht_lb("Counties Manukau")
#myplot_vht_lb("Waitemata")


# Wrap into function - Nurse
myplot_nurse_lb = function(dhb){
  # Create data set to be plotted
  toplot = com_all_year_lb_final %>% filter(DHB_name==dhb) 
  
  if (dhb == "Waitemata"){lowerlimit = 40}
  else (lowerlimit = 50)
  
  # Find out which is national average
  ta_text = levels(factor(toplot$CB2018_name))
  index = which(ta_text=="National average")
  toplot$CB2018_name = factor(toplot$CB2018_name, levels = c(ta_text[index], ta_text[-index]))
  
  ggplot(data = toplot, aes(x = YEAR, y = nurse_rate_perc, color = CB2018_name, 
                            size = CB2018_name, linetype = CB2018_name,
                            shape = CB2018_name)) +
    scale_x_continuous(breaks = seq(min(toplot$YEAR), max(toplot$YEAR), by = 1),
                       labels = c("11/12", "12/13", "13/14", "14/15", "15/16", "16/17", "17/18", "18/19")) + 
    xlab("") +
    ylab("") + 
    labs(title = "Nurse Attendance Rates (%)", subtitle = paste("")) +
    scale_y_continuous(limits = c(lowerlimit, 100),
                       breaks = seq(lowerlimit, 100, by = 10)) + 
    geom_line() + geom_point(size = 3.5) +
    scale_color_manual(values = col) +
    scale_linetype_manual(values = c("twodash", rep(c("solid"), length.out = length(ta_text)-1))) +
    scale_size_manual(values = c(1.2, rep(1, length(ta_text)-1))) +
    scale_shape_manual(values = c(20, 15:18, 0:2, 5, 6)) +
    theme_classic() +
    theme(legend.position="bottom", legend.title = element_blank(),
          legend.text = element_text(size = 12),
          panel.grid.major.y = element_line(color = "gray", linetype = 2),
          title = element_text(size = 16),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15))
}

myplot_nurse_lb("Auckland")
#myplot_nurse_lb("Counties Manukau")
#myplot_nurse_lb_1("Waitemata")

# Wrap into function - sdqt
myplot_sdqt_lb = function(dhb){
  # Create data set to be plotted
  toplot = com_all_year_lb_final %>% filter(DHB_name==dhb) 
  
  # Find out which is national average
  ta_text = levels(factor(toplot$CB2018_name))
  index = which(ta_text=="National average")
  toplot$CB2018_name = factor(toplot$CB2018_name, levels = c(ta_text[index], ta_text[-index]))
  
  ggplot(data = toplot, aes(x = YEAR, y = sdqt_rate_perc, color = CB2018_name, 
                            size = CB2018_name, linetype = CB2018_name,
                            shape = CB2018_name)) +
    scale_x_continuous(breaks = seq(min(toplot$YEAR), max(toplot$YEAR), by = 1),
                       labels = c("11/12", "12/13", "13/14", "14/15", "15/16", "16/17", "17/18", "18/19")) + 
    xlab("") +
    ylab("") + 
    labs(title = "SDQ-Teacher Attendance Rates (%)", subtitle = paste("")) +
    scale_y_continuous(limits = c(0, 100),
                       breaks = seq(0, 100, by = 10)) + 
    geom_line() + geom_point(size = 3.5) +
    scale_color_manual(values = col) +
    scale_linetype_manual(values = c("twodash", rep(c("solid"), length.out = length(ta_text)-1))) +
    scale_size_manual(values = c(1.2, rep(1, length(ta_text)-1))) +
    scale_shape_manual(values = c(20, 15:18, 0:2, 5, 6)) +
    theme_classic() +
    theme(legend.position="bottom", legend.title = element_blank(),
          legend.text = element_text(size = 12),
          panel.grid.major.y = element_line(color = "gray", linetype = 2),
          title = element_text(size = 16),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15))
}

myplot_sdqt_lb("Auckland")
#myplot_sdqt_lb("Waitemata")
#myplot_sdqt_lb("Counties Manukau")


# Forest plots for each DHB -----
# Read in raw data
dhb_incomplete = read.csv("dhb_fulldata.csv")

# Attach level labels
dhb_incomplete = left_join(dhb_incomplete, varlabs, by = c("var" = "V1", "level" = "V2")) %>% 
  rename("var_label" = "V4", "level_label" = "V5")

# Change some labels as they are too long for graph
dhb_incomplete = dhb_incomplete %>% 
  mutate(var_label = str_replace(var_label, "Mother highest qualification", "Mother high qual")) %>% 
  mutate(level_label = str_replace(level_label, "Tertiary below Bachelor", "Tertiary")) %>% 
  mutate(level_label = str_replace(level_label, "No qualifications", "No quals"))

# Attach ref values
refvalues = dhb_incomplete %>% 
  filter(V3 == "ref") %>% 
  select(var, DHB2015_name, total, vht_incomplete, nurse_incomplete, sdqt_incomplete) %>% 
  rename(ref_total = total, ref_vht = vht_incomplete, ref_nurse = nurse_incomplete, ref_sdqt = sdqt_incomplete)

# Transfer the reference category values back to the main file
dhb_incomplete_ref = left_join(dhb_incomplete, refvalues, by = c("var" = "var", "DHB2015_name" = "DHB2015_name"))

# Calculate RR for each level except ref level
# Note: numerical variables are factors here, need to convert them into characters first then to numeric
dhb_incomplete_ref = dhb_incomplete_ref %>%
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

dhb_incomplete_ref_ci = dhb_incomplete_ref %>% 
  mutate(ci_vht = 1.96*(sqrt((((total-vht_incomplete)/vht_incomplete)/total) + (((ref_total-ref_vht)/ref_vht)/ref_total)))) %>% 
  mutate(vht_ci_ll = exp(log(rr_vht)-ci_vht)) %>% 
  mutate(vht_ci_ul = exp(log(rr_vht)+ci_vht)) %>% 
  mutate(ci_nurse = 1.96*(sqrt((((total-nurse_incomplete)/nurse_incomplete)/total) + (((ref_total-ref_nurse)/ref_nurse)/ref_total)))) %>% 
  mutate(nurse_ci_ll = exp(log(rr_nurse)-ci_nurse)) %>% 
  mutate(nurse_ci_ul = exp(log(rr_nurse)+ci_nurse)) %>% 
  mutate(ci_sdqt = 1.96*(sqrt((((total-sdqt_incomplete)/sdqt_incomplete)/total) + (((ref_total-ref_sdqt)/ref_sdqt)/ref_total)))) %>% 
  mutate(sdqt_ci_ll = exp(log(rr_sdqt)-ci_sdqt)) %>% 
  mutate(sdqt_ci_ul = exp(log(rr_sdqt)+ci_sdqt)) 


# plot of RRs for each predictor, separate plot for each area
# For each DHB
allplotdata = dhb_incomplete_ref_ci %>% 
  mutate(varlevel = paste(dhb_incomplete_ref_ci$var_label, dhb_incomplete_ref_ci$level_label, sep = ": ")) %>% 
  mutate(varlevel = str_replace_all(varlevel, "N changes of residence to age 4", "Address changes")) %>% 
  mutate(varlevel = str_replace(varlevel, "Received disability support: Yes", "Disability support")) %>% 
  mutate(varlevel = str_replace(varlevel, "N inpatient hospital visits", "N inpatient visits")) %>% 
  mutate(varlevel = str_replace(varlevel, "N quarters enrolled GP", "N qtrs enrolled GP")) %>% 
  mutate(varlevel = str_replace(varlevel, "Rented home: Yes", "Rented home")) %>% 
  mutate(varlevel = str_replace(varlevel, "Pacific ethnicity: Yes", "Pacific ethnicity")) %>% 
  mutate(varlevel = str_replace(varlevel, "Mother speaks English: Yes [(]vs No[)]", "Mother speaks English")) %>% 
  mutate(varlevel = str_replace(varlevel, "Mother was born in NZ: Yes [(]vs No[)]", "Mother born in NZ")) %>% 
  mutate(varlevel = str_replace(varlevel, "Maori ethnicity: Yes", "Maori ethnicity")) %>% 
  mutate(varlevel = str_replace(varlevel, "European ethnicity: Yes", "European ethnicity")) %>% 
  mutate(varlevel = str_replace(varlevel, "ED visit ever: Yes", "ED visit ever")) %>% 
  mutate(varlevel = str_replace(varlevel, "Birthweight <2500g: Yes", "Birthweight <2500g")) %>% 
  mutate(varlevel = str_replace(varlevel, "Child was born in NZ: Yes [(]vs No[)]", "Child born in NZ")) %>% 
  mutate(varlevel = str_replace(varlevel, "Benefit income: Yes", "Benefit income")) %>% 
  mutate(varlevel = str_replace(varlevel, "Asian ethnicity: Yes", "Asian ethnicity")) %>% 
  mutate(varlevel = str_replace(varlevel, "Mother high qual: Tertiary", "Mother tertiary qual")) %>% 
  mutate(varlevel = str_replace(varlevel, "Mother high qual: No quals", "Mother no quals")) %>% 
  mutate(varlevel = str_replace(varlevel, "Mother high qual: High school", "Mother high school qual")) %>% 
  mutate(varlevel = str_replace(varlevel, "N motor vehicles available", "N motor vehicles")) %>% 
  mutate(varlevel = str_replace(varlevel, "Sex: Male [(]vs Female[)]", "Male")) %>% 
  mutate(varlevel = str_replace(varlevel, "Maori ethnicity", "M\u101ori ethnicity")) %>% 
  filter(V3 == "") %>% 
  filter(var != "los_days_grp") %>% 
  filter(var != "bw_grp")

varlevel = factor(allplotdata$varlevel)
varlevel_text = levels(varlevel)
varlevel_text[c(6, 40, 38:35, 25, 22, 23, 28:29, 
                8, 21, 41, 15, 24, 33:34, 5, 11, 39, 16, 1:4, 12, 13:14, 17:20,
                26:27, 10, 42:43, 7, 9, 30:32)]

allplotdata$varlevelnew = factor(allplotdata$varlevel, 
                                 levels = varlevel_text[c(6, 40, 38:35, 25, 22, 23, 28:29, 
                                                          8, 21, 41, 15, 24, 33:34, 5, 11, 39, 16, 1:4, 12, 13:14, 17:20,
                                                          26:27, 10, 42:43, 7, 9, 30:32)])
batch = factor(allplotdata$varlevelnew, 
               levels = varlevel_text[c(6, 40, 38:35, 25, 22, 23, 28:29,
                                        8, 21, 41, 15, 24, 33:34, 5, 11, 39, 16, 1:4, 12, 13:14, 17:20,
                                        26:27, 10, 42:43, 7, 9, 30:32)],
               label = c(rep("Family Socioeconomic Position", 11), rep("Family & Child Characteristics", 22), 
                         rep("Health & Health System", 10)))

allplotdata$batch = batch

# Note: x-axis limit varies across DHBs
# Limits are defined by rounding the upper CI to nearest 0.5
# This means that limits = c(xx, xx) should change according to DHB

###
# Wrap into a function
pred_forest_vht = function(dhb){
  plotdata = allplotdata %>% 
    filter(DHB2015_name == dhb) %>% 
    filter(rr_vht != "")
  
  # For limits
  if (dhb == "Auckland" | dhb == "Counties Manukau" | dhb == "Northland"){
    upperlimit = 3.5
  } else if (dhb == "Bay of Plenty" | dhb == "Hutt Valley" | dhb == "Nelson Malborough"){
    upperlimit = 5.5
  } else if (dhb == "Canterbury" | dhb == "Capital and Coast"){
    upperlimit = 4.5
  } else if (dhb == "Hawkes Bay"){
    upperlimit = 9
  } else if (dhb == "Lakes"){
    upperlimit = 7
  } else if (dhb == "MidCentral" | dhb == "Southern"){
    upperlimit = 5
  } else if (dhb == "South Canterbury"){
    upperlimit = 13
  } else if (dhb == "Tairawhiti"){
    upperlimit = 8
  } else if (dhb == "Taranaki" | dhb == "Whanganui"){
    upperlimit = 6
  } else if (dhb == "Waikato"){
    upperlimit = 9.5
  } else if (dhb == "Wairarapa"){
    upperlimit = 8.5
  } else if (dhb == "Waitemata"){
    upperlimit = 4
  } else if (dhb == "West Coast"){
    upperlimit = 16
  }
  
  fplot = ggplot(data=plotdata, aes(x=varlevelnew, y=rr_vht, ymin=vht_ci_ll, ymax=vht_ci_ul,
                                    color = batch)) +
    scale_colour_manual(values = c("Family Socioeconomic Position" = col[5], "Family & Child Characteristics" = col[4],
                                   "Health & Health System" = col[2])) + 
    geom_pointrange(fatten=4) +
    #geom_point() + geom_errorbar() +
    geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
    coord_flip() +  # flip coordinates (puts labels on y axis)
    ylab("Rate Ratio (95% CI)") +
    theme_bw() + # use a white background, theme_classic() removes the grid lines
    facet_grid(batch ~ .,space="free_x", scales="free_y", switch="y") + 
    labs(title=paste(dhb, "DHB", sep = " "), 
         subtitle = "VHT",
         y = "Rate Ratio (95% CI), log scale") +
    scale_y_log10(breaks=c(0, 0.25, 0.5, 1, 2, 3, 5, 9, 27, 81), position="top",
                  labels=c(0, 0.25, 0.5, 1, 2, 3, 5, 9, 27, 81),
                  limits = c(0.1, upperlimit)) +
    theme(title = element_text(size = 11),
          plot.subtitle = element_text(size = 15),
          axis.title = element_text(size = 12),
          strip.text = element_text(size = 11),
          axis.text.x = element_text(size = 9, colour="black"),
          axis.text.y = element_text(size = 10),
          strip.placement = "inside",
          strip.background = element_rect(fill=NA,colour=col[3]),
          panel.spacing=unit(0,"cm"), axis.title.y = element_blank(),
          legend.position="none")
  
  # Change strip colour
  g = ggplot_gtable(ggplot_build(fplot))
  
  strips = which(grepl('strip-', g$layout$name))
  
  pal = c("#E8731B50", "#2c86b450", "#6b9b5f50")
  
  for (i in seq_along(strips)) {
    k <- which(grepl('rect', g$grobs[[strips[i]]]$grobs[[1]]$childrenOrder))
    l <- which(grepl('titleGrob', g$grobs[[strips[i]]]$grobs[[1]]$childrenOrder))
    g$grobs[[strips[i]]]$grobs[[1]]$children[[k]]$gp$fill <- pal[i]
  }
  
  # Replace the default heights with relative heights:
  g$heights[7] = unit(1.7, "null")
  g$heights[9] = unit(3.3, "null")
  g$heights[11] = unit(1.5, "null")
  
  plot(g)
  
}


## Nurse
pred_forest_nurse = function(dhb){
  plotdata = allplotdata %>% 
    filter(DHB2015_name == dhb) %>% 
    filter(rr_nurse != "")
  
  # For limits
  if (dhb == "Auckland" | dhb == "Counties Manukau" | dhb == "Northland"){
    upperlimit = 3.5
  } else if (dhb == "Bay of Plenty" | dhb == "Hutt Valley" | dhb == "Nelson Malborough"){
    upperlimit = 5.5
  } else if (dhb == "Canterbury" | dhb == "Capital and Coast"){
    upperlimit = 4.5
  } else if (dhb == "Hawkes Bay"){
    upperlimit = 9
  } else if (dhb == "Lakes"){
    upperlimit = 7
  } else if (dhb == "MidCentral" | dhb == "Southern"){
    upperlimit = 5
  } else if (dhb == "South Canterbury"){
    upperlimit = 13
  } else if (dhb == "Tairawhiti"){
    upperlimit = 8
  } else if (dhb == "Taranaki" | dhb == "Whanganui"){
    upperlimit = 6
  } else if (dhb == "Waikato"){
    upperlimit = 9.5
  } else if (dhb == "Wairarapa"){
    upperlimit = 8.5
  } else if (dhb == "Waitemata"){
    upperlimit = 4
  } else if (dhb == "West Coast"){
    upperlimit = 16
  }
  
  fplot = ggplot(data=plotdata, aes(x=varlevelnew, y=rr_nurse, ymin=nurse_ci_ll, ymax=nurse_ci_ul, color = batch)) +
    scale_colour_manual(values = c("Family Socioeconomic Position" = col[5], "Family & Child Characteristics" = col[4],
                                   "Health & Health System" = col[2])) + 
    geom_pointrange(fatten=4) +
    #geom_point() + geom_errorbar() +
    geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
    coord_flip() +  # flip coordinates (puts labels on y axis)
    ylab("Rate Ratio (95% CI)") +
    theme_bw() + # use a white background, theme_classic() removes the grid lines
    facet_grid(batch ~ .,space="free_x", scales="free_y", switch="y") + 
    labs(title=paste(dhb, "DHB", sep = " "), 
         subtitle = "Nurse",
         y = "Rate Ratio (95% CI), log scale") +
    scale_y_log10(breaks=c(0, 0.25, 0.5, 1, 2, 3, 5, 9, 27, 81), position="top",
                  labels=c(0, 0.25, 0.5, 1, 2, 3, 5, 9, 27, 81),
                  limits = c(0.1, upperlimit)) +
    theme(title = element_text(size = 11),
          plot.subtitle = element_text(size = 15),
          axis.title = element_text(size = 12),
          strip.text = element_text(size = 11),
          axis.text.x = element_text(size = 9, colour="black"),
          axis.text.y = element_blank(),
          #axis.text.y = element_text(size = 10),
          strip.placement = "inside",
          strip.background = element_rect(fill=NA,colour=col[3]),
          panel.spacing=unit(0,"cm"), axis.title.y = element_blank(),
          legend.position="none")
  
  # Change strip colour
  g = ggplot_gtable(ggplot_build(fplot))
  
  strips = which(grepl('strip-', g$layout$name))
  
  pal = c("#E8731B50", "#2c86b450", "#6b9b5f50")
  
  for (i in seq_along(strips)) {
    k <- which(grepl('rect', g$grobs[[strips[i]]]$grobs[[1]]$childrenOrder))
    l <- which(grepl('titleGrob', g$grobs[[strips[i]]]$grobs[[1]]$childrenOrder))
    g$grobs[[strips[i]]]$grobs[[1]]$children[[k]]$gp$fill <- pal[i]
  }
  
  # Replace the default heights with relative heights:
  g$heights[7] = unit(1.7, "null")
  g$heights[9] = unit(3.3, "null")
  g$heights[11] = unit(1.5, "null")
  
  plot(g)
  
}

## SDQT

pred_forest_sdqt = function(dhb){
  plotdata = allplotdata %>% 
    filter(DHB2015_name == dhb) %>% 
    filter(rr_sdqt != "")
  
  # For limits
  if (dhb == "Auckland" | dhb == "Counties Manukau" | dhb == "Northland"){
    upperlimit = 3.5
  } else if (dhb == "Bay of Plenty" | dhb == "Hutt Valley" | dhb == "Nelson Malborough"){
    upperlimit = 5.5
  } else if (dhb == "Canterbury" | dhb == "Capital and Coast"){
    upperlimit = 4.5
  } else if (dhb == "Hawkes Bay"){
    upperlimit = 9
  } else if (dhb == "Lakes"){
    upperlimit = 7
  } else if (dhb == "MidCentral" | dhb == "Southern"){
    upperlimit = 5
  } else if (dhb == "South Canterbury"){
    upperlimit = 13
  } else if (dhb == "Tairawhiti"){
    upperlimit = 8
  } else if (dhb == "Taranaki" | dhb == "Whanganui"){
    upperlimit = 6
  } else if (dhb == "Waikato"){
    upperlimit = 9.5
  } else if (dhb == "Wairarapa"){
    upperlimit = 8.5
  } else if (dhb == "Waitemata"){
    upperlimit = 4
  } else if (dhb == "West Coast"){
    upperlimit = 16
  }
  
  fplot = ggplot(data=plotdata, aes(x=varlevelnew, y=rr_sdqt, ymin=sdqt_ci_ll, ymax=sdqt_ci_ul, color = batch)) +
    scale_colour_manual(values = c("Family Socioeconomic Position" = col[5], "Family & Child Characteristics" = col[4],
                                   "Health & Health System" = col[2])) + 
    geom_pointrange(fatten=4) +
    #geom_point() + geom_errorbar() +
    geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
    coord_flip() +  # flip coordinates (puts labels on y axis)
    ylab("Rate Ratio (95% CI)") +
    theme_bw() + # use a white background, theme_classic() removes the grid lines
    facet_grid(batch ~ .,space="free_x", scales="free_y", switch="y") + 
    labs(title=paste(dhb, "DHB", sep = " "), 
         subtitle = "Teacher",
         y = "Rate Ratio (95% CI), log scale") +
    scale_y_log10(breaks=c(0, 0.25, 0.5, 1, 2, 3, 5, 9, 27, 81), position="top",
                  labels=c(0, 0.25, 0.5, 1, 2, 3, 5, 9, 27, 81),
                  limits = c(0.1, upperlimit)) +
    scale_x_discrete(position = "left") +
    theme(title = element_text(size = 11),
          plot.subtitle = element_text(size = 15),
          axis.title = element_text(size = 12),
          strip.text = element_text(size = 11),
          axis.text.x = element_text(size = 9, colour="black"),
          axis.text.y = element_text(size = 10),
          strip.placement = "inside",
          strip.background = element_rect(fill=NA,colour=col[3]),
          panel.spacing=unit(0,"cm"), axis.title.y = element_blank(),
          legend.position="none")
  
  # Change strip colour
  g = ggplot_gtable(ggplot_build(fplot))
  
  strips = which(grepl('strip-', g$layout$name))
  
  pal = c("#E8731B50", "#2c86b450", "#6b9b5f50")
  
  for (i in seq_along(strips)) {
    k <- which(grepl('rect', g$grobs[[strips[i]]]$grobs[[1]]$childrenOrder))
    l <- which(grepl('titleGrob', g$grobs[[strips[i]]]$grobs[[1]]$childrenOrder))
    g$grobs[[strips[i]]]$grobs[[1]]$children[[k]]$gp$fill <- pal[i]
  }
  
  # Replace the default heights with relative heights:
  g$heights[7] = unit(1.7, "null")
  g$heights[9] = unit(3.3, "null")
  g$heights[11] = unit(1.5, "null")
  
  plot(g)

}


# Composition of predictors for each DHB ----
# National level distribution of predictors
pred = read.csv("missing_count.csv")

# Calculate percentage of predictor's level 
# Calculate total counts by predictor
pred_total = pred %>% 
  filter(level != "Missing") %>% 
  mutate(COUNT = as.numeric(as.character(COUNT))) %>% 
  group_by(var) %>% 
  summarise(var_total = sum(COUNT))

# Attach total counts
pred_perc = left_join(pred, pred_total, by = c("var" = "var")) %>% 
  filter(level != "Missing") %>% 
  mutate(COUNT = as.numeric(as.character(COUNT)), level = as.character(level)) %>% 
  mutate(perc = COUNT/var_total) 

# DHB level distribution of predictors
pred_dist_dhb = read.csv("pred_dist_dhb.csv")
pred_dist_dhb_total = pred_dist_dhb %>% 
  filter(level != "MISSING" & DHB2015_name != "") %>% 
  mutate(COUNT = as.numeric(as.character(COUNT))) %>% 
  group_by(DHB2015_name, var) %>% 
  summarise(var_total = sum(COUNT))

# Calculate percentage difference from national average
pred_dhb_perc = pred_dist_dhb %>% 
  filter(DHB2015_name != "") %>% 
  filter(!is.na(level)) %>% 
  left_join(pred_dist_dhb_total, by = c("var" = "var", "DHB2015_name" = "DHB2015_name")) %>% 
  filter(level != "MISSING") %>% 
  mutate(COUNT = as.numeric(as.character(COUNT)), level = as.character(level)) %>% 
  mutate(perc = COUNT/var_total) %>% 
  left_join(pred_perc, by = c("var" = "var", "level" = "level")) %>% 
  rename(count_dhb = COUNT.x, count_nation = COUNT.y) %>% 
  mutate(perc_diff = round((perc.x - perc.y)*100,2))

pred_dhb_perc_full =  varlabs %>% 
  mutate(V2 = as.character(V2), V1 = as.factor(V1)) %>% 
  right_join(pred_dhb_perc, by = c("V1" = "var", "V2" = "level"))

## Wrap in a function
comp_top_pred = function(dhb){
  plotdata = pred_dhb_perc_full %>% 
    filter(DHB2015_name == dhb & V3 != "NA" & V3 != "ref" & V4 != "birth weight" &
             V4 != "Received disability support") %>% 
    mutate(V4 = str_replace(V4, "Mother highest qualification", "Mother high qual")) %>% 
    mutate(V5 = str_replace(V5, "No qualifications", "No quals")) %>% 
    mutate(varlevel = paste(V4, V5, sep = ": ")) %>% 
    mutate(group = ifelse(perc_diff<=0, 1, 0)) %>%
    mutate(varlevel = str_replace(varlevel, "N quarters enrolled GP: 0-3 [(]vs 12[+][)]", "Continuously enrolled with GP \n for 9 months or less")) %>% 
    mutate(varlevel = str_replace(varlevel, "Rented home: Yes", "Living in a rented home")) %>% 
    mutate(varlevel = str_replace(varlevel, "Pacific ethnicity: Yes", "Pacific")) %>% 
    mutate(varlevel = str_replace(varlevel, "Maori ethnicity: Yes", "M\u101ori")) %>% 
    mutate(varlevel = str_replace(varlevel, "European ethnicity: Yes", "European")) %>% 
    mutate(varlevel = str_replace(varlevel, "Asian ethnicity: Yes", "Asian")) %>% 
    mutate(varlevel = str_replace(varlevel, "Mother high qual: No quals [(]vs degree[)]", "Mother no qualifications")) %>% 
    mutate(varlevel = str_replace(varlevel, "N motor vehicles available: 0 [(]vs 2[+][)]", "No Motor Vehicles")) %>% 
    mutate(varlevel = str_replace(varlevel, "Household size: 8[+] [(]vs <5[)]", "8 or more people reside in household")) %>% 
    mutate(varlevel = str_replace(varlevel, "Mother age: <20 [(]vs 30-34[)]", "Mother <20 years at birth")) %>% 
    mutate(varlevel = str_replace(varlevel, "NZDep: Quintile 5 [(]vs 1[)]", "Residing in deprived area (NZDep Q5)"))

  # Only plot a selection of predictors
  plotdata = plotdata[c(5,4,1,29,15,11,16,28,9,20,23),]
  plotdata$varlevel = factor(plotdata$varlevel, plotdata$varlevel[c(11:1)])
  
  # Limits
  if (dhb == "Counties Manukau"){
    lowerlimit = -30
    upperlimit = 30
  } else if (dhb == "Lakes" | dhb == "Northland"){
    lowerlimit = -20 
    upperlimit = 30
  } else if (dhb == "Nelson Marlborough" | dhb == "South Canterbury" | dhb == "West Coast"){
    lowerlimit = -20 
    upperlimit = 25
  } else if (dhb == "Tairawhiti"){
    lowerlimit = -20
    upperlimit = 45
  } else {
    lowerlimit = -20
    upperlimit = 20
  }
  
  fplot = ggplot(data = plotdata, aes(x = perc_diff, y = varlevel, color = factor(group))) +
    scale_colour_manual(values = c(col[6], col[5])) +
    geom_point(size = 5) +
    geom_vline(xintercept = 0, lty = 2) +  # add a dotted line at x=0 after flip
    xlab("Percentage difference from national average") + ylab("") +
    scale_x_continuous(limits = c(lowerlimit, upperlimit), 
                       breaks = seq(lowerlimit, upperlimit, by = 5)) +
    theme_minimal() # use a white background 
  
  fplot + theme() +
    labs(title = "Characteristics of four-year-olds and their families \n in the DHB compared to the national average") +
    theme(axis.title = element_text(size = 15),
          title = element_text(size = 16.5),
          axis.text.x = element_text(size = 14, colour="black"),
          axis.text.y = element_text(size = 14, colour="black"),
          legend.position = "none",
          panel.grid.major.x = element_line(size=0.1))
}

comp_top_pred("Tairawhiti")
#comp_top_pred("Whanganui")
comp_top_pred("Auckland")


## characteristics tables ------
pred_dist_ta = read.csv("pred_dist_ta.csv")

# Calculate total counts by predictor
pred_dist_ta_total = pred_dist_ta %>% 
  filter(level != "MISSING" & grp_ta_ward_name != "") %>% 
  mutate(COUNT = as.numeric(as.character(COUNT))) %>% 
  group_by(grp_ta_ward_name, var) %>% 
  summarise(var_total = sum(COUNT, na.rm = TRUE))

# Attach total counts and calculate percentage
pred_ta_perc = pred_dist_ta %>% 
  filter(grp_ta_ward_name != "") %>% 
  filter(!is.na(level)) %>% 
  left_join(pred_dist_ta_total, by = c("var" = "var", "grp_ta_ward_name" = "grp_ta_ward_name")) %>% 
  filter(level != "MISSING") %>% 
  mutate(COUNT = as.numeric(as.character(COUNT)), level = as.character(level)) %>% 
  mutate(perc = round(COUNT/var_total*100,0)) %>% 
  left_join(dhb_map, by = c("grp_ta_ward_name" = "WARD2013_name"))

pred_ta_perc_full = varlabs %>% 
  mutate(V2 = as.character(V2), V1 = as.factor(V1)) %>% 
  right_join(pred_ta_perc, by = c("V1" = "var", "V2" = "level"))

chartable = function(dhb){
  tabledata = pred_ta_perc_full %>% 
    filter(DHB_name == dhb & V3 != "NA" & V3 != "ref" & V4 != "birth weight") %>% 
    mutate(V4 = str_replace(V4, "Mother highest qualification", "Mother high qual")) %>% 
    mutate(V5 = str_replace(V5, "No qualifications", "No quals")) %>% 
    mutate(varlevel = paste(V4, V5, sep = ": ")) %>% 
    #mutate(varlevel = str_replace(varlevel, "Received disability support: Yes", "Disability support")) %>% 
    mutate(varlevel = str_replace(varlevel, "N quarters enrolled GP: 0-3 [(]vs 12[+][)]", "Continuously enrolled with GP for 9 months or less")) %>% 
    mutate(varlevel = str_replace(varlevel, "Rented home: Yes", "Living in a rented home")) %>% 
    mutate(varlevel = str_replace(varlevel, "Pacific ethnicity: Yes", "Pacific")) %>% 
    mutate(varlevel = str_replace(varlevel, "Maori ethnicity: Yes", "M\u101ori")) %>% 
    mutate(varlevel = str_replace(varlevel, "European ethnicity: Yes", "European")) %>% 
    mutate(varlevel = str_replace(varlevel, "Asian ethnicity: Yes", "Asian")) %>% 
    mutate(varlevel = str_replace(varlevel, "Mother high qual: No quals [(]vs degree[)]", "Mother no qualifications")) %>% 
    mutate(varlevel = str_replace(varlevel, "N motor vehicles available: 0 [(]vs 2[+][)]", "No Motor Vehicles")) %>% 
    mutate(varlevel = str_replace(varlevel, "Household size: 8[+] [(]vs <5[)]", "8 or more people reside in household")) %>% 
    mutate(varlevel = str_replace(varlevel, "Mother age: <20 [(]vs 30-34[)]", "Mother <20 years at birth")) %>% 
    mutate(varlevel = str_replace(varlevel, "NZDep: Quintile 5 [(]vs 1[)]", "Residing in deprived area (NZDep Q5)")) %>% 
    filter((V4 == "Mother high qual" & V2 == 0) | (V4 == "N motor vehicles available" & V2 == 0) |
             (V4 == "Maori ethnicity" | V4 == "Pacific ethnicity" | V4 == "European ethnicity" |  V4 == "Asian ethnicity") |
             (V4 == "NZDep" & V2 == 5) | (V4 == "Mother age" & V2 == 1) | (V4 == "Household size" & V2 == 3) |
             (V4 == "N quarters enrolled GP" & V2 == 1) | V4 == "Rented home") %>% 
    select(varlevel, grp_ta_ward_name, perc)
  
  table = reshape(tabledata, timevar="grp_ta_ward_name", idvar="varlevel", direction="wide")
  # update variable order
  table = table[c(3, 2, 1, 11, 6, 5, 7, 10, 4, 8, 9),]
  
  write.csv(table, paste0(dhb, "CharTable", ".csv"))
}

chartable("Bay of Plenty")

# Read in local board - DHB concordance table
lb_map = read.csv("lb_map.csv")

# Read in data file
pred_dist_lb = read.csv("lb_full_data.csv")

# Change columns class
pred_dist_lb = pred_dist_lb %>% 
  mutate(level = as.factor(level),
         total = as.numeric(as.character(total)),
         vht_incomplete = as.numeric(as.character(vht_incomplete)),
         nurse_incomplete = as.numeric(as.character(nurse_incomplete)),
         sdqt_incomplete = as.numeric(as.character(sdqt_incomplete)))

a = pred_dist_lb[(pred_dist_lb$CB2018_name == "Great Barrier and Waiheke" | 
                    pred_dist_lb$CB2018_name == "Waitemata"),]

b = a %>% group_by(var, level) %>% 
  summarise(total_new = sum(total, na.rm = TRUE), 
            vht_incomplete_new = sum(vht_incomplete, na.rm = TRUE),
            nurse_incomplete_new = sum(nurse_incomplete, na.rm = TRUE),
            sdqt_incomplete_new = sum(sdqt_incomplete, na.rm = TRUE))

GBWW = data.frame(CB2018_name = "Great Barrier, Waiheke and Waitemata", level = b$level, 
                  total = b$total_new, vht_incomplete = b$vht_incomplete_new,
                  nurse_incomplete = b$nurse_incomplete_new, 
                  sdqt_incomplete = b$sdqt_incomplete_new, var = b$var)

pred_dist_lb_new = rbind(pred_dist_lb, GBWW)

pred_dist_lb_total = pred_dist_lb_new %>% 
  filter(level != "Missing" & CB2018_name != "" & CB2018_name != "Great Barrier and Waiheke" & 
           CB2018_name != "Waitemata" & CB2018_name != "Onewhero-Tuakau") %>% 
  mutate(total = as.numeric(as.character(total))) %>% 
  group_by(CB2018_name, var) %>% 
  summarise(var_total = sum(total, na.rm = TRUE))

pred_lb_perc = pred_dist_lb_new %>% 
  filter(CB2018_name != "" & CB2018_name != "Great Barrier and Waiheke" & 
           CB2018_name != "Waitemata" & CB2018_name != "Onewhero-Tuakau") %>% 
  filter(!is.na(level)) %>% 
  left_join(pred_dist_lb_total, by = c("var" = "var", "CB2018_name" = "CB2018_name")) %>% 
  filter(level != "Missing") %>% 
  mutate(total = as.numeric(as.character(total)), level = as.character(level)) %>% 
  mutate(perc = round(total/var_total*100,0)) %>% 
  left_join(lb_map, by = c("CB2018_name" = "CB2018_name"))

pred_lb_perc_full = varlabs %>% 
  mutate(V2 = as.character(V2), V1 = as.factor(V1)) %>% 
  right_join(pred_lb_perc, by = c("V1" = "var", "V2" = "level"))

chartable_lb = function(DHB){
  tabledata = pred_lb_perc_full %>% 
    filter(DHB2015_name == DHB & V3 != "NA" & V3 != "ref" & V4 != "birth weight") %>% 
    mutate(V4 = str_replace(V4, "Mother highest qualification", "Mother high qual")) %>% 
    mutate(V5 = str_replace(V5, "No qualifications", "No quals")) %>% 
    mutate(varlevel = paste(V4, V5, sep = ": ")) %>% 
    #mutate(varlevel = str_replace(varlevel, "Received disability support: Yes", "Disability support")) %>% 
    mutate(varlevel = str_replace(varlevel, "N quarters enrolled GP: 0-3 [(]vs 12[+][)]", "Continuously enrolled with GP for 9 months or less")) %>% 
    mutate(varlevel = str_replace(varlevel, "Rented home: Yes", "Living in a rented home")) %>% 
    mutate(varlevel = str_replace(varlevel, "Pacific ethnicity: Yes", "Pacific")) %>% 
    mutate(varlevel = str_replace(varlevel, "Maori ethnicity: Yes", "M\u101ori")) %>% 
    mutate(varlevel = str_replace(varlevel, "European ethnicity: Yes", "European")) %>% 
    mutate(varlevel = str_replace(varlevel, "Asian ethnicity: Yes", "Asian")) %>% 
    mutate(varlevel = str_replace(varlevel, "Mother high qual: No quals [(]vs degree[)]", "Mother no qualifications")) %>% 
    mutate(varlevel = str_replace(varlevel, "N motor vehicles available: 0 [(]vs 2[+][)]", "No Motor Vehicles")) %>% 
    mutate(varlevel = str_replace(varlevel, "Household size: 8[+] [(]vs <5[)]", "8 or more people reside in household")) %>% 
    mutate(varlevel = str_replace(varlevel, "Mother age: <20 [(]vs 30-34[)]", "Mother <20 years at birth")) %>% 
    mutate(varlevel = str_replace(varlevel, "NZDep: Quintile 5 [(]vs 1[)]", "Residing in deprived area (NZDep Q5)")) %>% 
    filter((V4 == "Mother high qual" & V2 == 0) | (V4 == "N motor vehicles available" & V2 == 0) |
             (V4 == "Maori ethnicity" | V4 == "Pacific ethnicity" | V4 == "European ethnicity" |  V4 == "Asian ethnicity") |
             (V4 == "NZDep" & V2 == 5) | (V4 == "Mother age" & V2 == 1) | (V4 == "Household size" & V2 == 3) |
             (V4 == "N quarters enrolled GP" & V2 == 1) | V4 == "Rented home") %>% 
    select(varlevel, CB2018_name, perc)
  
  table = reshape(tabledata, timevar="CB2018_name", idvar="varlevel", direction="wide")
  # update variable order
  table = table[c(3, 2, 1, 11, 6, 5, 7, 10, 4, 8, 9),]
  write.csv(table, paste0(DHB, "CharTable_LB", ".csv"))
}

chartable_lb("Auckland")

