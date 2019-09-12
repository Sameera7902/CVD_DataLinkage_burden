# CVD data linckage project - 2019
# Economic Burden of patients with cardiovascular disease in the last years of life; a retrospective analysis using linked data


# get the data
# Office Comp
setwd("C:/Users/n10075283/Google Drive/PhD work - Sameera/Other studys/Other Research Work/CVD_Data Linkage Project/Data")
setwd("~/Google Drive/PhD work - Sameera/Other studys/Other Research Work/CVD_Data Linkage Project/Data")

# Library
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)
library(ggpubr)

# @@@@ Get the data sets @@@@
admit_data = read.delim("qhapdc_extract.dat", sep = ",")
death_data = read.delim("death_extract.dat", sep = ",")
admit_cost_data = read.delim("admit_cost_details.dat", sep = ",")
ed_data = read.delim("qheddc_extract.dat", sep = ",")
ed_cost_data = read.delim("ed_cost_details.dat", sep = ",")

###################### @@@@ Pre processing @@@@

# Hospital Admission
admit_data$ADMISSION_DATE = parse_date_time(admit_data$ADMISSION_DATE, orders = "mY")

admit_data = admit_data %>%
  select(1:2, 7, 9)

# Death data
death_data$DATE_DEATH = parse_date_time(death_data$DATE_DEATH, orders = "mY")

death_data = death_data %>%
  select(1:2)

# Hospital Admission cost data
admit_cost_data = admit_cost_data %>%
  select(1, 2, 7)

# ED admission
ed_data$PRESENTATION_DATE = dmy(ed_data$PRESENTATION_DATE)

ed_data = ed_data %>%
  select(1:3)

# Ed cost data
ed_cost_data = ed_cost_data %>%
  select(1, 2, 7)


###################### @@@@ Merging data sets @@@@

# @@@@ Hospoital Admissions @@@@

# Merge death and admisson data
admit_death_data = merge(admit_data, death_data, by = c("PERSON_ID"))

# Create admit-death duration column
admit_death_data = admit_death_data %>%
  mutate(admit_death = (DATE_DEATH - ADMISSION_DATE)/31536000)

admit_death_data = mutate(admit_death_data, admit_death_dur = case_when(
  admit_death < 1 ~ "0",
  admit_death >= 1 & admit_death < 2 ~ "-1",
  admit_death >= 2 & admit_death < 3 ~ "-2",
  admit_death >= 3 & admit_death < 4 ~ "-3",
  admit_death >= 4 & admit_death < 5 ~ "-4",
  admit_death >= 5 & admit_death < 6 ~ "-5",
  admit_death >= 6 & admit_death < 7 ~ "-6",
  admit_death >= 7 & admit_death < 8 ~ "-7",
  admit_death >= 8 & admit_death < 9 ~ "-8",
  admit_death >= 9  ~ "-9",
))

#  Merge Cost data to the admission data 
admit_death_cost_data = merge(admit_death_data, admit_cost_data, by = c("PERSON_ID", "PERSON_SEQ_ID"))



# @@@@ ED Admissions @@@@

# Merge death and ED admisson date @@@@
ed_death_data = merge(ed_data, death_data, by = c("PERSON_ID"))

# Create ed -death duration column
ed_death_data$ed_death =  difftime(ed_death_data$DATE_DEATH, ed_death_data$PRESENTATION_DATE, units = "days")

ed_death_data = ed_death_data %>%
  mutate(ed_death = ed_death/365)

ed_death_data = mutate(ed_death_data, ed_death_dur = case_when(
  ed_death < 1 ~ "0",
  ed_death >= 1 & ed_death < 2 ~ "-1",
  ed_death >= 2 & ed_death < 3 ~ "-2",
  ed_death >= 3 & ed_death < 4 ~ "-3",
  ed_death >= 4 & ed_death < 5 ~ "-4",
  ed_death >= 5 & ed_death < 6 ~ "-5",
  ed_death >= 6 & ed_death < 7 ~ "-6",
  ed_death >= 7 & ed_death < 8 ~ "-7",
  ed_death >= 8 & ed_death < 9 ~ "-8",
  ed_death >= 9  ~ "-9",
))

#  Merge Cost data to the ed data 
ed_death_cost_data = merge(ed_death_data, ed_cost_data, by = c("PERSON_ID", "PERSON_SEQ_ID"))


###################### @@@@ Data Analysis @@@@

# @@@ Table 2 - hospital admissions calculations

# Count number of admissions 
admit_frequency = aggregate(admit_death_data, by = list(admit_death_data$PERSON_ID, admit_death_data$admit_death_dur), FUN = length)

admit_frequency = admit_frequency %>%
  select(1:3)

table_admit = admit_frequency %>%
  group_by(Group.2)%>%
  summarise(prop = mean(PERSON_ID),
            Fx = n(),
            Tot = sum(PERSON_ID),
            median = median(PERSON_ID),
            sd = sd(PERSON_ID),
            IQR1 = quantile(PERSON_ID, 0.25),
            IQR2 = quantile(PERSON_ID, 0.75))

write.table(table_admit, file = "Sheet1.csv", sep = ",")


# Cost of admissions 
ad_cost_sum = aggregate(admit_death_cost_data$TOTAL_COST, by = list(admit_death_cost_data$PERSON_ID, admit_death_cost_data$admit_death_dur), FUN = sum)

table_ad_cost = ad_cost_sum %>%
  group_by(Group.2)%>%
  summarise(prop = mean(x),
            Fx = n(),
            Tot = sum(x),
            median = median(x),
            sd = sd(x),
            IQR1 = quantile(x, 0.25),
            IQR2 = quantile(x, 0.75))

write.table(table_ad_cost, file = "Sheet1.csv", sep = ",")


# @@@ Table 2 - ED admissions calculations

# Count number of ED admissions 
ed_frequency = aggregate(ed_death_data, by = list(ed_death_data$PERSON_ID, ed_death_data$ed_death_dur), FUN = length)

ed_frequency = ed_frequency %>%
  select(1:3)

table_ed = ed_frequency %>%
  group_by(Group.2)%>%
  summarise(prop = mean(PERSON_ID),
            Fx = n(),
            Tot = sum(PERSON_ID),
            median = median(PERSON_ID),
            sd = sd(PERSON_ID),
            IQR1 = quantile(PERSON_ID, 0.25),
            IQR2 = quantile(PERSON_ID, 0.75))

write.table(table_ed, file = "Sheet1.csv", sep = ",")


# Cost of admissions 
ed_cost_sum = aggregate(ed_death_cost_data$TOTAL_COST, by = list(ed_death_cost_data$PERSON_ID, ed_death_cost_data$ed_death_dur), FUN = sum)

table_ed_cost = ed_cost_sum %>%
  group_by(Group.2)%>%
  summarise(prop = mean(x),
            Fx = n(),
            Tot = sum(x),
            median = median(x),
            sd = sd(x),
            IQR1 = quantile(x, 0.25),
            IQR2 = quantile(x, 0.75))

write.table(table_ed_cost, file = "Sheet1.csv", sep = ",")



# $$$$$$$ Figure 1 $$$$$$$
fig_admit = merge(table_ad_cost,table_admit, by = c("Group.2"))
fig_ed = merge(table_ed_cost, table_ed,by = c("Group.2"))

fig_admit$Group.2 = as.numeric(fig_admit$Group.2)
fig_ed$Group.2 = as.numeric(fig_ed$Group.2)



plot_1 = ggplot(fig_admit, aes(x = (Group.2)))
plot_1 = plot_1 + geom_line(aes(y = Tot.x, colour = "Hospital admission cost"))
plot_1 = plot_1 + geom_line(aes(y = Tot.y*5000, colour = "Hospital admissions"))
plot_1 = plot_1 + scale_y_continuous(sec.axis = sec_axis(~./5000, name = "Hospital admissions"), labels = dollar)
plot_1 <- plot_1 + scale_colour_manual(values = c("blue", "red"))
plot_1 <- plot_1 + labs(y = "Hospital admission cost",
                        x = "Years from death",
                        colour = "Category")
plot_1 <- plot_1 + theme(legend.title = element_text(size = 7))
plot_1 <- plot_1 + theme(legend.position = c(0.225, 0.72))
plot_1 <- plot_1 +theme(legend.text = element_text(size = 7))
plot_1 <- plot_1 + theme(axis.title.x = element_text(size = 10))
plot_1 <- plot_1 + theme(axis.title.y = element_text(size = 10))


plot_2 = ggplot(fig_ed, aes(x = Group.2))
plot_2 = plot_2 + geom_line(aes(y = Tot.x, colour = "ED admission cost"))
plot_2 = plot_2 + geom_line(aes(y = Tot.y*1000, colour = "ED admissions"))
plot_2 = plot_2 + scale_y_continuous(sec.axis = sec_axis(~./1000, name = "ED admissions"), labels = dollar)
plot_2 <- plot_2 + scale_colour_manual(values = c("blue", "red"))
plot_2 <- plot_2 + labs(y = "ED admission cost",
                        x = "Years from death",
                        colour = "Category")
plot_2 <- plot_2 + theme(legend.position = c(0.2, 0.72))
plot_2 <- plot_2 + theme(legend.title = element_text(size = 7))
plot_2 <- plot_2 +theme(legend.text = element_text(size = 7))
plot_2 <- plot_2 + theme(axis.title.x = element_text(size = 10))
plot_2 <- plot_2 + theme(axis.title.y = element_text(size = 10))
plot_2 <- plot_2 +legend.key.size = unit(3)


figure <- ggarrange(plot_1,plot_2,
                    labels = c("Hospital admissions", "ED admission"),
                    ncol = 1, nrow = 2,
                    font.label = list(size = 10, face = "bold", color ="black"),
                    hjust = c(-1.8, -3.0),
                    vjust = 2,
                    align = "hv")
figure



