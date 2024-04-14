####################################################
## pairwise correlaion between variables in Fitbit
## jieun Lee
####################################################

## Set Working Env
setRepositories(ind=1:8)
setwd('/disk4/bilje/ABCD/ABCD_fitbit')

library(data.table)
library(dplyr)
library(corrplot)
library(ggcorrplot)
library(GGally)
library(lares)
library(PerformanceAnalytics)
library(RColorBrewer)

## Define Function ##
makePlot <- function(data, xVal, yVal, fillVal){
  color_khy_cool <- c('#7ee695','#5cace6','#acace6')
  
  if(class(xVal[1]) == "factor"){
    g <- ggplot(data, aes(x = xVal, y = yVal, fill = fillVal)) +
      geom_boxplot(alpha=0.3)+
      scale_fill_brewer(palette = "Dark2")
  }else{
    g <- ggplot(data, aes(x = xVal, y = yVal)) +
      geom_point(aes(colour = fillVal))+
      #scale_color_manual(values = color_khy_cool) #scale_fill_brewer(palette = "YlGnBu" or "Dark2")
      scale_fill_brewer(palette = "Dark2")
  }
  
  g <- g+
    #xlab(get_label(xVal))+
    #ylab(get_label(yVal))+
    #labs(colour=get_label(fillVal))+
    theme_classic()
  
  return(g)
}

############################## Load data ##############################
fbdpas01 <- fread('abcd_fbdpas01.txt')
fbdss01 <- fread('abcd_fbdss01.txt')
fbwpas01 <- fread('abcd_fbwpas01.txt')
fbwss01 <- fread('abcd_fbwss01.txt')
fbpap01 <- fread('abcd_fbpap01.txt')
fbpap_var <- fread('abcd_fpap01_variableSelection.txt',header = F)
ksad_parent <- fread('/disk2/bipsw/ABCD/0.Data/abcd_ksad01.txt')
ksad_child <- fread('/disk2/bipsw/ABCD/0.Data/abcd_ksad501.txt')
fbdpas01_dic <- fread('abcd_fbdpas01.csv')
fbdss01_dic <- fread('abcd_fbdss01.csv')

############################## Sample Selection##############################
fbpap01 <- fbpap01[-1,]
fbpap01_var_sel <- fbpap01 %>% 
  select(fbpap_var$V1 %>% as.character())
sum((fbpap01_var_sel$fitpo_change_p)=='')
for(i in (3:ncol(fbpap01_var_sel))){
  print(sum(fbpap01_var_sel[,..i] == 1))
}
var_sel_id <- fbpap01_var_sel %>% 
  filter(fitpo_unusual_routine_p___0==1 & fitpo_beyond_typical_p___0==1 & fitpo_sick_p___0==1) %>% 
  select(src_subject_id)
############################## Variable Selection ##############################
#### Daily
## fbdpas01
fbdpas01 <- fbdpas01[-1,]

fbdpas01_num <- fbdpas01 %>%
  mutate_at(vars(which(fbdpas01_dic$DataType=='Integer'|fbdpas01_dic$DataType=='Float')+3),as.numeric)
#filter(fit_ss_total_step > 4)

fbdpas01_numcol <- c("fit_ss_day_min","fit_ss_night_min","fit_ss_total_step","fit_ss_sleep_min",
                     "fit_ss_total_ave_met","fit_ss_total_light_active_min",
                     "fit_ss_total_fairly_active_min", "fit_ss_total_very_active_min","fit_ss_fitbit_restingheartrate",
                     "fit_ss_total_sedentary_min","fit_ss_fitbit_sedentarymin","fit_ss_dayt_sedentary_min")

ggplot(data=fbdpas01_cor, aes(abs(fbdpas01_cor$fit_ss_total_sedentary_min - fbdpas01_cor$fit_ss_fitbit_sedentarymin)-fbdpas01_cor$fit_ss_sleep_min)) + 
  geom_histogram(fill='dark blue')+
  theme_classic()
ggplot(data=fbdpas01_cor, aes(fbdpas01_cor$fit_ss_night_min)) + 
  geom_histogram(fill='dark blue')+
  theme_classic()
fbdpas01_cor <- fbdpas01_num %>%
  filter(abs(fit_ss_fitbit_sedentarymin - fit_ss_dayt_sedentary_min)<30) %>%  
  filter(fit_ss_day_min_gt_600 & fit_ss_sleep_min_gt_300 & !fit_ss_mstep_lt_80_dailystep) %>% 
  select(all_of(fbdpas01_numcol)) %>% 
  na.omit()

temp <- fbdpas01_num %>% 
  filter(abs(fit_ss_fitbit_sedentarymin - fit_ss_dayt_sedentary_min)<30) %>%  
  filter(fit_ss_day_min_gt_600 & fit_ss_sleep_min_gt_300 & !fit_ss_mstep_lt_80_dailystep) %>% 
  select(fit_ss_fitbit_sedentarymin,fit_ss_dayt_sedentary_min,fit_ss_total_sedentary_min) %>% 
  na.omit()

corrplot(cor(temp), method = "color",number.cex = 0.4,
         addrect = 5,tl.col = 'black',col=brewer.pal(n=8, name="PuOr"), 
         tl.cex = 0.7,addCoef.col = "black",cl.cex = 0.7)
## fbdss01
fbdss01 <- fbdss01[-1,]
fbdss01_num <- fbdss01 %>%
  mutate_at(vars(which(fbdss01_dic$DataType=='Integer'|fbdss01_dic$DataType=='Float')+3),as.numeric)

fbdss01_numcol <- c("fit_ss_sleepperiod_minutes","fit_ss_wake_minutes","fit_ss_light_minutes",      
                    "fit_ss_deep_minutes","fit_ss_rem_minutes","fit_ss_wake_count",         
                    "fit_ss_avg_hr_wake","fit_ss_avg_hr_light","fit_ss_avg_hr_deep","fit_ss_avg_hr_rem" )
fbdss01_cor <- fbdss01_num %>% 
  select(all_of(fbdss01_numcol)) %>% 
  na.omit()
ggplot(data=fbdss01_cor, aes(abs(fbdss01_cor$fit_ss_avg_hr_light - fbdss01_cor$fit_ss_avg_hr_rem))) + 
  geom_histogram(fill='dark blue')+
  theme_classic()
#### Weekly
## fbwpas01
fbwpas01 <- fbwpas01[-1,]
fbwpas01_num <- fbwpas01 %>% 
  mutate_at(vars(fit_ss_day_wkno:fit_ss_perday_veryactivemin),as.numeric)
fbwpas01_cor <- fbwpas01_num %>% 
  filter(fit_ss_meet_abcd_rule == 1) %>% 
  filter(src_subject_id %in% var_sel_id$src_subject_id) %>% 
  select(fit_ss_wk_avg_day_min:fit_ss_fitbit_rest_hr,c(fit_ss_wk_total_steps:fit_ss_wk_very_active_min)) %>% 
  na.omit()

## fbwss01
fbwss01 <- fbwss01[-1,]
fbwss01_num <- fbwss01 %>% 
  mutate_at(vars(fit_ss_sleep_period_minutes:fit_ss_sleep_avg_hr_rem),as.numeric)
fbwss01_cor <- fbwss01_num %>% 
  filter(src_subject_id %in% var_sel_id$src_subject_id) %>% 
  select(fit_ss_sleep_period_minutes:fit_ss_sleep_avg_hr_rem,-fit_ss_avg_sleep_period_min) %>% 
  na.omit()
############################## EDA ##############################

############################## correlation ##############################
corrplot(cor(fbdpas01_cor))

corrplot.mixed(cor(fbdpas01_cor),
               upper = 'square',
               lower = 'number',
               addgrid.col = 'black',
               tl.col = 'black')

corrplot(cor(fbdpas01_cor), method = "color",number.cex = 0.7,
         addrect = 5,tl.col = 'black',col=brewer.pal(n=8, name="PuOr"), 
         tl.cex = 0.7,addCoef.col = "black",cl.cex = 0.7)
corrplot(cor(fbdss01_cor), method = "color",number.cex = 0.7,
         addrect = 5,tl.col = 'black',col=brewer.pal(n=8, name="PuOr"), 
         tl.cex = 0.7,addCoef.col = "black",cl.cex = 0.7)
corrplot(cor(fbwpas01_cor), method = "color",number.cex = 0.7,
         addrect = 5,tl.col = 'black',col=brewer.pal(n=8, name="PuOr"), 
         tl.cex = 0.7,addCoef.col = "black",cl.cex = 0.7)
corrplot(cor(fbwss01_cor), method = "color",number.cex = 0.4,
         addrect = 5,tl.col = 'black',col=brewer.pal(n=8, name="PuOr"), 
         tl.cex = 0.7,addCoef.col = "black",cl.cex = 0.7)

ggcorrplot(cor(fbdpas01_cor))

corr_cross(fbdpas01_cor, rm.na = T, max_pvalue = 0.05, top = 15, grid = T)
corr_cross(fbdss01_cor, rm.na = T, max_pvalue = 0.05, top = 15, grid = T)
corr_cross(fbwpas01_cor, rm.na = T, max_pvalue = 0.05, top = 15, grid = T)
corr_cross(fbwss01_cor, rm.na = T, max_pvalue = 0.05, top = 15, grid = T)

ggcorr(fbdpas01_cor,
       nbreaks = 6,
       low = "steelblue",
       mid = "white",
       high = "darkred",
       geom = "circle")

chart.Correlation(cor(fbwss01_cor))

############################## fbdpas + fbdss ##############################
temp <- table(abs(fbdpas01_num$fit_ss_fitbit_sedentarymin-fbdpas01_num$fit_ss_total_sedentary_min)) %>% 
  as.data.table()

fbdpas01_agg <- fbdpas01_num %>% 
  filter(abs(fit_ss_fitbit_sedentarymin - fit_ss_dayt_sedentary_min)<30)%>%
  filter(fit_ss_day_min_gt_600 & fit_ss_sleep_min_gt_300 & !fit_ss_mstep_lt_80_dailystep) %>%
  select(all_of(fbdpas01_numcol),src_subject_id,fit_ss_wear_date,fit_ss_sleep_min) %>% 
  mutate(key=paste0(src_subject_id,'_',fit_ss_wear_date)) %>% 
  select(-src_subject_id,-fit_ss_wear_date) 

fbdss01_agg <- fbdss01_num %>% 
  select(all_of(fbdss01_numcol),src_subject_id,fit_ss_sleepdate) %>% 
  mutate(key=paste0(src_subject_id,'_',fit_ss_sleepdate)) %>% 
  select(-src_subject_id,-fit_ss_sleepdate) 
# %>% distinct(key,.keep_all = T)

sum(duplicated(fbdss01_agg$key))
mergeData <- merge(fbdpas01_agg, fbdss01_agg, by='key') %>% na.omit()
mergeData <- mergeData %>% 
  filter(abs(fit_ss_sleep_min-fit_ss_sleepperiod_minutes)<109.5)

ggplot(data=mergeData, aes(abs(mergeData$fit_ss_sleep_min-mergeData$fit_ss_sleepperiod_minutes))) + 
  geom_histogram(fill='dark blue')+
  theme_classic()

corr_cross(mergeData[,-1], rm.na = T, max_pvalue = 0.05, top = 15, grid = T)
corrplot(cor(mergeData[,-1]), method = "circle",number.cex = 0.4,
         addrect = 5,tl.col = 'black',col=brewer.pal(n=8, name="PuOr"), 
         tl.cex = 0.7,addCoef.col = "black",cl.cex = 0.7)

########################### fbwpas & fbwss ########################### 
fbwpas01_agg <- fbwpas01_num %>% 
  filter(fit_ss_meet_abcd_rule == 1) %>% 
  filter(src_subject_id %in% var_sel_id$src_subject_id) %>% 
  mutate(key=paste0(src_subject_id,'_',fit_ss_day_wkno)) %>% 
  select(fit_ss_wk_avg_day_min:fit_ss_fitbit_rest_hr,c(fit_ss_wk_total_steps:fit_ss_wk_very_active_min),key) %>%
  distinct(key,.keep_all = T) %>% 
  na.omit()

fbwss01_agg <- fbwss01_num %>% 
  filter(src_subject_id %in% var_sel_id$src_subject_id) %>% 
  mutate(key=paste0(src_subject_id,'_',fit_ss_wkno))%>% 
  select(fit_ss_sleep_period_minutes:fit_ss_sleep_avg_hr_rem,-fit_ss_avg_sleep_period_min,key) %>% 
  distinct(key,.keep_all = T) %>% 
  na.omit()

mergeW <- merge(fbwpas01_agg,fbwss01_agg, by='key') %>% na.omit()
corr_cross(mergeW[,-1], rm.na = T, max_pvalue = 0.05, top = 15, grid = T)
corrplot(cor(mergeW[,-1]), method = "circle",number.cex = 0.4,
         addrect = 5,tl.col = 'black',col=brewer.pal(n=8, name="PuOr"), 
         tl.cex = 0.7,addCoef.col = "black",cl.cex = 0.7)

########################### KSAD ########################### 
intersect(ksad_child$src_subject_id,unique(fbdpas01_num$src_subject_id))
depressive_child <- ksad_child %>% filter(ksads_1_1_t != '0')
intersect(depressive_child$src_subject_id,unique(fbdpas01_num$src_subject_id))




######################### 
summary(mergeData$fit_ss_total_sedentary_min)

mergeData_diff <- mergeData %>% 
  #filter((fit_ss_sleep_min - fit_ss_night_min)>250) %>% 
  filter(fit_ss_total_sedentary_min>1108)
corr_cross(mergeData_diff[,-1], rm.na = T, max_pvalue = 0.05, top = 15, grid = T)
corrplot(cor(mergeData_diff[,-1]), method = "color",number.cex = 0.4,
         addrect = 5,tl.col = 'black',col=brewer.pal(n=8, name="PuOr"), 
         tl.cex = 0.7,addCoef.col = "black",cl.cex = 0.7)


############################## Step Histogram ##############################
ggplot(data=fbdpas01_cor, aes(fbdpas01_cor$fit_ss_total_step)) + 
  geom_histogram(fill='dark blue')+
  theme_classic()
ggplot(data=fbdpas01_num, aes(abs(fbdpas01_num$fit_ss_total_sedentary_min-fbdpas01_num$fit_ss_fitbit_sedentarymin))) + 
  geom_histogram(fill='dark blue')+
  theme_classic()

ggplot(data=fbdpas01_num, aes(abs(fbdpas01_num$fit_ss_total_step-fbdpas01_num$fit_ss_fitbit_totalsteps))) + 
  geom_histogram(fill='dark blue')+
  theme_classic()

temp <- table(mergeData$fit_ss_sleep_min-mergeData$fit_ss_sleepperiod_minutes)
nrow(temp)

ggplot(data=mergeData, aes((mergeData$fit_ss_sleep_min-mergeData$fit_ss_night_min))) + 
  geom_histogram(fill='dark blue')+
  theme_classic()

fbdpas01_num$sex <- as.factor(fbdpas01_num$sex)
fbdpas01_num$fit_ss_weekend_ind <- as.factor(fbdpas01_num$fit_ss_weekend_ind)
fbdpas01_cor_fac <- fbdpas01_num %>%
  filter(abs(fit_ss_fitbit_sedentarymin - fit_ss_dayt_sedentary_min)<30) %>%  
  filter(fit_ss_day_min_gt_600 & fit_ss_sleep_min_gt_300 & !fit_ss_mstep_lt_80_dailystep) %>% 
  select(all_of(fbdpas01_numcol),sex,fit_ss_weekend_ind) %>% 
  na.omit()
ggplot(fbdpas01_cor_fac, aes(x=fit_ss_total_step,y=fit_ss_total_ave_met)) +
  geom_point(aes(colour = fit_ss_weekend_ind))+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic()
ggplot(fbdpas01_cor_fac, aes(x = fit_ss_total_step, fill = fit_ss_weekend_ind)) +
  geom_boxplot(alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic()
fbdss01_num$fit_ss_weekend_ind <- as.factor(fbdss01_num$fit_ss_weekend_ind)
fbdss01_num$sex <- as.factor(fbdss01_num$sex)
fbdss01_cor_fac <- fbdss01_num %>% 
  filter(fit_ss_wake_count<200)
select(all_of(fbdss01_numcol),sex,fit_ss_weekend_ind) %>% 
  na.omit()
fbdss01_num <- fbdss01_num %>% 
  filter(fit_ss_light_minutes<=1000)

ggplot(fbdss01_cor_fac, aes(x=fit_ss_wake_count,y=fit_ss_sleepperiod_minutes)) +
  geom_point(aes(colour = sex))+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic()
ggplot(fbdss01_num, aes(x = fit_ss_weekend_ind, y = fit_ss_wake_count, fill = sex)) +
  geom_boxplot(alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic()

## low step
summary(fbdpas01_num$fit_ss_total_step)
low_step <- fbdpas01_num %>% 
  filter(fit_ss_total_sedentary_min>1080)%>%
  filter(fit_ss_day_min_gt_600 & fit_ss_sleep_min_gt_300 & !fit_ss_mstep_lt_80_dailystep) %>% 
  select(all_of(fbdpas01_numcol),fit_ss_weekend_ind) %>% 
  na.omit()

low_step <- mergeData[,-1] %>% 
  filter(fit_ss_total_sedentary_min>1080)

corrplot(cor(low_step), method = "color",number.cex = 0.4,
         addrect = 5,tl.col = 'black',col=brewer.pal(n=8, name="PuOr"), 
         tl.cex = 0.7,addCoef.col = "black",cl.cex = 0.7, mar=c(1,1,1,1))


## high step
high_step <- fbdpas01_num %>% 
  filter(fit_ss_total_step>11443 & fit_ss_total_sedentary_min<694)%>%
  filter(fit_ss_day_min_gt_600 & fit_ss_sleep_min_gt_300 & !fit_ss_mstep_lt_80_dailystep) %>% 
  select(all_of(fbdpas01_numcol)) %>% 
  na.omit()

corrplot(cor(high_step), method = "color",number.cex = 0.4,
         addrect = 5,tl.col = 'black',col=brewer.pal(n=8, name="PuOr"), 
         tl.cex = 0.7,addCoef.col = "black",cl.cex = 0.7)

summary(fbdpas01_num$fit_ss_total_sedentary_min)

## Step & Sedentary minutes
ggplot(low_step, aes(x=fit_ss_total_step,y=fit_ss_total_sedentary_min)) +
  geom_point(aes(colour = fit_ss_weekend_ind))+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic()


#which(temp$key == temp$key[91])

#########################################################################################################################
#Study1. Daily physical activity summaries(fbdpas01.txt)
#hist
#1
ggplot(data=fbdpas01_cor, aes(fbdpas01_cor$fit_ss_day_min)) + 
  geom_bar(stat="bin", bins = 30, fill='#EDB9BB', col = "black") +
  coord_cartesian(xlim = c(500, 1550)) +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/hist/1.fit_ss_day_min(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))
#2
ggplot(data=fbdpas01_cor, aes(fbdpas01_cor$fit_ss_night_min)) + 
  geom_bar(stat="bin", bins = 30, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/hist/2.fit_ss_night_min(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))
#3
ggplot(data=fbdpas01_cor, aes(fbdpas01_cor$fit_ss_total_step)) + 
  geom_bar(stat="bin", bins = 30, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/hist/3.fit_ss_total_step(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))
#4
ggplot(data=fbdpas01_cor, aes(fbdpas01_cor$fit_ss_sleep_min)) + 
  geom_bar(stat="bin", bins = 30, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/hist/4.fit_ss_sleep_min(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))
#5
ggplot(data=fbdpas01_cor, aes(fbdpas01_cor$fit_ss_total_ave_met)) + 
  geom_bar(stat="bin", bins = 30, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/hist/5.fit_ss_total_ave_met(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))
#6
ggplot(data=fbdpas01_cor, aes(fbdpas01_cor$fit_ss_total_light_active_min)) + 
  geom_bar(stat="bin", bins = 30, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/hist/6.fit_ss_total_light_active_min(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))
#7
ggplot(data=fbdpas01_cor, aes(fbdpas01_cor$fit_ss_total_fairly_active_min)) + 
  geom_bar(stat="bin", bins = 30, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/hist/7.fit_ss_total_fairly_active_min(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))
#8
ggplot(data=fbdpas01_cor, aes(fbdpas01_cor$fit_ss_total_very_active_min)) + 
  geom_bar(stat="bin", bins = 30, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/hist/8.fit_ss_total_very_active_min(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))
#9
ggplot(data=fbdpas01_cor, aes(fbdpas01_cor$fit_ss_fitbit_restingheartrate)) + 
  geom_bar(stat="bin", bins = 30, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/hist/9.fit_ss_fitbit_restingheartrate(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))
#10
ggplot(data=fbdpas01_cor, aes(fbdpas01_cor$fit_ss_total_sedentary_min)) + 
  geom_bar(stat="bin", bins = 30, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/hist/10.fit_ss_total_sedentary_min(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))
#11
ggplot(data=fbdpas01_cor, aes(fbdpas01_cor$fit_ss_fitbit_sedentarymin)) + 
  geom_bar(stat="bin", bins = 30, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/hist/11.fit_ss_fitbit_sedentarymin(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))
#12
ggplot(data=fbdpas01_cor, aes(fbdpas01_cor$fit_ss_dayt_sedentary_min)) + 
  geom_bar(stat="bin", bins = 30, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/hist/12.fit_ss_dayt_sedentary_min(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))
#########################################################################################################################
#Study1. Daily physical activity summaries(fbdpas01.txt)
#boxplot
#d1495b", "#edae49", "#66a182"
ggplot(fbdpas01_cor, aes(x = fbdpas01_cor$fit_ss_day_min)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/box/1.fit_ss_day_min(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbdpas01_cor, aes(x = fbdpas01_cor$fit_ss_night_min)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/box/2.fit_ss_night_min(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbdpas01_cor, aes(x = fbdpas01_cor$fit_ss_total_step)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/box/3.fit_ss_total_step(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbdpas01_cor, aes(x = fbdpas01_cor$fit_ss_sleep_min)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/box/4.fit_ss_sleep_min(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbdpas01_cor, aes(x = fbdpas01_cor$fit_ss_total_ave_met)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/box/5.fit_ss_total_ave_met(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbdpas01_cor, aes(x = fbdpas01_cor$fit_ss_total_light_active_min)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/box/6.fit_ss_total_light_active_min(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbdpas01_cor, aes(x = fbdpas01_cor$fit_ss_total_fairly_active_min)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/box/7.fit_ss_total_fairly_active_min(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbdpas01_cor, aes(x = fbdpas01_cor$fit_ss_total_very_active_min)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/box/8.fit_ss_total_very_active_min(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbdpas01_cor, aes(x = fbdpas01_cor$fit_ss_fitbit_restingheartrate)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/box/9.fit_ss_fitbit_restingheartrate(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbdpas01_cor, aes(x = fbdpas01_cor$fit_ss_total_sedentary_min)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/box/10.fit_ss_total_sedentary_min(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbdpas01_cor, aes(x = fbdpas01_cor$fit_ss_fitbit_sedentarymin)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/box/11.fit_ss_fitbit_sedentarymin(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbdpas01_cor, aes(x = fbdpas01_cor$fit_ss_dayt_sedentary_min)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/box/12.fit_ss_dayt_sedentary_min(box).jpg",
       width=10.4, height=7.8, units=c("cm"))
#########################################################################################################################
#Study1. Daily physical activity summaries(fbdpas01.txt)
#violin plot
#d1495b", "#edae49", "#66a182"
ggplot(fbdpas01_cor_fac, aes(x= fbdpas01_cor_fac$sex, y = fbdpas01_cor_fac$fit_ss_day_min)) +
  geom_violin(trim= F, fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/violin/1.fit_ss_day_min(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))
  
ggplot(fbdpas01_cor_fac, aes(x= fbdpas01_cor_fac$sex, y = fbdpas01_cor_fac$fit_ss_night_min)) +
  geom_violin(trim= F, fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/violin/2.fit_ss_night_min(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))
  
ggplot(fbdpas01_cor_fac, aes(x= fbdpas01_cor_fac$sex, y = fbdpas01_cor_fac$fit_ss_total_step)) +
  geom_violin(trim= F, fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/violin/3.fit_ss_total_step(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))
  
ggplot(fbdpas01_cor_fac, aes(x= fbdpas01_cor_fac$sex, y = fbdpas01_cor_fac$fit_ss_sleep_min)) +
  geom_violin(trim= F, fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/violin/4.fit_ss_sleep_min(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))
  
ggplot(fbdpas01_cor_fac, aes(x= fbdpas01_cor_fac$sex, y = fbdpas01_cor_fac$fit_ss_total_ave_met)) +
  geom_violin(trim= F, fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/violin/5.fit_ss_total_ave_met(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))
  
ggplot(fbdpas01_cor_fac, aes(x= fbdpas01_cor_fac$sex, y = fbdpas01_cor_fac$fit_ss_total_light_active_min)) +
  geom_violin(trim= F, fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/violin/6.fit_ss_total_light_active_min(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))
  
ggplot(fbdpas01_cor_fac, aes(x= fbdpas01_cor_fac$sex, y = fbdpas01_cor_fac$fit_ss_total_fairly_active_min)) +
  geom_violin(trim= F, fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/violin/7.fit_ss_total_fairly_active_min(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))
  
ggplot(fbdpas01_cor_fac, aes(x= fbdpas01_cor_fac$sex, y = fbdpas01_cor_fac$fit_ss_total_very_active_min)) +
  geom_violin(trim= F, fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/violin/8.fit_ss_total_very_active_min(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))
  
ggplot(fbdpas01_cor_fac, aes(x= fbdpas01_cor_fac$sex, y = fbdpas01_cor_fac$fit_ss_fitbit_restingheartrate)) +
  geom_violin(trim= F, fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/violin/9.fit_ss_fitbit_restingheartrate(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))
  
ggplot(fbdpas01_cor_fac, aes(x= fbdpas01_cor_fac$sex, y = fbdpas01_cor_fac$fit_ss_total_sedentary_min)) +
  geom_violin(trim= F, fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/violin/10.fit_ss_total_sedentary_min(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))
  
ggplot(fbdpas01_cor_fac, aes(x= fbdpas01_cor_fac$sex, y = fbdpas01_cor_fac$fit_ss_fitbit_sedentarymin)) +
  geom_violin(trim= F, fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/violin/11.fit_ss_fitbit_sedentarymin(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))
  
ggplot(fbdpas01_cor_fac, aes(x= fbdpas01_cor_fac$sex, y = fbdpas01_cor_fac$fit_ss_dayt_sedentary_min)) +
  geom_violin(trim= F, fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/violin/12.fit_ss_dayt_sedentary_min(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))
#########################################################################################################################
#Study2. Daily physical activity summaries(fbdpas01.txt)
#hist
ggplot(data=fbdss01_cor, aes(fbdss01_cor$fit_ss_sleepperiod_minutes)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbdss01/hist/1.fit_ss_sleepperiod_minutes(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(data=fbdss01_cor, aes(fbdss01_cor$fit_ss_wake_minutes)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbdss01/hist/2.fit_ss_wake_minutes(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(data=fbdss01_cor, aes(fbdss01_cor$fit_ss_light_minutes)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbdss01/hist/3.fit_ss_light_minutes(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(data=fbdss01_cor, aes(fbdss01_cor$fit_ss_deep_minutes)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbdss01/hist/4.fit_ss_deep_minutes(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(data=fbdss01_cor, aes(fbdss01_cor$fit_ss_rem_minutes)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbdss01/hist/5.fit_ss_rem_minutes(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(data=fbdss01_cor, aes(fbdss01_cor$fit_ss_wake_count)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbdss01/hist/6.fit_ss_wake_count(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(data=fbdss01_cor, aes(fbdss01_cor$fit_ss_avg_hr_wake)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbdss01/hist/7.fit_ss_avg_hr_wake(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(data=fbdss01_cor, aes(fbdss01_cor$fit_ss_avg_hr_light)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbdss01/hist/8.fit_ss_avg_hr_light(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(data=fbdss01_cor, aes(fbdss01_cor$fit_ss_avg_hr_deep)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbdss01/hist/9.fit_ss_avg_hr_deep(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(data=fbdss01_cor, aes(fbdss01_cor$fit_ss_avg_hr_rem)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbdss01/hist/10.fit_ss_avg_hr_rem(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))
#########################################################################################################################
#Study2. Daily physical activity summaries(fbdpas01.txt)
#boxplot
ggplot(fbdss01_cor, aes(x = fbdss01_cor$fit_ss_sleepperiod_minutes)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbdss01/box/1.fit_ss_sleepperiod_minutes(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbdss01_cor, aes(x = fbdss01_cor$fit_ss_wake_minutes)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbdss01/box/2.fit_ss_wake_minutes(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbdss01_cor, aes(x = fbdss01_cor$fit_ss_light_minutes)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbdss01/box/3.fit_ss_light_minutes(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbdss01_cor, aes(x = fbdss01_cor$fit_ss_deep_minutes)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbdss01/box/4.fit_ss_deep_minutes(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbdss01_cor, aes(x = fbdss01_cor$fit_ss_rem_minutes)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbdss01/box/5.fit_ss_rem_minutes(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbdss01_cor, aes(x = fbdss01_cor$fit_ss_wake_count)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbdss01/box/6.fit_ss_wake_count(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbdss01_cor, aes(x = fbdss01_cor$fit_ss_avg_hr_wake)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbdss01/box/7.fit_ss_avg_hr_wake(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbdss01_cor, aes(x = fbdss01_cor$fit_ss_avg_hr_light)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbdss01/box/8.fit_ss_avg_hr_light(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbdss01_cor, aes(x = fbdss01_cor$fit_ss_avg_hr_deep)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbdss01/box/9.fit_ss_avg_hr_deep(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbdss01_cor, aes(x = fbdss01_cor$fit_ss_avg_hr_rem)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbdss01/box/10.fit_ss_avg_hr_rem(box).jpg",
       width=10.4, height=7.8, units=c("cm"))
#########################################################################################################################
#Study2. Daily physical activity summaries(fbdpas01.txt)
#violin plot
ggplot(fbdss01_cor_fac, aes(x= fbdss01_cor_fac$sex, y = fbdss01_cor_fac$fit_ss_sleepperiod_minutes)) +
  geom_violin(trim= F, fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbdss01/violin/1.fit_ss_sleepperiod_minutes(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))


ggplot(fbdss01_cor_fac, aes(x= fbdss01_cor_fac$sex, y = fbdss01_cor_fac$fit_ss_wake_minutes)) +
  geom_violin(trim= F, fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbdss01/violin/2.fit_ss_wake_minutes(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbdss01_cor_fac, aes(x= fbdss01_cor_fac$sex, y = fbdss01_cor_fac$fit_ss_light_minutes)) +
  geom_violin(trim= F, fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbdss01/violin/3.fit_ss_light_minutes(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbdss01_cor_fac, aes(x= fbdss01_cor_fac$sex, y = fbdss01_cor_fac$fit_ss_deep_minutes)) +
  geom_violin(trim= F, fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbdss01/violin/4.fit_ss_deep_minutes(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbdss01_cor_fac, aes(x= fbdss01_cor_fac$sex, y = fbdss01_cor_fac$fit_ss_rem_minutes)) +
  geom_violin(trim= F, fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbdss01/violin/5.fit_ss_rem_minutes(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbdss01_cor_fac, aes(x= fbdss01_cor_fac$sex, y = fbdss01_cor_fac$fit_ss_wake_count)) +
  geom_violin(trim= F, fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbdss01/violin/6.fit_ss_wake_count(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbdss01_cor_fac, aes(x= fbdss01_cor_fac$sex, y = fbdss01_cor_fac$fit_ss_avg_hr_wake)) +
  geom_violin(trim= F, fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbdss01/violin/7.fit_ss_avg_hr_wake(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbdss01_cor_fac, aes(x= fbdss01_cor_fac$sex, y = fbdss01_cor_fac$fit_ss_avg_hr_light)) +
  geom_violin(trim= F, fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbdss01/violin/8.fit_ss_avg_hr_light(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbdss01_cor_fac, aes(x= fbdss01_cor_fac$sex, y = fbdss01_cor_fac$fit_ss_avg_hr_deep)) +
  geom_violin(trim= F, fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbdss01/violin/9.fit_ss_avg_hr_deep(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbdss01_cor_fac, aes(x= fbdss01_cor_fac$sex, y = fbdss01_cor_fac$fit_ss_avg_hr_rem)) +
  geom_violin(trim= F, fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbdss01/violin/10.fit_ss_avg_hr_rem(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

#########################################################################################################################
#Study3. Daily physical activity summaries(fbdpas01.txt)
#hist
ggplot(data=fbwpas01_cor, aes(fbwpas01_cor$fit_ss_wk_avg_day_min)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/hist/1.fit_ss_wk_avg_day_min(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(data=fbwpas01_cor, aes(fbwpas01_cor$fit_ss_wk_avg_sleep_min)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/hist/2.fit_ss_wk_avg_sleep_min(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

  ggplot(data=fbwpas01_cor, aes(fbwpas01_cor$fit_ss_wk_total_steps)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/hist/3.fit_ss_wk_total_steps(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(data=fbwpas01_cor, aes(fbwpas01_cor$fit_ss_wk_sedentary_min)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/hist/4.fit_ss_wk_sedentary_min(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(data=fbwpas01_cor, aes(fbwpas01_cor$fit_ss_wk_light_active_min)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/hist/5.fit_ss_wk_light_active_min(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(data=fbwpas01_cor, aes(fbwpas01_cor$fit_ss_wk_farily_active_min)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/hist/6.fit_ss_wk_farily_active_min(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(data=fbwpas01_cor, aes(fbwpas01_cor$fit_ss_wk_very_active_min)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/hist/7.fit_ss_wk_very_active_min(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(data=fbwpas01_cor, aes(fbwpas01_cor$fit_ss_wk_avg_steps)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/hist/8.fit_ss_wk_avg_steps(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(data=fbwpas01_cor, aes(fbwpas01_cor$fit_ss_wk_average_met_value)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/hist/9.fit_ss_wk_average_met_value(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(data=fbwpas01_cor, aes(fbwpas01_cor$fit_ss_wk_avg_sedentary_min)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/hist/10.fit_ss_wk_avg_sedentary_min(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(data=fbwpas01_cor, aes(fbwpas01_cor$fit_ss_wk_avg_light_active_min)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/hist/11.fit_ss_wk_avg_light_active_min(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(data=fbwpas01_cor, aes(fbwpas01_cor$fit_ss_wk_avg_farily_at_min)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/hist/12.fit_ss_wk_avg_farily_at_min(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(data=fbwpas01_cor, aes(fbwpas01_cor$fit_ss_wk_avg_very_active_min)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/hist/13.fit_ss_wk_avg_very_active_min(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(data=fbwpas01_cor, aes(fbwpas01_cor$fit_ss_fitbit_rest_hr)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/hist/14.fit_ss_fitbit_rest_hr(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))
#########################################################################################################################
#Study3. (fbwpas01)
#boxplot
ggplot(fbwpas01_cor, aes(x = fbwpas01_cor$fit_ss_wk_avg_day_min)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/box/1.fit_ss_wk_avg_day_min(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwpas01_cor, aes(x = fbwpas01_cor$fit_ss_wk_avg_sleep_min)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/box/2.fit_ss_wk_avg_sleep_min(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwpas01_cor, aes(x = fbwpas01_cor$fit_ss_wk_total_steps)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/box/3.fit_ss_wk_total_steps(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwpas01_cor, aes(x = fbwpas01_cor$fit_ss_wk_sedentary_min)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/box/4.fit_ss_wk_sedentary_min(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwpas01_cor, aes(x = fbwpas01_cor$fit_ss_wk_light_active_min)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/box/5.fit_ss_wk_light_active_min(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwpas01_cor, aes(x = fbwpas01_cor$fit_ss_wk_farily_active_min)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/box/6.fit_ss_wk_farily_active_min(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwpas01_cor, aes(x = fbwpas01_cor$fit_ss_wk_very_active_min)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/box/7.fit_ss_wk_very_active_min(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwpas01_cor, aes(x = fbwpas01_cor$fit_ss_wk_avg_steps)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/box/8.fit_ss_wk_avg_steps(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwpas01_cor, aes(x = fbwpas01_cor$fit_ss_wk_average_met_value)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/box/9.fit_ss_wk_average_met_value(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwpas01_cor, aes(x = fbwpas01_cor$fit_ss_wk_avg_sedentary_min)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/box/10.fit_ss_wk_avg_sedentary_min(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwpas01_cor, aes(x = fbwpas01_cor$fit_ss_wk_avg_light_active_min)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/box/11.fit_ss_wk_avg_light_active_min(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwpas01_cor, aes(x = fbwpas01_cor$fit_ss_wk_avg_farily_at_min)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/box/12.fit_ss_wk_avg_farily_at_min(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwpas01_cor, aes(x = fbwpas01_cor$fit_ss_wk_avg_very_active_min)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/box/13.fit_ss_wk_avg_very_active_min(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwpas01_cor, aes(x = fbwpas01_cor$fit_ss_fitbit_rest_hr)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/box/14.fit_ss_fitbit_rest_hr(box).jpg",
       width=10.4, height=7.8, units=c("cm"))


#########################################################################################################################
#Study3. (fbwpas01)
fbwpas01_cor_fac <- cbind(fbwpas01_cor, fbwpas01$sex)
colnames(fbwpas01_cor_fac)[15] <- "sex"
#violin plot
ggplot(fbwpas01_cor_fac, aes(x= fbwpas01_cor_fac$sex, y = fbwpas01_cor_fac$fit_ss_wk_avg_day_min)) +
  geom_violin(fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/violin/1.fit_ss_sleepperiod_minutes(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwpas01_cor_fac, aes(x= fbwpas01_cor_fac$sex, y = fbwpas01_cor_fac$fit_ss_wk_avg_sleep_min)) +
  geom_violin(trim= F, fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/violin/2.fit_ss_wk_avg_sleep_min(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwpas01_cor_fac, aes(x= fbwpas01_cor_fac$sex, y = fbwpas01_cor_fac$fit_ss_wk_avg_sleep_min)) +
  geom_violin(trim= F, fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/violin/3.fit_ss_wk_avg_sleep_min(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwpas01_cor_fac, aes(x= fbwpas01_cor_fac$sex, y = fbwpas01_cor_fac$fit_ss_wk_sedentary_min)) +
  geom_violin(trim= F, fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/violin/4.fit_ss_wk_sedentary_min(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwpas01_cor_fac, aes(x= fbwpas01_cor_fac$sex, y = fbwpas01_cor_fac$fit_ss_wk_light_active_min)) +
  geom_violin(trim= F, fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/violin/5.fit_ss_wk_light_active_min(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwpas01_cor_fac, aes(x= fbwpas01_cor_fac$sex, y = fbwpas01_cor_fac$fit_ss_wk_farily_active_min)) +
  geom_violin(trim= F, fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/violin/6.fit_ss_wk_farily_active_min(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwpas01_cor_fac, aes(x= fbwpas01_cor_fac$sex, y = fbwpas01_cor_fac$fit_ss_wk_very_active_min)) +
  geom_violin(trim= F, fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/violin/7.fit_ss_wk_very_active_min(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwpas01_cor_fac, aes(x= fbwpas01_cor_fac$sex, y = fbwpas01_cor_fac$fit_ss_wk_avg_steps)) +
  geom_violin(trim= F, fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/violin/8.fit_ss_wk_avg_steps(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwpas01_cor_fac, aes(x= fbwpas01_cor_fac$sex, y = fbwpas01_cor_fac$fit_ss_wk_average_met_value)) +
  geom_violin(trim= F, fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/violin/9.fit_ss_wk_average_met_value(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwpas01_cor_fac, aes(x= fbwpas01_cor_fac$sex, y = fbwpas01_cor_fac$fit_ss_wk_avg_sedentary_min)) +
  geom_violin(trim= F, fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/violin/10.fit_ss_wk_avg_sedentary_min(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwpas01_cor_fac, aes(x= fbwpas01_cor_fac$sex, y = fbwpas01_cor_fac$fit_ss_wk_avg_light_active_min)) +
  geom_violin(trim= F, fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/violin/11.fit_ss_wk_avg_light_active_min(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwpas01_cor_fac, aes(x= fbwpas01_cor_fac$sex, y = fbwpas01_cor_fac$fit_ss_wk_avg_farily_at_min)) +
  geom_violin(trim= F, fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/violin/12.fit_ss_wk_avg_farily_at_min(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwpas01_cor_fac, aes(x= fbwpas01_cor_fac$sex, y = fbwpas01_cor_fac$fit_ss_wk_avg_very_active_min)) +
  geom_violin(trim= F, fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/violin/13.fit_ss_wk_avg_very_active_min(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwpas01_cor_fac, aes(x= fbwpas01_cor_fac$sex, y = fbwpas01_cor_fac$fit_ss_fitbit_rest_hr)) +
  geom_violin(trim= F, fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwpas01/violin/14.fit_ss_fitbit_rest_hr(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))
#######################################################################################################################
#Study4 (fbwss01)
#hist
ggplot(data=fbwss01_cor, aes(fbwss01_cor$fit_ss_sleep_period_minutes)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/hist/1.fit_ss_sleep_period_minutes(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(data=fbwss01_cor, aes(fbwss01_cor$fit_ss_sleep_sum_wake_minutes)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/hist/2.fit_ss_sleep_sum_wake_minutes(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(data=fbwss01_cor, aes(fbwss01_cor$fit_ss_sleep_sum_light_minutes)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/hist/3.fit_ss_sleep_sum_light_minutes(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(data=fbwss01_cor, aes(fbwss01_cor$fit_ss_sleep_sum_deep_minutes)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/hist/4.fit_ss_sleep_sum_deep_minutes(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(data=fbwss01_cor, aes(fbwss01_cor$fit_ss_sleep_sum_rem_minutes)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/hist/5.fit_ss_sleep_sum_rem_minutes(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(data=fbwss01_cor, aes(fbwss01_cor$fit_ss_sleep_sum_wake_count)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/hist/6.fit_ss_sleep_sum_wake_count(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(data=fbwss01_cor, aes(fbwss01_cor$fit_ss_sleep_avg_wake_minutes)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/hist/7.fit_ss_sleep_avg_wake_minutes(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(data=fbwss01_cor, aes(fbwss01_cor$fit_ss_sleep_avg_light_minutes)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/hist/8.fit_ss_sleep_avg_light_minutes(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(data=fbwss01_cor, aes(fbwss01_cor$fit_ss_sleep_avg_deep_minutes)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/hist/9.fit_ss_sleep_avg_deep_minutes(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(data=fbwss01_cor, aes(fbwss01_cor$fit_ss_sleep_avg_rem_minutes)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/hist/10.fit_ss_sleep_avg_rem_minutes(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(data=fbwss01_cor, aes(fbwss01_cor$fit_ss_sleep_avg_wake_count)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/hist/11.fit_ss_sleep_avg_wake_count(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(data=fbwss01_cor, aes(fbwss01_cor$fit_ss_sleep_avg_hr_wake)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/hist/12.fit_ss_sleep_avg_hr_wake(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(data=fbwss01_cor, aes(fbwss01_cor$fit_ss_sleep_avg_hr_light)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/hist/13.fit_ss_sleep_avg_hr_light(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(data=fbwss01_cor, aes(fbwss01_cor$fit_ss_sleep_avg_hr_deep)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/hist/14.fit_ss_sleep_avg_hr_deep(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(data=fbwss01_cor, aes(fbwss01_cor$fit_ss_sleep_avg_hr_rem)) + 
  geom_histogram(stat="bin", bins = 40, fill='#EDB9BB', col = "black") +
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/hist/15.fit_ss_sleep_avg_hr_rem(hist).jpg",
       width=10.4, height=7.8, units=c("cm"))
#######################################################################################################################
#Study4 (fbwss01)
#boxplot
ggplot(fbwss01_cor, aes(x = fbwss01_cor$fit_ss_sleep_period_minutes)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/box/1.fit_ss_sleep_period_minutes(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwss01_cor, aes(x = fbwss01_cor$fit_ss_sleep_sum_wake_minutes)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/box/2.fit_ss_sleep_sum_wake_minutes(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwss01_cor, aes(x = fbwss01_cor$fit_ss_sleep_sum_light_minutes)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/box/3.fit_ss_sleep_sum_light_minutes(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwss01_cor, aes(x = fbwss01_cor$fit_ss_sleep_sum_deep_minutes)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/box/4.fit_ss_sleep_sum_deep_minutes(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwss01_cor, aes(x = fbwss01_cor$fit_ss_sleep_sum_rem_minutes)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/box/5.fit_ss_sleep_sum_rem_minutes(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwss01_cor, aes(x = fbwss01_cor$fit_ss_sleep_sum_wake_count)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/box/6.fit_ss_sleep_sum_wake_count(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwss01_cor, aes(x = fbwss01_cor$fit_ss_sleep_avg_wake_minutes)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/box/7.fit_ss_sleep_avg_wake_minutes(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwss01_cor, aes(x = fbwss01_cor$fit_ss_sleep_avg_light_minutes)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/box/8.fit_ss_sleep_avg_light_minutes(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwss01_cor, aes(x = fbwss01_cor$fit_ss_sleep_avg_deep_minutes)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/box/9.fit_ss_sleep_avg_deep_minutes(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwss01_cor, aes(x = fbwss01_cor$fit_ss_sleep_avg_rem_minutes)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/box/10.fit_ss_sleep_avg_rem_minutes(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwss01_cor, aes(x = fbwss01_cor$fit_ss_sleep_avg_wake_count)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/box/11.fit_ss_sleep_avg_wake_count(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwss01_cor, aes(x = fbwss01_cor$fit_ss_sleep_avg_hr_wake)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/box/12.fit_ss_sleep_avg_hr_wake(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwss01_cor, aes(x = fbwss01_cor$fit_ss_sleep_avg_hr_light)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/box/13.fit_ss_sleep_avg_hr_light(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwss01_cor, aes(x = fbwss01_cor$fit_ss_sleep_avg_hr_deep)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/box/14.fit_ss_sleep_avg_hr_deep(box).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwss01_cor, aes(x = fbwss01_cor$fit_ss_sleep_avg_hr_rem)) +
  geom_boxplot(fill = "#8BBEE8FF", alpha=0.3)+
  scale_fill_brewer(palette = "Dark2")+
  coord_flip()+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/box/15.fit_ss_sleep_avg_hr_rem(box).jpg",
       width=10.4, height=7.8, units=c("cm"))
#######################################################################################################################
#Study4 (fbwss01)
fbwss01_cor_fac <- cbind(fbwss01_cor, fbwss01$sex)
colnames(fbwss01_cor_fac)[16] <- "sex"
#violin plot
ggplot(fbwss01_cor_fac, aes(x= fbwss01_cor_fac$sex, y = fbwss01_cor_fac$fit_ss_sleep_period_minutes)) +
  geom_violin(fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/violin/1.fit_ss_sleep_period_minutes(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwss01_cor_fac, aes(x= fbwss01_cor_fac$sex, y = fbwss01_cor_fac$fit_ss_sleep_sum_wake_minutes)) +
  geom_violin(fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/violin/2.fit_ss_sleep_sum_wake_minutes(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwss01_cor_fac, aes(x= fbwss01_cor_fac$sex, y = fbwss01_cor_fac$fit_ss_sleep_sum_light_minutes)) +
  geom_violin(fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/violin/3.fit_ss_sleep_sum_light_minutes(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwss01_cor_fac, aes(x= fbwss01_cor_fac$sex, y = fbwss01_cor_fac$fit_ss_sleep_sum_deep_minutes)) +
  geom_violin(fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/violin/4.fit_ss_sleep_sum_deep_minutes(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwss01_cor_fac, aes(x= fbwss01_cor_fac$sex, y = fbwss01_cor_fac$fit_ss_sleep_sum_rem_minutes)) +
  geom_violin(fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/violin/5.fit_ss_sleep_sum_rem_minutes(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwss01_cor_fac, aes(x= fbwss01_cor_fac$sex, y = fbwss01_cor_fac$fit_ss_sleep_sum_wake_count)) +
  geom_violin(fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/violin/6.fit_ss_sleep_sum_wake_count(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwss01_cor_fac, aes(x= fbwss01_cor_fac$sex, y = fbwss01_cor_fac$fit_ss_sleep_avg_wake_minutes)) +
  geom_violin(fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/violin/7.fit_ss_sleep_avg_wake_minutes(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwss01_cor_fac, aes(x= fbwss01_cor_fac$sex, y = fbwss01_cor_fac$fit_ss_sleep_avg_light_minutes)) +
  geom_violin(fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/violin/8.fit_ss_sleep_avg_light_minutes(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwss01_cor_fac, aes(x= fbwss01_cor_fac$sex, y = fbwss01_cor_fac$fit_ss_sleep_avg_deep_minutes)) +
  geom_violin(fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/violin/9.fit_ss_sleep_avg_deep_minutes(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwss01_cor_fac, aes(x= fbwss01_cor_fac$sex, y = fbwss01_cor_fac$fit_ss_sleep_avg_rem_minutes)) +
  geom_violin(fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/violin/10.fit_ss_sleep_avg_rem_minutes(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwss01_cor_fac, aes(x= fbwss01_cor_fac$sex, y = fbwss01_cor_fac$fit_ss_sleep_avg_wake_count)) +
  geom_violin(fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/violin/11.fit_ss_sleep_avg_wake_count(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwss01_cor_fac, aes(x= fbwss01_cor_fac$sex, y = fbwss01_cor_fac$fit_ss_sleep_avg_hr_wake)) +
  geom_violin(fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/violin/12.fit_ss_sleep_avg_hr_wake(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwss01_cor_fac, aes(x= fbwss01_cor_fac$sex, y = fbwss01_cor_fac$fit_ss_sleep_avg_hr_light)) +
  geom_violin(fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/violin/13.fit_ss_sleep_avg_hr_light(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwss01_cor_fac, aes(x= fbwss01_cor_fac$sex, y = fbwss01_cor_fac$fit_ss_sleep_avg_hr_deep)) +
  geom_violin(fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/violin/14.fit_ss_sleep_avg_hr_deep(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))

ggplot(fbwss01_cor_fac, aes(x= fbwss01_cor_fac$sex, y = fbwss01_cor_fac$fit_ss_sleep_avg_hr_rem)) +
  geom_violin(fill = "#A8D5BAFF")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic() +
  theme_bw()
ggsave(file="/disk4/bilje/ABCD/fbwss01/violin/15.fit_ss_sleep_avg_hr_rem(violin).jpg",
       width=10.4, height=7.8, units=c("cm"))