
######
# GETTING SICK OVER TIME ----
######

setwd('/Users/tylerreny/Dropbox/_nationscape/Democracy Tracker - UCLA team/Tyler/covid/covId_weekly/')

bind_rows(barplot_pid3_sick(df, extra_sick_you ,"You've Gotten Sick", group=T),
          barplot_pid3_sick(df, extra_sick_family ,'Family Sick', group=T),
          barplot_pid3_sick(df, extra_sick_work ,'Coworker Sick', group=T),
          barplot_pid3_sick(df, extra_sick_other ,'Other Sick', group=T)) %>%
  na.omit() %>%
  mutate(Outcome=factor(Outcome, levels=c(
    "You've Gotten Sick",'Family Sick','Coworker Sick','Other Sick'
  ))) %>%
  filter(wave > 35) %>%
  ggplot(aes(wave, y, color=pid3)) +
  geom_line() + 
  facet_wrap(~Outcome,ncol=5) +
  geom_point(size=6, fill='white',shape=21) +
  scale_color_manual(values=c(colors[2],colors[3],colors[1]), name='') +
  #scale_x_discrete(labels=c('Dem','Ind','Rep')) +
  theme_bw() + 
  labs(x='', y='') +
  scale_x_continuous(breaks=c(36,37,38,39), 
                     limits=c(35.5,39.5),  
                     labels=c('March 26','April 1','April 8','April 16')) +
  scale_y_continuous(limits=c(0,0.32), breaks=c(0,0.05,0.10,0.15,0.20,0.25,0.3),
                     labels=c('0%','5%','10%','15%','20%','25%','30%')) +
  geom_text(aes(x=wave, y=y, label=round(y*100,0)), show.legend = F) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  ggsave(width=8,height=4, file='sick.png')


bind_rows(barplot_pid3(df,extra_covid_wash,"Wash Hands", group=T),
          barplot_pid3(df,extra_covid_cancel_travel,"Cancel Travel", group=T),
          barplot_pid3(df,extra_covid_stock_goods,"Stock Goods", group=T),
          barplot_pid3(df,extra_covid_visit_family,"Stop Visiting Family", group=T),
          barplot_pid3(df,extra_covid_quarantine,"Staying Home", group=T)) %>%
  na.omit() %>%
  filter(wave > 35) %>%
  ggplot(aes(wave, y, color=pid3)) +
  geom_line() + 
  facet_wrap(~Outcome,ncol=5) +
  geom_point(size=6, fill='white',shape=21) +
  scale_color_manual(values=c(colors[2],colors[3],colors[1]), name='') +
  #scale_x_discrete(labels=c('Dem','Ind','Rep')) +
  theme_bw() + 
  labs(x='', y='') +
  scale_x_continuous(breaks=c(36,37,38,39), 
                     limits=c(35.5,39.5),  
                     labels=c('March 26','April 1','April 8','April 16')) +
  scale_y_continuous(limits=c(0.6,1), breaks=c(0.6,0.7,0.8,0.9,1),
                     labels=c('60%','70%','80%','90%','100%')) +
  geom_text(aes(x=wave, y=y, label=round(y*100,0)), show.legend=F) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  ggsave(width=8,height=4, file='precautions.png')

barplot_pid3_worry(df, extra_corona_concern,outcome='Concerned' ,group=T) %>%
  filter(wave > 35) %>%
  na.omit() %>%
  ggplot(aes(wave, y, color=pid3)) +
  geom_line() + 
  geom_point(size=6, fill='white',shape=21) +
  scale_color_manual(values=c(colors[2],colors[3],colors[1]), name='') +
  theme_bw() + 
  labs(x='', y='Percent "Very Concerned"\nAbout Coronavirus') +
  scale_x_continuous(breaks=c(36,37,38,39), 
                     limits=c(35.5,39.5),  
                     labels=c('March 26','April 1','April 8','April 16')) +
  scale_y_continuous(breaks=c(0.5,0.6,0.7),
                     labels=c('50%','60%','70%')) +
  geom_text(aes(x=wave, y=y, label=round(y*100,0)), show.legend=F) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  ggsave(width=5,height=4, file='concerned_corona.png')


barplot_pid3_incomechange(df, extra_income_corona ,outcome='Concerned', group=T) %>%
  na.omit() %>%
  filter(wave>35) %>%
  ggplot(aes(wave, y, color=pid3)) +
  geom_line() + 
  geom_point(size=6, fill='white',shape=21) +
  scale_color_manual(values=c(colors[2],colors[3],colors[1]), name='') +
  #scale_x_discrete(labels=c('Dem','Ind','Rep')) +
  theme_bw() + 
  labs(x='', y='Percent Negatively\nImpacted Financially') +
  scale_x_continuous(breaks=c(36,37,38,39), 
                     limits=c(35.5,39.5),  
                     labels=c('March 26','April 1','April 8','April 16')) +
  scale_y_continuous(limits=c(0.25,0.5), breaks=c(0.25,0.3,0.35,0.4,0.45,0.5),
                     labels=c('25%','30%','35%','40%','45%','50%')) +
  geom_text(aes(x=wave, y=y, label=round(y*100,0)),show.legend = F) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  ggsave(width=5,height=4, file='econ_impact_corona.png')

barplot_pid3_trumpapprove(df, extra_trump_corona ,outcome='Concerned', group=T) %>%
  na.omit() %>%
  filter(wave>35) %>%
  ggplot(aes(wave, y, color=pid3)) +
  geom_line() + 
  geom_point(size=6, fill='white',shape=21) +
  scale_color_manual(values=c(colors[2],colors[3],colors[1]), name='') +
  #scale_x_discrete(labels=c('Dem','Ind','Rep')) +
  theme_bw() + 
  labs(x='', y='Percent Approve Trump\nHandling Coronavirus') +
  scale_x_continuous(breaks=c(36,37,38,39), 
                     limits=c(35.5,39.5),  
                     labels=c('March 26','April 1','April 8','April 16')) +
  scale_y_continuous(limits=c(0,1), breaks=c(0,0.25,0.5,0.75,1),
                     labels=c('0%','25%','50%','75%','100%')) +
  geom_text(aes(x=wave, y=y, label=round(y*100,0)), show.legend = F) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  ggsave(width=5,height=4, file='trump_approve_corona.png')


######
# INTERESTING PLOTS ON SELF INTEREST AND IDEOLOGY
######

df$precaution_scale <- df$extra_covid_wash + df$extra_covid_cancel_travel + df$extra_covid_stock_goods + df$extra_covid_visit_family + df$extra_covid_quarantine

prescript <- df %>%
  filter(wave > 35) %>%
  mutate(prescriptions = case_when(
    extra_prescriptions == 1 ~ 'No Prescription\nMeds',
    extra_prescriptions > 1 ~ '1 or More Prescription\nMeds'
  )) %>%
  group_by(prescriptions, pid3, wave) %>%
  summarise(y = weighted.mean(extra_corona_concern %in% c(4), na.rm=T, w= weight),
            y2 = weighted.mean(precaution_scale, na.rm=T, w= weight)) %>%
  na.omit()

prescript%>%
  ggplot(aes(wave, y, color=factor(prescriptions),
             shape=factor(prescriptions))) +
  geom_line() + 
  geom_point(size=6, fill='white',shape=21) +
  geom_text(aes(x=wave, y=y, label=round(y*100,0)), show.legend = F) +
  facet_wrap(~pid3) +
  scale_color_brewer(palette='Dark2',name='',labels=rev(c("No Prescription\nMeds","1 or More Prescription\nMeds"))) +
  theme_bw() +
  labs(x='',y='Percent "Very Concerned"\nAbout Coronavirus') +
  scale_x_continuous(breaks=c(36,37,38,39), 
                     limits=c(35.5,39.5),  
                     labels=c('March 26','April 1','April 8','April 16')) +
  scale_y_continuous(breaks=c(0.5,0.6,0.7),
                     labels=c('50%','60%','70%')) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  ggsave(width=8,height=4, file='personal_impact.png')
prescript%>%
  ggplot(aes(wave, y2, color=factor(prescriptions),
             shape=factor(prescriptions))) +
  geom_line() + 
  geom_point(size=6, fill='white',shape=21) +
  geom_text(aes(x=wave, y=y2, label=round(y2,1)), show.legend = F, size=2.5) +
  facet_wrap(~pid3) +
  scale_color_brewer(palette='Dark2',name='',labels=rev(c("No Prescription\nMeds","1 or More Prescription\nMeds"))) +
  theme_bw() +
  labs(x='',y='Average Precautions Taken (0-5)') +
  scale_x_continuous(breaks=c(36,37,38,39), 
                     limits=c(35.5,39.5),  
                     labels=c('March 26','April 1','April 8','April 16')) +
  scale_y_continuous(limits=c(3.5,4.5), breaks=seq(3.5,4.5,by=0.25),
                     labels=paste(seq(3.5,4.5,by=0.25))) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  ggsave(width=8,height=4, file='personal_impact_count_precautions.png')


df$sick_scale_bi <- ifelse(df$extra_sick_you == 1 | df$extra_sick_family == 1 | df$extra_sick_work == 1 | df$extra_sick_other == 1, 1,0)
sick <- df %>%
  filter(wave > 35) %>%
  group_by(sick_scale_bi, pid3, wave) %>%
  summarise(y = weighted.mean(extra_corona_concern==4, na.rm=T),
            y2 = weighted.mean(precaution_scale, na.rm=T, w= weight)) %>%
  na.omit() 
sick %>%
  ggplot(aes(wave, y, color=factor(sick_scale_bi),
             shape=factor(sick_scale_bi))) +
  geom_line() + 
  geom_point(size=6, fill='white',shape=21) +
  geom_text(aes(x=wave, y=y, label=round(y*100,0)), show.legend = F) +
  facet_wrap(~pid3) +
  scale_color_brewer(palette='Dark2',name='',labels=c("No Exposure\nCOVID-19","Exposure\nCOVID-19")) +
  theme_bw() +
  labs(x='',y='Percent "Very Concerned"\nAbout Coronavirus') +
  scale_x_continuous(breaks=c(36,37,38,39), 
                     limits=c(35.5,39.5),  
                     labels=c('March 26','April 1','April 8','April 16')) +
  scale_y_continuous(breaks=c(.5,.6,.7,.8),
                     labels=c('50%','60%','70%','80%')) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  ggsave(width=8,height=4, file='personal_impact_sick.png')

sick %>%
  ggplot(aes(wave, y2, color=factor(sick_scale_bi),
             shape=factor(sick_scale_bi))) +
  geom_line() + 
  geom_point(size=6, fill='white',shape=21) +
  geom_text(aes(x=wave, y=y2, label=round(y2,1)), show.legend = F, size=2.5) +
  facet_wrap(~pid3) +
  scale_color_brewer(palette='Dark2',name='',labels=c("No Exposure\nCOVID-19","Exposure\nCOVID-19")) +
  theme_bw() +
  labs(x='',y='Average Precautions Taken (0-5)') +
  scale_x_continuous(breaks=c(36,37,38,39), 
                     limits=c(35.5,39.5),  
                     labels=c('March 26','April 1','April 8','April 16')) +
  scale_y_continuous(limits=c(3.5,4.5), breaks=seq(3.5,4.5,by=0.25),
                     labels=paste(seq(3.5,4.5,by=0.25))) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  ggsave(width=8,height=4, file='personal_impact_sick_count_precautions.png')

income <- df %>%
  filter(wave > 35) %>%
  mutate(income_cats = case_when(
    household_income <=4 ~ 'Lower Tercile',
    household_income > 4 & household_income <= 12  ~ 'Middle Tercile',
    household_income > 12 ~ 'Upper Tercile'
  ), income_cats = factor(income_cats,levels=c(
    'Lower Tercile','Middle Tercile', 'Upper Tercile'
  ))) %>%
  group_by(income_cats, pid3, wave) %>%
  summarise(y = weighted.mean(extra_corona_concern %in% c(4), na.rm=T, w= weight),
            y2 = weighted.mean(precaution_scale, na.rm=T, w= weight)) %>%
  na.omit() 
income %>%
  ggplot(aes(wave, y, color=factor(income_cats),
             shape=factor(income_cats))) +
  geom_line() + 
  geom_point(size=6, fill='white',shape=21) +
  geom_text(aes(x=wave, y=y, label=round(y*100,0)), show.legend = F) +
  facet_wrap(~pid3) +
  scale_color_brewer(palette='Dark2',name='Household\nIncome') +
  theme_bw() +
  labs(x='',y='Percent "Very Concerned"\nAbout Coronavirus') +
  scale_x_continuous(breaks=c(36,37,38,39), 
                     limits=c(35.5,39.5),  
                     labels=c('March 26','April 1','April 8','April 16')) +
  scale_y_continuous( breaks=c(0.5,0.6,0.7),
                      labels=c('50%','60%','70%')) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  ggsave(width=8,height=4, file='personal_impact_income.png')


income %>%
  ggplot(aes(wave, y2, color=factor(income_cats),
             shape=factor(income_cats))) +
  geom_line() + 
  geom_point(size=6, fill='white',shape=21) +
  geom_text(aes(x=wave, y=y2, label=round(y2,1)), show.legend = F, size=2.5) +
  facet_wrap(~pid3) +
  scale_color_brewer(palette='Dark2',name='Household\nIncome') +
  theme_bw() +
  labs(x='',y='Average Precautions Taken (0-5)') +
  scale_x_continuous(breaks=c(36,37,38,39), 
                     limits=c(35.5,39.5),  
                     labels=c('March 26','April 1','April 8','April 16')) +
  scale_y_continuous(limits=c(3.4,4.5), breaks=seq(3.5,4.5,by=0.25),
                     labels=paste(seq(3.5,4.5,by=0.25))) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  ggsave(width=8,height=4, file='personal_impact_income_count_precautions.png')


regions <- df %>%
  filter(wave > 35) %>%
  mutate(regions = case_when(
    census_region == 1 ~ 'Northeast',
    census_region == 2 ~ 'Midwest',
    census_region == 3 ~ 'South',
    census_region == 4 ~ 'West'
  )) %>%
  group_by(regions, wave) %>%
  summarise(y = weighted.mean(extra_corona_concern %in% c(4), na.rm=T, w= weight),
            y2 = weighted.mean(precaution_scale, na.rm=T, w= weight)) %>%
  na.omit()

regions %>%
  ggplot(aes(wave, y, color=factor(regions),
             shape=factor(regions))) +
  geom_line() + 
  geom_point(size=6, fill='white',shape=21) +
  geom_text(aes(x=wave, y=y, label=round(y*100,0)), show.legend = F) +
  #facet_wrap(~pid3) +
  scale_color_brewer(palette='Dark2',name='Census Region') +
  theme_bw() +
  labs(x='',y='Percent "Very Concerned"\nAbout Coronavirus') +
  scale_x_continuous(breaks=c(36,37,38,39), 
                     limits=c(35.5,39.5),  
                     labels=c('March 26','April 1','April 8','April 16')) +
  scale_y_continuous(limits=c(0.5,0.7),breaks=c(0.5,0.6,0.7),
                     labels=c('50%','60%','70%')) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  ggsave(width=5,height=4, file='regions_worry.png')

regions %>%
  ggplot(aes(wave, y2, color=factor(regions),
             shape=factor(regions))) +
  geom_line() + 
  geom_point(size=6, fill='white',shape=21) +
  geom_text(aes(x=wave, y=y2, label=round(y2,1)), show.legend = F, size=2.5) +
  #facet_wrap(~pid3) +
  scale_color_brewer(palette='Dark2',name='Census Region') +
  theme_bw() +
  labs(x='',y='Average Precautions Taken (0-5)') +
  scale_x_continuous(breaks=c(36,37,38,39), 
                     limits=c(35.5,39.5),  
                     labels=c('March 26','April 1','April 8','April 16')) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  ggsave(width=5,height=4, file='regions_precautions.png')
