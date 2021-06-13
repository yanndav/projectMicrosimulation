##########################
#                        #
# MICROSIMULATION PART 2 #
#                        #
#       Yann David       #
#                        #
##########################



# 00. LOADING DATASET AND LIBRARIES ---------------------------------------
# Loading libraries
library("haven")
library("tidyverse")
if(!("matrixStats"%in%installed.packages()[,'Package'])){
  install.packages('matrixStats')
}

library("matrixStats")

# Setting working directory as the folder containing this script
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) 

# Opening dataset
data <-  read_dta("mensen_pia.dta")


# PART 1 - Construct dataset ----------------------------------------------
# Viewing column names 
View(colnames(data))

# Creating list of columns to select
reg_col = unique(grep(paste(c('^JA\\d{1,2}_\\d{1,2}','^I\\d{1,2}_\\d{1,2}'),collapse = "|"), colnames(data),value=T))
full_col =c('milieu','a01','poids3_a','rev_tot','Deptot','taille','id_men')
to_select = c(reg_col,full_col)

# Selecting columns
data_clean <-  data[,to_select]%>% 
  # Data cleaning
  
  mutate_at(vars(JA14_4, I9_7,I9_8),~ifelse(.>30000,./100,.)) %>% 
  mutate_at(vars(JA14_4, I9_7,I9_8),~ifelse(.>3000,./10,.)) %>% 
  mutate_at(vars(I9_6),~ifelse(.>10000,./100,.)) %>% 
  mutate_at(vars(I9_6),~ifelse(.>1000,./10,.)) %>% 
  mutate_at(vars(JA12_4),~ifelse(.>50000,./100,.)) %>% 
  mutate_at(vars(JA12_4),~ifelse(.>5000,./10,.)) %>% 
  mutate(I9_6=case_when(I10_6==1~I9_6*1000,T~I9_6),
         I9_7=case_when(I10_7==1~I9_7*1000,T~I9_7),
         I9_8=case_when(I10_8==1 ~ I9_8*1000,T~I9_8)) 



# PART 2 - Descriptive Statistics -----------------------------------------
# Modifying dataset to include needed variables
data_stats = data_clean %>% 
  mutate(expenditure_pc = Deptot/taille,
         total_area = select(.,JA11_1:JA11_27) %>% rowSums(na.rm = T),
         rice_share = JA11_4*100/total_area) %>% 
  select(expenditure_pc,taille,total_area,rice_share,poids3_a,id_men)
  
stats_desc <- c('taille','total_area','rice_share','expenditure_pc')

# Creating the table of desc stats
stats = data.frame(t(sapply(stats_desc, function(name){
  mean = weighted.mean(data_stats[[name]],data_stats$poids3_a, na.rm = T)
  median = matrixStats::weightedMedian(data_stats[[name]],data_stats$poids3_a, na.rm = T)
  min = min(data_stats[[name]],na.rm = T)
  max = max(data_stats[[name]],na.rm = T)
  return(c(mean,median,min,max))
}))) %>% 
  rownames_to_column()
names(stats)= c("Variable","Mean","Median",'Min','Max')
stats <- stats %>% 
  mutate(Variable = str_replace(Variable,'_',' ')) %>% 
  mutate_if(is.numeric,round,digits=2)

write.table(stats,
            file = "output/part2_2.tex", eol='\\\\',
            sep='&',quote = F,row.names = F,
            col.names = F,na="")



# PART 3 - Rice consumption -----------------------------------------------
  
  data_rice = data_clean %>% 
  filter(!(I10_6%in%c(3,4)),!(I10_7%in%c(3,4)),!(I10_8%in%c(3,4))) %>% 
    select(JA14_4,I9_6:I9_8,I12_6:I12_8,a01,taille,id_men,milieu) %>% 
    pivot_longer(cols = c('JA14_4',"I9_6","I9_7","I9_8","I12_6","I12_7","I12_8"),
                 names_to="rice_type",
                 values_to="quantity") %>% 
    mutate(unit = ifelse(str_detect(rice_type,"I9_.*|JA14_4"),"Quantity","Frequency"),
         rice_type = str_remove(rice_type,'I\\d+_')) %>% 
  pivot_wider(id_cols = c('a01','taille','rice_type','id_men','milieu'),
              names_from=unit,
              values_from=quantity) %>% 
  mutate(Frequency = ifelse(Quantity==0,0,Frequency),
         total_year = ifelse(rice_type!='JA14_4',Frequency*Quantity,Quantity)) %>% 
  mutate(kg_pc = total_year/taille) %>%
  replace_na(list(kg_pc=0,total_year=0)) %>% 
  group_by(rice_type,milieu) %>% 
  summarise(total_year = weighted.mean(total_year,taille,na.rm=T),
            kg_pc = weighted.mean(kg_pc,taille,na.rm=T)) %>%
  mutate(rice_type = case_when(rice_type=="6"~"Imported broken rice (I9_6)",
                               rice_type=="7"~"Imported whole rice (I9_7)",
                               rice_type=="8"~"Local rice (I9_8)",
                               rice_type=="JA14_4"~"Own production (JA14_4)",
                               T~"error"))

ggplot(data_rice)+
  geom_col(aes(x=milieu,y=kg_pc))+
  facet_wrap(rice_type~.,scales="free",nrow=1)+
  labs(x="Region",
       y="Quantity per capita (in kg)")+
  scale_x_continuous(breaks = seq(1,15))+
  theme_bw()

ggsave(filename='output/part2_3.png',
       last_plot(),
       width = 15,
       height = 8,
       units = "cm")


# PART 4 - Rice distribution ----------------------------------------------


data_rice_concentration = data_clean %>% 
  select(JA14_4,I9_6:I9_8,a01,taille,I11_6:I11_8) %>% 
  pivot_longer(cols = c('JA14_4',"I9_6","I9_7","I9_8"),
               names_to="rice_type",
               values_to="kg_per_hh") %>% 
  mutate(kg_pc = kg_per_hh/taille)
  
