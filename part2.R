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
full_col =c('milieu','a01','poids3_a','rev_tot','Deptot','taille')
to_select = c(reg_col,full_col)
# Selecting columns
data_clean <-  data[,to_select]


# PART 2 - Descriptive Statistics -----------------------------------------
# Modifying dataset to include needed variables
data_stats = data_clean %>% 
  mutate(expenditure_pc = Deptot/taille,
         total_area = select(.,JA11_1:JA11_27) %>% rowSums(na.rm = T),
         rice_share = JA11_4*100/total_area) %>% 
  select(expenditure_pc,taille,total_area,rice_share,poids3_a)
  
# Creating the table of desc stats
stats = data.frame(t(sapply(colnames(data_stats), function(name){
  mean = weighted.mean(data_stats[[name]],data_stats$poids3_a, na.rm = T)
  median = matrixStats::weightedMedian(data_stats[[name]],data_stats$poids3_a, na.rm = T)
  min = min(data_stats[[name]],na.rm = T)
  max = max(data_stats[[name]],na.rm = T)
  return(c(mean,median,min,max))
})))
names(stats)= c("Mean","Median",'Min','Max')


write.table(stats,
            file = "output/part2_2.tex", eol='\\\\',
            sep='&',quote = F,row.names = T,
            col.names = T,na="")
