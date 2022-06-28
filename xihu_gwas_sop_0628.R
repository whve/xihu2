# ____________________----
# 😊  hi,it is nice tips----
## Heading 1----
### Heading 2 ----
print("hi")
# ____________________----



# use mac?----
## upload----
# rsync -avzP ~/Desktop/0628_GWAS表型及产科信息4946.xls wangzhe@172.16.55.11:/home/wangzhe/projects/xihu2_0628/data
rsync -avzP ~/Desktop/V1_phe华科队列巨大儿_低出生体重.xls wangzhe@172.16.55.11:/home/wangzhe/projects/xihu2_0628/data
## download----
# re_linear_00.assoc.linear.gz
# rsync -avzP wangzhe@172.16.55.11:/home/wangzhe/projects/xihu2gwas_github/re_linear_00.assoc.linear.gz ./
# download summary data summary
rsync -avzP wangzhe@172.16.55.11:/home/wangzhe/projects/xihu2_0628/output/re_logistic_pca10_age_4342_low.assoc_e-2.logistic.gz /Users/vw/Projects/Github_projects/gwas_xihu/
rsync -avzP wangzhe@172.16.55.11:/home/wangzhe/projects/xihu2_0628/output/re_logistic_pca10_age_4342_huge.assoc_e-2.logistic /Users/vw/Projects/Github_projects/gwas_xihu/


library(tidyverse)
library(readxl) 
ph_gwas <- read_excel("/home/wangzhe/projects/xihu2_0628/data/0628_GWAS表型及产科信息4946.xls",sheet = 1) #4946
ph_gwas1 <- read_excel("/home/wangzhe/projects/xihu2_0628/data/V1_phe华科队列巨大儿_低出生体重.xls",sheet = 1) #4897
# 取ID交集----
##semi-join
## 体会ABC不同-A----
> data <- ph_gwas %>% 
  +   semi_join(ph_gwas1)
Joining, by = c("ID", "lowbirthwt", "macrosomia")
## 体会ABC不同-B----
data1 <- ph_gwas %>% 
  semi_join(ph_gwas1,c("ID" = "ID")) # 4895
## 体会ABC不同-C----
data2 <- ph_gwas1 %>% 
  semi_join(ph_gwas,by = c("ID" = "ID"))

# 这里我们用B的结果-data1
famid <- read_delim("/home/wangzhe/projects/gwas/SNP_data/selected_two_cohorts.fam",col_names = F) # 4945

data3 <- data1 %>% 
  semi_join(famid,c("id" = "X1")) # 这样省一步去掉str—3 # 4342

# 看看变量，方便使用----
colnames(data2) # id:number
# [1] "ID"         "lowbirthwt" "macrosomia"
# make phe file----
phe_huge <- data3 %>% 
  mutate(id1 = id) %>%
  drop_na(lowbirthwt) %>% 
  mutate(lowbirthwt1 = if_else(lowbirthwt == 0,
                            true = 2,
                            false = 1)) %>% 
  select(id, id1, lowbirthwt1) # 4342

phe_sma <- data3 %>% 
  mutate(id1 = id) %>%
  drop_na(macrosomia) %>% 
  mutate(macrosomia1 = if_else(macrosomia == 0,
                               true = 2,
                               false = 1)) %>% 
  select(id, id1, macrosomia1) # 4342

# save data----
getwd()
write_delim(phe_huge, "data/phe_huge.txt",col_names = F)# 
write_delim(phe_sma, "data/phe_sma.txt",col_names = F)# 

# cov make----
pca1 <- read.table("/home/wangzhe/projects/gwas/SNP_data/gcta_pca/pca10.eigenvec") #4945
# plus age| id:age |id:pca
# 取id交集,plus age----
# inner_join VS semi_join
x = select(data1,id,age)
cov_pca_age <- pca1 %>% 
  inner_join(x,by=c("V1" = "id")) 


write_delim(cov_pca_age, "data/pca10_age_4342.txt",col_names = F)


# ____________________----
# 😊  tips ----
## 🪝任务在一个rstudio内会出问题，要学会建立单独的运行空间----
### Heading 2 ----



# ____________________----



# ____________________----
# 😊  gcta ----
## Heading 1----
### Heading 2 ----
# gcta64 ‐‐bfile xxx ‐‐autosome ‐‐make‐grm ‐‐out xxx/pca_grm
# gcta64 ‐‐grm xxx/pca_grm ‐‐pca 10 ‐‐out xxx/pca10
# gcta64

#step 1----
#gcta64 --bfile /home/wangzhe/projects/gwas/SNP_data/selected_two_cohorts --autosome --make-grm --out pca_grm
#step 2----
#gcta64 --grm pca_grm --pca 10 --out pca10


# ____________________----