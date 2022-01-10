#############################################################
########## DATA ANALYSIS AND HIERARCHICAL CLUSTERING ########
############ OF A RELOCATED POPULATION ######################
#############################################################

# Cleaning environment 

rm(list=ls(all=TRUE)) #clue environment 

# Libraries

library("readxl")
library("dplyr")
library("xlsx")

# Setting folder

getwd()
setwd("C:/Users/rassi/OneDrive/Documentos/Documentos Doutorado/IIASA_YSSP/DATA_RELOCATION")


# Loading original data 

relocation20102020 <- read_excel("original_db.xlsx", sheet = 1)

# Subsetting data from 2010 - 2019

relocation20102019 <- relocation20102020 %>% filter(Year<2020)




# Household Head Information
# Subsetting household head data 

HHHrelocation20102019 <- relocation20102019 %>% filter(SIF==1|SIF==7|SIF==131)

# Selecting needed variables
# Age Info

age_HH <- HHHrelocation20102019 %>% group_by(ID) %>% filter(SIF==1|SIF==7|SIF==131) %>% select(Age)

# Checking for duplicated IDs 

age_HH[duplicated(age_HH$ID),]

# Sex Info
sex_HH <- HHHrelocation20102019 %>% group_by(ID) %>% filter(SIF==1|SIF==7|SIF==131) %>% select(Sex)
# Checking proportion by sex
prop.table(table(sex_HH$Sex))
# Checking for duplicated IDs 
sex_HH[duplicated(sex_HH$ID),]

# Occupational Status Info
OS_HH <- relocation20102019 %>% group_by(ID) %>% filter(SIF==1|SIF==7|SIF==131) %>% select(OccSit)
# Employment Status
ES_HH <- relocation20102019 %>% group_by(ID) %>% filter(SIF==1|SIF==7|SIF==131) %>% select(Llink)
# Education Info
Educ_HH <- relocation20102019 %>% group_by(ID) %>% filter(SIF==1|SIF==7|SIF==131) %>% select(Educ)

# Merging Household Head variables
HH_data1 <- merge(age_HH,sex_HH)
HH_data2 <- merge(OS_HH,ES_HH)
HH_data3 <- merge(HH_data1,Educ_HH)
HHdataset <- merge(HH_data2,HH_data3)

# Conditional Cash Transfer Programs Info

cash_transfer_Program <- relocation20102019 %>%
  select(ID:SOI) %>%
  mutate(
    welfare_program = case_when(
      SOI == 4 ~ "1",
      SOI != 4 ~ "0"
    )
  )

HasCTP <- cash_transfer_Program %>% group_by(ID) %>%  summarize(HasCTP = max(welfare_program))
HasCTP[duplicated(HasCTP$ID),]

# Extended Family info

family_arrangement <- relocation20102019 %>%
  select(ID:SIF) %>%
  mutate(
    faminterg = case_when(
      SIF == 5 ~ "1",
      SIF == 10 ~ "1",
      SIF == 11 ~ "1",
      SIF == 12 ~ "1",
      SIF == 14 ~ "1",
      SIF == 15 ~ "1",
      TRUE ~ "0"
    )
  )

famInt <- family_arrangement %>% group_by(ID) %>%  summarize(ExtFam = max(faminterg))


# Number of Household Members
# Regional, Year of Relocation and ID as grouping key
summary_household <-  relocation20102019 %>%
  group_by(Year,ID,Regional) %>%
  summarize(members = n())       #651 households

# Presence of children
# Number of children
n_children <- relocation20102019 %>% group_by(ID, .drop = FALSE) %>% filter(Age<=15)  %>% summarise(count_children = n()) 
n_children[duplicated(n_children$ID),]

# Presence of partner
n_partner <- relocation20102019 %>% group_by(ID, .drop = FALSE) %>% 
  filter(SIF==2)  %>% summarise(count_partner = n()) 

# Presence of elder
n_elder <- relocation20102019 %>% group_by(ID, .drop = FALSE) %>% 
  filter(Age>=65)  %>% summarise(count_elder = n())


# Merging children, partner and elder
MergedData <- merge(n_children, n_partner) %>%
  merge(n_elder)

# Turning children, partner, elder into Categorical Variable
MergedData <- MergedData %>%
  mutate(
    children = case_when(
      count_children > 0 ~ "1",
      TRUE ~ "0"
    ),
    partner = case_when(
      count_partner > 0 ~ "1",
      TRUE ~ "0"
    ),
    elder = case_when(
      count_elder > 0 ~ "1",
      TRUE ~ "0")
  )

# Reducing subset
data_dem <- MergedData[-c(3,4)]

# Income per capita
agrup_household <- relocation20102019 %>%
  group_by(ID, Regional) %>%
  summarise(members = n(), soma = sum(T_inc_def, na.rm = TRUE)) %>% mutate(rpc = soma/members)

agrup_household[duplicated(agrup_household$ID),]

# Merging other datasets
dom_var1 <- merge(HasCTP,famInt)
dom_var2 <- merge(data_dem,agrup_household)
dom_var3 <- merge(dom_var1,dom_var2)
DOMdataset <- merge(dom_var3, summary_household)

# Final dataset
final_dataset <- merge(HHdataset,DOMdataset)
str(final_dataset)

# Checking missing variables for the entire database
missmap(final_dataset, main = "Missing values vs observed")

#### Using ~Case When~ to adequate variables based on categories

# OccSit 11 levels -> 9 levels
prop.table(table(final_dataset$OccSit))
final_dataset <- final_dataset %>%
  mutate(
    occ_status = case_when(
      OccSit == 1 ~ "emp",
      OccSit == 2 ~ "unemp",
      OccSit == 3 ~ "retiree",
      OccSit == 4 ~ "employer",
      OccSit == 5 ~ "aut",
      OccSit == 6 ~ "student",
      OccSit == 7 ~ "infw",
      OccSit == 8 ~ "hw",
      OccSit == 0 ~ "notans",
      OccSit == 9 ~ "notans",
      OccSit == NA ~ "notans",
      
    )
  )
prop.table(table(final_dataset$occ_status))

#Llink   9 levels -> 5 levels
prop.table(table(final_dataset$Llink))
final_dataset <- final_dataset %>%
  mutate(
    labor_l = case_when(
      Llink == 1 ~ "owner",
      Llink == 2 ~ "formal",
      Llink == 3 ~ "formal",
      Llink == 4 ~ "formal",
      Llink == 5 ~ "informal",
      Llink == 6 ~ "noans",
      Llink == 7 ~ "noans",
      Llink == 0 ~ "noans",
      Llink == NA ~ "noans",
      
    )
  )
prop.table(table(final_dataset$labor_l))

#Sex     5 levels -> 2 levels
prop.table(table(final_dataset$Sex))
final_dataset <- final_dataset %>%
  mutate(
    newsex = case_when(
      Sex == 1 ~ "M",
      Sex == 2 ~ "F",
      Sex == "M" ~ "M",
      Sex == "F" ~ "F",
      Sex == NA ~ "N",
    )
  )

prop.table(table(final_dataset$newsex))

#Educ   11 levels -> 5 levels
prop.table(table(final_dataset$Educ))
final_dataset <- final_dataset %>%
  mutate(
    neweduc = case_when(
      Educ == 1 ~ "NFE",
      Educ == 2 ~ "EF",
      Educ == 3 ~ "EF",
      Educ == 4 ~ "EM",
      Educ == 5 ~ "ES",
      Educ == 6 ~ "notansw",
      Educ == 8 ~ "notansw",
      Educ == 9 ~ "notansw",
      Educ == 11 ~ "notansw",
      Educ == 0 ~ "notansw",
      Educ == NA ~ "notansw",
      
    )
  )

prop.table(table(final_dataset$neweduc))

#Regional 11 levels ->9 levels
prop.table(table(final_dataset$Regional))
final_dataset <- final_dataset %>%
  mutate(
    newreg = case_when(
      Regional == "CS" ~ "Centro-Sul",
      Regional == "Centro-Sul" ~ "Centro-Sul",
      Regional == "Barreiro" ~ "Barreiro",
      Regional == "Leste" ~ "Leste",
      Regional == "Nordeste" ~ "Nordeste",
      Regional == "Noroeste" ~ "Noroeste",
      Regional == "Norte" ~ "Norte",
      Regional == "Norte" ~ "Norte",
      Regional == "Oeste" ~ "Oeste",
      Regional == "Pampulha" ~ "Pampulha",
      Regional == "Venda Nova" ~ "Venda Nova",
      Regional == NA ~ "didnotans",
    )
  )
prop.table(table(final_dataset$newreg))


#Renaming levels of cat vars
prop.table(table(final_dataset$HasCTP))
final_dataset <- final_dataset %>%
  mutate(
    ctp = case_when(
      HasCTP == 0 ~ "no",
      HasCTP == 1 ~ "yes",
    )
  )

prop.table(table(final_dataset$famInt))
final_dataset <- final_dataset %>%
  mutate(
    intfam = case_when(
      famInt == 0 ~ "no",
      famInt == 1 ~ "yes",
    )
  )

prop.table(table(final_dataset$have_children))
final_dataset <- final_dataset %>%
  mutate(
    children = case_when(
      famInt == 0 ~ "dnhc",
      famInt == 1 ~ "dhc",
    )
  )

prop.table(table(final_dataset$lives_with_partner))
final_dataset <- final_dataset %>%
  mutate(
    partner = case_when(
      famInt == 0 ~ "dnhp",
      famInt == 1 ~ "dhp",
    )
  )


# Final dataset for Hierarchical Cluster Analysis
data_hc <- final_dataset[,c(-2,-3,-5,-6,-7)]
write.xlsx(as.data.frame(data_hc), file = "data_hc.xlsx", col.names = TRUE)


#####################################
###### Hierarchical Clustering ######
#####################################

# Loading dataframe

hc <- read_excel("data_hc.xlsx", sheet = 1)
str(hc)

# Renaming levels of categoric variables
# Analysing proportion of each category

prop.table(table(hc$occ_status))
hc <- hc %>%
  mutate(
    ctp = case_when(
      HasCTP == 0 ~ "doesnotHasCTP",
      HasCTP == 1 ~ "HasCTP",
    )
  )


prop.table(table(hc$HasCTP))
hc <- hc %>%
  mutate(
    ctp = case_when(
      HasCTP == 0 ~ "doesnotHasCTP",
      HasCTP == 1 ~ "HasCTP",
    )
  )

prop.table(table(hc$ExtFam))
hc <- hc %>%
  mutate(
    extfam = case_when(
      ExtFam == 0 ~ "doesnotHasExtFam",
      ExtFam == 1 ~ "HasExtFam",
    )
  )

prop.table(table(hc$children))
hc <- hc %>%
  mutate(
    newchildren = case_when(
      children == 0 ~ "NoChildren",
      children == 1 ~ "WithChildren",
    )
  )

prop.table(table(hc$partner))
hc <- hc %>%
  mutate(
    newpartner = case_when(
      partner == 0 ~ "NoPartner",
      partner == 1 ~ "WithPartner",
    )
  )
prop.table(table(hc$elder))
hc <- hc %>%
  mutate(
    newelder = case_when(
      elder == 0 ~ "NoElder",
      elder == 1 ~ "WithElder",
    )
  )
prop.table(table(hc$labor_l))

# Subsetting database
hc_upd <- hc[,c(-4,-5,-7,-8,-9)]
hc_ca <- hc_upd[,c(-1,-7)]

# Converting chr variables in factor variables
hc_ca[sapply(hc_ca, is.character)] <- lapply(hc_ca[sapply(hc_ca, is.character)], as.factor)
str(hc_ca)

# Defining NAs and omitting it
hc_ca[hc_ca=="NA"] <- NA
hc_upd[hc_upd=="NA"] <- NA
nona_hc_ca <- na.omit(hc_ca)
str(nona_hc_ca)
nona_hc_upd <- na.omit(hc_upd)                              
str(nona_hc_upd)


#N for each variable
str(nona_hc_ca)
prop.table(table(hc$newsex))
prop.table(table(hc$neweduc))
prop.table(table(hc$occ_status))
prop.table(table(hc$newchildren))
prop.table(table(hc$newelder))
prop.table(table(hc$newpartner))
prop.table(table(hc$ctp))
prop.table(table(hc$extfam))
   

# Step 1: 
#----- Dissimilarity Matrix -----#
library(cluster) 
# to perform different types of hierarchical clustering
# package functions used: daisy(), diana(), clusplot()
gower.dist <- daisy(nona_hc_ca, metric = c("gower"))
# class(gower.dist) 
## dissimilarity , dist


# Step 2: Hierarchical Clustering
# DIVISIVE Approach (TOP-BOTTOM)
divisive.clust <- diana(as.matrix(gower.dist), 
                        diss = TRUE, keep.diss = TRUE)
plot(divisive.clust, main = "Divisive")


# AGGLOMERATIVE Approach (BOTTOM-TOP)
aggl.clust.c <- hclust(gower.dist, method = "complete")
plot(aggl.clust.c,
     main = "Agglomerative, complete linkages")

# Step 3: Decide between different clustering algorithms and a different number of clusters
#install.packages("fpc")
library(fpc)
cstats.table <- function(dist, tree, k) {
  clust.assess <- c("cluster.number","n","within.cluster.ss","average.within","average.between",
                    "wb.ratio","dunn2","avg.silwidth")
  clust.size <- c("cluster.size")
  stats.names <- c()
  row.clust <- c()
  output.stats <- matrix(ncol = k, nrow = length(clust.assess))
  cluster.sizes <- matrix(ncol = k, nrow = k)
  for(i in c(1:k)){
    row.clust[i] <- paste("Cluster-", i, " size")
  }
  for(i in c(2:k)){
    stats.names[i] <- paste("Test", i-1)
    
    for(j in seq_along(clust.assess)){
      output.stats[j, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.assess])[j]
      
    }
    
    for(d in 1:k) {
      cluster.sizes[d, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.size])[d]
      dim(cluster.sizes[d, i]) <- c(length(cluster.sizes[i]), 1)
      cluster.sizes[d, i]
      
    }
  }
  output.stats.df <- data.frame(output.stats)
  cluster.sizes <- data.frame(cluster.sizes)
  cluster.sizes[is.na(cluster.sizes)] <- 0
  rows.all <- c(clust.assess, row.clust)
  # rownames(output.stats.df) <- clust.assess
  output <- rbind(output.stats.df, cluster.sizes)[ ,-1]
  colnames(output) <- stats.names[2:k]
  rownames(output) <- rows.all
  is.num <- sapply(output, is.numeric)
  output[is.num] <- lapply(output[is.num], round, 2)
  output
}
# I am capping the maximum amout of clusters by 8
#Divisive
stats.df.divisive <- cstats.table(gower.dist, divisive.clust, 8)
stats.df.divisive
#this can be biased due to the size of clusters
# Agglomerative
stats.df.aggl <-cstats.table(gower.dist, aggl.clust.c, 8) 
stats.df.aggl
#complete linkages looks like the most balanced approach


#Choosing the number of clusters ---------#
  # Using "Elbow" and "Silhouette" methods to identify the best number of clusters
  # to better picture the trend, I will go for more than 7 clusters.
library(ggplot2)
# Elbow
# Divisive clustering
ggplot(data = data.frame(t(cstats.table(gower.dist, divisive.clust, 15))), 
       aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point()+
  geom_line()+
  ggtitle("Divisive clustering") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5)) # 8 clusters


# Agglomerative clustering,provides a more ambiguous picture
ggplot(data = data.frame(t(cstats.table(gower.dist, aggl.clust.c, 15))), 
       aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point()+
  geom_line()+
  ggtitle("Agglomerative clustering") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5)) # 8 clusters


# Silhouette
# Divisive clustering
ggplot(data = data.frame(t(cstats.table(gower.dist, divisive.clust, 15))), 
       aes(x=cluster.number, y=avg.silwidth)) + 
  geom_point()+
  geom_line()+
  ggtitle("Divisive clustering") +
  labs(x = "Num.of clusters", y = "Average silhouette width") +
  theme(plot.title = element_text(hjust = 0.5)) # 5 clusters


# Agglomerative clustering
ggplot(data = data.frame(t(cstats.table(gower.dist, aggl.clust.c, 15))), 
       aes(x=cluster.number, y=avg.silwidth)) + 
  geom_point()+
  geom_line()+
  ggtitle("Agglomerative clustering") +
  labs(x = "Num.of clusters", y = "Average silhouette width") +
  theme(plot.title = element_text(hjust = 0.5)) # 11 clusters

# Step 4: Clustering and Visualization
# a) see how observations are clustered, 
# b) know is how observations are distributed across categories
# For this I do:
# a) a colored dendrogram, 
# b) a heatmap of observations count per variable within each cluster.


library("ggplot2")
library("reshape2")
library("purrr")
library("dplyr")
# let's start with a dendrogram
library("dendextend")
dendro <- as.dendrogram(aggl.clust.c)
dendro.col <- dendro %>%
  set("branches_k_color", k = 8, value =   c("orange","red","cyan3", "darkslategray", "darkslategray4", "darkslategray3", "gold3", "darkcyan")) %>%
  set("branches_lwd", 0.6) %>%
  set("labels_colors", 
      value = c("darkslategray")) %>% 
  set("labels_cex", 0.5)
ggd1 <- as.ggdend(dendro.col)
ggplot(ggd1, theme = theme_minimal()) +
  labs(x = "Num. observations", y = "Height", title = "Dendrogram, k = 8")


# Radial plot looks less cluttered (and cooler)
ggplot(ggd1, labels = T) + 
  scale_y_reverse(expand = c(0.5, 0)) +
  coord_polar(theta="x")



# Categoric Variables
# Time for the heatmap
# the 1st step here is to have 1 variable per row
# factors have to be converted to characters in order not to be dropped
clust.num <- cutree(aggl.clust.c, k = 8)
hc.cl <- cbind(nona_hc_ca, clust.num)



#HEatmap
hc.cl.id <- cbind(nona_hc_upd, clust.num)

hc.long <- melt(data.frame(lapply(hc.cl.id, as.character), stringsAsFactors=FALSE), 
                  id = c("ID", "clust.num"), factorsAsStrings=T)
hc.long.q <- hc.long %>%
  group_by(clust.num, variable, value) %>%
  mutate(count = n_distinct(ID)) %>%
  distinct(clust.num, variable, value, count)

# heatmap.c will be suitable in case you want to go for absolute counts - but it doesn't tell much to my taste
heatmap.c <- ggplot(hc.long.q, aes(x = clust.num, y =factor(value, levels = c("WithChildren","WithPartner","WithElder",
                                                                              "F","M","NFE","ElemSchool","HighSchool","Undergrad","notansw",
                                                                              "HasCTP","HasExtFam","formal", "informal","noans","owner",
                                                                              "employed", "employer", "informalworker", "retiree", "self-employed",
                                                                              "stay-at-home","unemployed","notans"), ordered = T))) +
  geom_tile(aes(fill = count))+
  scale_fill_gradient2(low = "darkslategray1", mid = "yellow", high = "turquoise4")

heatmap.c

# calculating the percent of each factor level in the absolute count of cluster members
hc.long.p <- hc.long.q %>%
  group_by(clust.num, variable) %>%
  mutate(perc = count / sum(count)) %>%
  arrange(clust.num)

# ordering levels
# make a factor variable
hc.long.p$value <- as.factor(hc.long.p$value)

# check the current levels
levels(hc.long.p$value)
# reorder the levels
hc.long.p$value <- factor(hc.long.p$value, levels = c("HasCTP","HasExtFam","noans", "informal","formal","owner",
                                                      "notans","employed", "employer", "informalworker", "retiree", "self-employed",
                                                      "stay-at-home","unemployed","F","M",
                                                      "notansw","Undergrad","HighSchool","ElemSchool","NFE",
                                                      "WithChildren","WithPartner","WithElder"))


heatmap.p <- ggplot(hc.long.p, aes(x = clust.num, y = factor(value))) +
  geom_tile(aes(fill = perc), alpha = 0.85)+
  labs(title = "Distribution of characteristics across clusters", x = "Cluster number", y = NULL) +
  geom_hline(yintercept = 2.5) + 
  geom_hline(yintercept = 6.5) + 
  geom_hline(yintercept = 14.5) + 
  geom_hline(yintercept = 16.5) + 
  geom_hline(yintercept = 21.5) + 
  scale_fill_gradient2(low = "blue", mid = "yellow", high = "red")
heatmap.p


####NUMERIC VARIABLES
#Age
ggplot() +
  # Points for each car
  geom_point(data = hc.cl, mapping = aes(y = clust.num, x = Age)) +
  # Vertical bars for the means
  geom_point(data = hc.cl %>% 
               # Group the data by brand then get means
               group_by(clust.num) %>% 
               summarise(median_age = median(Age)), 
             # Specify aesthetics
             mapping = aes(y = clust.num, x = median_age), 
             size = 10, color = 'red', shape = '|') +
             labs(y="Clusters")

#Members
ggplot() +
  # Points for each car
  geom_point(data = hc.cl, mapping = aes(y = clust.num, x = members)) +
  # Vertical bars for the means
  geom_point(data = hc.cl %>% 
               # Group the data by brand then get means
               group_by(clust.num) %>% 
               summarise(mean_members = mean(members)), 
             # Specify aesthetics
             mapping = aes(y = clust.num, x = mean_members), 
             size = 10, color = 'red', shape= '|')+
  labs(y="Clusters")
 
#number of children
ggplot() +
  # Points for each car
  geom_point(data = hc.cl, mapping = aes(y = clust.num, x = count_children)) +
  # Vertical bars for the means
  geom_point(data = hc.cl %>% 
               # Group the data by brand then get means
               group_by(clust.num) %>% 
               summarise(mean_chi = mean(count_children)), 
             # Specify aesthetics
             mapping = aes(y = clust.num, x = mean_chi), 
             size = 10, color = 'red', shape= '|') +
             labs(y="Clusters",x="Number of children")

#Total household income
ggplot() +
  # Points for each car
  geom_point(data = hc.cl, mapping = aes(y = clust.num, x = soma)) +
  # Vertical bars for the means
  geom_point(data = hc.cl %>% 
               # Group the data by brand then get means
               group_by(clust.num) %>% 
               summarise(mean_total_income = mean(soma)), 
             # Specify aesthetics
             mapping = aes(y = clust.num, x = mean_total_income), 
             size = 10, color = 'red', shape= '|')

#Income per capita
ggplot() +
  # Points for each car
  geom_point(data = hc.cl, mapping = aes(y = clust.num, x = rpc)) +
  # Vertical bars for the means
  geom_point(data = hc.cl %>% 
               # Group the data by brand then get means
               group_by(clust.num) %>% 
               summarise(mean_income_capita = mean(rpc)), 
             # Specify aesthetics
             mapping = aes(y = clust.num, x = mean_income_capita), 
             size = 10, color = 'red', shape= '|')

# Sinaplot - rpc
install.packages("ggforce")
library(ggforce)

ggplot(hc.cl, aes(as.factor(clust.num), rpc)) +
  geom_sina(aes(color = as.factor(clust.num)), size = 1)+
  scale_color_manual(values =  c("#00AFBB", "#E7B800", "#FC4E07",
                                 "black","red","blue","yellow","green"))+
  labs(y="Income per Capita",x="Clusters")+
  theme(legend.position="none")


# Sinaplot - soma

ggplot(hc.cl, aes(as.factor(clust.num), soma)) +
  geom_sina(aes(color = as.factor(clust.num)), size = 1)+
  scale_color_manual(values =  c("#00AFBB", "#E7B800", "#FC4E07",
                                 "black","red","blue","yellow","green"))+
  theme(legend.position="none")
#Distribuir clusters num mapa de BH
#hc.cl.reg <- hc.cl[,c(-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-13,-14,-15,-16,-17)]
#write.xlsx(as.data.frame(hc.cl.reg), file = "hc.cl.reg.xlsx", col.names = TRUE)

hc.cl.reg <- read_excel("hc.cl.reg.xlsx", sheet = 1)
str(hc.cl.reg)

library(sf)
library(rgdal)
library(broom)
library(ggplot2)

regionais <- readOGR(dsn = "REGIONAL", layer = "REGIONAL")
plot(regionais)


### TMAP is used to generate maps with color and legend ###
library(tmap)
tmap_mode("plot")
tm_shape(regionais)+
  tm_fill()+
  tm_borders()

#Adding data to map
rem_reg=merge(regionais,hc.cl.reg,by="NOME", all.x=T,duplicateGeoms=TRUE)
names(rem_reg)


rem_reg$clust.num <- as.factor(rem_reg$clust.num)
#Plotting map
tm_shape(rem_reg)+
  #tm_format_Europe2()+
  #tm_style_classic()+
  tm_legend(position=c("left","bottom"))+
  tm_compass()+
  tm_scale_bar()+
  tm_borders(alpha=.5)+
  tm_dots(col = "clust.num", 
          breaks = c(0,1,2,3,4,5,6,7,8,Inf),
          palette = "-Spectral",
          title = "Cluster")
#tm_legend(legend.outside = TRUE)


#Summarizing Cluster Information
cl_summary <- hc.cl %>%
  group_by(clust.num) %>%
  do(cluster_summary = summary(.))
# Cluster 1
cl_summary$cluster_summary[[1]]
# Cluster 2
cl_summary$cluster_summary[[2]]
# Cluster 3
cl_summary$cluster_summary[[3]]
# Cluster 4
cl_summary$cluster_summary[[4]]
# Cluster 5
cl_summary$cluster_summary[[5]]
# Cluster 6
cl_summary$cluster_summary[[6]]
# Cluster 7
cl_summary$cluster_summary[[7]]
# Cluster 8
cl_summary$cluster_summary[[8]]


#### Sources of information for this script
#https://www.sciencedirect.com/topics/computer-science/hierarchical-cluster-analysis
#https://towardsdatascience.com/hierarchical-clustering-on-categorical-data-in-r-a27e578f2995
#https://www.r-graph-gallery.com/340-custom-your-dendrogram-with-dendextend.html
#https://www.sciencedirect.com/topics/computer-science/hierarchical-cluster-analysis


