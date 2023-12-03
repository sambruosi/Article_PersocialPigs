#### Libraries ####

library(VIM) # NA visualisation
library(psych) # test assumptions for PCA (Bartlett's test and KMO)
library(corrplot) # correlations
library(FactoMineR) # PCA
library(factoextra) # extras for PCA visualisation (eigevalues, individuals, variables)
library(multiway) # Tucker's congruence

library(irr) # ICC calculation

library(igraph) # Create networks and calculate metrics
library(dplyr) # dataframe manipulation

library(lme4) # GLMMs
library(car) # check multicolinearity

#### PCA ####

##### Data importation ######

# Importation
personality <- read.table("data_personality_persocial_pigs.csv", sep=",", header=TRUE)
# Set factor variables as factor
personality[1:6] <- lapply(personality[1:6], as.factor) 
# Split into 2 datasets for each test period
personality_rep1 <- personality[personality$Test_Nr %in% 1,]
personality_rep2 <- personality[personality$Test_Nr %in% 2,]
personality_rep1 <- personality_rep1[-5] # remove column with the test period
personality_rep2 <- personality_rep2[-5] # remove column with the test period

##### Data visualisation #####
# Week 2
hist(personality_rep1$OFT_voc_freq)
hist(personality_rep1$OFT_expl_floor_wall_dur)
hist(personality_rep1$NOT_expl_obj_dur)
hist(personality_rep1$HAT_exploration_human_dur)
hist(personality_rep1$NPT_walking_by_fence_dur)
hist(personality_rep1$NPT_NP_nose.nose_freq)
hist(personality_rep1$NPT_sudden_display_freq) # mostly zeros > exclude from PCA 
# Week 4
hist(personality_rep2$OFT_voc_freq)
hist(personality_rep2$OFT_expl_floor_wall_dur)
hist(personality_rep2$NOT_expl_obj_dur)
hist(personality_rep2$HAT_exploration_human_dur)
hist(personality_rep2$NPT_walking_by_fence_dur)
hist(personality_rep2$NPT_NP_nose.nose_freq)
hist(personality_rep2$NPT_sudden_display_freq) # mostly zeros > exclude from PCA 

##### PCA 1 - week 2 #####

###### Handling NA values ######
# Highlight the NAs in red
matrixplot(personality_rep1, sortby = 2, xlab="personality_rep1") # sort by Batch
# Remove lines with NA because they cannot be reliably imputed
personality_rep1 <- na.omit(personality_rep1) 

###### Correlation matrix ######
# Exclude factor variables and sudden display
Spearman_correlations_personality_rep1 <- cor(personality_rep1[-c(1:5,10)], method="spearman")
# plot of the correlations
corrplot(Spearman_correlations_personality_rep1, sig.level = 0.05)
# table with the correlation coefficients
Correlation_matrix_personality_rep1 <- data.frame(Spearman_correlations_personality_rep1)

###### Sampling adequacy ######
# Bartlett's sphericity test with H0: all null correlations
cortest.bartlett(Correlation_matrix_personality_rep1, n = 64)
# Kaiser-Meyer- Olkin (KMO) measure of sampling adequacy
KMO(Correlation_matrix_personality_rep1) 

###### Number of principal components (PC) ######
Horn_parallel_personality_rep1 <- fa.parallel(Correlation_matrix_personality_rep1, n.obs=64 , fa = "pc", cor = "spearman")

###### Eigenvalues ######
res.pca <- FactoMineR::PCA(personality_rep1,  graph=FALSE, quali.sup =c(1:5), quanti.sup = 10, scale.unit=TRUE)
get_eig(res.pca)

###### Variables visualisation ######
fviz_pca_var(res.pca, geom = c("text", "arrow"),
             col.var = "cos2", 
             axes=1:2, 
             select.var = list(name =NULL, cos2 = 0.2, contrib = NULL),
             title ="PCA - Variables 1:2", 
             repel=TRUE) + theme_light() 

###### PCA Coordinates ######
# Loadings of the behaviours
Dim_var_personality_rep1 = res.pca[["var"]][["coord"]]
# Pigs scores on the dimensions
Dim_ind_personality_rep1 = merge(x=personality_rep1[3], y=res.pca[["ind"]][["coord"]], by=0)
Dim_ind_personality_rep1 <- Dim_ind_personality_rep1[2:4] # keep only the first two dimensions
colnames(Dim_ind_personality_rep1) <- c("PVC_Nr","PCA1Dim1","PCA1Dim2")

##### PCA 2 - week 4 #####

###### Handling NA values ######
# Highlight the NAs in red
matrixplot(personality_rep2, sortby = 2, xlab="personality_rep2") # sort by Batch
# Remove lines with NA
personality_rep2 <- na.omit(personality_rep2) 

###### Correlation matrix ######
Spearman_correlations_personality_rep2 <- cor(personality_rep2[-c(1:5,10)], method="spearman")
corrplot(Spearman_correlations_personality_rep2, sig.level = 0.05)
Correlation_matrix_personality_rep2 <- data.frame(Spearman_correlations_personality_rep2)

###### Sampling adequacy ######
cortest.bartlett(Correlation_matrix_personality_rep2, n = 66)
KMO(Correlation_matrix_personality_rep2) 

###### Number of principal components (PC) ######
Horn_parallel_personality_rep2 <- fa.parallel(Correlation_matrix_personality_rep2, n.obs=66 , fa = "pc", cor = "spearman")

###### Eigenvalues ######
res.pca <- FactoMineR::PCA(personality_rep2,  graph=FALSE, quali.sup =c(1:5), quanti.sup = 10, scale.unit = TRUE)
get_eig(res.pca)

###### Variables visualisation ######
fviz_pca_var(res.pca, geom = c("text", "arrow"),
             col.var = "cos2", 
             axes=1:2, 
             select.var = list(name =NULL, cos2 = 0.2, contrib = NULL),
             title ="PCA - Variables 1:2", 
             repel=TRUE) + theme_light() 

###### PCA Coordinates ######
Dim_var_personality_rep2 = res.pca[["var"]][["coord"]]
Dim_ind_personality_rep2 = merge(x=personality_rep2[3], y=res.pca[["ind"]][["coord"]], by=0)
Dim_ind_personality_rep2 <- Dim_ind_personality_rep2[2:4]
colnames(Dim_ind_personality_rep2) <- c("PVC_Nr","PCA2Dim1","PCA2Dim2")

#### Repeatability assessment ####

##### Repeatability PCA #####

###### Congruence PCA dimensions ######
# Dim1 between week 2 and 4
congru(Dim_var_personality_rep2[,1], Dim_var_personality_rep1[,1])
# Dim2 between week 2 and 4
congru(Dim_var_personality_rep2[,2], Dim_var_personality_rep1[,2])
# Dim1 between week 2 and 4 without nose-nose
congru(Dim_var_personality_rep2[c(1:5),2], Dim_var_personality_rep1[c(1:5),2])

###### Repeatability pigs' individual scores ######

Pig_scores_PC <- merge(x = Dim_ind_personality_rep1, y = Dim_ind_personality_rep2, by = "PVC_Nr")
Pig_scores_PC <- na.omit(Pig_scores_PC)

# ICC first principal component
pc1_for_icc <- cbind(Pig_scores_PC$PCA1Dim1,Pig_scores_PC$PCA2Dim1)
irr::icc(pc1_for_icc, model = "twoway", 
         type = "consistency", 
         unit = "single", 
         conf.level = 0.95)

# ICC second principal component
pc2_for_icc <- cbind(Pig_scores_PC$PCA1Dim2,Pig_scores_PC$PCA2Dim2)
irr::icc(pc2_for_icc, model = "twoway", 
         type = "consistency", 
         unit = "single", 
         conf.level = 0.95)

##### Repeatability individual behaviours #####

# Merge data test 1 and 2 
compareTest_1_2 <- merge(x=personality_rep1, y=personality_rep2,by=c("PVC_Nr","Subject","Batch","Sow"),all=FALSE)
compareTest_1_2 <- na.omit(compareTest_1_2)

###### Repeatability OFT vocalisations ######

# ICC OFT vocalisations
voc_for_icc <- cbind(compareTest_1_2$OFT_voc_freq.x,compareTest_1_2$OFT_voc_freq.y)
irr::icc(voc_for_icc, model = "twoway", 
         type = "consistency", 
         unit = "single", 
         conf.level = 0.95)

###### Repeatability OFT exploration ######

# ICC OFT exploration novel arena
OFT_explo_for_icc <- cbind(compareTest_1_2$OFT_expl_floor_wall_dur.x,compareTest_1_2$OFT_expl_floor_wall_dur.y)
irr::icc(OFT_explo_for_icc, model = "twoway", 
         type = "consistency", 
         unit = "single", 
         conf.level = 0.95)

###### Repeatability NOT exploration ######

# ICC NOT exploration novel object
NOT_explo_for_icc <- cbind(compareTest_1_2$NOT_expl_obj_dur.x,compareTest_1_2$NOT_expl_obj_dur.y)
irr::icc(NOT_explo_for_icc, model = "twoway", 
         type = "consistency", 
         unit = "single", 
         conf.level = 0.95)

###### Repeatability HAT exploration ######

# ICC HAT exploration novel human
HAT_explo_for_icc <- cbind(compareTest_1_2$HAT_exploration_human_dur.x,compareTest_1_2$HAT_exploration_human_dur.y)
irr::icc(HAT_explo_for_icc, model = "twoway", 
         type = "consistency", 
         unit = "single", 
         conf.level = 0.95)

###### Repeatability NPT walk fence ######

# ICC NPT walking by fence
walk_fence_for_icc <- cbind(compareTest_1_2$NPT_walking_by_fence_dur.x,compareTest_1_2$NPT_walking_by_fence_dur.y)
irr::icc(walk_fence_for_icc, model = "twoway", 
         type = "consistency", 
         unit = "single", 
         conf.level = 0.95)

###### Repeatability NPT nose-nose ######

# ICC 
nose.nose_for_icc <- cbind(compareTest_1_2$NPT_NP_nose.nose_freq.x,compareTest_1_2$NPT_NP_nose.nose_freq.y)
irr::icc(nose.nose_for_icc, model = "twoway", 
         type = "consistency", 
         unit = "single", 
         conf.level = 0.95)

#### SNA ####

##### Data importation #####

SNA <- read.table("data_SNA_persocial_pigs.csv", sep=",", header = TRUE)
SNA[1:7] <- lapply(SNA[1:7], as.factor)
SNA$Behavior <- gsub(" ", ".", SNA$Behavior) # replace whitespaces with a dot

##### Network settings #####

Batches = 5 # Number of groups
batchLabels <- c("1","2","3","4","5")
Nodes = 18 # Number of individuals per group
nodeLabels <- c(1:18)
Layers = 7 # Number of behaviours
layerLabels <- c("Agonistic","Mount","Nose.ano","Nose.body","Nose.front","Play","Social.resting")

##### Network lists per batch #####

G_list <- list() # list for the igraph objects
M_list <- list() # list for the adjacency matrices

for (b in 1:Batches){
  
  G_list_per_batch <- list() # list for the igraph objects for 1 group
  M_list_per_batch <- list() # list for the adjacency matrices for 1 group
  
  for (l in 1:Layers){
    
    G_list_per_batch[[l]] <- graph.data.frame(
      SNA[which(SNA$Batch==batchLabels[b] & SNA$Behavior==layerLabels[l]),c(2,7,8)], # we select only the subject, recipient and frequency of the behaviour
      directed= TRUE,
      vertices=nodeLabels)
    M_list_per_batch[[l]] <- as_adjacency_matrix(G_list_per_batch[[l]], attr = "n")
    # Add weights (nb of occurences) to the igraph objects
    G_list_per_batch[[l]] <- graph_from_adjacency_matrix(M_list_per_batch[[l]],mode = "directed",weighted = TRUE,diag = FALSE)
  }
  
  # Additional non agonistic plot
  G_list_per_batch[[Layers+1]] <- graph.data.frame(
    SNA[which(SNA$Batch==batchLabels[b] & SNA$Behavior!="Aggression"),c(2,7,8)], # we select only the subject, recipient and frequency of the behaviour
    directed= TRUE,
    vertices=nodeLabels)
  M_list_per_batch[[Layers+1]] <- as_adjacency_matrix(G_list_per_batch[[Layers+1]], attr = "n")
  G_list_per_batch[[Layers+1]] <- graph_from_adjacency_matrix(M_list_per_batch[[Layers+1]],mode = "directed",weighted = TRUE,diag = FALSE)
  
  G_list[[b]] <- G_list_per_batch
  M_list[[b]] <- M_list_per_batch
}

##### Network metrics #####

# Area under the curve (AUC) of the cumulative distribution of the percentage weights (Fc)
# function adapted from the instructions in Candeloro et al. (2016)
AUC_cumul_weight<-function(x){
  x %>%
    group_by(Subject) %>%
    arrange(n, .by_group=TRUE)%>%
    mutate(cumulative.weight = cumsum(n))%>%
    mutate(fj = cumsum(n)/last(cumulative.weight))%>%
    mutate(fc = sum(fj)-last(fj))%>%
    mutate(AUC.Fc = fc+0.5) %>%
    select(Subject,AUC.Fc) %>%
    distinct(AUC.Fc, .keep_all = TRUE)
}

metrics_list <- list()

for (b in 1:Batches){
  
  metrics_list_per_batch <- list()
  metrics_per_behaviour <- data.frame(Subject = c(1:Nodes))
  metrics_per_behaviour$Batch <- batchLabels[b]
  
  for (l in 1:(Layers)){ # for each network + the non-agonistic aggregate
    
    # calculate out-degree
    metrics_per_behaviour$out_degree <- degree(G_list[[b]][[l]], mode="out")
    # calculate out-strength (sum of weights)
    metrics_per_behaviour$out_strength <- strength(G_list[[b]][[l]], mode="out")
    # calculate betweenness
    metrics_per_behaviour$betweenness <- betweenness(G_list[[b]][[l]],directed=TRUE,weights = E(G_list[[b]][[l]])$weight)
    # calculate AUC according to the function AUC_cumul_weight()
    AUC <- AUC_cumul_weight(SNA[which(SNA$Batch==batchLabels[b] & SNA$Behavior==layerLabels[l]),c(2,7,8)])
    # merge AUC to metrics dataframe
    complete_metrics_per_behaviour <- merge(metrics_per_behaviour, AUC, by="Subject", all = TRUE)
    # calculate weighted degree centrality 
    complete_metrics_per_behaviour$WDC <- 2*complete_metrics_per_behaviour$AUC.Fc
    # Set NA values as 0
    complete_metrics_per_behaviour[is.na(complete_metrics_per_behaviour)] <- 0
    
    # Add a weighted degree property to the graph
    V(G_list[[b]][[l]])$WeightedDegree <- complete_metrics_per_behaviour$WDC
    
    colnames(complete_metrics_per_behaviour)[3:7] <- paste(layerLabels[[l]],colnames(complete_metrics_per_behaviour)[3:7], sep = "_")
    metrics_list_per_batch[[l]] <- complete_metrics_per_behaviour
  }
  
  # combine the behaviours into one dataframe in columns
  metrics_list_per_batch <- do.call(cbind, lapply(metrics_list_per_batch, as.data.frame))
  # remove duplicate columns of "Subject" and "Batch"
  metrics_list_per_batch <- metrics_list_per_batch[!duplicated(colnames(metrics_list_per_batch))]
  # attribute the dataframe to the relative group
  metrics_list[[b]] <- metrics_list_per_batch
}

# Combine the groups into one dataframe in lines 
metrics_list <- do.call(rbind, lapply(metrics_list, as.data.frame))

# Let's do the same for the aggregate of non-agonistic behaviours

l = Layers+1
non_agonistic_list <- list()

for (b in 1:Batches){
  
  metrics_non_agonistic <- data.frame(Subject = c(1:Nodes))
  metrics_non_agonistic$Batch <- batchLabels[b]
  
  # calculate out-degree
  metrics_non_agonistic$out_degree <- degree(G_list[[b]][[l]], mode="out")
  # calculate out-strength (sum of weights)
  metrics_non_agonistic$out_strength <- strength(G_list[[b]][[l]], mode="out")
  # calculate betweenness
  metrics_non_agonistic$betweenness <- betweenness(G_list[[b]][[l]],directed=TRUE,weights = E(G_list[[b]][[l]])$weight)
  # calculate AUC according to the function AUC_cumul_weight()
  SNA_non_agonistic <- SNA[which(SNA$Batch==batchLabels[b] & SNA$Behavior!="Aggression"),c(2,7,8)]
  SNA_non_agonistic <- SNA_non_agonistic %>%
    group_by(Subject,Recipient) %>%
    summarise(n = sum(n))
  AUC <- AUC_cumul_weight(SNA_non_agonistic)
  # merge AUC to metrics dataframe
  complete_metrics_non_agonistic <- merge(metrics_non_agonistic, AUC, by="Subject", all = TRUE)
  # calculate weighted degree centrality 
  complete_metrics_non_agonistic$WDC <- 2*complete_metrics_non_agonistic$AUC.Fc
  # Set NA values as 0
  complete_metrics_non_agonistic[is.na(complete_metrics_non_agonistic)] <- 0
  
  # Add a weighted degree property to the graph
  V(G_list[[b]][[l]])$WeightedDegree <- complete_metrics_non_agonistic$WDC
  
  colnames(complete_metrics_non_agonistic)[3:7] <- paste("Non.agonistic",colnames(complete_metrics_non_agonistic)[3:7], sep = "_")
  
  # attribute the dataframe to the relative group
  non_agonistic_list[[b]] <- complete_metrics_non_agonistic
}

# Combine the groups into one dataframe in lines 
non_agonistic_list <- do.call(rbind, lapply(non_agonistic_list, as.data.frame))

# Merge with the rest of the behaviours
SNA_metrics <- merge(metrics_list, non_agonistic_list, by = c("Subject", "Batch"))

##### Network plots ######

# update with the non-agonistic aggregate
layerLabels <- c("Agonistic","Mount","Nose.ano","Nose.body","Nose.front","Play","Social.resting","Non.agonistic")
palette = c("red","pink","purple","lightgreen","skyblue","orange","gold","grey80")

# One plot per behaviour across groups
for (l in 1:(Layers+1)){
  par(mfrow = c(1, Batches+1), mai = rep(0, 4), oma= rep(0, 4))
  plot.new()
  title(main = layerLabels[l], line = -11, adj = 0.9, cex.main = 2) # adjust position to your plot screen
  for (b in 1:Batches){
    plot(G_list[[b]][[l]], layout=layout.circle, edge.width = 0.15*E(G_list[[b]][[l]])$weight, 
         vertex.size = V(G_list[[b]][[l]])$WeightedDegree*2,  
         vertex.color = palette[l],edge.arrow.size=0.05)
    title(main = "Group", line = -1.3, adj = 0.45, cex.main = 2) # adjust position to your plot screen
    title(main = batchLabels[b], line = -1.3, adj = 0.65, cex.main = 2) # adjust position to your plot screen
  }
  
}

dev.off()

#### GLMMs ####

##### Data preparation #####

# Merge
GLMM_data <- merge(x=personality[c(1:6,11:13)],y=SNA_metrics,by=c("Batch","Subject"), all = TRUE)
GLMM_data <- na.omit(GLMM_data)
GLMM_data[7:49] <- lapply(GLMM_data[7:49], as.numeric) 
GLMM_data[1:6] <- lapply(GLMM_data[1:6], as.factor) 

##### Data visualisation #####

# visualise count predictor
table(GLMM_data$PVC_Nr,GLMM_data$Batch)
table(GLMM_data$Subject,GLMM_data$Batch)

# visualise continuous predictor
hist(GLMM_data$Agonistic_WDC)
hist(GLMM_data$Agonistic_betweenness)
hist(GLMM_data$Play_WDC)
hist(GLMM_data$Play_betweenness)
hist(GLMM_data$Nose.front_WDC)
hist(GLMM_data$Nose.front_betweenness)
hist(GLMM_data$Non.agonistic_WDC)
hist(GLMM_data$Non.agonistic_betweenness)

# visualise potential link between non-independant predictors
plot(GLMM_data$Agonistic_WDC,GLMM_data$Agonistic_betweenness)
plot(GLMM_data$Play_WDC,GLMM_data$Play_betweenness)
plot(GLMM_data$Nose.front_WDC,GLMM_data$Nose.front_betweenness)
plot(GLMM_data$Non.agonistic_WDC,GLMM_data$Non.agonistic_betweenness)

boxplot(GLMM_data$NPT_NP_nose.nose_freq ~ GLMM_data$Novel_pig)
boxplot(GLMM_data$NPT_NP_nose.nose_freq ~ GLMM_data$Sow)
boxplot(GLMM_data$NPT_NP_nose.nose_freq ~ GLMM_data$Subject)

hist(GLMM_data$NPT_NP_nose.nose_freq, breaks=15)
table(GLMM_data$NPT_NP_nose.nose_freq)
table(GLMM_data$PVC_Nr,GLMM_data$Novel_pig)
table(GLMM_data$Novel_pig,GLMM_data$Subject)
table(GLMM_data$PVC_Nr,GLMM_data$Subject)
table(GLMM_data$Novel_pig,GLMM_data$Nose.front_out_degree)
table(GLMM_data$Novel_pig,GLMM_data$Agonistic_out_degree)
table(GLMM_data$Novel_pig,GLMM_data$Non.agonistic_betweenness)
table(GLMM_data$Novel_pig,GLMM_data$Test_Nr)
table(GLMM_data$Sow,GLMM_data$Nose.front_WDC)
table(GLMM_data$Sow,GLMM_data$Agonistic_WDC)
table(GLMM_data$Sow,GLMM_data$Non.agonistic_betweenness)
table(GLMM_data$Sow,GLMM_data$Test_Nr)
table(GLMM_data$Subject,GLMM_data$Test_Nr)
plot(GLMM_data$NPT_NP_nose.nose_freq,GLMM_data$Nose.front_WDC)
plot(GLMM_data$NPT_NP_nose.nose_freq,GLMM_data$Agonistic_WDC)
plot(GLMM_data$NPT_NP_nose.nose_freq,GLMM_data$Non.agonistic_betweenness)
plot(GLMM_data$Subject,GLMM_data$NPT_NP_nose.nose_freq)

##### Models #####

# create a dummy column for Test_Nr as numeric to be used in the random slopes
GLMM_data$Test_Nr2 = as.numeric(GLMM_data$Test_Nr == levels(GLMM_data$Test_Nr)[2])
# shift value to comparison to mean
GLMM_data$Test_Nr2 = GLMM_data$Test_Nr2 - mean(GLMM_data$Test_Nr2)

###### NPT Nose.nose ######

# increase max iterations for model convergence if necessary or change optimizer
myControl = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))

model.a <- lmerTest::lmer(NPT_NP_nose.nose_freq ~ Nose.front_WDC + Nose.front_betweenness 
                          + Agonistic_WDC + Agonistic_betweenness
                          + Non.agonistic_WDC + Non.agonistic_betweenness   
                          + Test_Nr 
                          + (1 |PVC_Nr) 
                          + (1 + Nose.front_WDC + Nose.front_betweenness 
                             + Agonistic_WDC + Agonistic_betweenness
                             + Non.agonistic_WDC + Non.agonistic_betweenness||Novel_pig)
                          + (1 + Nose.front_WDC + Nose.front_betweenness 
                             + Agonistic_WDC + Agonistic_betweenness
                             + Non.agonistic_WDC + Non.agonistic_betweenness 
                             + Test_Nr2||Sow)
                          + (1 + Nose.front_WDC + Nose.front_betweenness 
                             + Agonistic_WDC + Agonistic_betweenness
                             + Non.agonistic_WDC + Non.agonistic_betweenness 
                             + Test_Nr2||Batch),
                          data = GLMM_data,
                          REML = FALSE,
                          control = myControl)
# Note: the lmer() from lmerTest is to get the p-values in the summary 

model=model.a

# Check multi-colinearity
vif(model) 

# Normal distribution of residuals
car::qqPlot(resid(model))
hist(residuals(model), main = "Histrogram", ylab="Residuals",breaks=15)

# Homogeneity of variance
fitted.values=fitted(model)
residual.values=scale(residuals(model))
plot(residual.values~fitted.values, main="Residuals vs. fitted values")
abline(h = 0, col = "red")

# Normal distribution of random effects
hist(ranef(model)$PVC_Nr[,1]) # good
hist(ranef(model)$Batch[,1]) # don't really know because not many values
hist(ranef(model)$Batch[,2])
hist(ranef(model)$Batch[,3])
hist(ranef(model)$Batch[,4])
hist(ranef(model)$Sow[,1])
hist(ranef(model)$Novel_pig[,1])

summary(model)

boxplot(NPT_NP_nose.nose_freq ~ Batch, data = GLMM_data) # no variation across batches

boxplot(NPT_NP_nose.nose_freq ~ PVC_Nr, data = GLMM_data) # variation between individuals

plot(NPT_NP_nose.nose_freq ~ Non.agonistic_WDC, data = GLMM_data)

plot(NPT_NP_nose.nose_freq ~ Test_Nr, data = GLMM_data)

###### NPT sudden display ######

hist(GLMM_data$NPT_sudden_display_freq)
table(GLMM_data$NPT_sudden_display_freq) # many zeros

# Transform into a binary variable
GLMM_data$NPT_sudden_display_binom <- as.numeric(GLMM_data$NPT_sudden_display_freq != 0)
table(GLMM_data$NPT_sudden_display_binom)

# increase max iterations for model convergence if necessary or change optimizer
myControl = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))

model.b <- glmer(NPT_sudden_display_binom ~ Play_WDC + Play_betweenness
                 + Test_Nr 
                 + (1 |PVC_Nr) 
                 + (1 +  Play_WDC + Play_betweenness||Novel_pig)
                 + (1 +  Play_WDC + Play_betweenness
                    + Test_Nr2||Sow)
                 + (1 +  Play_WDC + Play_betweenness
                    + Test_Nr2||Batch),
                 family = binomial(link = "logit"),
                 data=GLMM_data,
                 control = myControl)

model=model.b
vif(model)
summary(model)

###### NPT walking by fence - Gaussian #######

# Transform duration into proportion of time during the test
GLMM_data$NPT_walking_by_fence_prop <- GLMM_data$NPT_walking_by_fence_dur/270

hist(GLMM_data$NPT_walking_by_fence_dur)
table(GLMM_data$NPT_walking_by_fence_dur,exclude = FALSE)
hist(GLMM_data$NPT_walking_by_fence_prop)
hist(asin(sqrt(GLMM_data$NPT_walking_by_fence_prop)))

GLMM_data$NPT_log <- log(GLMM_data$NPT_walking_by_fence_dur + sqrt(GLMM_data$NPT_walking_by_fence_dur^GLMM_data$NPT_walking_by_fence_dur +1))

# increase max iterations for model convergence if necessary or change optimizer
myControl = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))

model.c <- lmerTest::lmer(asin(sqrt(NPT_walking_by_fence_prop)) ~  Nose.front_WDC + Agonistic_WDC 
                          + Non.agonistic_WDC +Nose.front_betweenness + Agonistic_betweenness 
                          + Non.agonistic_betweenness 
                          + Test_Nr 
                          + (1 |PVC_Nr) 
                          + (1 + Nose.front_WDC + Agonistic_WDC 
                             + Non.agonistic_WDC +Nose.front_betweenness + Agonistic_betweenness 
                             + Non.agonistic_betweenness||Novel_pig)
                          + (1 + Nose.front_WDC + Agonistic_WDC 
                             + Non.agonistic_WDC +Nose.front_betweenness + Agonistic_betweenness 
                             + Non.agonistic_betweenness
                             + Test_Nr2||Sow)
                          + (1 + Nose.front_WDC + Agonistic_WDC 
                             + Non.agonistic_WDC +Nose.front_betweenness + Agonistic_betweenness 
                             + Non.agonistic_betweenness
                             + Test_Nr2||Batch),
                          data=GLMM_data,
                          control = myControl,
                          REML = FALSE)

model=model.c

# Normal distribution of residuals
car::qqPlot(resid(model))
hist(residuals(model), main = "Histrogram", ylab="Residuals",breaks=15)

# Homogeneity of variance
fitted.values=fitted(model)
residual.values=scale(residuals(model))
plot(residual.values~fitted.values, main="Residuals vs. fitted values")
abline(h = 0, col = "red")

# Normal distribution of random effects
hist(ranef(model)$PVC_Nr[,1]) # good
hist(ranef(model)$Batch[,1]) # don't really know because not many values
hist(ranef(model)$Batch[,2])
hist(ranef(model)$Batch[,3])
hist(ranef(model)$Batch[,4])
hist(ranef(model)$Sow[,1])
hist(ranef(model)$Novel_pig[,1])

vif(model)
summary(model)

plot(NPT_walking_by_fence_prop ~ Agonistic_WDC, data = GLMM_data)
plot(NPT_walking_by_fence_prop ~ Non.agonistic_WDC, data = GLMM_data)
plot(NPT_walking_by_fence_prop ~ Test_Nr, data = GLMM_data)

###### NPT walking by fence - Binary logistic regression ######
library(ordinal) # function clmm for ordinal logistic regression
library(emmeans) # get back probability from logit scale for the binary regression
library(RVAideMemoire) # get back probability from logit scale for the full regression

# Binary logistic regression 0 versus >0s
GLMM_data$NPT_binary_log <- 0
GLMM_data$NPT_binary_log[GLMM_data$NPT_walking_by_fence_dur > 0] <- 1
table(GLMM_data$NPT_binary_log)
table(GLMM_data$NPT_binary_log,GLMM_data$Test_Nr)

# increase max iterations for model convergence if necessary or change optimizer
myControl = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))

model.log1 <- glmer(NPT_binary_log ~  Nose.front_WDC + Agonistic_WDC 
                    + Non.agonistic_WDC +Nose.front_betweenness + Agonistic_betweenness 
                    + Non.agonistic_betweenness 
                    + Test_Nr 
                    + (1 |PVC_Nr) 
                    + (1 + Nose.front_WDC + Agonistic_WDC 
                       + Non.agonistic_WDC +Nose.front_betweenness + Agonistic_betweenness 
                       + Non.agonistic_betweenness||Novel_pig)
                    + (1 + Nose.front_WDC + Agonistic_WDC 
                       + Non.agonistic_WDC +Nose.front_betweenness + Agonistic_betweenness 
                       + Non.agonistic_betweenness
                       + Test_Nr2||Sow)
                    + (1 + Nose.front_WDC + Agonistic_WDC 
                       + Non.agonistic_WDC +Nose.front_betweenness + Agonistic_betweenness 
                       + Non.agonistic_betweenness
                       + Test_Nr2||Batch),
                    family = binomial(link = "logit"),
                    control = myControl,
                    data=GLMM_data,
                    nAGQ = 0)

summary(model.log1)

# Binary logistic regression <=50s versu >50s

GLMM_data$NPT_binary_log <- 0
GLMM_data$NPT_binary_log[GLMM_data$NPT_walking_by_fence_dur > 50] <- 1
table(GLMM_data$NPT_binary_log)
table(GLMM_data$NPT_binary_log,GLMM_data$Test_Nr)

# increase max iterations for model convergence if necessary or change optimizer
myControl = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))

model.log2 <- glmer(NPT_binary_log ~  Nose.front_WDC + Agonistic_WDC 
                    + Non.agonistic_WDC +Nose.front_betweenness + Agonistic_betweenness 
                    + Non.agonistic_betweenness 
                    + Test_Nr 
                    + (1 |PVC_Nr) 
                    + (1 + Nose.front_WDC + Agonistic_WDC 
                       + Non.agonistic_WDC +Nose.front_betweenness + Agonistic_betweenness 
                       + Non.agonistic_betweenness||Novel_pig)
                    + (1 + Nose.front_WDC + Agonistic_WDC 
                       + Non.agonistic_WDC +Nose.front_betweenness + Agonistic_betweenness 
                       + Non.agonistic_betweenness
                       + Test_Nr2||Sow)
                    + (1 + Nose.front_WDC + Agonistic_WDC 
                       + Non.agonistic_WDC +Nose.front_betweenness + Agonistic_betweenness 
                       + Non.agonistic_betweenness
                       + Test_Nr2||Batch),
                    family = binomial(link = "logit"),
                    control = myControl,
                    data=GLMM_data,
                    nAGQ = 0)

summary(model.log2)

# Confidence intervals
coef1 <- as.data.frame(coef(summary(model.log1)))[2:8,]
coef2 <- as.data.frame(coef(summary(model.log2)))[2:8,]
coef1$upper <- coef1$Estimate+1.96*coef1$`Std. Error`
coef2$upper <- coef2$Estimate+1.96*coef2$`Std. Error`
coef1$lower <- coef1$Estimate-1.96*coef1$`Std. Error`
coef2$lower <- coef2$Estimate-1.96*coef2$`Std. Error`

coef1$names <- row.names(coef1)
row.names(coef1) <- 1:7
coef2$names <- row.names(coef2)
row.names(coef2) <- 1:7

coef1$coef <- "coef1"
coef2$coef <- "coef2"

coefs <- rbind(coef1,coef2)

# Check that the two models have largely overlapping estimates 
#(which is an assumption before moving to the 3-levels regression)
ggplot(coefs, aes(x = names, y = Estimate, col = coef)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_pointrange(aes(x = names, y = Estimate, ymin = lower, ymax = upper), position = position_dodge(width = 0.5))

# Full binary logistic regression (3 levels: 0, <=50s, >50s)
GLMM_data$NPT_binary_log <- 0
GLMM_data$NPT_binary_log[GLMM_data$NPT_walking_by_fence_dur > 0] <- 1
GLMM_data$NPT_binary_log[GLMM_data$NPT_walking_by_fence_dur > 50] <- 2
table(GLMM_data$NPT_binary_log)
table(GLMM_data$NPT_binary_log,GLMM_data$Test_Nr)

# increase max iterations for model convergence if necessary or change optimizer
#myControl = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
GLMM_data$NPT_binary_log <- ordered(GLMM_data$NPT_binary_log)
str(GLMM_data[,110:119])
model.ord <- clmm(NPT_binary_log ~  Nose.front_WDC + Agonistic_WDC 
                  + Non.agonistic_WDC +Nose.front_betweenness + Agonistic_betweenness 
                  + Non.agonistic_betweenness 
                  + Test_Nr 
                  + (1 |PVC_Nr),
                  data=GLMM_data,
                  nAGQ = 0)

summary(model.ord)
emmeans(model.log1, pairwise ~ Test_Nr, type = "response")
emmeans(model.log2, pairwise ~ Test_Nr, type = "response")
emmeans(model.ord, pairwise ~ Test_Nr) # on the logit scale
rating.emmeans(emmeans(model.ord, ~Test_Nr|cut,mode="linear.predictor",type = "response"))

##### Conclusion #####

summary(model.a)
summary(model.b)
summary(model.c)
