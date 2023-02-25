setwd("C:/Users/Jean Kevin MOTCHIAN/Desktop/semestre3/vcod_ems")
#-----------------library ---------------------------------
library(ggplot2)
library(GGally)
library(FactoMineR)
library(factoextra)
library(nFactors)
library(dplyr)
library(tibble)
library(tidyr)
library(corrplot)

#i-------------------------mportation de fichier data apres nettoyage ------------------------
data_acp <- read.csv("data.csv")

# -----------------selection de variable pour acp-----------------------------
data_acp <- data_acp %>% select(Date,
                                mean_temperature,
                                mean_humidity,
                                mean_wind.speed,
                                mean_visibility,
                                mean_dew.point.temperature,
                                mean_solar_radiation,
                                mean_Rainfall,
                                mean_Snowfall,
                                nbre_location)
head(data_acp)

#------------------ transformation de variable pluie et neige en  variable categorielle avec des conditions arbitraire----------------------
data_acp$pluie <- ifelse(data_acp$mean_Rainfall == 0,
                     "pas pluie",ifelse(data_acp$mean_Rainfall > 0
                                  & data_acp$mean_Rainfall <= 0.5, "peu pluie", "beaucoup pluie"))

data_acp$neige <- ifelse(data_acp$mean_Snowfall == 0, "pas de neige",
                     ifelse(data_acp$mean_Snowfall > 0 & data_acp$mean_Snowfall <= 1, "peu neige", "beaucoup neige"))

#----------------------formatage de la colonne Date en Date mois et jour---------------------
data_acp$Date <- as.Date(data_acp$Date)
data_acp$Date <- format(data_acp$Date, format="%d %b")

#-------------------Pour réaliser ACP transformation des lignes de notre table par la colonne Date------------------
rownames(data_acp) <- data_acp$Date
data_acp <- data_acp[,-1]
colnames(data_acp)
summary(data_acp)

#--------------------------Trouver des  corrélation avant de faire l'acp -------------------------
ggpairs (data_acp)# existance de correlation donc acp


#-------------------------- Réalisation de l'ACP -----------------------

quan_sup <- c(6,7,8) 
quali_sup <- c(10,11)
res_acp <- PCA(data_acp, quanti.sup = quan_sup, quali.sup = quali_sup)

# ------------extraitre des valeur  proprele premier ---------------------
eigenvalue <- get_eigenvalue(res_acp)
eigenvalue[,1] # valeur propre
#----------------------Contribution des axes --------------------------- 
fviz_eig(res_acp, addlabels = TRUE)

nScree(eigenvalue[,1]) # me choix des axes

var <- get_pca_var(res_acp) # 
var
ind <- get_pca_ind(res_acp) # 

eigenvalue <- get_eigenvalue(res_acp)


apply(var$contrib,1,sum)

#----visualition des variable de l'acp un peu amelioré---------------------
fviz_pca_var(res_acp, col.var = "steelblue")
fviz_pca_var(res_acp, col.var = "steelblue",repel=TRUE)

fviz_pca_var(res_acp, col.var = "steelblue",repel=TRUE,axes = c(2,3))

axe1 <- cbind(
  var$contrib[,1],
  var$coord[,1]
)
axe1 <- axe1[axe1[,1] > 100/15,]
axe1 <- axe1[rev(order(axe1[,1])),]
axe1


#------------correlation entre les variables en fonction des dim -------------
corrplot(var$contrib[,1:3], is.corr=FALSE)
corrplot(var$cor[,1:3], is.corr=FALSE)
corrplot(var$coord[,1:3], is.corr=FALSE)


#--------------------contribution des variables a la constrution des axes--------------
fviz_cos2(res_acp, choice = "var", axes = 1:2)
fviz_contrib(res_acp, choice = "var", axes = 1:2)

#-----------------visualisataion ACP ----------------------
fviz_pca_var(res_acp, col.var = "cos2",
             gradient.cols = c("steelblue", "orange", "red"),
             repel = TRUE, title="Repartition des variables selon les deux axes en fonction de leur importance" )

# --------------------- une qualité de representaion avec la coleur selon le cos2 = importance - -----------
fviz_pca_var(res_acp, alpha.var = "cos2",col.var="cos2",
             gradient.cols = c("steelblue", "orange", "red"),
             repel = TRUE) + scale_alpha_continuous(range=c(0.05,0.7))

#---------------------visualisation des individus ------------------------

fviz_pca_ind(res_acp,axes = c(3,4))

fviz_pca_ind(res_acp, col.ind = "cos2",
             gradient.cols = c("steelblue", "orange", "red"),
             repel = TRUE )

# visualisation individu selon beaucoup de pluie/ peu pluie / pas de pluie
fviz_pca_ind(res_acp,repel=TRUE,habillage = "pluie") +
  scale_color_manual(values = c("green","black", "red"))

fviz_pca_ind(res_acp,repel=TRUE,habillage = "pluie",addEllipses = T,
             ellipse.level=0.8) +
  scale_color_manual(values = c("green","black", "red"))


# visualisation individu selon beaucoup de neige/ peu neige / pas de neige

fviz_pca_ind(res_acp,repel=TRUE,habillage = "neige") +
  scale_color_manual(values = c("green","black", "red"))

fviz_pca_ind(res_acp,repel=TRUE,habillage = "neige",addEllipses = T,
             ellipse.level=0.8) +
  scale_color_manual(values = c("green","black", "red"))


fviz_pca_ind(res_acp, alpha.ind = "cos2",col.ind="cos2",
             gradient.cols = c("steelblue", "orange", "red"),
             repel = TRUE) + scale_alpha_continuous(range=c(0.05,0.7))

#contribution des individus à la construction des axes
fviz_cos2(res_acp,choice="ind")
fviz_contrib(res_acp,choice="ind")
