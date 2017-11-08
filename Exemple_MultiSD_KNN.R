#Ceci c'est un script qu'on peut utiliser pour faire rouler la fonction KNN

#Note: Les packages suivants doivent être instalés:
#   class : pour faire le KNN
#   tibble : pour le traitement des données
#   tidyr: pour le traitement des données
#   dplyr : pour le traitement des données
#   dtwclust : pour trouver la courbe la plus semblable des groupes évolutifs
#   trop petits

#1. On peut définir la fonction "faireKnn()" qu'on a écrit dans un autre script avec
#la fonction "source( «directoire du script.R»)". Cette fonction attribue
#les points d'attachement de la courbe aux polygones fournis et a besoin des
#arguments suivants:
#dfDonneesPoly: dataframe des polygones qu'ont veut attacher aux courbes correspondantes. 
#                  Chaque ligne  de ce fichier doit représenter un polygone. 
#               Ex. Dataframe enregistré sous "BD_clust.dbf"
#                
#dfPointsAttach: dataframe des courbes d'attachement. Dans ce fichier chaque ligne réprésente 
#                un point d'attachement de la courbe d'un groupe évolutif. La colonne 
#                `classec` dit-nous si les points d'attachement se trouvent à la
#                gauche (1) ou à la droite (2) du sommet de la courbe (i.e. senescence). 
#                La colonne `extrapol` dit-nous si cette partie de la courbe a été 
#                extrapolé. Chaque courbe a plusieurs points d'attachement.  
#                Ex. Dataframe enregistré sous "Courbes_6O.dbf"
#                
#supMinimale: la superficie minimale de chaque cluster. Ceci doit être une valeur en HA
#             
#nombreMaxClusterCroissance: nombre maximale de clusters dans une courbe de croissance
#                            (le côté droit de la courbe; classec == 1)
#                            
#nombreMaxClusterSenescence: nombre maximale de clusters dans une courbe de sénescence
#                            (le côté gauche de la courbe; classec == 2)


###################################################################################
###################################################################################
###################################################################################
#Partie de teste hors Oracle avec des fichiers .dbf
library(foreign)
library(dplyr)
library(readr)


#Comme je n'avais pas le bon fichier pour tester ce script dans une UA
#qui a plus qu'un sous-domaine, j'ai du l'adapter un peu
# testDfDonneesPoly <- 
#   read.dbf(file.path("T:", "Donnees", "Courant", "Projets", "Chantier_M7M",
#                      "Script_Plusieurs_SDOMs\\7MP\\Inputs", "BD_6O.dbf"))
#     
#     #Créer une variable ID_BFEC biddon qui n'existaient pas dans le fichier que j'avais
#     testDfDonneesPoly$ID_BFEC <- 1:nrow(testDfDonneesPoly)
#     
#     #On remplace les SDOM avec leur valeur originale (sdom_bio_O). Je pense que c'est ça,
#     #en tout cas on a juste besoin d'avoir 2 sous-domaines dans notre jeu de données
#     testDfDonneesPoly$SDOM_BIO <- testDfDonneesPoly$sdom_bio_o
#     
#     #Il faut aussi adapter le champs COURBE pour reflechir le nouveau sous domaine
#     testDfDonneesPoly$COURBE <- ifelse(testDfDonneesPoly$SDOM_BIO %in% "5O",
#                                    gsub(pattern = "6O",
#                                    replacement = "5O",
#                                    x = testDfDonneesPoly$COURBE),
#                                    as.character(testDfDonneesPoly$COURBE))
#Test avec une autre UA
test2751 <- 
  read_csv(file.path("T:", "Donnees", "Courant", "Projets", "Chantier_M7M",
                     "Script_Plusieurs_SDOMs\\7MP\\Inputs", "dfpoly_02751.csv"))
test8551 <- 
  read_csv(file.path("T:", "Donnees", "Courant", "Projets", "Chantier_M7M",
                     "Script_Plusieurs_SDOMs\\7MP\\Inputs", "dfpoly_08551.csv"))

# test8551 <- test8551 %>% rename(TYF = Gtyf, GR_STATION = gr_stat,
#                                 v_TOT = gTOT)

#Créer une variable ID_BFEC biddon qui n'existaient pas dans le fichier que j'avais
testData <- test8551

testData <- 
  testData %>% mutate(ID_BFEC = 1:nrow(.)) %>% 
  filter(SDOM_BIO %in% c("6O", "6E", "5O"))


#Catalogue des courbes 6O
testCourbes6O <-
  read_csv(file.path("T:", "Donnees", "Courant", "Projets", "Chantier_M7M",
                     "Script_Plusieurs_SDOMs\\7MP\\Inputs",
                     "Courbes_6ouest_Horizon_   30 octobre 2017.csv"))

#Catalogue des courbes 5O
testCourbes5O <-
  read_csv(file.path("T:", "Donnees", "Courant", "Projets", "Chantier_M7M",
                     "Script_Plusieurs_SDOMs\\7MP\\Inputs",
                     "Courbes_5ouest_Horizon_   6 novembre 2017.csv"))

#Catalogue des courbes 5O
testCourbes6E <-
  read_csv(file.path("T:", "Donnees", "Courant", "Projets", "Chantier_M7M",
                     "Script_Plusieurs_SDOMs\\7MP\\Inputs",
                     "Courbes_6est_Horizon_   30 octobre 2017.csv"))


#Joindre les catalogues
testCatCourbes <- 
  bind_rows(testCourbes6O %>% select(NOM_FAMC:VOL_HA),
            testCourbes6E %>% select(NOM_FAMC:VOL_HA),
            testCourbes5O %>% select(NOM_FAMC:VOL_HA))


testCatCourbes <-
  testCatCourbes %>%
  
  #Calculer le volume total (i.e. la somme du volume de chaque essence;
  #dans le format HORIZON des courbes, chaque ligne a le volume d'une
  #essence donnée et on veut la somme de toutes les essences)
  group_by(NOM_FAMC, DESC_FAMC, gr_station, tyf, enjeux, classe, 
           duree_crb, age, AGE_EXPL, AGE_BRIS) %>%
  summarize(VOL_HA = sum(VOL_HA)) %>%
  group_by(NOM_FAMC, DESC_FAMC, gr_station, tyf) %>% 
  
####################################################################
  #Générer des variables manquantes
  mutate(classec = which(VOL_HA == max(VOL_HA))[1],
       rowNumber = 1:n(),
       classec = ifelse(rowNumber <= classec, "1", "2")) %>% 
       # extrapol = 1:n(),
       # extrapol = ifelse(extrapol <= 5, "oui", "non")) %>% 
  select(-rowNumber) %>% 
####################################################################
  ungroup() %>% 
  rename(GR_STATION = gr_station,
         TYF = tyf) %>% 
  mutate(SDOM = substr(DESC_FAMC, 1,2))


###################################################################
#Une autre patch. Très rarement, une courbe peut pas avoir de classec
#avec la condition que j'ai fait (i.e. le dernier point de la courbe est
#le point qui a le volume le plus gros). Quand ça arrive, on va dire 
#que la sénescence commence à l'âge 90
testCatCourbes <- 
  testCatCourbes %>% 
  group_by(NOM_FAMC, DESC_FAMC, GR_STATION, TYF) %>% 
  mutate(classec = ifelse(!"2" %in% classec & age >= 90, "2", classec)) %>% 
  ungroup()

###################################################################


##############################################################################
##############################################################################
##############################################################################
#Ces cas seront traités par Francis avant qu'on fasse rouler les scripts.
# miss5O <- testData %>% filter(SDOM_BIO %in% "5O", 
#                               !GE1 %in% testCourbes5O$DESC_FAMC &
#                                 !GE3 %in% testCourbes5O$DESC_FAMC & 
#                                 !GE5 %in% testCourbes5O$DESC_FAMC) %>%
#   select(GE1) %>% unlist %>% unname %>% unique
# 
# miss6O <- testData %>% filter(SDOM_BIO %in% "6O", 
#                               !GE1 %in% testCourbes6O$DESC_FAMC &
#                                 !GE3 %in% testCourbes6O$DESC_FAMC & 
#                                 !GE5 %in% testCourbes6O$DESC_FAMC) %>%
#   select(GE1) %>% unlist %>% unname %>% unique
# 
# miss6E <- testData %>% filter(SDOM_BIO %in% "6E", 
#                               !GE1 %in% testCourbes6E$DESC_FAMC &
#                                 !GE3 %in% testCourbes6E$DESC_FAMC & 
#                                 !GE5 %in% testCourbes6E$DESC_FAMC) %>%
#   select(GE1) %>% unlist %>% unname %>% unique
# 
# 
# testData <- 
#   testData %>% 
#   filter(!GE1 %in% c(as.character(miss5O), as.character(miss6O),
#                      as.character(miss6E))) %>% 
#   # rename(v_TOT = gTOT,
#   #        GR_STATION = gr_stat) %>% 
#   
#   
#   #Enlever les peuplements improductifs
#   filter(!Improd %in% "_SNAT",
#          !GE1 %in% "6O_RES_RH_Ep_En95",
#          !is.na(GE3))   #quelques polygones 6E avaient des GE3 et GE5 NAs
##############################################################################
##############################################################################
##############################################################################



#1. Charger la fonction "faireKnn" définie dans le script "Fonction_KNN_Oracle.R"

#2. Faire rouler la fonction qui fait le KNN. Maintenant, au lieu d'enregistrer 
#automatiquement un fichier .dbf, cette fonction retourne une liste avec 3 dataframes:
#  - dfPoly: dataframe avec l'id de chaque polygone (ID_BFEC), le côté de la courbe 
#            (classec; variable qui représente le côté de la courbe: 
#            1 = croissance; 2 = sénescence) et le point d'attachement (clusterAttach; 
#            ceci c'est une variable de caractères même si tous les caractères sont des 
#            chiffres)
#  - dfStrates: dataframe avec l'appelation de chaque courbe (COURBE; ex. 
#            "6O_RES_R_En_v2"), le côté de la courbe (classec) et le point d'attachement 
#            (clusterAttach)
#  - dfCourbesPetites: dataframe avec les courbes auxquelles les groupes évolutifs 
#             trop petits (COURBE, id, classec), leur superficie totale (sumSup) et
#             les courbes auxquelles ils ont été attachés (courbeEquiv, idEquiv)

#Ce script est structuré de la suivante façon:
#  1. Charger des packages
#  2. Traiter les intrants (e.g. vérifier que toutes les colonnes nécessaires 
#  sont la, qu'on a toute les courbes...)
#  3. Attribuer un groupe évolutif alternatif aux groupes évolutifs qui sont 
#  plus petits que le seuil spécifié par l'argument "supMinimale". On utilise
#  une méthodologie the DTW (Detrended Time Warping avec le package dtwclust)
#  pour trouver la courbe la plus semblable parmi toutes les possibilités
#  4. Attacher les polygones au point d'attachement le plus proche. Pour faire ça
#  on a besoin de créer un jeu de données d'entraînement. La nombre maximale de
#  clusters par groupe est spécifiée par les arguments "nombreMaxClusterCroissance"
#  (courbes de croissance) et "nombreMaxClusterSenescence" (courbes de sénescence).
#  Une description plus détaillée du processus est présente au début de la section 4.
source(file.path("T:", "Donnees", "Courant", "Projets", "Chantier_M7M",
                 "Script_Plusieurs_SDOMs\\7MP\\7MP_Git",
                 "Fonction_MultiSD_KNN.R"))

supMin_courbe <- 5000
supMin_pointAttach <- 1000

exempleKNN <- 
  faireKnn(dfDonneesPoly = testData,
           catCourbes = testCatCourbes,
           supMin_pointAttach = supMin_pointAttach,
           supMin_courbe = supMin_courbe,
           nombreMaxClusterCroissance = 10, 
           nombreMaxClusterSenescence = 5)



#3. Après avoir faite rouler la fonction, on peut créer 2 dataframes en extrayant les 
#dataframes qui sont stockés dans la liste créée par la fonction
#3.1 Extraire les extrants (list to datafrane)
dfPoly <- exempleKNN$dfPoly
dfStrate <- exempleKNN$dfStrate
dfCourbesPetites <- exempleKNN$dfCourbesPetites


# asd <- dfStrate %>% group_by(COURBE) %>% summarise(sumSup = sum(SUPERFICIE))


#3.2 Faire le join avec le jeux de données initiel
tempPoly <- left_join(testData, dfPoly, 
                      by = c("ID_BFEC"))


##############################################################################
##############################################################################
#L'exemple du script finit ici.
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################

#3.3 Enregistrer les extrants
write_csv(tempPoly, 
          file.path("T:", "Donnees", "Courant", "Projets", "Chantier_M7M",
                    "Script_Plusieurs_SDOMs\\7MP\\Test outputs", "Test 8551",
                    paste0("poly8551_courbe", supMin_courbe,"_",
                           supMin_pointAttach, ".csv")))

write_csv(dfStrate,
          file.path("T:", "Donnees", "Courant", "Projets", "Chantier_M7M",
                    "Script_Plusieurs_SDOMs\\7MP\\Test outputs", "Test 8551",
                    paste0("strate8551_courbe", supMin_courbe,"_",
                           supMin_pointAttach, ".csv")))

write_csv(dfCourbesPetites,
          file.path("T:", "Donnees", "Courant", "Projets", "Chantier_M7M",
                    "Script_Plusieurs_SDOMs\\7MP\\Test outputs", "Test 8551",
                    paste0("petit8551_courbe", supMin_courbe,"_",
                           supMin_pointAttach, ".csv")))







#4. Validation
#4.1 Joindre 
supPondOri <- 
  test8551 %>% 
  group_by(SDOM_BIO) %>% 
  #mutate(supPro = SUPERFICIE / sum(SUPERFICIE)) %>% 
  summarize(supPondOri = sum(SUPERFICIE * v_TOT),
            supPondFin = sum(SUPERFICIE * clusterAttach)) %>% 
  mutate(fin = supPondFin/supPondOri)



supPondFin <- 
  test2751 %>% 
  group_by(SDOM_BIO) %>% 
  #mutate(supProd = SUPERFICIE / sum(SUPERFICIE)) %>% 
  summarize(fin_ori = sum(SUPERFICIE * clusterAttach))










#3. Après avoir faite rouler la fonction, on peut créer 2 dataframes en extrayant les 
#dataframes qui sont stockés dans la liste créée par la fonction
dfPoly_pond <- exempleKNN_pond$dfPoly %>% distinct %>% 
  rename(clusterAttachPond = clusterAttach,
         rtPond = volm3haRT_, riPond = volm3haRI_,
         sabPond = volm3haSAB, ftPond = volm3haFT_,
         fiPond = volm3haFI_)

dfPoly_noPond <- exempleKNN_noPond$dfPoly %>% 
  rename(rtOri = volm3haRT_, riOri = volm3haRI_,
         sabOri = volm3haSAB, ftOri = volm3haFT_,
         fiOri = volm3haFI_)

dfPoly_join <- left_join(dfPoly_noPond, dfPoly_pond, by = c("ID_BFEC", "classec", "COURBE"))


polyCluster <- 
  left_join(testDfDonneesPoly %>% 
              mutate(classec = as.character(classec)) %>% 
              select(ID_BFEC, classec, US_FOR, SUPERFICIE,
                     starts_with("V_")), 
            dfPoly_join, 
            by = c("ID_BFEC","classec"))



#4. Validation
#4.1 Joindre 
supPondOri <- 
  polyCluster %>% 
  filter(!US_FOR %in% c("08751", "NA", NA, "A082")) %>% 
  group_by(US_FOR) %>% 
  #mutate(supPro = SUPERFICIE / sum(SUPERFICIE)) %>% 
  summarize(totPoly = sum(SUPERFICIE * v_TOT),
            totCluster_noPond = sum(SUPERFICIE * clusterAttach),
            totCluster_Pond = sum(SUPERFICIE * clusterAttachPond),
            
            riPoly = sum(SUPERFICIE * v_Ri),
            riCluster_noPond = sum(SUPERFICIE * riOri),
            riCluster_Pond = sum(SUPERFICIE * riPond),
            
            rtPoly = sum(SUPERFICIE * V_Rt),
            rtCluster_noPond = sum(SUPERFICIE * rtOri),
            rtCluster_Pond = sum(SUPERFICIE * rtPond),
            
            sabPoly = sum(SUPERFICIE * v_Sab),
            sabCluster_noPond = sum(SUPERFICIE * sabOri),
            sabCluster_Pond = sum(SUPERFICIE * sabPond),
            
            fiPoly = sum(SUPERFICIE *  V_Fi),
            fiCluster_noPond = sum(SUPERFICIE * fiOri),
            fiCluster_Pond = sum(SUPERFICIE * fiPond)) %>% 
  
  group_by(US_FOR) %>% 
  transmute(
    ratioTot_Ori = totCluster_noPond/totPoly,
    ratioTot_Pond = totCluster_Pond/totPoly,
    
    ratioRi_Ori = riCluster_noPond/riPoly,
    ratioRi_Pond = riCluster_Pond/riPoly,
    
    ratioRt_Ori = rtCluster_noPond/rtPoly,
    ratioRt_Pond = rtCluster_Pond/rtPoly,
    
    ratioSab_Ori = sabCluster_noPond/sabPoly,
    ratioSab_Pond = sabCluster_Pond/sabPoly,
    
    ratioFi_Ori = fiCluster_noPond/fiPoly,
    ratioFi_Pond = fiCluster_Pond/fiPoly) %>% 
  ungroup()


ratioTot <- 
  supPondOri %>% gather(type, ratioTot, ratioTot_Ori, ratioTot_Pond) %>% 
  select(US_FOR, type, ratioTot) %>% 
  mutate(type = ifelse(grepl("Ori", type), "ori", "pond"))
ratioRt <- 
  supPondOri %>% gather(type, ratioRt, ratioRt_Ori, ratioRt_Pond) %>% 
  select(US_FOR, type, ratioRt)%>% 
  mutate(type = ifelse(grepl("Ori", type), "ori", "pond"))
ratioRi <- 
  supPondOri %>% gather(type, ratioRi, ratioRi_Ori, ratioRi_Pond) %>% 
  select(US_FOR, type, ratioRi)%>% 
  mutate(type = ifelse(grepl("Ori", type), "ori", "pond"))
ratioSab <- 
  supPondOri %>% gather(type, ratioSab, ratioSab_Ori, ratioSab_Pond) %>% 
  select(US_FOR, type, ratioSab)%>% 
  mutate(type = ifelse(grepl("Ori", type), "ori", "pond"))
ratioFi <- 
  supPondOri %>% gather(type, ratioFi, ratioFi_Ori, ratioFi_Pond) %>% 
  select(US_FOR, type, ratioFi)%>% 
  mutate(type = ifelse(grepl("Ori", type), "ori", "pond"))

ratio <- 
  left_join(ratioTot, ratioRt) %>% 
  left_join(ratioRi) %>% 
  left_join(ratioSab) %>% 
  left_join(ratioFi) %>% 
  gather(essence, ratio, ratioTot:ratioFi)

ratio <- ratio %>% mutate(biais = 1 - ratio)

#Make figures
library(ggplot2)
figDir <- 
  file.path("T:", "Donnees", "Courant", "Projets", 
            "Chantier_M7M","Script_Plusieurs_SDOMs",
            "7MP\\Test ponderation")

for(i in unique(ratio$essence)){
  
  tempFig <- 
    ggplot(ratio %>% filter(essence %in% i), 
           aes(x = US_FOR, y = biais, 
               colour = US_FOR,
               shape = type)) +
    geom_jitter(size = 3) +
    #ylim(0.65, 1.12) +
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "red", size = 2) +
    ggtitle(i)
  
  ggsave(plot = tempFig,
         filename = file.path(figDir, paste0(i, ".jpeg")), 
         width = 6, height = 6)
  
}


for(i in unique(ratio$essence)){
  
  tempFig <-  
    ggplot(ratio,# %>% filter(essence %in% i), 
           aes(x = essence, y = biais, 
               fill = type)) +
    geom_boxplot(position = "dodge") +
    #ylim(0.65, 1.12) +
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "red", size = 2) +
    ggtitle(i)
  
  ggsave(plot = tempFig,
         filename = file.path(figDir, paste0(i, ".jpeg")), 
         width = 6, height = 6)
  
}

