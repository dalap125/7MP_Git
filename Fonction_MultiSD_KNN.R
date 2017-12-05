#Ceci est le script pour attacher les polygones à une courbe KNN.
#Ce script peut être roulé directement ou peux être appelé par un autre script
#avec la fonction "source(«directoire de ce script.R»)"

#Ce script est structuré de la suivante façon:
#  1. Charger les packages
#  2. Traiter les intrants (e.g. vérifier que toutes les colonnes nécessaires 
#  sont la, qu'on a toute les courbes...)
#  3. Attribuer un groupe évolutif alternatif aux groupes évolutifs qui sont 
#  plus petits que le seuil spécifié par l'argument "supMin_pointAttach". On utilise
#  une méthodologie the DTW (Detrended Time Warping avec le package dtwclust)
#  pour trouver la courbe la plus semblable parmi toutes les possibilités
#  4. Attacher les polygones au point d'attachement le plus proche. Pour faire ça
#  on a besoin de créer un jeu de données d'entraînement. La nombre maximale de
#  clusters par groupe est spécifiée par les arguments "nombreMaxClusterCroissance"
#  (courbes de croissance) et "nombreMaxClusterSenescence" (courbes de sénescence).
#  Une description plus détaillée du processus est présente au début de la section 4.

#Définir la fonction qu'on va utiliser pour faire le KNN. 

#Cette fonction a 5 arguments. Les 2 premiers sont des dataframes et les 3 derniers sont 
#des paramètres de la fonction. 
#   dfDonneesPoly: dataframe des polygones qu'ont veut attacher aux courbes correspondantes. 
#                  Chaque ligne  de ce fichier doit représenter un polygone. 
#                
#   courbes6O: dataframe des courbes d'attachement. Dans ce fichier chaque ligne réprésente 
#                   un point d'attachement de la courbe d'un groupe évolutif. La colonne 
#                   `classec` dit-nous si les points d'attachement se trouvent à la
#                   gauche (1) ou à la droite (2) du sommet de la courbe (i.e. senescence). 
#                   La colonne `extrapol` dit-nous si cette partie de la courbe a été 
#                   extrapolé. Chaque courbe a plusieurs points d'attachement.  
#                
#   supMin_pointAttach: la superficie minimale de chaque cluster. Ceci doit être une valeur en HA
#             
#   nombreMaxClusterCroissance: nombre maximale de clusters dans une courbe de croissance
#                               (le côté droit de la courbe; classec == 1)
#                            
#   nombreMaxClusterSenescence: nombre maximale de clusters dans une courbe de sénescence
#                               (le côté gauche de la courbe; classec == 2)



#Maintenant, au lieu d'enregistrer automatiquement un fichier .dbf, cette fonction retourne 
#une liste avec 3 dataframes:
#   - dfPoly: dataframe avec l'id de chaque polygone (ID_BFEC), le côté de la courbe (classec;
#             variable numérique  qui représente le côté de la courbe: 1 = croissance; 2 = sénescence) 
#             et le point d'attachement (clusterAttach; ceci c'est une variable de caractères même si 
#             tous les caractères sont des chiffres)
#   - dfStrates: dataframe avec l'appelation de chaque courbe (COURBE; ex. "6O_RES_R_En_v2"),
#                le côté de la courbe (classec) et le point d'attachement (clusterAttach)
#   - dfCourbesPetites: dataframe avec les courbes des groupes évolutifs 
#             trop petits (COURBE, classec), leur superficie totale (sumSup) et
#             les courbes auxquelles ils ont été rattachés (courbeEquiv, idEquiv)



# ####################################################
# ####################################################
# library(foreign)
# library(dplyr)
# 
# dfDonneesPoly <- read.dbf(file.path("T:", "Donnees", "Courant", "Projets", "Chantier_M7M",
#                                     "David", "Clustering 7MP", "Plusieurs sous-domaines", 
#                                     "Inputs", "BD_6O.dbf"))
# 
# #Créer une variable ID_BFEC biddon qui n'existaient pas dans le fichier que j'avais
# dfDonneesPoly$ID_BFEC <- 1:nrow(dfDonneesPoly)
# 
# #####
# #On remplace les SDOM avec leur valeur originale (sdom_bio_O). Je pense que c'est ça,
# #en tout cas on a juste besoin d'avoir 2 sous-domaines dans notre jeu de données
# dfDonneesPoly$SDOM_BIO <- dfDonneesPoly$sdom_bio_o
# 
# #Il faut aussi adapter le champs COURBE pour reflechir le nouveau sous domaine 
# dfDonneesPoly$COURBE <- ifelse(dfDonneesPoly$SDOM_BIO %in% "5O",
#                                gsub(pattern = "6O",
#                                replacement = "5O",
#                                x = dfDonneesPoly$COURBE),
#                                as.character(dfDonneesPoly$COURBE))
# 
# #####
# #On va créer un catalogue de courbes biddon pour la 5O
# 
# courbes6O <- read.dbf(file.path("T:", "Donnees", "Courant", "Projets", "Chantier_M7M",
#                                 "David", "Clustering 7MP", "Plusieurs sous-domaines", 
#                                 "Inputs", "Courbes_6O.dbf"))
# 
# courbes5O <- courbes6O
# courbes5O$courbe <- gsub("6O", "5O", courbes5O$courbe)



#####
#Avant faire la fonction principale, il va falloir définir la fonction
#qui nous vas permettre de trouver des courbes de compromis quand une 
#courbe n'est pas présente dans le catalogue (e.g. si la courbe v4 
#n'est pas disponible, mais la courbe v34 l'est, on va mettre la courbe
#comme une v34)
#Cette fonction va être appliqué avec la fontion do() du package dplyr au
#catalogue de courbes regroupé par SDOM; GR_STATION, TYF et enjeux
trouverCourbesCompromis <- function(valeurUniqueCourbes){
  
  #0.0 Faire une exception pour les SNATs et finir cette fonction tout de suite
  if(any(grepl('SNAT', valeurUniqueCourbes$DESC_FAMC))){
    
    catCompromis <- valeurUniqueCourbes
    catCompromis$DESC_FAMC_Comp <- catCompromis$DESC_FAMC
    
    return(catCompromis)
    
  } 
  
  #0.1 Créer catalogue avec toutes les possibilités
  clVols <- 
    data.frame(classe = c(NA, "v1", "v12", "v2", "v3", "v34", "v4", "v5"))
  catIdeal <- valeurUniqueCourbes %>% distinct(SDOM, GR_STATION, TYF, enjeux)
  catIdeal <- cbind(catIdeal, clVols) %>% mutate_all(as.character)
  
  #0.2 Joindre ce catalogue idéal avec le catalogue réel (qui peut avoir des
  #courbes manquantes)
  catIdeal <- 
    left_join(catIdeal, valeurUniqueCourbes, 
              by = c("SDOM", "GR_STATION", "TYF", "enjeux", "classe"))  
  
  #0.3 On va faire les compromis d'une façon manuelle.
  #0.3.1 Extraire la valeur de chaque niveaux possible (incluant les NAs)
  temp_NA <- catIdeal[catIdeal$classe %in% c(NA, "NA", "Na", "na"), "DESC_FAMC"]
  temp_v1 <- catIdeal[catIdeal$classe %in% "v1", "DESC_FAMC"]
  temp_v2 <- catIdeal[catIdeal$classe %in% "v2", "DESC_FAMC"]
  temp_v12 <- catIdeal[catIdeal$classe %in% "v12", "DESC_FAMC"]
  temp_v3 <- catIdeal[catIdeal$classe %in% "v3", "DESC_FAMC"]
  temp_v4 <- catIdeal[catIdeal$classe %in% "v4", "DESC_FAMC"]
  temp_v34 <- catIdeal[catIdeal$classe %in% "v34", "DESC_FAMC"]
  temp_v5 <- catIdeal[catIdeal$classe %in% "v5", "DESC_FAMC"]
  
  
  #0.3.2 Assigner les valeurs manuellement en faisant des compromis
  #Il y a quelques compromis un peu bizarres (e.g. v4 en v12) qui probablement
  #ne seront jamais appliqués. Ils sont la simplement pour éviter que le 
  #programme plante au cas où ça arrive
  catCompromis <- 
    catIdeal %>% 
    mutate(DESC_FAMC_Comp =
             
             #V5
             case_when(is.na(.$DESC_FAMC) & .$classe %in% "v5" & !is.na(temp_v4) ~ temp_v4,
                       is.na(.$DESC_FAMC) & .$classe %in% "v5" & !is.na(temp_v34) ~ temp_v34,
                       is.na(.$DESC_FAMC) & .$classe %in% "v5" & !is.na(temp_NA) ~ temp_NA,
                       is.na(.$DESC_FAMC) & .$classe %in% "v5" & !is.na(temp_v3) ~ temp_v3,
                       
                       #V4
                       is.na(.$DESC_FAMC) & .$classe %in% "v4" & !is.na(temp_v34) ~ temp_v34,
                       is.na(.$DESC_FAMC) & .$classe %in% "v4" & !is.na(temp_NA) ~ temp_NA,
                       is.na(.$DESC_FAMC) & .$classe %in% "v4" & !is.na(temp_v3) ~ temp_v3,
                       is.na(.$DESC_FAMC) & .$classe %in% "v4" & !is.na(temp_v5) ~ temp_v5,
                       is.na(.$DESC_FAMC) & .$classe %in% "v4" & !is.na(temp_v12) ~ temp_v12,
                       
                       #V3
                       is.na(.$DESC_FAMC) & .$classe %in% "v3" & !is.na(temp_v34) ~ temp_v34,
                       is.na(.$DESC_FAMC) & .$classe %in% "v3" & !is.na(temp_NA) ~ temp_NA,
                       is.na(.$DESC_FAMC) & .$classe %in% "v3" & !is.na(temp_v4) ~ temp_v4,
                       is.na(.$DESC_FAMC) & .$classe %in% "v3" & !is.na(temp_v12) ~ temp_v12,
                       
                       #V2
                       is.na(.$DESC_FAMC) & .$classe %in% "v2" & !is.na(temp_v12) ~ temp_v12,
                       is.na(.$DESC_FAMC) & .$classe %in% "v2" & !is.na(temp_NA) ~ temp_NA,
                       is.na(.$DESC_FAMC) & .$classe %in% "v2" & !is.na(temp_v1) ~ temp_v1,
                       is.na(.$DESC_FAMC) & .$classe %in% "v2" & !is.na(temp_v34) ~ temp_v34,
                       
                       #V1
                       is.na(.$DESC_FAMC) & .$classe %in% "v1" & !is.na(temp_v12) ~ temp_v12,
                       is.na(.$DESC_FAMC) & .$classe %in% "v1" & !is.na(temp_NA) ~ temp_NA,
                       is.na(.$DESC_FAMC) & .$classe %in% "v1" & !is.na(temp_v2) ~ temp_v2,
                       is.na(.$DESC_FAMC) & .$classe %in% "v1" & !is.na(temp_v34) ~ temp_v34,
                       
                       #V34
                       is.na(.$DESC_FAMC) & .$classe %in% "v34" & !is.na(temp_v4) ~ temp_v4,
                       is.na(.$DESC_FAMC) & .$classe %in% "v34" & !is.na(temp_v3) ~ temp_v3,
                       is.na(.$DESC_FAMC) & .$classe %in% "v34" & !is.na(temp_NA) ~ temp_NA,
                       is.na(.$DESC_FAMC) & .$classe %in% "v34" & !is.na(temp_v12) ~ temp_v12,
                       
                       #V12
                       is.na(.$DESC_FAMC) & .$classe %in% "v12" & !is.na(temp_v2) ~ temp_v2,
                       is.na(.$DESC_FAMC) & .$classe %in% "v12" & !is.na(temp_v1) ~ temp_v1,
                       is.na(.$DESC_FAMC) & .$classe %in% "v12" & !is.na(temp_NA) ~ temp_NA,
                       is.na(.$DESC_FAMC) & .$classe %in% "v12" & !is.na(temp_v34) ~ temp_v34,
                       
                       #Sinon, on guarde la valeur originales
                       TRUE ~ .$DESC_FAMC)
    )
  
  
  #0.4 Recréer la valeur de DESC_FAMC "originale" pour éviter avoir des NAs
  catCompromis <- catCompromis %>% mutate_all(as.character)
  char_descFamc <- unique(catCompromis$DESC_FAMC_Comp)
  char_descFamc <- char_descFamc[grepl("_v", char_descFamc)][1]
  char_descFamc <- sub("_v.*", "", char_descFamc)
  if(is.na(char_descFamc)){
    
    char_descFamc <- unique(catCompromis$DESC_FAMC_Comp)
    char_descFamc <- char_descFamc[grepl("_NA", char_descFamc)][1]
    
  } 
  
  catCompromis <- 
    catCompromis %>% 
    mutate(DESC_FAMC = 
             ifelse(is.na(classe), 
                    char_descFamc,
                    paste(char_descFamc, classe, sep = "_")))
  
  
  #0.5 Retourner l'output de la fonction
  rm(char_descFamc)
  return(catCompromis)
  
}



####################################################
####################################################

faireKnn <- function(dfDonneesPoly,
                     catCourbes,  #ceci doit avoir toutes les courbes des catalogues de tous 
                     #les SDOMs ensemble. Chaque ligne doit représenter le 
                     #volume total du point de la courbe, pas le volume 
                     #par essence
                     supMin_pointAttach = 500, 
                     supMin_courbe = 5000,
                     nombreMaxClusterCroissance = 10, 
                     nombreMaxClusterSenescence = 5){
  
  
  
  #1. Charger les packages. Il faut charger les packages chaque fois qu'on ouvre R
  require(dtwclust)
  require(class)
  require(tibble)
  require(tidyr)
  require(dplyr)
  
  
  
  #2. Traitement des données
  #2.1 Vérifier que toutes les variables dont on a besoin sont la
  #2.1.1 Catalogue des courbes
  #2.1.1.1 Identifier les variables nécessaires
  varsCatCourbes <- c("age", "SDOM",    #le sous-domaine
                      "GR_STATION", "TYF", "enjeux",   #l'enjeux stratégique (e.g. En95)
                      "classe",   #classe de volume (v1, v2,...)
                      "VOL_HA", "classec")     #Le côté de la courbe
  
  
  
  #2.1.1.2  S'il y a au moins une variable manquante
  if(!all(varsCatCourbes %in% names(catCourbes))){
    
    #On l'identifie
    varsManq <- varsCatCourbes[!varsCatCourbes %in% 
                                 names(catCourbes)]
    
    #Et on arréte la fonction
    stop("La ou les variables ", paste(varsManq, collapse = ", "),
         " ne sont pas présentes dans le jeu de données défini par 'catCourbes'.",
         "Faites attention que cette fonction est sensible aux minuscules ",
         "et aux majuscules.")
  }
  
  
  #2.1.2 Jeu de données
  #2.1.2.1 Identifier les variables nécessaires
  varsDonneesPoly <- c("ID_BFEC", "v_TOT", "SDOM_BIO", "GR_STATION", 
                       "TYF", "Enjeux_evo", "Enjeux_str", "Improd",
                       "cl_vol3", "cl_vol5", 
                       "clage", "SUPERFICIE")   
  
  
  
  #2.1.2.2  S'il y a au moins une variable manquante
  if(!all(varsDonneesPoly %in% names(dfDonneesPoly))){
    
    #On l'identifie
    varsManq <- varsDonneesPoly[!varsDonneesPoly %in% 
                                  names(dfDonneesPoly)]
    
    #Et on arrète la fonction
    stop("La ou les variables ", paste(varsManq, collapse = ", "),
         " ne sont pas présentes dans le jeu de données défini par 'dfDonneesPoly'.",
         "Faites attention que cette fonction est sensible aux minuscules ",
         "et aux majuscules.")
  }
  
  
  #2.2 Ajouter des champs dont on a besoin mais qu'Olivier n'a peut être pas
  #2.2.0 Pour l'instant il faut enlever les "_" de l'appelation de GR_STAT
  #des polygones. Adrian m'a dit que cette appelation (sans "_") va
  #être le nouveau standard
  dfDonneesPoly$GR_STATION <- gsub("_", "", dfDonneesPoly$GR_STATION)
  catCourbes$GR_STATION <- gsub("_", "", catCourbes$GR_STATION)
  
  
  #2.2.1 GE1
  if(!"GE1" %in% names(dfDonneesPoly)){
    dfDonneesPoly <- 
      dfDonneesPoly %>% 
      mutate(GE1 = ifelse(Improd %in% "SNAT",
                          paste(SDOM_BIO, substr(GR_STATION, 1 ,3), Improd, sep = "_"),
                          paste(SDOM_BIO, GR_STATION, TYF, Enjeux_evo, sep = "_")),
             GE1 = gsub("__", "_", GE1)) #le paste des SNAT génère deux "_" qu'il faut enlever
  }
  
  #2.2.2 GE3
  if(!"GE3" %in% names(dfDonneesPoly)){
    dfDonneesPoly <- 
      dfDonneesPoly %>% 
      mutate(GE3 = ifelse(Improd %in% "SNAT" | cl_vol3 %in% c(NA, "NA", "Na", "na"),
                          NA,
                          paste(SDOM_BIO, GR_STATION, TYF, 
                                Enjeux_evo, cl_vol3, sep = "_")))
  }
  
  #2.2.3 GE5
  if(!"GE5" %in% names(dfDonneesPoly)){
    dfDonneesPoly <- 
      dfDonneesPoly %>% 
      mutate(GE5 = ifelse(Improd %in% "SNAT" | cl_vol5 %in% c(NA, "NA", "Na", "na"),
                          NA,
                          paste(SDOM_BIO, GR_STATION, TYF, 
                                Enjeux_evo, cl_vol5, sep = "_")))
  }
  
  #2.2.4 DESC_FAMC dans le catalogue des courbes
  if(!"DESC_FAMC" %in% names(catCourbes)){
    catCourbes <- 
      catCourbes %>% 
      mutate(DESC_FAMC = 
               case_when(.$Improd %in% "SNAT" ~ 
                           paste(.$SDOM, substr(.$GR_STATION, 1, 3), "SNAT", sep = "_"),
                         .$classe %in% c(NA, "NA", "Na", "na") ~
                           paste(.$SDOM, .$GR_STATION, .$TYF, .$enjeux, sep = "_"),
                         TRUE ~ paste(.$SDOM, .$GR_STATION, .$TYF, .$enjeux, 
                                      .$classe, sep = "_")))
                }
  
  
  
  
  #2.2 Vérifier les données manquantes
  #Il faut aussi vérifier qu'on n'a pas des données de volume manquantes. Ça devrait
  #jamais arriver non plus.
  #2.2.1 Si au moins un polygone n'a pas une valeur de volume total
  if(any(is.na(dfDonneesPoly$v_TOT)) ){
    
    #2.2.2 Identifier les groupes où ça a arrivé 
    volManq <- 
      dfDonneesPoly %>% 
      filter(v_TOT %in% NA) %>%   #Trouver les lignes avec NA
      select(GE5) %>% unlist() %>% unname() %>%  #Sélectionner la colonne "COURBE"
      as.character()
    
    
    #2.3.3 Remplacer les NA par des 0
    dfDonneesPoly$v_TOT[is.na(dfDonneesPoly$v_TOT)] <- 0
    
    #2.3.4 Donner un message d'avertissement
    warning("Faites attention : ", length(volManq), " polygones dans les courbes ", 
            unique(volManq), " ont des données 'v_TOT' manquantes!", 
            "Leur valeur a été remplacé par '0' lors de cette analyse.")
  }
  
 
  
  #3. Gérer l'échelle de la courbe: i.e est-ce qu'on utilise une courbe v1,
  #v12 ou génerale?
  #3.0 Crisser (poliment) les v5 dans les v4 si leur superficie est plus 
  #petite que le minimum spécifié
  #3.0.1 Identifier les v5 qui sont trop petits
  v5Petits <- 
    dfDonneesPoly %>% 
    group_by(GE5, cl_vol5) %>% 
    summarise(sumSup = sum(SUPERFICIE)) %>% 
    filter(cl_vol5 %in% "v5", sumSup < supMin_courbe) %>% 
    distinct(GE5) %>% unlist %>% unname %>% as.character()
  
  #3.0.2 Changer-les en "v4"
  dfDonneesPoly <- 
    dfDonneesPoly %>% 
    mutate(cl_vol3 = ifelse(GE5 %in% v5Petits, 
                            "v34", as.character(cl_vol3)),
           cl_vol5 = ifelse(GE5 %in% v5Petits, 
                            "v4", as.character(cl_vol5)),
           GE3 = ifelse(GE5 %in% v5Petits, 
                        gsub("v5", "v34", GE3), 
                        as.character(GE3)),
           GE5 = ifelse(GE5 %in% v5Petits, 
                        gsub("v5", "v4", GE5),  
                        as.character(GE5)))
  
 
  #3.1 Calculer la superficie de chaque GE + v12 et v34
  echelle_2g <- 
    dfDonneesPoly %>% 
    group_by(SDOM_BIO, GR_STATION, TYF, Enjeux_evo, cl_vol3) %>% 
    summarise(echelle_2 = sum(SUPERFICIE)) %>% 
    ungroup()
  
  
  #3.2 Calculer le ratio entre la v12 et la v34
  ratio_2g <- 
    echelle_2g %>% 
    group_by(SDOM_BIO, GR_STATION, TYF, Enjeux_evo) %>% 
    summarise(ratio_2g = round(echelle_2[1] / sum(echelle_2)))
  
  
  #3.3 Calculer la superficie de chaque GE + v1, v2 ,v3 et v34
  echelle_5g <- 
    dfDonneesPoly %>% 
    group_by(SDOM_BIO, GR_STATION, TYF, Enjeux_evo, cl_vol3, cl_vol5) %>% 
    summarise(echelle_5 = sum(SUPERFICIE)) %>% 
    ungroup()
  
  
  #3.4 Calculer le ratio entre la v12 et la v34
  ratio_5g <- 
    echelle_5g %>%
    group_by(SDOM_BIO, GR_STATION, TYF, Enjeux_evo, cl_vol3) %>%
    summarise(ratio_5g = round(echelle_5[1] / sum(echelle_5), 2)) %>% 
    ungroup()
  
  
  #3.5 Calculer la superficie de chaque GE + courbe generale (v12 + v34) 
  echelle_gen <- 
    dfDonneesPoly %>% 
    group_by(SDOM_BIO, GR_STATION, TYF, Enjeux_evo) %>% 
    summarise(echelle_gen = sum(SUPERFICIE)) %>% 
    ungroup()
  
  
  #3.6 Joindre ces 2 indicateurs au jeu de données principal
  dfDonneesPoly <- 
    left_join(dfDonneesPoly, echelle_gen,
              by = c("SDOM_BIO", "GR_STATION", "TYF", "Enjeux_evo"))
  
  dfDonneesPoly <- 
    left_join(dfDonneesPoly, echelle_2g,
              by = c("SDOM_BIO", "GR_STATION", "TYF", "Enjeux_evo", "cl_vol3"))
  
  dfDonneesPoly <- 
    left_join(dfDonneesPoly, ratio_2g,
              by = c("SDOM_BIO", "GR_STATION", "TYF", "Enjeux_evo"))
  
  dfDonneesPoly <- 
    left_join(dfDonneesPoly, echelle_5g,
              by = c("SDOM_BIO", "GR_STATION", "TYF", "Enjeux_evo", 
                     "cl_vol3", "cl_vol5"))
  
  dfDonneesPoly <- 
    left_join(dfDonneesPoly, ratio_5g,
              by = c("SDOM_BIO", "GR_STATION", "TYF", "Enjeux_evo", "cl_vol3"))
  
  
  #3.7 Calculer la vraie courbe à utiliser avec ces 2 indicateurs
  #On met tous les SNATs (improductifs) comme des GE1 (les autres 
  #echèlles n'existent pas)
  dfDonneesPoly <- 
    dfDonneesPoly %>% 
    mutate_at(vars(starts_with("GE")), as.character) %>% 
    
    mutate(COURBE =
           
           #3.7.1 Improductifs sont toujours GE1
           case_when(
             .$Improd %in% "SNAT" ~ .$GE1,
             
             #3.7.2 Si sup GE5 >= supMinCourbe, on guarde les groupes GE5
             .$echelle_5 >= supMin_courbe & !is.na(.$GE5) ~ .$GE5,
             .$echelle_5 >= supMin_courbe & !is.na(.$GE3) ~ .$GE3,
             .$echelle_5 >= supMin_courbe & !is.na(.$GE1) ~ .$GE1,
             
             
             #3.7.3 Si sup GE5 < supMinCourbe MAIS sup v12/v34 >= supMinCourbe et
             #le ratio entre les 2 courbes (e.g. v1 et v2) est >= 0.8 et 
             #<= 1.2 (i.e. les 2  groupes sont bien equilibrés), on les donne 
             #un GE3
             .$echelle_5 < supMin_courbe & .$echelle_2 >= supMin_courbe &
               (.$ratio_5g >= 0.80 & .$ratio_5g <= 1.20) &
               (.$echelle_2 - .$echelle_5 < supMin_courbe) &
               !is.na(.$GE3) ~ .$GE3,
             
             .$echelle_5 < supMin_courbe & .$echelle_2 >= supMin_courbe &
               (.$ratio_5g >= 0.80 & .$ratio_5g <= 1.20) &
               (.$echelle_2 - .$echelle_5 < supMin_courbe) &
               !is.na(.$GE1) ~ .$GE1,
           
             
           #3.7.4 Si sup v12/v34 < supMinCourbe MAIS sup totale >= supMinCourbe et
           #le ratio entre les 2 courbes (v12 et v34) est >= 0.8 et <= 1.2 
           #(i.e. les 2 groupes sont bien equilibrés), on les donne un GE1
             .$echelle_2 < supMin_courbe & .$echelle_gen >= supMin_courbe &
             (.$ratio_2g >= 0.80 & .$ratio_2g <= 1.20) &
             (.$echelle_gen - .$echelle_2 < supMin_courbe) &
             !is.na(.$GE1) ~ .$GE1,
  
           
           #3.7.5 Pour toutes les autres courbes, on laise le GE5 qui va être
           #trop petit et va être regroupe avec DTW
             TRUE & !is.na(.$GE5) ~ .$GE5,
             TRUE & !is.na(.$GE3) ~ .$GE3,
             TRUE & !is.na(.$GE1) ~ .$GE1)) 

  
  
  #3.8 Ça se peut que la courbe choisie par l'algorithme d'échelle ci-dessus
  #choisi une courbe que n'existe pas dans le catalogue de courbes. Alors,
  #il faut trouver des compromis dans ces cas. Pour les trouver, on va
  #utiliser la fonction "verifierCourbesVolExist()" qu'on a écrit avant
  #le début de cette fonction
  #3.8.1 Assurer que les valeurs NA's de la classe de volume sont des vrais
  #NAs (et non le mot "NA" en caractères)
  compromisCatCourbes <- 
    catCourbes %>% 
    mutate(classe = ifelse(classe %in% c(NA, "NA", "na", "Na"), 
                           NA, as.character(classe))) %>% 
    
    #3.8.2 Sélectionner les valeurs uniques de ces combinaisons
    distinct(SDOM, GR_STATION, TYF, enjeux, classe, DESC_FAMC) %>%
    
    #3.8.3 Appliquer la fonction qu'on a déjà écrit
    group_by(SDOM, GR_STATION, TYF, enjeux) %>% 
    do(trouverCourbesCompromis(.)) %>% 
    ungroup() 
  
  #tempCompromisCatCourbes <- compromisCatCourbes
  #3.8.4 Sélectionner les 2 colonnes qu'on veut
  compromisCatCourbes <- 
    compromisCatCourbes %>% 
    mutate_all(as.character) %>% 
    select(SDOM, DESC_FAMC, DESC_FAMC_Comp) %>% 
    
    #3.8.5 Faire une corréction pour les SNAT (improductifs)
    mutate(DESC_FAMC = ifelse(grepl("SNAT", DESC_FAMC_Comp), 
                              DESC_FAMC_Comp, DESC_FAMC)) %>% 
    
    #3.8.6 Pour enlever des doublons SNAT
    distinct()
  
  
  #3.8.7 Utiliser ce catalogue pour trouver les courbes de compromis des
  #polygones
  dfDonneesPoly <- left_join(dfDonneesPoly,
                             compromisCatCourbes, 
                             by = c("SDOM_BIO" = "SDOM", "COURBE" = "DESC_FAMC"))
  
  
  #3.8.8 Faire un avertissement qui nous dit quelles courbes nous manquent
  courbesManq <- dfDonneesPoly %>% filter(is.na(DESC_FAMC_Comp))
  if(nrow(courbesManq) > 0){
    
    warning(nrow(courbesManq), " polygones ne sont pas étés regroupés parce que ",
            "les courbes ", paste(unique(courbesManq$COURBE), collapse = ", "),
            " n'existent pas dans le catalogue de courbes.")
  }
  
 
  #3.8.9 Enlever des peuplements qui n'ont pas une courbe et changer le nom
  #de la variable
  dfDonneesPoly <- 
    dfDonneesPoly %>% 
    filter(!is.na(DESC_FAMC_Comp)) %>% 
    select(-COURBE) %>%
    rename(COURBE = DESC_FAMC_Comp)
 

  
  #3.9 Déterminer la classec du polygone
  #3.9.1 Calculer le point maximale de chaque courbe
  dfDonneesPoly <-
    dfDonneesPoly %>%
    mutate(clage = as.numeric(as.character(clage)),
           classec = ifelse(clage <= 70, "1", "2"))

  
  
  
  #4. Créer la variable ID_COURBE (un identifiant unique de chaque groupe de
  #courbes) en concatenant les variables "COURBE" et "classec"
  #4.1 Créer la variable dans le jeu de données
  dfDonneesPoly <-
    dfDonneesPoly %>%
    mutate(COURBE = as.character(COURBE),
           ID_COURBE = paste0(COURBE, "_c", classec))
  
  #4.2 Créer cette variable dans le catalogue des courbes
  catCourbes <-
    catCourbes %>% 
    mutate(ID_COURBE = ifelse(classe %in% c("NA", "Na", "na", NA),
                              paste0(DESC_FAMC, "_c", classec),
                              paste0(DESC_FAMC, "_c", classec)))
  
  
  
  #5.Assigner les polygones qui sont dans des groupes évolutifs plus petits que la
  #superficie minimale définie à un groupe évolutif plus gros
  #5.0 Pour faire ça on va avoir besoin de plusieurs variables (SDOM_BIO,
  #Calculer le grand tyf
  dfDonneesPoly$grandTYF <- substr(dfDonneesPoly$TYF, 1, 2)
  catCourbes$grandTYF <- substr(catCourbes$TYF, 1, 2)
  
  #Calculer la famille de station
  dfDonneesPoly$FAM_STAT <- substr(dfDonneesPoly$GR_STATION, 1, 3)
  catCourbes$FAM_STAT <- substr(catCourbes$GR_STATION, 1, 3)
  
  #Ajouter le type de couvert
  dfDonneesPoly$typeCouv <- ifelse(dfDonneesPoly$TYF %in% c("Ep", "EpRx", "Sb",
                                                            "PgRx", "SbRx", "Pg",
                                                            "To", "ToRx", "Rx"),
                                   "R",
                                   ifelse(dfDonneesPoly$TYF %in% c("BjFx", "Es", "EsFx",
                                                                   "PeFx", "BpFx", "Fx"), 
                                          "F",
                                          "M"))
  catCourbes$typeCouv <- ifelse(catCourbes$TYF %in% c("Ep", "EpRx", "Sb",
                                                      "PgRx", "SbRx", "Pg",
                                                      "To", "ToRx", "Rx"),
                                "R",
                                ifelse(catCourbes$TYF %in% c("BjFx", "Es", "EsFx",
                                                             "PeFx", "BpFx", "Fx"), 
                                       "F",
                                       "M"))
 
  #5.1 Si on a des peuplements "SNAT", on va remplacer les valeurs de quelques
  #variables par des NA
  dfDonneesPoly <- 
    dfDonneesPoly %>% 
    mutate(FAM_STAT = ifelse(grepl("SNAT", COURBE), NA, as.character(FAM_STAT)),
           GR_STATION = ifelse(grepl("SNAT", COURBE), NA, as.character(GR_STATION)),
           TYF = ifelse(grepl("SNAT", COURBE), NA, as.character(TYF)),
           Enjeux_evo = ifelse(grepl("SNAT", COURBE), NA, as.character(Enjeux_evo)),
           Enjeux_str = ifelse(grepl("SNAT", COURBE), NA, as.character(Enjeux_str)),
           typeCouv = ifelse(grepl("SNAT", COURBE), NA, as.character(typeCouv)),
           grandTYF = ifelse(grepl("SNAT", COURBE), NA, as.character(grandTYF)))
  
  
 
  #5.2 Identifier les groupes évolutifs dont la courbe complète
  #(i.e. les 2 côtés) a moins que supMin_courbe
  idCourbesPetites <-
    dfDonneesPoly %>%
    
    #5.2.1 Calculer la superficie de chaque courbe
    group_by(COURBE) %>%
    summarise(sumSup = sum(SUPERFICIE)) %>%
    ungroup %>%
    
    #5.2.2 Filtrer ceux qui sont plus petits
    filter(sumSup < supMin_courbe) %>% 
    distinct(COURBE) %>% unlist %>% unname
  
  
  #5.2.3 Identifier les côtés de ces courbes qui sont plus 
  #petites que supMin_pointAttach
  idCotePetit <- 
    dfDonneesPoly %>%
    filter(!COURBE %in% idCourbesPetites) %>% 
    group_by(COURBE, classec) %>%
    summarise(sumSup = sum(SUPERFICIE)) %>%
    ungroup %>%
    filter(sumSup < supMin_pointAttach) %>% 
    distinct(COURBE, classec)
  
  
  #5.2.4. Identifier les polygones qui font partie de ces 2 jeux de données 
  courbesPetites <- 
    dfDonneesPoly %>% 
    filter(COURBE %in% idCourbesPetites |
             paste(COURBE, classec) %in% 
             paste(idCotePetit$COURBE, idCotePetit$classec)) %>% 
    
    
    #5.2.5 Chercher les courbes plus spécifiques (e.g. v1 au lieu de la générale)
    #5.2.5.1 Re-ajouter les courbes compromis en utilisant la GE5 au lieu
    #de la courbe choisie par l'algorithme
    ####################################################################
  #Correction SNAT et courbes manquantes. Si la plus détaillée n'est pas 
  #disponible, on essaie de mettre une autre
  mutate(GE5 = ifelse(grepl("SNAT", GE1), as.character(GE1), as.character(GE5)),
         GE5 = ifelse(GE5 %in% c(NA, "NA","Na","na"), 
                      as.character(GE1), as.character(GE5)),
         GE5 = ifelse(GE5 %in% c(NA, "NA","Na","na"), 
                      as.character(GE3), as.character(GE5))) %>% 
    ####################################################################
  
  left_join(compromisCatCourbes, by = c("GE5" = "DESC_FAMC")) %>%
    
    #5.2.5.2 Changer le nom de la courbe selon le compromis trouvé
    mutate(COURBE = DESC_FAMC_Comp, 
           ID_COURBE = paste0(COURBE, "_c", classec))
  
  
  #5.2.5.3 Mettre les variables "COURBE" et "ID_COURBE" de ces variables
  #à jour dans le jeu de données principal. Pour faire ça, il faut enlever
  #ces polygones du jeu de donnèes et ajouter le jeu de données des
  #courbes petites (où on vient de mettre ces variables à jour)
  dfDonneesPoly <- 
    dfDonneesPoly %>%  
    filter(!ID_BFEC %in% courbesPetites$ID_BFEC) %>% 
    bind_rows(courbesPetites) %>% 
    arrange(ID_BFEC)
  
  
  
  #5.2.6 Calculer la superficie des groupes évolutifs
  #(courbe + côté de la courbe = ID_COURBE)
  courbesPetites <- 
    courbesPetites %>% 
    group_by(ID_COURBE, COURBE, classec, SDOM_BIO, FAM_STAT, GR_STATION,
             TYF, Enjeux_evo, typeCouv, grandTYF) %>%
    summarise(sumSup = sum(SUPERFICIE)) %>%
    ungroup %>%
    
    
    #5.2.5 Créer des colonnes où on peut enregistrer les courbes auxquelles
    #ce groupes évolutifs trop petits ont été attachés
    mutate(courbeEquiv = NA,
           idEquiv = NA,
           condNumero = NA) #Pour stocker le numéro de la condition utilisée
  
  
  #5.3 Si on a des peuplements qui sont trop petits, il faut qu'on trouve la courbe
  #disponible la plus pareille avec le package "dtwclust"
  if(nrow(courbesPetites) > 0){
    
    #5.3.1 Créer le catalogue des courbes des groupes évolutifs qui ont une superficie
    #plus grosse que le minimum spécifié
    grosCatCourbes <-
      catCourbes %>%
      
      #5.3.2 On enlève les groupes évolutifs qui ne sont pas dans le jeu de
      filter(ID_COURBE %in% dfDonneesPoly$ID_COURBE,
             
             #Et on enlève ceux qui appartiennent à des courbes petites
             !ID_COURBE %in% c(NA, courbesPetites$ID_COURBE)) %>%
      
      ##########################################################
    # On ne fait plus cette selection selon les points non-extrapolés
    # #3.2.3 Sélectionner seulement les points non-extrapolés
    # 
    # extrapol %in% "non") %>%
    ##########################################################
    
    #5.3.4 Sélectionner les colonnes dont on a besoin
    select(ID_COURBE, classec, SDOM, FAM_STAT, GR_STATION, typeCouv, 
           grandTYF, TYF, enjeux, VOL_HA)
    
    
    #5.3.5 Comme la fonction a besoin d'une liste de courbes, on va créer
    #une liste où chaque élément est un vecteur de volumes (qui représente
    #la courbe correspondante). Évidement, on a besoin de 2 groupes de courbes : un
    #groupe de courbes de croissance et un de courbes de sénescence
    #5.3.5.1 Courbes de croissance
    grosCatCourbesCrois <-
      split(grosCatCourbes[grosCatCourbes$classec %in% "1", "VOL_HA"],
            f = grosCatCourbes[grosCatCourbes$classec %in% "1", "ID_COURBE"])
    grosCatCourbesCrois <- 
      lapply(grosCatCourbesCrois, function(x) unlist(unname(x)))
    
    #5.3.5.2 Courbes de sénescense
    grosCatCourbesSen <-
      split(grosCatCourbes[grosCatCourbes$classec %in% "2", "VOL_HA"],
            f = grosCatCourbes[grosCatCourbes$classec %in% "2", "ID_COURBE"])
    grosCatCourbesSen <- 
      lapply(grosCatCourbesSen, function(x) unlist(unname(x)))
    
    
    #5.4 Alors, on peut finalement trouver les courbes les plus proches avec une
    #boucle
    #5.4.0 D'abord il faut vérifier qu'on a une superficie totale de peuplements
    #improductifs assez grosse
    #5.4.0.1 Calculer la somme de toutes les peuplements improductifs
    supImprod <- 
      dfDonneesPoly %>% 
      filter(grepl("SNAT", COURBE)) %>% 
      summarise(sumSup = sum(SUPERFICIE)) %>% 
      unlist %>% unname
    
    #5.4.0.2 Si la superficie totale des improductifs est plus petite que
    #le seuil spécifié
    if(supImprod < supMin_courbe){
      
      #5.4.0.3 Identifier le groupe improductif le plus gros
      courbesImprod <- 
        dfDonneesPoly %>% 
        filter(grepl("SNAT", COURBE)) %>% 
        group_by(ID_COURBE, COURBE, classec) %>% 
        summarise(sumSup = sum(SUPERFICIE)) %>% 
        ungroup() %>% 
        arrange(desc(sumSup)) %>% 
        slice(1) %>% 
        select(ID_COURBE, COURBE, classec) %>%
        as.data.frame()
      
      
      #5.4.0.4 Créer le tableau qu'on va ajouter à l'extrant "dfCourbesPetites"
      improdCourbesPetites <- 
        dfDonneesPoly %>% 
        filter(grepl("SNAT", COURBE)) %>% 
        group_by(COURBE, classec) %>% 
        summarise(sumSup = sum(SUPERFICIE)) %>% 
        ungroup() %>% 
        mutate(courbeEquiv = courbesImprod[1, "COURBE"],
               condNumero = "cond6") %>% 
        rename(courbeOri = COURBE) %>% 
        select(courbeOri, courbeEquiv, classec, sumSup, condNumero)
      
      
      #5.4.0.5 On remplace la courbe dans le jeu de données principal
      dfDonneesPoly <- 
        dfDonneesPoly %>% 
        mutate(COURBE = ifelse(grepl("SNAT", COURBE), 
                               courbesImprod[1, "COURBE"],
                               as.character(COURBE)),
               ID_COURBE = ifelse(grepl("SNAT", ID_COURBE), 
                                  courbesImprod[1, "ID_COURBE"],
                                  as.character(ID_COURBE)),
               classec = ifelse(grepl("SNAT", classec), 
                                courbesImprod[1, "classec"],
                                as.character(classec)))
      
      
      #5.4.0.6 On enleve ces cas de l'objet "courbesPetites" parce qu'on n'a
      #plus besoin de faire des DTWs pour eux
      courbesPetites <- 
        courbesPetites %>% 
        filter(!grepl("SNAT", COURBE))
      
      
      #5.4.0.7 Créer un indicateur pour dire que la superficie des
      #improductifs était trop petite pour qu'on puisse l'ajouter 
      #au jeu de données des peuplements trop petits
      snat_trop_petit <- TRUE
      
      
    }
    
    
    
    # #5.4.0 Finalement il faut faire la même chose pour les EPCs
    # #5.4.0.1 Calculer la somme de toutes les EPCs
    # supEPC <- 
    #   dfDonneesPoly %>% 
    #   filter(Enjeux_evo %in% "EPC") %>% 
    #   summarise(sumSup = sum(SUPERFICIE)) %>% 
    #   unlist %>% unname
    # 
    # #5.4.0.2 Si la superficie totale des improductifs est plus petite que
    # #le seuil spécifié
    # if(supEPC < supMin_courbe){
    #   
    #   #5.4.0.3 Identifier le groupe EPC le plus gros
    #   courbesEPC <- 
    #     dfDonneesPoly %>% 
    #     filter(Enjeux_evo %in% "EPC") %>% 
    #     group_by(ID_COURBE, COURBE, classec) %>% 
    #     summarise(sumSup = sum(SUPERFICIE)) %>% 
    #     ungroup() %>% 
    #     arrange(desc(sumSup)) %>% 
    #     slice(1) %>% 
    #     select(ID_COURBE, COURBE, classec) %>%
    #     as.data.frame()
    #   
    #   
    #   #5.4.0.4 Créer le tableau qu'on va ajouter à l'extrant "dfCourbesPetites"
    #   EPC_CourbesPetites <- 
    #     dfDonneesPoly %>% 
    #     filter(Enjeux_evo %in% "EPC") %>% 
    #     group_by(COURBE, classec) %>% 
    #     summarise(sumSup = sum(SUPERFICIE)) %>% 
    #     ungroup() %>% 
    #     mutate(courbeEquiv = courbesEPC[1, "COURBE"],
    #            condNumero = "cond6") %>% 
    #     rename(courbeOri = COURBE) %>% 
    #     select(courbeOri, courbeEquiv, classec, sumSup, condNumero)
      # 
      # 
      # #5.4.0.5 On remplace la courbe dans le jeu de données principal
      # dfDonneesPoly <- 
      #   dfDonneesPoly %>% 
      #   mutate(COURBE = ifelse(Enjeux_evo %in% "EPC", 
      #                          courbesEPC[1, "COURBE"],
      #                          as.character(COURBE)),
      #          ID_COURBE = ifelse(Enjeux_evo %in% "EPC", 
      #                             courbesEPC[1, "ID_COURBE"],
      #                             as.character(ID_COURBE)),
      #          classec = ifelse(Enjeux_evo %in% "EPC", 
      #                           courbesEPC[1, "classec"],
      #                           as.character(classec)))
    #   
    #   
    #   #5.4.0.6 On enleve ces cas de l'objet "courbesPetites" parce qu'on n'a
    #   #plus besoin de faire des DTWs pour eux
    #   courbesPetites <- 
    #     courbesPetites %>% 
    #     filter(!Enjeux_evo %in% "EPC")
    #   
    #   
    #   #5.4.0.7 Créer un indicateur pour dire que la superficie des
    #   #EPCs était trop petite pour qu'on puisse l'ajouter 
    #   #au jeu de données des peuplements trop petits
    #   EPC_trop_petit <- TRUE
    #   
    #   
    # }
    
    
    
    for(i in 1:nrow(courbesPetites)){
      
      #5.4.1 D'abord on a besoin de savoir si on veut une courbe de croissance ou 
      #de sénescence. Pour décider quelle courbe utiliser (i.e. appliquer des
      #compromis selon le sous domaine, le type de couvert...) on a besoin
      #des courbes dans un dataframe et pour faire le clustering des courbes (DTW)
      #on a besoin des courbes dans une liste.
      
      #On fait une exception pour les SNATs: le côté de la courbe n'est pas 
      #vraiment important, et ça va nous aider à éviter de mélanger des SNATs
      #avec d'autres courbes
      if(grepl("SNAT", courbesPetites[i, "COURBE"])){
        
        list_tempCatCourbes <- c(grosCatCourbesCrois, grosCatCourbesSen)
        df_tempCatCourbes <- grosCatCourbes
        
      } else if(courbesPetites[i, "classec"] %in% "1"){
        
        list_tempCatCourbes <- grosCatCourbesCrois
        df_tempCatCourbes <- grosCatCourbes %>% filter(classec %in% "1")
        
      } else {
        
        list_tempCatCourbes <- grosCatCourbesSen
        df_tempCatCourbes <- grosCatCourbes %>% filter(classec %in% "2")
        
      }
      
      
      #5.4.2 Définer les filtres qu'on veut appliquer
      #5.4.2.1 Courbe équivalente dans un autre sous domaine AVEC l'enjeux
      cond_sDomEnj <- 
        paste(df_tempCatCourbes$GR_STATION, df_tempCatCourbes$TYF,
              df_tempCatCourbes$enjeux, sep = "_") %in%
        paste(courbesPetites$GR_STATION[i], courbesPetites$TYF[i],
              courbesPetites$Enjeux_evo[i], sep = "_") &
        !df_tempCatCourbes$enjeux %in% "EPC"  #on ne veux pas les EPCs
      
      #5.4.2.2 Courbe équivalente dans le même groupe de station et TYF
      cond_sDom <- 
        paste(df_tempCatCourbes$GR_STATION, df_tempCatCourbes$TYF, sep = "_") %in%
        paste(courbesPetites$GR_STATION[i], courbesPetites$TYF[i], sep = "_") &
        !df_tempCatCourbes$enjeux %in% "EPC"  #on ne veux pas les EPCs
      
      
      #5.4.2.3 Courbe équivalente dans le même famStat et TYF
      cond_famTyf <-
        paste(df_tempCatCourbes$FAM_STAT, df_tempCatCourbes$TYF, sep = "_") %in%
        paste(courbesPetites$FAM_STAT[i], courbesPetites$TYF[i], sep = "_") &
        !df_tempCatCourbes$enjeux %in% "EPC"  #on ne veux pas les EPCs
      
      
      #5.4.2.4 Courbe équivalente dans le même grStat, type de couvert,
      #mais un grand TYF différent 
      cond_grandTyfCouv <-
        paste(df_tempCatCourbes$GR_STATION, df_tempCatCourbes$typeCouv, 
              df_tempCatCourbes$grandTYF,sep = "_") %in%
        paste(courbesPetites$GR_STATION[i], courbesPetites$typeCouv[i], 
              courbesPetites$grandTYF[i],sep = "_") &
        !df_tempCatCourbes$enjeux %in% "EPC"  #on ne veux pas les EPCs
      
      
      #5.4.2.5 Courbe équivalente dans le même grStat et type de couvert
      cond_couv <-
        paste(df_tempCatCourbes$GR_STATION, df_tempCatCourbes$typeCouv, sep = "_") %in%
        paste(courbesPetites$GR_STATION[i], courbesPetites$typeCouv[i], sep = "_") &
        !df_tempCatCourbes$enjeux %in% "EPC"  #on ne veux pas les EPCs
      
      
      #5.4.2.6 Courbe équivalente dans le même TYF
      cond_tyf <- 
        paste(df_tempCatCourbes$TYF, sep = "_") %in%
        paste(courbesPetites$TYF[i], sep = "_") &
        !df_tempCatCourbes$enjeux %in% "EPC"  #on ne veux pas les EPCs
      
      
      #5.4.2.7 Courbe équivalente dans le même groupe de station
      cond_grStat <-
        paste(df_tempCatCourbes$GR_STATION, sep = "_") %in% 
        paste(courbesPetites$GR_STATION[i], sep = "_") &
        !df_tempCatCourbes$enjeux %in% "EPC"  #on ne veux pas les EPCs
      
      
      #5.4.2.8 N'importe quel courbe dans le même sous-domaine 
      cond_SDomSeule <-
        paste(df_tempCatCourbes$SDOM) %in% 
        paste(courbesPetites$SDOM_BIO[i]) &
        !df_tempCatCourbes$enjeux %in% "EPC"  #on ne veux pas les EPCs
      
     
      #5.4.2.9 Conditions des EPCs: 
      if(courbesPetites$Enjeux_evo[i] %in% "EPC"){
        
        courbesPetites_EPC <- courbesPetites[i,]
        
        #4.2.9.1 Laisser tomber le sdom
        cond_EPC_sDom <- 
          paste(df_tempCatCourbes$GR_STATION, df_tempCatCourbes$TYF, sep = "_") %in%
          paste(courbesPetites_EPC$GR_STATION, courbesPetites_EPC$TYF, 
                sep = "_") &
          df_tempCatCourbes$enjeux %in% "EPC" 
        
        #4.2.9.2 Laisser tomber le groupe de station
        cond_EPC_famStat <- 
          paste(df_tempCatCourbes$FAM_STAT, df_tempCatCourbes$TYF, sep = "_") %in%
          paste(courbesPetites_EPC$FAM_STAT, courbesPetites_EPC$TYF, 
                sep = "_") &
          df_tempCatCourbes$enjeux %in% "EPC" 
        
        #4.2.9.3 Guarder que le TYF
        cond_EPC_tyf <- 
         df_tempCatCourbes$TYF %in% courbesPetites_EPC$TYF &
          df_tempCatCourbes$enjeux %in% "EPC" 
        
        #4.2.9.4 Guarder que l'enjeux EPC
        cond_EPC_dernier <- 
          df_tempCatCourbes$enjeux %in% "EPC" 
     
      }

        
      
      #5.4.3 Appliquer les filters
      #5.4.3.1 Laisser tomber juste le sous domaine (en gardant l'enjeux)
      if(any(cond_sDomEnj)){
        
        #Extraire l'ID des courbes qui sont semblables
        nomFiltreCourbe <- 
          df_tempCatCourbes %>% filter(cond_sDomEnj) %>% 
          distinct(ID_COURBE) %>% unlist() %>% unname()
        
        #Mettre à jour le numéro de la condition utilisée
        condNumero <- "cond1"
        
        
        #5.4.3.2 Laisser tomber juste le sous domaine
      } else if (any(cond_sDom)){ 
        
        #Extraire l'ID des courbes qui sont semblables
        nomFiltreCourbe <- 
          df_tempCatCourbes %>% filter(cond_sDom) %>% 
          distinct(ID_COURBE) %>% unlist() %>% unname()
        
        #Mettre à jour le numéro de la condition utilisée
        condNumero <- "cond2"
        
        
        #5.4.3.3 Selon la famille de stattion et le TYF
      } else if (any(cond_famTyf)){ 
        
        #Extraire l'ID des courbes qui sont semblables
        nomFiltreCourbe <- 
          df_tempCatCourbes %>% filter(cond_famTyf) %>% 
          distinct(ID_COURBE) %>% unlist() %>% unname()
        
        #Mettre à jour le numéro de la condition utilisée
        condNumero <- "cond3"
        
        
        #5.4.3.4 Selon la famille de stattion et le TYF
      } else if (any(cond_grandTyfCouv)){ 
        
        #Extraire l'ID des courbes qui sont semblables
        nomFiltreCourbe <- 
          df_tempCatCourbes %>% filter(cond_grandTyfCouv) %>% 
          distinct(ID_COURBE) %>% unlist() %>% unname()
        
        #Mettre à jour le numéro de la condition utilisée
        condNumero <- "cond4"
        
        
        #5.4.3.5 Selon le type de couvert         
      } else if (any(cond_couv)){ 
        
        #Extraire l'ID des courbes qui sont semblables
        nomFiltreCourbe <- 
          df_tempCatCourbes %>% filter(cond_couv) %>% 
          distinct(ID_COURBE) %>% unlist() %>% unname()
        
        #Mettre à jour le numéro de la condition utilisée
        condNumero <- "cond5"
        
        
        #5.4.3.6 Selon le TYF
      } else if (any(cond_tyf)){ 
        
        #Extraire l'ID des courbes qui sont semblables
        nomFiltreCourbe <- 
          df_tempCatCourbes %>% filter(cond_tyf) %>% 
          distinct(ID_COURBE) %>% unlist() %>% unname()
        
        #Mettre à jour le numéro de la condition utilisée
        condNumero <- "cond6"
        
        
        #5.4.3.7 Selon le groupe de station
      } else if (any(cond_grStat)){
        
        #Extraire l'ID des courbes qui sont semblables
        nomFiltreCourbe <- 
          df_tempCatCourbes %>% filter(cond_grStat) %>% 
          distinct(ID_COURBE) %>% unlist() %>% unname()
        
        #Mettre à jour le numéro de la condition utilisée
        condNumero <- "cond7"
        
        
        #5.4.3.8 Finalement, si on a rien on sélectionne selon le SDOM_BIO
      } else if (any(cond_SDomSeule)){
        
        #Extraire l'ID des courbes qui sont semblables
        nomFiltreCourbe <- 
          df_tempCatCourbes %>% filter(cond_SDomSeule) %>% 
          distinct(ID_COURBE) %>% unlist() %>% unname()
        
        #Mettre à jour le numéro de la condition utilisée
        condNumero <- "cond8"
        
      } 
      
      # if(grepl("6O_RFi_F_Sb", courbesPetites$COURBE[i])){browser()}
      #5.4.3.9 Traiter des EPCs
      if(courbesPetites$Enjeux_evo[i] %in% "EPC"){
        
        #5.4.3.9.1 Condition EPC 1
        if(any(cond_EPC_sDom)){
          
          #Extraire l'ID des courbes qui sont semblables
          nomFiltreCourbe <- 
            df_tempCatCourbes %>% filter(cond_EPC_sDom ) %>% 
            distinct(ID_COURBE) %>% unlist() %>% unname()
          
          #Mettre à jour le numéro de la condition utilisée
          condNumero <- "cond_EPC1"
          
          
          #5.4.3.9.2 Condition EPC 2
        } else if (any(cond_EPC_famStat)){ 
          
          #Extraire l'ID des courbes qui sont semblables
          nomFiltreCourbe <- 
            df_tempCatCourbes %>% filter(cond_EPC_famStat  ) %>% 
            distinct(ID_COURBE) %>% unlist() %>% unname()
          
          #Mettre à jour le numéro de la condition utilisée
          condNumero <- "cond_EPC2"
          
          
          #5.4.3.9.3 Condition EPC 3
        } else if (any(cond_EPC_tyf)){ 
          
          #Extraire l'ID des courbes qui sont semblables
          nomFiltreCourbe <- 
            df_tempCatCourbes %>% filter(cond_EPC_tyf) %>% 
            distinct(ID_COURBE) %>% unlist() %>% unname()
          
          #Mettre à jour le numéro de la condition utilisée
          condNumero <- "cond_EPC3"
         
          
          #5.4.3.9.4 Condition EPC 4 
        } else if (any(cond_EPC_dernier)){ 
         
          #Extraire l'ID des courbes qui sont semblables
          nomFiltreCourbe <- 
            df_tempCatCourbes %>% filter(cond_EPC_dernier ) %>% 
            distinct(ID_COURBE) %>% unlist() %>% unname()
          
          #Mettre à jour le numéro de la condition utilisée
          condNumero <- "cond_EPC4"
        }
      }
      
      
      #5.4.4 Sélectionner ce sous-ensemble de courbes à partir du gros catalogue
      #5.4.4.1 Si on a trouvé des courbes avec ses conditions, on utilise 
      #les courbes trouvés. Si on n'a trouvé aucune courbe (ce qui peut arriver
      #quand on choisi des seuils trop élevés), on utilise toute le catalogue
      #disponible
      if(exists("nomFiltreCourbe")){
        
        list_tempCatCourbes <- list_tempCatCourbes[nomFiltreCourbe]
        rm(nomFiltreCourbe)
      } 
      
      
      #5.4.5 Après on attache la courbe qu'on veut comparer à la fin de cette liste
      list_tempCatCourbes$tempCourbe <-
        catCourbes %>%
        filter(ID_COURBE %in% courbesPetites[i, "ID_COURBE"]) %>%
        select(VOL_HA) %>% unlist %>% unname
      
      if(length(list_tempCatCourbes$tempCourbe) == 0){
        
        miss_Courbe <- unlist(unname(courbesPetites[i, "COURBE"]))
        miss_IDBFEC <- 
          dfDonneesPoly %>% 
          filter(COURBE %in% miss_Courbe) %>% 
          select(ID_BFEC) %>% unlist %>% unname
        
        stop("La courbe ", miss_Courbe,
             " n'existe pas dans le catalogue de courbes. Voici la liste ",
             "de l'ID BFEC des polygones qui ont cette courbe: ", 
             paste(miss_IDBFEC, collapse = ", "))
      }
      
      
      #5.4.6 Maintenant on calcule les distances
      tempDtw <-
        tsclust(series = list_tempCatCourbes,
                
                #La méthode de distance à utiliser
                distance = "DTW",
                
                #Le nombre de clusters (on n'a pas vraiment besoin de ça parce qu'on
                #veut juste sélectionner la distance entre les courbes)
                k = 2,
                
                #Des arguments extra à passer à la fonction. On défine
                #que les courbes commencent toutes à la même année (open.begin = FALSE),
                #mais qu'elles peuvent finir dans des années différentes
                #(open.end = TRUE). Même si c'est pas vraiment notre cas, des testes
                #préliminaires on montré que la fonction est plus précise si on
                #le laisse comme ça. Le step pattern que j'ai utilisé est le
                #"asymmetric parce que c'est "normalizable" (une caracteristique
                #obligatoire quand on compare des courbes avec des longueures
                #différentes) et semble bien marcher
                args = tsclust_args(dist = list(open.end = TRUE,
                                                open.begin = FALSE,
                                                step.pattern = asymmetric)),
                
                #On va augmenter le numéro maximale d'itérations au cas où 
                #quelques courbes on de la misère à converger
                control = partitional_control(iter.max = 250L))
      
      
      #5.5 On extrait l'id de la courbe la plus pareille à la courbe (i.e. celle
      #avec la distance la plus petite)
      #5.5.1 On extrait la colonne de notre courbe de la matrice de distance
      idPlusProche <- tempDtw@distmat[, "tempCourbe"]
      
      #5.5.2 Enlever la courbe 'tempCourbe' pour laquelle on veut trouver la courbe
      #la plus proche
      idPlusProche <- idPlusProche[!names(idPlusProche) == 'tempCourbe']
      
      #5.5.3 On l'organize par ordre croissante de la valeur de la distance 
      idPlusProche <- idPlusProche[order(idPlusProche)]
      
      #5.5.4 Et on extrait le premier élément du vecteur (i.e. cellui
      #avec la distance la plus petite)
      idPlusProche <- names(idPlusProche)[1]
      
      #5.5.5 On a aussi besoin de trouver la courbe las plus proche (pour la
      #remplacer après)
      courbePlusProche <-
        catCourbes %>%
        filter(ID_COURBE %in% idPlusProche) %>%
        distinct(DESC_FAMC) %>% slice(1) %>%  #select first row just in case there's an error
        unlist %>% unname
      
      
      #5.6 Maintenant on a juste besoin de remplacer la courbe et l'id des
      #polygones dans le jeu de données principal
      dfDonneesPoly <-
        dfDonneesPoly %>%
        mutate(ID_COURBE = ifelse(ID_COURBE %in% courbesPetites[i, "ID_COURBE"] & 
                                    classec %in% courbesPetites[i, "classec"],
                                  idPlusProche,
                                  as.character(ID_COURBE)),
               
               COURBE = ifelse(COURBE %in% courbesPetites[i, "COURBE"] & 
                                 classec %in% courbesPetites[i, "classec"],
                               courbePlusProche,
                               as.character(COURBE)))
      
      #5.7 Mettre le data frame avec les courbes petites à jour en ajoutant
      #la courbe qui a été attribuée à ce groupe évolutif
      courbesPetites$idEquiv[i] <- idPlusProche
      courbesPetites$courbeEquiv[i] <- courbePlusProche
      courbesPetites$condNumero[i] <- condNumero
      
    }
    
    
    #5.8 Sélectionner les variables qu'on veut garder dans l'extrant
    courbesPetites <- 
      courbesPetites %>% 
      transmute(courbeOri = COURBE, 
                courbeEquiv, classec, sumSup, condNumero)
    
    
    #5.9 Si la superficie des Improds était trop petite, on les a mis tous
    #dans un seul groupe mais on n'a pas fait de DTW. Alors, il faut rajouter
    #ça à l'extrant des courbes petites maintenant
    if(exists("snat_trop_petit")){
      
      courbesPetites <- bind_rows(courbesPetites, improdCourbesPetites)
    }
    
    # #5.10 Si la superficie des Improds était trop petite, on les a mis tous
    # #dans un seul groupe mais on n'a pas fait de DTW. Alors, il faut rajouter
    # #ça à l'extrant des courbes petites maintenant
    # if(exists("EPC_trop_petit")){
    #   
    #   courbesPetites <- bind_rows(courbesPetites, EPC_CourbesPetites)
    # }
    
    
    #5.11 Faire un petit avertissement pour dire au utilisateur si on a 
    #changé les courbes de quelques groupes évolutifs
    warning(paste0(nrow(courbesPetites), " groupes évolutifs avaient une superficie ",
                   "totale inférieure à la superficie minimale spécifiée (", 
                   supMin_pointAttach, "HA). Pour chaque cas la courbe la plus semblable ",  
                   "parmi toute les courbes disponibles a été trouvée avec une ",
                   "méthodologie de déformation temporelle dynamique (DTW). Les ",
                   "courbes trouvées ont été stockées dans 'dfCourbesPetites'."))
    
  }
  
  
  
  #6. Le clustering avec k-NN (k plus proches voisins)
  #Maintenant on peut faire le clustering avec `k-NN` dans une boucle. Voici la structure 
  #du processus:  
  #1.Définir les paramètres de la fonction (ex. superficie minimale de chaque cluster)
  #2 Sélectionner les points d'attachement sur la courbe. Quand le nombre de points 
  #disponibles est plus gros que le nombre maximale spécifié, on enlève le point qui a 
  #0m3/HA (s'il existe), et on choisit les points d'attachement les plus représentatifs
  #3.Pour chaque groupe, on a besoin de créer un jeu de données d'entraînement pour 
  #entraîner le modèle KNN. Le jeu de données est une séquence de points de volume qui 
  #vont d'une valeur plus petite que le point d'attachement plus petit à une valeur 
  #plus grosse que celle du dernier point d'attachement. Ces points sont classifiés 
  #selon les point d'attachement correspondants
  #4. On fait le KNN 
  #5. On contrôle la superficie des clusters. Quand on a des clusters avec une superficie 
  #plus petite que celle spécifiée par `supMin_pointAttach`, on élimine séquentiellement les 
  #clusters plus petits en les reclassifiant avec la valeur du point d'attachement le 
  #plus proche (ex., si on a 4 points (20, 25, 40, 50), dont le point 25 est le 
  #plus petit, on calcule la différence absolue entre ce cluster et les autres 
  #(cluster20: 5; cluster40: 15; cluster50: 25); dans ce cas on reclassifie les points 
  #du cluster 25 en 20). Si la somme de la superficie de tous les clusters est toujours 
  #inferieure à la limite spécifiée, on fait sortir un seul cluster qui est plus petit 
  #que la limite spécifiée.
  dfDonneesPoly <- 
    dfDonneesPoly %>% select(ID_BFEC, ID_COURBE, COURBE, classec, Improd,
                             Enjeux_evo, Enjeux_str, SUPERFICIE, v_TOT)
  
  
  
  #6.0 Pour qu'on s'assure qu'on a toujours les mêmes résultats quand on 
  #utilise des méthodes stochastiques
  set.seed(333)  
  
  #6.1 D'abord on initialise une boucle où on va faire le clustering par groupe ID_COURBE
  for(i in unique(dfDonneesPoly$ID_COURBE)){ 
    
    #6.2 Extraire le groupe "i" 
    tempDonnees <- dfDonneesPoly %>% filter(ID_COURBE %in% i)
    
    #6.3 Extraire les points d'attachement correspondants
    tempAttach <- 
      catCourbes %>% 
      #6.3.1 Sélectionner la courbe correcte
      filter(DESC_FAMC %in% unique(tempDonnees$COURBE)[1],
             #6.3.2 Sélectionner le bon côté de la courbe
             classec %in% unique(tempDonnees$classec)[1]) %>% 
      #6.3.3 Sélectionner la colonne du volume
      select(VOL_HA) %>% unlist %>% unname
    
    
    #6.4 Il faut contrôler le nombre de clusters maximale qu'on a dans chaque groupe. 
    #6.4.1 D'abord, on sélectionne le nombre maximal de clusters selon le côté
    #de la courbe (croissance (classec == 1) vs senescence (classec == 2))
    #Par contre, si on a des SNAT on veut un maximum de 2 points d'attachement
    #de chaque côté
    if(any(grepl("SNAT", tempDonnees$COURBE))){
      
      nombreMaxCluster <- 2
      
    } else if (unique(tempDonnees$classec) %in% "1"){
      
      nombreMaxCluster <- nombreMaxClusterCroissance
      
    } else { #si le groupe est du côté droit de la courbe de croissance:
      
      nombreMaxCluster <- nombreMaxClusterSenescence
      
    } 
    
    
    #6.4.2 On commence par éliminer des points d'attachement qu'ont un volume de 0
    #(s'ils existent)
    if(any(tempAttach %in% 0)){
      tempAttach <- tempAttach[tempAttach > 0]
    }
    
    
    #6.4.3 Si on a toujours plus de points que ce qu'on devrait avoir, il faut choisir
    #les points d'attachement les plus représentatifs. Pour faire ça, on va créer une 
    #séquence de la valeur minimale à la valeur maximale des points d'attachement qui 
    #a une longueur égale au nombre maximal de clusters qu'on veut. Après, on sélectionne 
    #les points d'attachement disponibles les plus proches de ces points moyens.
    #6.4.3.1 Créer la séquence de points moyens
    seqAttach <- seq(from = min(tempAttach), to = max(tempAttach),
                     length.out = nombreMaxCluster) #longueur : nombre max de clusters désiré
    
    #6.4.3.2 Trouver la différence entre ces points equidistantes et les points d'attachement 
    #disponibles
    rankAttach <- 
      
      #mapply permet-nous d'appliquer des fonctions multivariées vectorielles
      mapply(
        function(x,y){
          which.min(        #on va sélectionner l'index de la valeur la plus petite
            abs(x - y))     #de la différence absolue entre les points d'attachement
        },                    #moyens et les points disponibles
        x = seqAttach,    #cette fonction est appliquée a chaque élément de x
        MoreArgs = list(y = tempAttach))  #en utilisant tous les éléments de y
    
    #6.4.3.3 Au cas où on a des valeurs répétées, on peut les enlever
    rankAttach <- unique(rankAttach)
    
    #6.4.3.4 Maintenant il faut juste sélectionner les points qu'on a choisis
    tempAttach <- tempAttach[rankAttach]
    
    
    
    #6.5 Pour chaque groupe, on va avoir besoin de créer un jeu de données d'entraînement
    #pour entraîner le modèle KNN avec les points d'attachement du groupe évolutif choisi.
    #Pour le faire, on a besoin d'identifier le point moyen entre la valeur de chaque
    #cluster et ses clusters voisins, créer une séquence de valeurs qui couvrent tous
    #les valeurs proches des clusters en les attribuant la valeur du cluster plus proche
    #6.5.1 Calculer les points moyens entre les clusters
    midpoints <- tempAttach[-length(tempAttach)] + diff(tempAttach)/2
    
    
    #6.5.2 Faire deux vecteur de plus avec ça: lagMidpoints, qui va être
    #le vecteur "midpoints" avec les éléments déplacés une fois en arrière;
    #et leadMidpoints, qui va être le vecteur "midpoints" avec les éléments 
    #déplacés une fois en avant (ex. si midpoints = 1,2,3, 
    #lagMidpoints = NA, 1, 2 et leadMidpoints = 2, 3, NA)   
    #6.5.2.1 Ajouter des valeurs sensibles à la fin et au début de la 
    #séquence (-5 et +20 doit bien marcher)
    midpoints <- c(midpoints[1]-5, midpoints, last(midpoints) + 20)
    
    #6.5.2.2 Faire ces vecteurs en éliminant les NAs que les fonctions génèrent
    lagMidpoints <- lag(midpoints)[2:length(midpoints)]
    leadMidPoints <- lead(midpoints)[1:(length(midpoints)-1)]
    
    #6.5.2.3 Faire une séquence de 20 éléments pour chaque cluster 
    #qui couvre la valeur du cluster +/- la valeur moyen entre le cluster
    #et ses clusters voisins (ex. pour le cluster "6" d'une liste de 3 
    #clusters (4, 6 et 8), on va faire une séquence de 5 à 7 de 20 éléments
    seqMidpoints <- 
      mapply(seq,
             from = lagMidpoints, to = leadMidPoints,
             length.out = 30)
    
    #6.5.2.4 La dernière fonction nous a créé un data frame de 30 lignes avec
    #un nombre de colonnes égal au nombre de clusters. Il faut la transformer 
    #dans un vecteur. Comme les colonnes sont bien organisées, on a juste besoin
    #de faire "as.numeric()"
    seqMidpoints <- as.numeric(seqMidpoints)
    
    
    #6.5.2.5 Maintenant il faut faire le data frame d'entraînement final qui a 2
    #colonnes : le volume de chaque point d'entraînement et le cluster auquel 
    #il est associé
    tempTrain <- data.frame(cluster = rep(tempAttach, each = 30),
                            v_TOT = seqMidpoints)
    
    
    
    #6.6 On peut finalement faire le clustering avec k-NN pour ce groupe avec "knn()"
    #Avec "$" on ajoute automatiquement le résultat du cluster à une colonne
    #de l'objet "tempDonnees"
    tempDonnees$clusterAttach <- 
      knn(train = tempTrain[,"v_TOT", drop = FALSE],   #les valeurs d'entraînement
          cl = tempTrain$cluster,  #les clusters correspondants aux valeurs d'entraînement
          test = tempDonnees[,"v_TOT", drop = FALSE],  #les valeurs qu'on veut prédire
          k = 5)     #le nombre de voisins à chercher
    
    tempDonnees$clusterAttach <- as.character(tempDonnees$clusterAttach)
    
    
    
    #6.7 Alors, il faut contrôler la superficie minimale de chaque cluster
    #6.7.1 Calculer la superficie de chaque cluster
    supCluster <- 
      tempDonnees %>% 
      group_by(clusterAttach) %>% 
      summarise(supCluster = sum(SUPERFICIE)) %>% 
      ungroup %>% 
      mutate(clusterAttach = as.numeric(as.character(clusterAttach)))
    
    #6.7.2 Pendant qu'on a au moins un cluster plus petit que la superficie
    #minimale demandée (supMin_pointAttach)
    while(any(supCluster$supCluster < supMin_pointAttach)){
      
      #6.7.2.0 Alors, si on a déjà fait plusieurs itérations de cette boucle et
      #on a juste un point d'attachement dans la courbe parce
      #que ce groupe est trop petit, il faut passer au prochain groupe
      if(nrow(supCluster) %in% 1){
        break
      }
      
      
      #6.7.2.1 On identifie la valeur du cluster le plus petit
      clusterPlusPetit <- 
        supCluster %>% 
        top_n(wt = supCluster, -1) %>% 
        select(clusterAttach) %>% 
        slice(1) %>% #ou cas où il y a 2 clusters avec la même superficie
        unlist %>% unname 
      
      
      
      #6.7.2.2 On détermine la différence entre les clusters
      clusterDiff <- abs(clusterPlusPetit - supCluster$clusterAttach)
      
      
      #6.7.2.3 On sélectionne la différence la plus petite (sans lui-même, aka
      #clusterDiff > 0)
      clusterDiff <- min(clusterDiff[clusterDiff > 0])
      
      
      #6.7.2.4 On détermine l'index du cluster le plus proche (i.e. son 
      #positionnement dans le data frame)
      clusterDiffIndex <- 
        which(abs(clusterPlusPetit - supCluster$clusterAttach) %in% clusterDiff)
      
      
      #6.7.2.5 On sélectionne ça valeur
      clusterVoisin <- 
        supCluster %>% 
        slice(clusterDiffIndex) %>% 
        select(clusterAttach) %>% unlist
      
      
      #6.7.2.6 On change les valeurs dans le jeu de données principal avec ifelse
      tempDonnees$clusterAttach <-
        #Aux polygones du cluster le plus petit
        ifelse(tempDonnees$clusterAttach %in% as.character(clusterPlusPetit),
               #On les assigne la valeur du cluster le plus proche
               as.character(clusterVoisin), 
               #Sinon on fait rien
               tempDonnees$clusterAttach)
      
      
      #6.7.2.7 Il faut recalculer l'objet de la somme des superficies qu'on
      #utilise dans cette boucle "while" pour éviter une boucle infinie
      supCluster <- 
        tempDonnees %>% 
        group_by(clusterAttach) %>% 
        summarise(supCluster = sum(SUPERFICIE)) %>% 
        ungroup %>% 
        mutate(clusterAttach = as.numeric(as.character(clusterAttach)))
      
      
    }
    
    
    
    #6.8 Finalement, on fusionne les résultats dans un seul objet d'extrant
    #6.8.1 Si c'est la première tournée de la boucle (i.e. si la variable 
    #"donneesCluster" n'existe pas encore), on la crée avec "tempDonnees"...
    if(!exists("donneesCluster")){
      
      donneesCluster <- tempDonnees
      
      
    } else { #...sinon on ajoute les lignes du nouveau groupe
      
      donneesCluster <- bind_rows(donneesCluster, tempDonnees)
      
    }
    
    
    #6.8.2 À la fin de chaque itération de la boucle, on peut effacer les 
    #objets intermédiaires
    rm(tempDonnees, tempAttach, tempTrain, seqMidpoints, midpoints, 
       lagMidpoints, leadMidPoints)
    
  }  #On ferme la boucle qui fait le knn 
  #pour chaque groupe évolutif. Le résultat
  #est stocké dans donneesCluster
  
  
  
  #6.9 Vérifier si on peut cinder des points d'attachement en 2 s'ils ont plus
  #de "supMin_pointAttach" de superficie
  #6.9.1 Calculer la somme de la superficie de chaque COURBE + 
  #point d'attachement + Enjeux_strategique
  enjeuxConf <- 
    donneesCluster %>% 
    group_by(ID_COURBE, clusterAttach, Enjeux_str) %>% 
    summarise(sumSup = sum(SUPERFICIE)) %>% 
    
    #6.9.2 S'il y a des clusters qui ont moins que la superficie minimale,
    #on va les donner la valeur de l'enjeux dominante du point d'attachement
    #correspondante. Si les groupes qui ont un enjeux sont plus grands que
    #la superficie minimale, on garde l'enjeux
    arrange(desc(sumSup)) %>% 
    mutate(Enjeux_strConf = ifelse(sumSup >= supMin_pointAttach, 
                                   as.character(Enjeux_str), 
                                   
                                   #Comme chaque groupe est organisé selon l'ordre décroissante
                                   #de la superficie, en sélectionnant le prémier élément 
                                   #de l'enjeux, on choisit l'enjeux dominante
                                   as.character(Enjeux_str[1]))) %>% 
    select(-sumSup)
  
  
  #6.9.3 Joindre l'enjeux confirmé au jeu de données principal
  donneesCluster <- left_join(donneesCluster, enjeuxConf,
                              by = c("ID_COURBE", "clusterAttach", "Enjeux_str")) 
  
  
  #7. Ajouter le NOM_FAMC du catalogue de courbes
  #7.1 Sélectionner les colonnes qu'on veut
  nomFam <-
    catCourbes %>%
    select(DESC_FAMC, NOM_FAMC) %>%
    distinct()
  
  #7.2 Faire le join
  donneesCluster <- 
    left_join(donneesCluster, nomFam, by = c("COURBE" = "DESC_FAMC"))
  
  
  
  #8. Créer les 2 des 3 dataframes d'extrant: un avec l'id des polygones et 
  #le point d'attachement sur la courbe; et un avec la COURBE et le point 
  #d'attachement sur la courbe
  #8.1 Ajouter l'âge d'attachement au jeu de données principal
  donneesCluster <- 
    left_join(donneesCluster,
              catCourbes %>% transmute(ID_COURBE, 
                                       clusterAttach = as.character(VOL_HA), 
                                       ageAttach = age),
              by = c("ID_COURBE", "clusterAttach")) %>% 
    
    #Enlever des doublons possibles à cause des points d'attachement qui ont
    #des âges différentes mais des volumes pareilles
    distinct(ID_BFEC, .keep_all = TRUE)  
                                         
  
  
  
  #8.2 Créer le dataframe des polygones en sélectionnant les 2 colonnes qu'on 
  #veut. Il faut d'abord joindre l'âge de la courbe au point d'attachement
  dfPoly <- 
    donneesCluster %>% 
    select(ID_BFEC, classec, COURBE, NOM_FAMC, 
           Enjeux_strConf, clusterAttach, ageAttach)
  
  
  #8.3 Créer le dataframe des strates
  dfStrates <- 
    donneesCluster %>% 
    
    #6.10.2.1 Regrouper le jeu de données selon les variables qu'on veut
    group_by(NOM_FAMC, COURBE, Enjeux_strConf, classec, clusterAttach, ageAttach) %>% 
    
    #6.10.2.2 Calculer la somme de la superficie de chaque GE
    summarise(SUPERFICIE = sum(SUPERFICIE)) %>% 
    ungroup() %>% 
    
    #6.10.2.3 Assurer la bonne ordre des points d'attachement
    mutate(clusterAttach = as.numeric(clusterAttach)) %>% 
    arrange(COURBE, Enjeux_strConf, classec, clusterAttach) %>% 
    mutate(clusterAttach = as.character(clusterAttach))
  
  
  #6.10.3 Créer une liste avec ces 2 jeux de données plus le data frame qui a 
  #les courbes auxquelles les groupes évolutifs trop petits ont été attachés
  listeKnn <- list(dfPoly = dfPoly, 
                   dfStrates = dfStrates,
                   dfCourbesPetites = as.data.frame(courbesPetites))
  
  
  #6.10 Faire "return" de cette liste pour finir la fonction
  return(listeKnn)   
  
}


