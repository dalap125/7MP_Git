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
  varsCatCourbes <- c("DESC_FAMC", "age", "SDOM",    #le sous-domaine
                   "GR_STATION", "TYF", "enjeux",   #l'enjeux stratégique (e.g. En95)
                   "classe",   #classe de volume (v1, v2,...)
                   "VOL_HA", "classec")     #Le côté de la courbe


  
  #2.1.1.2  S'il y a au moins une variable manquante
  if(!all(varsCatCourbes %in% names(catCourbes))){
    
    #On l'identifie
    varsManq <- varsCatCourbes[!varsCatCourbes %in% 
                                  names(catCourbes)]
    
    #Et on arr?te la fonction
    stop("La ou les variables ", paste(varsManq, collapse = ", "),
         " ne sont pas pr?sentes dans le jeu de donn?es d?fini par 'catCourbes'.",
         "Faites attention que cette fonction est sensible aux minuscules ",
         "et aux majuscules.")
  }
  
  
  #2.1.2 Jeu de données
  #2.1.2.1 Identifier les variables nécessaires
  varsDonneesPoly <- c("ID_BFEC", "v_TOT", "SDOM_BIO", "GR_STATION", 
                       "TYF", "Enjeux_evo", "Enjeux_str", 
                       "cl_vol3", "cl_vol5", "GE1", "GE3", "GE5",
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
  
  
  
  
  ##############################################################
  # ##############################################################
  # ##############################################################
  # #2.2 Vérifier s'ils nous manquent des courbes dans le catalogue
  # #Aucune courbe ne devrait jamais manquer, mais c'est une bonne idée de le vérifier avant
  # #de faire rouler le reste du script
  # 
  # #Alors, s'il y a au moins un polygone dans le jeu de données sans une courbe
  # #correspondante dans le catalogue de courbes...
  # if(any(!as.character(dfDonneesPoly$COURBE) %in% catCourbes$DESC_FAMC)){
  #   
  #   ###   Prenez note que dans la déclaration ci-dessus le "!"   ###
  #   ### transforme la déclaration dans une déclaration négative. ###
  #   
  #   #... on identifie les courbes qui manquent...
  #   courbesManq <-
  #     dfDonneesPoly %>%
  #     filter(!COURBE %in% catCourbes$DESC_FAMC) %>%  #Sélectionner les courbes qui n'ont pas un match
  #     select(COURBE) %>%                            #Sélectionner la colonne des courbes
  #     unlist() %>% unname() %>%                     #Convertir cette colonne dans un vecteur
  #     unique() %>% as.character()                   #et sélectionner juste les valeurs uniques
  #   
  #   #Et on arrête la fonction
  #   stop("Les courbes suivantes ne sont pas présentes dans le catalogue de courbes :   ",
  #        paste(courbesManq, collapse = "   "))
  #   
  # }
  
  
  
  #2.3 Vérifier les données manquantes
  #Il faut aussi vérifier qu'on n'a pas des données de volume manquantes. Ça devrait
  #jamais arriver non plus.
  #2.3.1 Si au moins un polygone n'a pas une valeur de volume total
  if(any(is.na(dfDonneesPoly$v_TOT)) ){
    
    #2.3.2 Identifier les groupes où ça a arrivé 
    volManq <- 
      dfDonneesPoly %>% 
      filter(v_TOT %in% NA) %>%   #Trouver les lignes avec NA
      select(COURBE) %>% unlist() %>% unname() %>%  #Sélectionner la colonne "COURBE"
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
  
  
  #3.1 Calculer si un groupe rentre dans les generales (i.e courbe NA au
  #lieu de v12 ou v1)
  #3.1.1 Enlever les v5
  echelle_gen <- 
    dfDonneesPoly %>% 
    filter(cl_vol3 %in% c("v12", "v34")) %>% 
    
    #3.1.2 Calculer la superficie de chaque GE + v12 et v34
    group_by(SDOM_BIO, GR_STATION, TYF, Enjeux_evo, cl_vol3) %>% 
    summarise(supEch3 = sum(SUPERFICIE)) %>% 
    
    #3.1.3 Déterminer si les valeurs sont plus gros ou plus petits
    #que le minimum spécifiée
    mutate(supGrande = ifelse(supEch3 >= supMin_courbe, TRUE, FALSE)) %>% 
    
    #3.1.4 Voir s'il y a au moins une valeur FAUSSE pour chaque groupe. Alors,
    #si toutes les valeurs sont vraies, la somme des TRUES devrait donner
    #le nombre d'éléments du groupe (TRUE == 1 dans une somme)
    summarise(echelle_gen = ifelse(sum(supGrande) < n(), TRUE, FALSE))
  
  
  #3.2 Faire la même chose pour la v12 et v34
  echelle_3 <- 
    dfDonneesPoly %>% 
    filter(cl_vol5 %in% c("v1", "v2", "v3", "v4")) %>% 
    group_by(SDOM_BIO, GR_STATION, TYF, Enjeux_evo, cl_vol5) %>% 
    summarise(supEch3 = sum(SUPERFICIE)) %>% 
    mutate(supGrande = ifelse(supEch3 >= supMin_courbe, TRUE, FALSE)) %>% 
    summarise(echelle_3 = ifelse(sum(supGrande) < n(), TRUE, FALSE))
  
  
  #3.3 Joindre ces 2 indicateurs au jeu de données principal
  dfDonneesPoly <- 
    left_join(dfDonneesPoly, echelle_gen,
              by = c("SDOM_BIO", "GR_STATION", "TYF", "Enjeux_evo"))
  
  dfDonneesPoly <- 
    left_join(dfDonneesPoly, echelle_3,
              by = c("SDOM_BIO", "GR_STATION", "TYF", "Enjeux_evo"))
  
  
  #3.4 Calculer la vraie courbe à utiliser avec ces 2 indicateurs
  dfDonneesPoly <- 
    dfDonneesPoly %>% 
    mutate(COURBE = ifelse(echelle_gen %in% TRUE, as.character(GE1),
                           ifelse(echelle_3 %in% TRUE, as.character(GE3),
                                  as.character(GE5))))
  
  #3.5 Ça se peut que la courbe choisie par l'algorithme d'échelle ci-dessus
  #choisi une courbe que n'existe pas dans le catalogue de courbes. Alors,
  #il faut trouver des compromis dans ces cas. Pour les trouver, on va
  #utiliser la fonction "verifierCourbesVolExist()" qu'on a écrit avant
  #le début de cette fonction
  #3.5.1 Assurer que les valeurs NA's de la classe de volume sont des vrais
  #NAs (et non le mot "NA" en caractères)
  compromisCatCourbes <- 
    catCourbes %>% 
    mutate(classe = ifelse(classe %in% c(NA, "NA", "na", "Na"), 
                           NA, as.character(classe))) %>% 
    
    #3.5.2 Sélectionner les valeurs uniques de ces combinaisons
    distinct(SDOM, GR_STATION, TYF, enjeux, classe, DESC_FAMC) %>%
    
    #3.5.3 Appliquer la fonction qu'on a déjà écrit
    group_by(SDOM, GR_STATION, TYF, enjeux) %>% 
    do(trouverCourbesCompromis(.)) %>% 
    ungroup() 
    
  #tempCompromisCatCourbes <- compromisCatCourbes
    #3.5.4 Sélectionner les 2 colonnes qu'on veut
  compromisCatCourbes <- 
    compromisCatCourbes %>% 
    mutate_all(as.character) %>% 
    select(SDOM, DESC_FAMC, DESC_FAMC_Comp)
  
 
  #3.5.5 Utiliser ce catalogue pour trouver les courbes de compromis des
  #polygones
  dfDonneesPoly <- left_join(dfDonneesPoly,
                             distinct(compromisCatCourbes), #distinct à causde des SNATs...
                   by = c("SDOM_BIO" = "SDOM", "COURBE" = "DESC_FAMC"))
  
  #3.5.6 Faire un avertissement qui nous dit quelles courbes nous manquent
  courbesManq <- dfDonneesPoly %>% filter(is.na(DESC_FAMC_Comp))
  if(nrow(courbesManq > 0)){
   
     warning(nrow(courbesManq), " polygones ne sont pas étés regroupés parce que ",
             "les courbes ", paste(unique(courbesManq$COURBE), collapse = ", "),
             " n'existent pas dans le catalogue de courbes.")
  }
  
  #3.5.7 Enlever des peuplements qui n'ont pas une courbe et changer le nom
  #de la variable
  dfDonneesPoly <- 
    dfDonneesPoly %>% 
    filter(!is.na(DESC_FAMC_Comp)) %>% 
    select(-COURBE) %>%
    rename(COURBE = DESC_FAMC_Comp)

  
  #3.6 Déterminer la classec du polygone
  #3.6.1 Calculer le point maximale de chaque courbe
  # pointMaxCourbe <-
  #   catCourbes %>% group_by(DESC_FAMC) %>%
  #   slice(which(VOL_HA == max(VOL_HA))[1]) %>%
  #   ungroup() %>%
  # 
  #   #3.6.2 Attribuer une classe de age a chaque point maxile (selon
  #   #les classes d?âge des polygones)
  #   mutate(clageMaxCourbe =
  #            case_when(.$age <= 55 ~ 50,
  #                      .$age <= 75 ~ 70,
  #                      .$age <= 95 ~ 90,
  #                      TRUE ~ 120)) %>%
  #   select(DESC_FAMC, clageMaxCourbe)
  # 
  # 
  # #3.6.3 Joindre l'âge maximale de la courbe au jeu de données
  # dfDonneesPoly <- left_join(dfDonneesPoly, pointMaxCourbe,
  #                            by = c("COURBE" = "DESC_FAMC"))
  # 
  # 
  # #3.6.4 Créer la classec avec la classe d'âge de la courbe au point maximale
  # dfDonneesPoly <-
  #   dfDonneesPoly %>%
  #   mutate(classec = ifelse(clage <= clageMaxCourbe, "1", "2")) %>%
  #   select(-clageMaxCourbe)
  #######################################################################
  #Cette partie a été simplifiée, mais j'ai décidé de garder le code en
  #commentaire, juste au cas où ils veulent que je change ça plus tard
  dfDonneesPoly <-
    dfDonneesPoly %>%
    mutate(clage = as.numeric(as.character(clage)),
           classec = ifelse(clage <= 70, "1", "2"))
  #######################################################################
  
  
  
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
  ######################################################################
  ######################################################################
  #Pour l'instant il faut enlever les "_" de l'appelation de GR_STAT
  #des polyognes. Adrian m'a dit que cette appelation (sans "_") va
  #être le nouveau standard
  dfDonneesPoly$GR_STATION <- gsub("_", "", dfDonneesPoly$GR_STATION)
  
  #Calculer le grand tyf
  dfDonneesPoly$grandTYF <- substr(dfDonneesPoly$TYF, 1, 2)
  catCourbes$grandTYF <- substr(catCourbes$TYF, 1, 2)
  
  #Calculer la famille de station
  dfDonneesPoly$FAM_STAT <- substr(dfDonneesPoly$GR_STATION, 1, 3)
  catCourbes$FAM_STAT <- substr(catCourbes$GR_STATION, 1, 3)
  
  #Ajouter le type de couvert
  dfDonneesPoly$typeCouv <- ifelse(dfDonneesPoly$TYF %in% c("Ep", "EpRx", "Sb",
                                                            "PgRx", "SbRx", "Pg"),
                                   "R",
                                   ifelse(dfDonneesPoly$TYF %in% c("PeFx", "BpFx"), "F",
                                          "M"))
  catCourbes$typeCouv <- ifelse(catCourbes$TYF %in% c("Ep", "EpRx", "Sb",
                                                      "PgRx", "SbRx", "Pg"),
                                "R",
                                ifelse(catCourbes$TYF %in% c("PeFx",
                                                             "BpFx"), 
                                       "F",
                                       "M"))
  
  
  #5.1 Identifier les groupes évolutifs dont la courbe complète
  #(i.e. les 2 côtés) a moins que supMin_courbe
  idCourbesPetites <-
    dfDonneesPoly %>%
    
    #5.1.1 Calculer la superficie de chaque courbe
    group_by(COURBE) %>%
    summarise(sumSup = sum(SUPERFICIE)) %>%
    ungroup %>%
    
    #5.1.2 Filtrer ceux qui sont plus petits
    filter(sumSup < supMin_courbe) %>% 
    distinct(COURBE) %>% unlist %>% unname
  
  
  #5.1.3 Identifier les côtés de ces courbes qui sont plus 
  #petites que supMin_pointAttach
  idCotePetit <- 
    dfDonneesPoly %>%
    filter(!COURBE %in% idCourbesPetites) %>% 
    group_by(COURBE, classec) %>%
    summarise(sumSup = sum(SUPERFICIE)) %>%
    ungroup %>%
    filter(sumSup < supMin_pointAttach) %>% 
    distinct(COURBE, classec)
  
 
  #5.1.4. Identifier les polygones qui font partie de ces 2 jeux de données 
  courbesPetites <- 
    dfDonneesPoly %>% 
    filter(COURBE %in% idCourbesPetites |
             paste(COURBE, classec) %in% 
             paste(idCotePetit$COURBE, idCotePetit$classec)) %>% 

   
    # #5.1.5 Chercher les courbes plus spécifiques (e.g. v1 au lieu de la générale)
    # #5.1.5.1 Re-ajouter les courbes compromis en utilisant la GE5 au lieu
    # #de la courbe choisie par l'algorithme
    left_join(compromisCatCourbes, by = c("GE5" = "DESC_FAMC")) %>%
    
    #5.1.5.2 Changer le nom de la courbe selon le compromis trouvé
    mutate(COURBE = DESC_FAMC_Comp, 
           ID_COURBE = paste0(COURBE, "_c", classec))
  
  
  #5.1.5.3 Mettre les variables "COURBE" et "ID_COURBE" de ces variables
  #à jour dans le jeu de données principal. Pour faire ça, il faut enlever
  #ces polygones du jeu de donnèes et ajouter le jeu de données des
  #courbes petites (où on vient de mettre ces variables à jour)
  dfDonneesPoly <- 
    dfDonneesPoly %>%  
    filter(!ID_BFEC %in% courbesPetites$ID_BFEC) %>% 
    bind_rows(courbesPetites) %>% 
    arrange(ID_BFEC)


    #5.1.6 Calculer la superficie des groupes évolutifs
    #(courbe + côté de la courbe = ID_COURBE)
  courbesPetites <- 
    courbesPetites %>% 
    group_by(ID_COURBE, COURBE, classec, SDOM_BIO, FAM_STAT, GR_STATION,
             TYF, Enjeux_evo, typeCouv, grandTYF) %>%
    summarise(sumSup = sum(SUPERFICIE)) %>%
    ungroup %>%
    
    
    #5.1.5 Créer des colonnes où on peut enregistrer les courbes auxquelles
    #ce groupes évolutifs trop petits ont été attachés
    mutate(courbeEquiv = NA,
           idEquiv = NA,
           condNumero = NA) #Pour stocker le numéro de la condition utilisée
  
  
  #5.2 Si on a des peuplements qui sont trop petits, il faut qu'on trouve la courbe
  #disponible la plus pareille avec le package "dtwclust"
  if(nrow(courbesPetites) > 0){
    
    #5.2.1 Créer le catalogue des courbes des groupes évolutifs qui ont une superficie
    #plus grosse que le minimum spécifié
    grosCatCourbes <-
      catCourbes %>%
      
      #5.2.2 On enlève les groupes évolutifs qui ne sont pas dans le jeu de
      filter(ID_COURBE %in% dfDonneesPoly$ID_COURBE,
             
             #Et on enlève ceux qui appartiennent à des courbes petites
             !ID_COURBE %in% c(NA, courbesPetites$ID_COURBE)) %>%
             
             ##########################################################
             # On ne fait plus cette selection selon les points non-extrapolés
             # #3.2.3 Sélectionner seulement les points non-extrapolés
             # 
             # extrapol %in% "non") %>%
             ##########################################################
      
      #5.2.4 Sélectionner les colonnes dont on a besoin
      select(ID_COURBE, classec, SDOM, FAM_STAT, GR_STATION, typeCouv, 
             grandTYF, TYF, enjeux, VOL_HA)
    
    
    #5.2.5 Comme la fonction a besoin d'une liste de courbes, on va créer
    #une liste où chaque élément est un vecteur de volumes (qui représente
    #la courbe correspondante). Évidement, on a besoin de 2 groupes de courbes : un
    #groupe de courbes de croissance et un de courbes de sénescence
    #5.2.5.1 Courbes de croissance
    grosCatCourbesCrois <-
      split(grosCatCourbes[grosCatCourbes$classec %in% "1", "VOL_HA"],
            f = grosCatCourbes[grosCatCourbes$classec %in% "1", "ID_COURBE"])
    grosCatCourbesCrois <- 
      lapply(grosCatCourbesCrois, function(x) unlist(unname(x)))
    
    #5.2.5.2 Courbes de sénescense
    grosCatCourbesSen <-
      split(grosCatCourbes[grosCatCourbes$classec %in% "2", "VOL_HA"],
            f = grosCatCourbes[grosCatCourbes$classec %in% "2", "ID_COURBE"])
    grosCatCourbesSen <- 
      lapply(grosCatCourbesSen, function(x) unlist(unname(x)))
    
    
    #5.3 Alors, on peut finalement trouver les courbes les plus proches avec une
    #boucle
    for(i in 1:nrow(courbesPetites)){
      
      #5.3.1 D'abord on a besoin de savoir si on veut une courbe de croissance ou 
      #de sénescence. Pour décider quelle courbe utiliser (i.e. appliquer des
      #compromis selon le sous domaine, le type de couvert...) on a besoin
      #des courbes dans un dataframe et pour faire le clustering des courbes (DTW)
      #on a besoin des courbes dans une liste.
      if(courbesPetites[i, "classec"] %in% "1"){
        
        list_tempCatCourbes <- grosCatCourbesCrois
        df_tempCatCourbes <- grosCatCourbes %>% filter(classec %in% "1")
        
      } else {
        
        list_tempCatCourbes <- grosCatCourbesSen
        df_tempCatCourbes <- grosCatCourbes %>% filter(classec %in% "2")
        
      }
      
      
      #5.3.2 Définer les filtres qu'on veut appliquer
      #5.3.2.1 Courbe équivalente dans un autre sous domaine AVEC l'enjeux
      cond_sDomEnj <- 
        paste(df_tempCatCourbes$GR_STATION, df_tempCatCourbes$TYF,
              df_tempCatCourbes$enjeux, sep = "_") %in%
        paste(courbesPetites$GR_STATION[i], courbesPetites$TYF[i],
              courbesPetites$Enjeux_evo[i], sep = "_")
      
      #5.3.2.2 Courbe équivalente dans le même groupe de station et TYF
      cond_sDom <- 
        paste(df_tempCatCourbes$GR_STATION, df_tempCatCourbes$TYF, sep = "_") %in%
        paste(courbesPetites$GR_STATION[i], courbesPetites$TYF[i], sep = "_")
      
      
      #5.3.2.3 Courbe équivalente dans le même famStat et TYF
      cond_famTyf <-
        paste(df_tempCatCourbes$FAM_STAT, df_tempCatCourbes$TYF, sep = "_") %in%
        paste(courbesPetites$FAM_STAT[i], courbesPetites$TYF[i], sep = "_")
      
      
      #5.3.2.4 Courbe équivalente dans le même grStat, type de couvert,
      #mais un grand TYF différent 
      cond_grandTyfCouv <-
        paste(df_tempCatCourbes$GR_STATION, df_tempCatCourbes$typeCouv, 
              df_tempCatCourbes$grandTYF,sep = "_") %in%
        paste(courbesPetites$GR_STATION[i], courbesPetites$typeCouv[i], 
              courbesPetites$grandTYF[i],sep = "_") 
      
      
      #5.3.2.5 Courbe équivalente dans le même grStat et type de couvert
      cond_couv <-
        paste(df_tempCatCourbes$GR_STATION, df_tempCatCourbes$typeCouv, sep = "_") %in%
        paste(courbesPetites$GR_STATION[i], courbesPetites$typeCouv[i],sep = "_")
      
      
      #5.3.2.6 Courbe équivalente dans le même TYF
      cond_tyf <-
        paste(df_tempCatCourbes$TYF, sep = "_") %in%
        paste(courbesPetites$TYF[i], sep = "_")
      
      
      #5.3.2.7 Courbe équivalente dans le même groupe de station
      cond_grStat <-
        paste(df_tempCatCourbes$GR_STATION, sep = "_") %in% 
        paste(courbesPetites$GR_STATION[i], sep = "_") 
      
      
      #5.3.2.8 N'importe quel courbe dans le même sous-domaine 
      cond_SDomSeule <-
        paste(df_tempCatCourbes$SDOM) %in% 
        paste(courbesPetites$SDOM_BIO[i]) 
      
      
      
      #5.3.3 Appliquer les filters
      #5.3.3.1 Laisser tomber juste le sous domaine (en gardant l'enjeux)
      if(any(cond_sDomEnj)){
        
        #Extraire l'ID des courbes qui sont semblables
        nomFiltreCourbe <- 
          df_tempCatCourbes %>% filter(cond_sDomEnj) %>% 
          distinct(ID_COURBE) %>% unlist() %>% unname()
        
        #Mettre à jour le numéro de la condition utilisée
        condNumero <- "cond1"
        
        
        #5.3.3.2 Laisser tomber juste le sous domaine
      } else if (any(cond_sDom)){ 
        
        #Extraire l'ID des courbes qui sont semblables
        nomFiltreCourbe <- 
          df_tempCatCourbes %>% filter(cond_sDom) %>% 
          distinct(ID_COURBE) %>% unlist() %>% unname()
        
        #Mettre à jour le numéro de la condition utilisée
        condNumero <- "cond1"
        
        
        #5.3.3.3 Selon la famille de stattion et le TYF
      } else if (any(cond_famTyf)){ 
        
        #Extraire l'ID des courbes qui sont semblables
        nomFiltreCourbe <- 
          df_tempCatCourbes %>% filter(cond_famTyf) %>% 
          distinct(ID_COURBE) %>% unlist() %>% unname()
        
        #Mettre à jour le numéro de la condition utilisée
        condNumero <- "cond3"
        
        
        #5.3.3.4 Selon la famille de stattion et le TYF
      } else if (any(cond_grandTyfCouv)){ 
        
        #Extraire l'ID des courbes qui sont semblables
        nomFiltreCourbe <- 
          df_tempCatCourbes %>% filter(cond_grandTyfCouv) %>% 
          distinct(ID_COURBE) %>% unlist() %>% unname()
        
        #Mettre à jour le numéro de la condition utilisée
        condNumero <- "cond4"
        
        
        #5.3.3.5 Selon le type de couvert         
      } else if (any(cond_couv)){ 
        
        #Extraire l'ID des courbes qui sont semblables
        nomFiltreCourbe <- 
          df_tempCatCourbes %>% filter(cond_couv) %>% 
          distinct(ID_COURBE) %>% unlist() %>% unname()
        
        #Mettre à jour le numéro de la condition utilisée
        condNumero <- "cond5"
        
        
        #5.3.3.6 Selon le TYF
      } else if (any(cond_tyf)){ 
        
        #Extraire l'ID des courbes qui sont semblables
        nomFiltreCourbe <- 
          df_tempCatCourbes %>% filter(cond_tyf) %>% 
          distinct(ID_COURBE) %>% unlist() %>% unname()
        
        #Mettre à jour le numéro de la condition utilisée
        condNumero <- "cond6"
        
        
        #5.3.3.7 Selon le groupe de station
      } else if (any(cond_grStat)){
        
        #Extraire l'ID des courbes qui sont semblables
        nomFiltreCourbe <- 
          df_tempCatCourbes %>% filter(cond_grStat) %>% 
          distinct(ID_COURBE) %>% unlist() %>% unname()
        
        #Mettre à jour le numéro de la condition utilisée
        condNumero <- "cond7"
        
        
        #5.3.3.8 Finalement, si on a rien on sélectionne selon le SDOM_BIO
      } else if (any(cond_SDomSeule)){
        
        #Extraire l'ID des courbes qui sont semblables
        nomFiltreCourbe <- 
          df_tempCatCourbes %>% filter(cond_SDomSeule) %>% 
          distinct(ID_COURBE) %>% unlist() %>% unname()
        
        #Mettre à jour le numéro de la condition utilisée
        condNumero <- "cond8"
      }
      
      
      #5.3.4 Sélectionner ce sous-ensemble de courbes à partir du gros catalogue
      #5.3.4.1 Si on a trouvé des courbes avec ses conditions, on utilise 
      #les courbes trouvés. Si on n'a trouvé aucune courbe (ce qui peut arriver
      #quand on choisi des seuils trop élevés), on utilise toute le catalogue
      #disponible
      if(exists("nomFiltreCourbe")){
        
        list_tempCatCourbes <- list_tempCatCourbes[nomFiltreCourbe]
        rm(nomFiltreCourbe)
      } 
      
      
      #5.3.5 Après on attache la courbe qu'on veut comparer à la fin de cette liste
      list_tempCatCourbes$tempCourbe <-
        catCourbes %>%
        filter(ID_COURBE %in% courbesPetites[i, "ID_COURBE"]) %>%
        select(VOL_HA) %>% unlist %>% unname
  
 
      #5.3.6 Maintenant on calcule les distances
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
      
      
      #5.4 On extrait l'id de la courbe la plus pareille à la courbe (i.e. celle
      #avec la distance la plus petite)
      #5.4.1 On extrait la colonne de notre courbe de la matrice de distance
      idPlusProche <- tempDtw@distmat[, "tempCourbe"]
      
      #5.4.2 Enlever la courbe 'tempCourbe' pour laquelle on veut trouver la courbe
      #la plus proche
      idPlusProche <- idPlusProche[!names(idPlusProche) == 'tempCourbe']
      
      #5.4.3 On l'organize par ordre croissante de la valeur de la distance 
      idPlusProche <- idPlusProche[order(idPlusProche)]
      
      #5.4.4 Et on extrait le premier élément du vecteur (i.e. cellui
      #avec la distance la plus petite)
      idPlusProche <- names(idPlusProche)[1]
      
      #5.4.5 On a aussi besoin de trouver la courbe las plus proche (pour la
      #remplacer après)
      courbePlusProche <-
        catCourbes %>%
        filter(ID_COURBE %in% idPlusProche) %>%
        distinct(DESC_FAMC) %>% slice(1) %>%  #select first row just in case there's an error
        unlist %>% unname
      
      
      #5.5 Maintenant on a juste besoin de remplacer la courbe et l'id des
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
      
      #5.6 Mettre le data frame avec les courbes petites à jour en ajoutant
      #la courbe qui a été attribuée à ce groupe évolutif
      courbesPetites$idEquiv[i] <- idPlusProche
      courbesPetites$courbeEquiv[i] <- courbePlusProche
      courbesPetites$condNumero[i] <- condNumero
      
    }
    
    
    #5.7 Sélectionner les variables qu'on veut garder dans l'extrant
    courbesPetites <- 
      courbesPetites %>% 
      transmute(courbeOri = COURBE, 
                courbeEquiv, classec, sumSup, condNumero)
    
    #5.7 Faire un petit avertissement pour dire au utilisateur si on a 
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
    dfDonneesPoly %>% select(ID_BFEC, ID_COURBE, COURBE, classec,
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
    if(unique(tempDonnees$classec) %in% "1"){
      
      nombreMaxCluster <-  nombreMaxClusterCroissance
      
    } else { #si le groupe est du côté gauche de la courbe de croissance:
      
      nombreMaxCluster <-  nombreMaxClusterSenescence
      
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
  
  
  #6.10 Créer les 2 des 3 dataframes d'extrant: un avec l'id des polygones et le point d'attachement
  #sur la courbe; et un avec la COURBE et le point d'attachement sur la courbe
  #6.10.0 Ajouter l'âge d'attachement au jeu de données principal
  donneesCluster <- 
    left_join(donneesCluster,
              catCourbes %>% transmute(ID_COURBE, 
                                       clusterAttach = as.character(VOL_HA), 
                                       ageAttach = age),
              by = c("ID_COURBE", "clusterAttach"))
  
 
  #6.10.1 Créer le dataframe des polygones en sélectionnant les 2 colonnes qu'on 
  #veut. Il faut d'abord joindre l'âge de la courbe au point d'attachement
  dfPoly <- 
    donneesCluster %>% 
    select(ID_BFEC, classec, COURBE, Enjeux_strConf, clusterAttach, ageAttach)

  
  #6.10.2 Créer le dataframe des strates
  dfStrates <- 
    donneesCluster %>% 
    
    #6.10.2.1 Regrouper le jeu de données selon les variables qu'on veut
    group_by(COURBE, Enjeux_strConf, classec, clusterAttach, ageAttach) %>% 
    
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


