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

choixCourbe <- 
  function(dfDonneesPoly,
           catCourbes,  #ceci doit avoir toutes les courbes des catalogues de tous 
                        #les SDOMs ensemble. Chaque ligne doit représenter le 
                        #volume total du point de la courbe, pas le volume 
                        #par essence
           supMin_pointAttach = 500, 
           supMin_courbe = 5000){
  
  
  
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
                       "TYF", "Enjeux_evo", "Enjeux_str", "Improd",
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
  
 
  #2.3 Vérifier les données manquantes
  #Il faut aussi vérifier qu'on n'a pas des données de volume manquantes. Ça devrait
  #jamais arriver non plus.
  #2.3.1 Si au moins un polygone n'a pas une valeur de volume total
  if(any(is.na(dfDonneesPoly$v_TOT)) ){
    
    #2.3.2 Identifier les groupes où ça a arrivé  
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
  
  #3.0.2 Remplacer les v5 avec des v4
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
  #On met tous les SNATs (improductifs) comme des GE1 (les autres 
  #echèlles n'existent pas)
  dfDonneesPoly <- 
    dfDonneesPoly %>% 
    mutate(COURBE =
             case_when(.$Improd %in% "_SNAT" ~ as.character(.$GE1),
                       .$echelle_gen %in% TRUE ~ as.character(.$GE1),
                       .$echelle_3 %in% TRUE ~ as.character(.$GE3),
                       TRUE ~ as.character(.$GE5))) 
  
  
  #3.5 Ça se peut que la courbe choisie par l'algorithme d'échelle ci-dessus
  #choisi une courbe que n'existe pas dans le catalogue de courbes. Alors,
  #il faut trouver des compromis dans ces cas. Pour les trouver, on va
  #utiliser la fonction "trouverCourbesCompromis()" qu'on a écrit avant
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
    select(SDOM, DESC_FAMC, DESC_FAMC_Comp) %>% 
    
    #3.5.5 Faire une corréction pour les SNAT (improductifs)
    mutate(DESC_FAMC = ifelse(grepl("SNAT", DESC_FAMC_Comp), 
                              DESC_FAMC_Comp, DESC_FAMC)) %>% 
    
    #3.5.6 Pour enlever des doublons SNAT
    distinct()
  
 
  #3.5.7 Utiliser ce catalogue pour trouver les courbes de compromis des
  #polygones
  dfDonneesPoly <- left_join(dfDonneesPoly,
                             compromisCatCourbes, 
                             by = c("SDOM_BIO" = "SDOM", "COURBE" = "DESC_FAMC"))
  
  
  #3.5.8 Faire un avertissement qui nous dit quelles courbes nous manquent
  courbesManq <- dfDonneesPoly %>% filter(is.na(DESC_FAMC_Comp))
  if(nrow(courbesManq > 0)){
    
    warning(nrow(courbesManq), " polygones ne sont pas étés regroupés parce que ",
            "les courbes ", paste(unique(courbesManq$COURBE), collapse = ", "),
            " n'existent pas dans le catalogue de courbes.")
  }
  

  #3.5.9 Changer le nom de la variable
  dfDonneesPoly <- 
    dfDonneesPoly %>% 
    filter(!is.na(DESC_FAMC_Comp)) %>% 
    select(-COURBE) %>%
    rename(COURBE = DESC_FAMC_Comp)
  

  #3.6 Déterminer la classec du polygone: age inférieure ou égale à 70: classec 1
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
    for(i in 1:nrow(courbesPetites)){
      
      #5.4.1 D'abord on a besoin de savoir si on veut une courbe de croissance ou 
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
      
      
      #5.4.2 Définer les filtres qu'on veut appliquer
      #5.4.2.1 Courbe équivalente dans un autre sous domaine AVEC l'enjeux
      cond_sDomEnj <- 
        paste(df_tempCatCourbes$GR_STATION, df_tempCatCourbes$TYF,
              df_tempCatCourbes$enjeux, sep = "_") %in%
        paste(courbesPetites$GR_STATION[i], courbesPetites$TYF[i],
              courbesPetites$Enjeux_evo[i], sep = "_")
      
      #5.4.2.2 Courbe équivalente dans le même groupe de station et TYF
      cond_sDom <- 
        paste(df_tempCatCourbes$GR_STATION, df_tempCatCourbes$TYF, sep = "_") %in%
        paste(courbesPetites$GR_STATION[i], courbesPetites$TYF[i], sep = "_")
      
      
      #5.4.2.3 Courbe équivalente dans le même famStat et TYF
      cond_famTyf <-
        paste(df_tempCatCourbes$FAM_STAT, df_tempCatCourbes$TYF, sep = "_") %in%
        paste(courbesPetites$FAM_STAT[i], courbesPetites$TYF[i], sep = "_")
      
      
      #5.4.2.4 Courbe équivalente dans le même grStat, type de couvert,
      #mais un grand TYF différent 
      cond_grandTyfCouv <-
        paste(df_tempCatCourbes$GR_STATION, df_tempCatCourbes$typeCouv, 
              df_tempCatCourbes$grandTYF,sep = "_") %in%
        paste(courbesPetites$GR_STATION[i], courbesPetites$typeCouv[i], 
              courbesPetites$grandTYF[i],sep = "_") 
      
      
      #5.4.2.5 Courbe équivalente dans le même grStat et type de couvert
      cond_couv <-
        paste(df_tempCatCourbes$GR_STATION, df_tempCatCourbes$typeCouv, sep = "_") %in%
        paste(courbesPetites$GR_STATION[i], courbesPetites$typeCouv[i],sep = "_")
      
      
      #5.4.2.6 Courbe équivalente dans le même TYF
      cond_tyf <- 
        paste(df_tempCatCourbes$TYF, sep = "_") %in%
        paste(courbesPetites$TYF[i], sep = "_")
      
      
      #5.4.2.7 Courbe équivalente dans le même groupe de station
      cond_grStat <-
        paste(df_tempCatCourbes$GR_STATION, sep = "_") %in% 
        paste(courbesPetites$GR_STATION[i], sep = "_") 
      
      
      #5.4.2.8 N'importe quel courbe dans le même sous-domaine 
      cond_SDomSeule <-
        paste(df_tempCatCourbes$SDOM) %in% 
        paste(courbesPetites$SDOM_BIO[i]) 
      
      
      
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
        condNumero <- "cond1"
        
        
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
    
    #5.9 Faire un petit avertissement pour avertir l'utilisateur si on a 
    #changé les courbes de quelques groupes évolutifs
    warning(paste0(nrow(courbesPetites), " groupes évolutifs avaient une superficie ",
                   "totale inférieure à la superficie minimale spécifiée (", 
                   supMin_pointAttach, "HA). Pour chaque cas la courbe la plus semblable ",  
                   "parmi toute les courbes disponibles a été trouvée avec une ",
                   "méthodologie de déformation temporelle dynamique (DTW). Les ",
                   "courbes trouvées ont été stockées dans 'dfCourbesPetites'."))
    
  }
  
  
  #######################
  
  #6. Définir l'extrant et terminer la fonction
  #6.1 Extrant au niveau des polygones: 
  #   - ID_BFEC : l'id des polygones
  #   - COURBE : la courbe trouvée par l'algorithme de compromis des courbes 
  #   qui n'existent pas (e.g. si l'algorithme de l'échèlle avait trouvé
  #   une courbe v1, mais cette courbe n'existent pas dans le catalogue,
  #   le deuxième algorithme va chercher la courbe v12 (si elle existe) ou 
  #   la courbe générale (si elle existe))
  #   classec : le côté de la courbe du du polygone (1 = croissance; 2 = sénescence)
  extrantPoly <- 
    dfDonneesPoly %>% 
    select(ID_BFEC, COURBE, classec)
  
  
  #6.2 Créer le dataframe des strates
  #6.2.1 Regrouper le jeu de données selon les variables qu'on veut
  dfStrates <- 
    dfDonneesPoly %>% 
    group_by(COURBE, classec) %>% 
    
    #6.2.2 Calculer la somme de la superficie de chaque GE
    summarise(SUPERFICIE = sum(SUPERFICIE)) %>% 
    ungroup()
  
  
  #6.3 Un vecteur avec les courbes qui n'existent pas
  courbesManq <- unique(courbesManq$COURBE)
  
  
  #6.4 Faire la liste
  listeExtrant <- list(dfPoly = extrantPoly, 
                       dfStrates = dfStrates,
                       dfCourbesPetites = as.data.frame(courbesPetites),
                       courbesManq = courbesManq)
  
  return(listeExtrant)
  
}
  

