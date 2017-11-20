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
#   catCourbes: dataframe des courbes d'attachement de tous les sous domaines. 
#               Dans ce fichier chaque ligne réprésente 
#               un point d'attachement de la courbe d'un groupe évolutif. La colonne 
#               `classec` dit-nous si les points d'attachement se trouvent à la
#               gauche (1) ou à la droite (2) du sommet de la courbe (i.e. senescence). 
#               La colonne `extrapol` dit-nous si cette partie de la courbe a été 
#               extrapolé. Chaque courbe a plusieurs points d'attachement.  
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



faireKnn <- function(dfDonneesPoly,
                     catCourbes,  #ceci doit avoir toutes les courbes des catalogues de tous 
                                  #les SDOMs ensemble. Chaque ligne doit représenter le 
                                  #volume total du point de la courbe, pas le volume 
                                  #par essence
                     supMin_pointAttach = 500, 
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
  varsCatCourbes <- c("NOM_FAMC", "age", 
                      "SDOM", "GR_STATION", "TYF", "enjeux", "classe",
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
  varsDonneesPoly <- c("ID_BFEC", "v_TOT", "Enjeux_str",
                       "COURBE", "classec", "SUPERFICIE")   
  
  
  
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
  #2.2.1 DESC_FAMC dans le catalogue des courbes
  if(!"DESC_FAMC" %in% names(catCourbes)){
    catCourbes <- 
      catCourbes %>% 
      mutate(DESC_FAMC = ifelse(classe %in% c(NA, "NA", "Na", "na"),
                                paste(SDOM, GR_STATION, TYF, enjeux, sep = "_"),
                                paste(SDOM, GR_STATION, TYF, enjeux, classe, sep = "_")))
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
      select(COURBE) %>% unlist() %>% unname() %>%  #Sélectionner la colonne "COURBE"
      as.character()
    
    
    #2.3.3 Remplacer les NA par des 0
    dfDonneesPoly$v_TOT[is.na(dfDonneesPoly$v_TOT)] <- 0
    
    #2.3.4 Donner un message d'avertissement
    warning("Faites attention : ", length(volManq), " polygones dans les courbes ", 
            unique(volManq), " ont des données 'v_TOT' manquantes!", 
            "Leur valeur a été remplacé par '0' lors de cette analyse.")
  }
  
  
  #2.4 Enlever les polygones qui ont aucune courbe associée
  dfDonneesPoly <- dfDonneesPoly %>% filter(!COURBE %in% NA)
  
  
  
  #3. Créer la variable ID_COURBE (un identifiant unique de chaque groupe de
  #courbes) en concatenant les variables "COURBE" et "classec"
  #3.1 Créer la variable dans le jeu de données
  dfDonneesPoly <-
    dfDonneesPoly %>%
    mutate(COURBE = as.character(COURBE),
           ID_COURBE = paste0(COURBE, "_c", classec))
  
  #3.2 Créer cette variable dans le catalogue des courbes
  catCourbes <-
    catCourbes %>% 
    mutate(ID_COURBE = ifelse(classe %in% c("NA", "Na", "na", NA),
                              paste0(DESC_FAMC, "_c", classec),
                              paste0(DESC_FAMC, "_c", classec)))
  
  
  
  #4. Le clustering avec k-NN (k plus proches voisins)
  #Maintenant on peut faire le clustering avec `k-NN` dans une boucle. Voici la structure 
  #du processus:  
  #1.Définir les paramètres de la fonction (ex. superficie minimale de chaque cluster)
  #
  #2 Sélectionner les points d'attachement sur la courbe. Quand le nombre de points 
  #disponibles est plus gros que le nombre maximale spécifié, on enlève le point qui a 
  #0m3/HA (s'il existe), et on choisit les points d'attachement les plus représentatifs
  #
  #3.Pour chaque groupe, on a besoin de créer un jeu de données d'entraînement pour 
  #entraîner le modèle KNN. Le jeu de données est une séquence de points de volume qui 
  #vont d'une valeur plus petite que le point d'attachement plus petit à une valeur 
  #plus grosse que celle du dernier point d'attachement. Ces points sont classifiés 
  #selon les point d'attachement correspondants
  #
  #4. On fait le KNN 
  #
  #5. On contrôle la superficie des clusters. Quand on a des clusters avec une superficie 
  #plus petite que celle spécifiée par `supMin_pointAttach`, on élimine séquentiellement les 
  #clusters plus petits en les reclassifiant avec la valeur du point d'attachement le 
  #plus proche (ex., si on a 4 points (20, 25, 40, 50), dont le point 25 est le 
  #plus petit, on calcule la différence absolue entre ce cluster et les autres 
  #(cluster20: 5; cluster40: 15; cluster50: 25); dans ce cas on reclassifie les points 
  #du cluster 25 en 20). Si la somme de la superficie de tous les clusters est toujours 
  #inferieure à la limite spécifiée, on fait sortir un seul cluster qui est plus petit 
  #que la limite spécifiée.
  
  #4.0 Pour qu'on s'assure qu'on a toujours les mêmes résultats quand on 
  #utilise des méthodes stochastiques
  set.seed(333)  
  
  
  dfDonneesPoly <- 
    dfDonneesPoly %>% select(ID_BFEC, ID_COURBE, COURBE, classec,
                             Enjeux_str, SUPERFICIE, v_TOT)
  
  
  #4.1 D'abord on initialise une boucle où on va faire le clustering par groupe ID_COURBE
  for(i in unique(dfDonneesPoly$ID_COURBE)){ 
    
    #4.2 Extraire le groupe "i" 
    tempDonnees <- dfDonneesPoly %>% filter(ID_COURBE %in% i)
    
    #4.3 Extraire les points d'attachement correspondants
    tempAttach <- 
      catCourbes %>% 
      #4.3.1 Sélectionner la courbe correcte
      filter(DESC_FAMC %in% unique(tempDonnees$COURBE)[1],
             #4.3.2 Sélectionner le bon côté de la courbe
             classec %in% unique(tempDonnees$classec)[1]) %>% 
      #4.3.3 Sélectionner la colonne du volume
      select(VOL_HA) %>% unlist %>% unname
    
    
    #4.4 Il faut contrôler le nombre de clusters maximale qu'on a dans chaque groupe. 
    #4.4.1 D'abord, on sélectionne le nombre maximal de clusters selon le côté
    #de la courbe (croissance (classec == 1) vs senescence (classec == 2))
    #Par contre, si on a des SNAT on veut un maximum de 2 points d'attachement
    #de chaque côté
    if(unique(tempDonnees$classec) %in% "1"){
      
      nombreMaxCluster <- unique(ifelse(grepl("SNAT", tempDonnees$COURBE), 2, 
                                        nombreMaxClusterCroissance))
      
    } else { #si le groupe est du côté gauche de la courbe de croissance:
      
      nombreMaxCluster <- unique(ifelse(grepl("SNAT", tempDonnees$COURBE), 2, 
                                        nombreMaxClusterSenescence))
      
    } 
    
    #4.4.2 On commence par éliminer des points d'attachement qu'ont un volume de 0
    #(s'ils existent)
    if(any(tempAttach %in% 0)){
      tempAttach <- tempAttach[tempAttach > 0]
    }
    
    
    #4.4.3 Si on a toujours plus de points que ce qu'on devrait avoir, il faut choisir
    #les points d'attachement les plus représentatifs. Pour faire ça, on va créer une 
    #séquence de la valeur minimale à la valeur maximale des points d'attachement qui 
    #a une longueur égale au nombre maximal de clusters qu'on veut. Après, on sélectionne 
    #les points d'attachement disponibles les plus proches de ces points moyens.
    #4.4.3.1 Créer la séquence de points moyens
    seqAttach <- seq(from = min(tempAttach), to = max(tempAttach),
                     length.out = nombreMaxCluster) #longueur : nombre max de clusters désiré
    
    #4.4.3.2 Trouver la différence entre ces points equidistantes et les points d'attachement 
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
    
    #4.4.3.3 Au cas où on a des valeurs répétées, on peut les enlever
    rankAttach <- unique(rankAttach)
    
    #4.4.3.4 Maintenant il faut juste sélectionner les points qu'on a choisis
    tempAttach <- tempAttach[rankAttach]
    
    
    
    #4.5 Pour chaque groupe, on va avoir besoin de créer un jeu de données d'entraînement
    #pour entraîner le modèle KNN avec les points d'attachement du groupe évolutif choisi.
    #Pour le faire, on a besoin d'identifier le point moyen entre la valeur de chaque
    #cluster et ses clusters voisins, créer une séquence de valeurs qui couvrent tous
    #les valeurs proches des clusters en les attribuant la valeur du cluster plus proche
    #4.5.1 Calculer les points moyens entre les clusters
    midpoints <- tempAttach[-length(tempAttach)] + diff(tempAttach)/2
    
    
    #4.5.2 Faire deux vecteur de plus avec ça: lagMidpoints, qui va être
    #le vecteur "midpoints" avec les éléments déplacés une fois en arrière;
    #et leadMidpoints, qui va être le vecteur "midpoints" avec les éléments 
    #déplacés une fois en avant (ex. si midpoints = 1,2,3, 
    #lagMidpoints = NA, 1, 2 et leadMidpoints = 2, 3, NA)   
    #4.5.2.1 Ajouter des valeurs sensibles à la fin et au début de la 
    #séquence (-5 et +20 doit bien marcher)
    midpoints <- c(midpoints[1]-5, midpoints, last(midpoints) + 20)
    
    #4.5.2.2 Faire ces vecteurs en éliminant les NAs que les fonctions génèrent
    lagMidpoints <- lag(midpoints)[2:length(midpoints)]
    leadMidPoints <- lead(midpoints)[1:(length(midpoints)-1)]
    
    #4.5.2.3 Faire une séquence de 20 éléments pour chaque cluster 
    #qui couvre la valeur du cluster +/- la valeur moyen entre le cluster
    #et ses clusters voisins (ex. pour le cluster "6" d'une liste de 3 
    #clusters (4, 6 et 8), on va faire une séquence de 5 à 7 de 20 éléments
    seqMidpoints <- 
      mapply(seq,
             from = lagMidpoints, to = leadMidPoints,
             length.out = 30)
    
    #4.5.2.4 La dernière fonction nous a créé un data frame de 30 lignes avec
    #un nombre de colonnes égal au nombre de clusters. Il faut la transformer 
    #dans un vecteur. Comme les colonnes sont bien organisées, on a juste besoin
    #de faire "as.numeric()"
    seqMidpoints <- as.numeric(seqMidpoints)
    
    
    #4.5.2.5 Maintenant il faut faire le data frame d'entraînement final qui a 2
    #colonnes : le volume de chaque point d'entraînement et le cluster auquel 
    #il est associé
    tempTrain <- data.frame(cluster = rep(tempAttach, each = 30),
                            v_TOT = seqMidpoints)
    
    
    
    #4.6 On peut finalement faire le clustering avec k-NN pour ce groupe avec "knn()"
    #Avec "$" on ajoute automatiquement le résultat du cluster à une colonne
    #de l'objet "tempDonnees"
    tempDonnees$clusterAttach <- 
      knn(train = tempTrain[,"v_TOT", drop = FALSE],   #les valeurs d'entraînement
          cl = tempTrain$cluster,  #les clusters correspondants aux valeurs d'entraînement
          test = tempDonnees[,"v_TOT", drop = FALSE],  #les valeurs qu'on veut prédire
          k = 5)     #le nombre de voisins à chercher
    
    tempDonnees$clusterAttach <- as.character(tempDonnees$clusterAttach)
    
    
    
    #4.7 Alors, il faut contrôler la superficie minimale de chaque cluster
    #4.7.1 Calculer la superficie de chaque cluster
    supCluster <- 
      tempDonnees %>% 
      group_by(clusterAttach) %>% 
      summarise(supCluster = sum(SUPERFICIE)) %>% 
      ungroup %>% 
      mutate(clusterAttach = as.numeric(as.character(clusterAttach)))
    
    #4.7.2 Pendant qu'on a au moins un cluster plus petit que la superficie
    #minimale demandée (supMin_pointAttach)
    while(any(supCluster$supCluster < supMin_pointAttach)){
      
      #4.7.2.0 Alors, si on a déjà fait plusieurs itérations de cette boucle et
      #on a juste un point d'attachement dans la courbe parce
      #que ce groupe est trop petit, il faut passer au prochain groupe
      if(nrow(supCluster) %in% 1){
        break
      }
      
      
      #4.7.2.1 On identifie la valeur du cluster le plus petit
      clusterPlusPetit <- 
        supCluster %>% 
        top_n(wt = supCluster, -1) %>% 
        select(clusterAttach) %>% 
        slice(1) %>% #ou cas où il y a 2 clusters avec la même superficie
        unlist %>% unname 
      
      
      
      #4.7.2.2 On détermine la différence entre les clusters
      clusterDiff <- abs(clusterPlusPetit - supCluster$clusterAttach)
      
      
      #4.7.2.3 On sélectionne la différence la plus petite (sans lui-même, aka
      #clusterDiff > 0)
      clusterDiff <- min(clusterDiff[clusterDiff > 0])
      
      
      #4.7.2.4 On détermine l'index du cluster le plus proche (i.e. son 
      #positionnement dans le data frame)
      clusterDiffIndex <- 
        which(abs(clusterPlusPetit - supCluster$clusterAttach) %in% clusterDiff)
      
      
      #4.7.2.5 On sélectionne ça valeur
      clusterVoisin <- 
        supCluster %>% 
        slice(clusterDiffIndex) %>% 
        select(clusterAttach) %>% unlist
      
      
      #4.7.2.6 On change les valeurs dans le jeu de données principal avec ifelse
      tempDonnees$clusterAttach <-
        #Aux polygones du cluster le plus petit
        ifelse(tempDonnees$clusterAttach %in% as.character(clusterPlusPetit),
               #On les assigne la valeur du cluster le plus proche
               as.character(clusterVoisin), 
               #Sinon on fait rien
               tempDonnees$clusterAttach)
      
      
      #4.7.2.7 Il faut recalculer l'objet de la somme des superficies qu'on
      #utilise dans cette boucle "while" pour éviter une boucle infinie
      supCluster <- 
        tempDonnees %>% 
        group_by(clusterAttach) %>% 
        summarise(supCluster = sum(SUPERFICIE)) %>% 
        ungroup %>% 
        mutate(clusterAttach = as.numeric(as.character(clusterAttach)))
      
      
    }
    
    
    
    #4.8 Finalement, on fusionne les résultats dans un seul objet d'extrant
    #4.8.1 Si c'est la première tournée de la boucle (i.e. si la variable 
    #"donneesCluster" n'existe pas encore), on la crée avec "tempDonnees"...
    if(!exists("donneesCluster")){
      
      donneesCluster <- tempDonnees
      
      
    } else { #...sinon on ajoute les lignes du nouveau groupe
      
      donneesCluster <- bind_rows(donneesCluster, tempDonnees)
      
    }
    
    
    #4.8.2 À la fin de chaque itération de la boucle, on peut effacer les 
    #objets intermédiaires
    rm(tempDonnees, tempAttach, tempTrain, seqMidpoints, midpoints, 
       lagMidpoints, leadMidPoints)
    
  }  #On ferme la boucle qui fait le knn 
  #pour chaque groupe évolutif. Le résultat
  #est stocké dans donneesCluster
  
  
  
  #4.9 Vérifier si on peut cinder des points d'attachement en 2 s'ils ont plus
  #de "supMin_pointAttach" de superficie
  #4.9.1 Calculer la somme de la superficie de chaque COURBE + 
  #point d'attachement + Enjeux_strategique
  enjeuxConf <- 
    donneesCluster %>% 
    group_by(ID_COURBE, clusterAttach, Enjeux_str) %>% 
    summarise(sumSup = sum(SUPERFICIE)) %>% 
    
    #4.9.2 S'il y a des clusters qui ont moins que la superficie minimale,
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
  
  
  #4.9.3 Joindre l'enjeux confirmé au jeu de données principal
  donneesCluster <- left_join(donneesCluster, enjeuxConf,
                              by = c("ID_COURBE", "clusterAttach", "Enjeux_str")) 
  
  
  
  #5. Ajouter la variable NOM_FAMC du catalogue de courbes
  #5.1 Sélectionner les colonnes qu'on veut
  nomFam <-
    catCourbes %>%
    select(DESC_FAMC, NOM_FAMC) %>%
    distinct()
  
  #5.2 Faire le join
  donneesCluster <- 
    left_join(donneesCluster, nomFam, by = c("COURBE" = "DESC_FAMC"))
  
  
  
  #6. Créer les 2 des 3 dataframes d'extrant: un avec l'id des polygones et le point d'attachement
  #sur la courbe; et un avec la COURBE et le point d'attachement sur la courbe
  #6.1 Ajouter l'âge d'attachement au jeu de données principal
  donneesCluster <- 
    left_join(donneesCluster,
              catCourbes %>% transmute(ID_COURBE, 
                                       clusterAttach = as.character(VOL_HA), 
                                       ageAttach = age),
              by = c("ID_COURBE", "clusterAttach"))
  
  
  #6.2 Créer le dataframe des polygones en sélectionnant les 2 colonnes qu'on 
  #veut. Il faut d'abord joindre l'âge de la courbe au point d'attachement
  dfPoly <- 
    donneesCluster %>% 
    select(ID_BFEC, classec, COURBE, NOM_FAMC, 
           Enjeux_strConf, clusterAttach, ageAttach)
  
  
  #6.3 Créer le dataframe des strates
  dfStrates <- 
    donneesCluster %>% 
    
    #6.3.1 Regrouper le jeu de données selon les variables qu'on veut
    group_by(COURBE, Enjeux_strConf, classec, clusterAttach, ageAttach) %>% 
    
    #6.3.2 Calculer la somme de la superficie de chaque GE
    summarise(SUPERFICIE = sum(SUPERFICIE)) %>% 
    ungroup() %>% 
    
    #6.3.3 Assurer la bonne ordre des points d'attachement
    mutate(clusterAttach = as.numeric(clusterAttach)) %>% 
    arrange(COURBE, Enjeux_strConf, classec, clusterAttach) %>% 
    mutate(clusterAttach = as.character(clusterAttach))
  
  
  #6.4 Créer une liste avec ces 2 jeux de données plus le data frame qui a 
  #les courbes auxquelles les groupes évolutifs trop petits ont été attachés
  listeKnn <- list(dfPoly = dfPoly, 
                   dfStrates = dfStrates)
  
  
  #6.5 Faire "return" de cette liste pour finir la fonction
  return(listeKnn)   
  
}



