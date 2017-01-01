################################################
# Informations utiles à la lecture des scripts #
################################################


#Tout d'abord Voici la façon dont j'organise mon dossier de travail dans explorer:
#Ma racine nommée outils_git contient le projet Rstudio et plusieurs sous dossier:
  #Un sous dossier "src" qui contient tous mes scripts .R
  #Un sous dossier "data" qui contient les jeux de données de base et les tableaux crées à partir de ceux-ci
  #Un sous dossier "writing" qui contient les exports de graphiques, le document word et le pdf



#Voici les 4 scipts que j'ai utilisé pour le devoir outils:

#1. Un script nommé "data_management.r" est l'étape préliminaire me permettant d'obtenir les différents tableaux que j'utiliserai au cours du devoir : 
#les tableaux normalisés et dénormalisés, le calcul des scores, les données à J0 et J56 pour hamilton et scl90, mais aussi le tableau utilisé
#pour l'analyse de survie. Tous les tableaux obtenus sont enregistrés au format rds.

#2. Un script nommé "objects_outils.R" qui execute le chargement des objets crées dans le script "data_management.r"
#   et qui charge toutes les librairies utilisées lors de l'analyse

#3. Un script nommé "functions_outils.R" qui contient les fonctions crées pour le devoir

#4. Un script nommé "analyses_outils.R" qui execute d'abord les scripts 2 et 3 
#   (c'est à dire qui charge tous les objets nécessaires, les librairies et les fonctions)
#   puis qui fait les analyses statistiques au fur et à mesure



#J'ai ensuite rédigé sur un script rmarkdown executant les scripts 2 et 3 et j'ai copié les bout de code correspondant aux graphiques et tables
#pour éviter de les exporter un à un et de les réimporter dans mon document word.
#J'ai par contre du exporter le graphe survie à la main car l'export de l'abcisse ne se fait pas bien pour des raisons
#que j'ignore.