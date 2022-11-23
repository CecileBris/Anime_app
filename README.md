# Dashboard interactif sous Rshiny

Après une analyse des données contenues dans la base `anime.csv` et disponible dans le fichier `AnalysisReport_Anime.Rmd`, nous avons effectué un dashboard accessible [ici]("https://showiz.shinyapps.io/Animes_BRISSARD_TSO/") et faisant ressortir cette analyse.

Pour lancer l'application en local, il suffit d'installer les librairies requise dans R, puis de lancer le code intitulé `Ui.R`. Le script `server.R` est automatiquement appelé par ce premier script et il n'est donc pas nécessaire de le faire tourner. Pour que l'application s'affiche, il est nécessaire de télécharger l'ensemble de ce repo et de concerver l'arborescence des fichiers où les données sont dans `data` & les images et vidéos affiché dans l'interface dans `www`. 

# Interactive dashboard in Rshiny

After an analysis of the data contained in the `anime.csv` database and available in the `AnalysisReport_Anime.Rmd` file, we have made a dashboard accessible [here]("https://showiz.shinyapps.io/Animes_BRISSARD_TSO/"): showing this analysis.

To run the application locally, you just have to install the required libraries in R, then run the code called `Ui.R`. The `server.R` script is automatically called by this first script, so there is no need to run it. For the application to be displayed, it is necessary to download the whole repo and reserve the file tree where the data is in `data` & the images and videos displayed in the interface in `www`. 
