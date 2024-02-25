# Aperçu
Ce projet vise à estimer et backtester la valeur en risque (VaR) d'un portefeuille composé de deux indices boursiers à l'aide d'un modèle GARCH(1,1)-Normal. Nous sommes chargé de corriger et compléter le code fourni pour assurer son bon fonctionnement. Le projet est conçu pour être exécuté de façon autonome sur n'importe quelle plateforme, avec une génération automatique de tous les éléments nécessaires.
_______________________________________

# Comment exécuter le projet
Pour exécuter ce projet, lancez simplement le script run_TP1.R (ou run_TP1.Rmd) depuis votre environnement de développement R. Ce script est conçu pour orchestrer l'ensemble du processus, de l'installation des packages nécessaires au chargement des fonctions et à l'exécution des analyses. Il n'est pas nécessaire d'exécuter manuellement d'autres scripts ou de charger des données spécifiques en dehors de ce qui est prévu dans run_TP1.R (ou run_TP1.Rmd).
_______________________________________

# Emplacement des fichiers générés
À la suite de l'exécution du script run_TP1.R (ou run_TP1.Rmd), tous les fichiers générés, y compris les graphiques et les résultats de backtest, seront sauvegardés dans des dossiers spécifiques au sein du répertoire du projet :

Graphiques : Les graphiques combinant les rendements et les estimations de la VaR seront enregistrés sous forme de fichier PNG dans le dossier Output.

Résultats de backtest : Les résultats du backtesting seront sauvegardés dans un fichier RDA situé dans le sous-dossier Data/Clean Data sous le nom Backtest_results.rda.
Ces emplacements sont définis pour faciliter l'accès et la revue des résultats. 
_______________________________________

# Structure du projet
Ce projet est structuré de manière à simplifier son exécution et la revue des résultats. En suivant les instructions ci-dessus et en lançant le script run_TP1.R (ou run_TP1.Rmd), vous serez en mesure de générer tous les éléments nécessaires au projet de manière automatique et indépendante de la plateforme.
