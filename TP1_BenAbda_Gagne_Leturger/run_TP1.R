#**********************************************************************************************
#**********************************************************************************************
# ----------------------------------    MATH60633 – TP1    ------------------------------------
#**********************************************************************************************
# Logiciel utilisé : R  
#**********************************************************************************************
# ÉQUIPE :
#         Achille Leturger
#         Philippe Gagné
#         Tarak Ben Abda
#
#**********************************************************************************************
# Description : L’objectif est d’estimer et de backtester la valeur en risque (VaR) d’un 
#               portefeuille de deux indices en utilisant le modèle GARCH(1,1)-Normal. 
#               Le code fourni dans les fichiers ne fonctionne pas. Vous devez le corriger et
#               le compléter. Vous devez créer une structure de projet appropriée avec un 
#               script principal pour exécuter le code. Cela doit être indépendant de la 
#               plateforme. Tout doit être généré automatiquement.
#**********************************************************************************************
#**********************************************************************************************
#**********************************************************************************************

# Charger (et installer si nécessaire) les packages en exécutant le script run_install_packages.R
source("run_install_packages.R")

# Charger tous les fichiers des fonctions R dans le dossier "Functions" et les exécuter
sapply(list.files(path = here('Functions'), pattern = "\\.R$",
                  full.names = TRUE), source)

#### Données #### 

# Charger le fichier indices.rda dans R
prices <- f_load_data()

# considérer que les valeurs des indices depuis janvier 2005
prices <- prices["2005-01/"]
prices_sp500 <- prices$SP500
prices_ftse100 <- prices$FTSE100

# Calculer les rendements logarithmiques pour les deux séries
rets_sp500 <- f_calculate_returns(prices_sp500)[1:2000]
rets_ftse100 <- f_calculate_returns(prices_ftse100)[1:2000]

#### Estimation statique de la VaR ####

# Utiliser les premiers rendements logarithmiques de T = 1000 pour estimer 
# la VaR de chaque indice au niveau de risque de 95%.
VaR_sp500 <- f_forecast_var(rets_sp500[1:1000], 0.95)
VaR_ftse100 <- f_forecast_var(rets_ftse100[1:1000], 0.95)

# Lequel est le plus risqué à l’horizon T + 1 ?
VaR_sp500$VaR_Forecast
VaR_ftse100$VaR_Forecast
f_comparer_risque_indices(VaR_sp500$VaR_Forecast, VaR_ftse100$VaR_Forecast)

#### Backtesting ####

# En utilisant une fenêtre glissante de T = 1000 jours, calculez et stockez la VaR 
# de la prochaine étape à venir au niveau de risque de 95% pour les 1000 prochains jours.
sp500_backtest <- f_backtest_var_rolling(rets_sp500)
ftse100_backtest <- f_backtest_var_rolling(rets_ftse100)

# Affichez les séries de rendements réalisés et les estimations de la VaR 
# pour les deux séries.
sp500_predictions <- xts(sp500_backtest$VaR_Predictions, 
                         order.by = index(rets_sp500)[1001:2000])
ftse100_predictions <- xts(ftse100_backtest$VaR_Predictions, 
                           order.by = index(rets_ftse100)[1001:2000])
# Assigner les valeurs moments de violations de la VaR
sp500_is_violation <- xts(sp500_backtest$Is_Violation, 
                          order.by = index(rets_sp500)[1001:2000])
ftse100_is_violation <- xts(ftse100_backtest$Is_Violation, 
                            order.by = index(rets_ftse100)[1001:2000])

# Afficher le nombre des violations de la VaR
sp500_backtest$VaR_Violations
ftse100_backtest$VaR_Violations

# Enregistrer dans un fichier png.
f_plot_combined_returns_and_var("Output/combined_plots.png", 
                                rets_sp500, 
                                sp500_predictions, 
                                rets_ftse100, 
                                ftse100_predictions)

# Merge les valeurs de violations de la VaR aux prédictions
sp500_infos <- merge(sp500_predictions, sp500_is_violation, all = FALSE)
ftse100_infos <- merge(ftse100_predictions, ftse100_is_violation, all = FALSE)

# Enregistrer les résultats du backtest dans un fichier rda.
Backtest_results <- merge(sp500_infos, ftse100_infos, all = FALSE)
save(Backtest_results, file = here("Data", "Clean Data", "Backtest_results.rda"))

#**********************************************************************************************
# -----------------------------   Fin de MATH60633 – TP1    -----------------------------------
#**********************************************************************************************