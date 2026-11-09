# Value-at-Risk sur le marché actions de la BRVM

Estimation et backtesting de la **Value-at-Risk (VaR)** sur l'indice BRVM Composite, en comparant trois approches : **paramétrique (RiskMetrics)**, **semi-paramétrique (GARCH-EVT)** et **simulation Monte Carlo**.

Mémoire de Master II, Data Science, Statistique & Économétrie (INSSEDS, Abidjan, 2025-2026), accompagné d'une **application Shiny** de monitoring du risque.

Dashboard de l'application : C:\Users\HP\Documents\GitHub\var-brvm\Results\Figure\shiny_app_dashboard

---

## Objectif

Déterminer quelle approche mesure le mieux le risque de marché sur la BRVM, un marché frontière peu liquide dont les rendements ne suivent pas une loi normale.

## Données

- **Indice** : BRVM Composite
- **Période** : du 1er juillet 2025 au 30 juin 2026
- **Observations** : 259 rendements logarithmiques journaliers
- **Découpage** : 207 observations pour l'estimation (80 %), 52 pour la validation hors échantillon (20 %)
- **Source** : Sika Finance

## Méthodologie

1. **Analyse exploratoire** : statistiques descriptives, stationnarité (ADF), normalité (Jarque-Bera), effets ARCH, autocorrélation (Ljung-Box)
2. **Sélection itérative du modèle** (GARCH, EGARCH, AR-GARCH), comparaison par AIC et BIC
3. **Estimation de la VaR à 95 %** selon trois approches :
   - Paramétrique : RiskMetrics
   - Semi-paramétrique : GARCH-EVT (méthode Peaks Over Threshold)
   - Simulation Monte Carlo
4. **Backtesting hors échantillon** : tests de Kupiec et de Christoffersen
5. **Robustesse** : bootstrap par blocs (B = 1 000, blocs de 5 observations)

## Principaux résultats

**Données** : queues épaisses (Jarque-Bera p = 0,007), pas d'effet ARCH classique (ARCH-LM p = 0,83), mais forte autocorrélation des rendements (Ljung-Box p = 0,006), typique d'un marché peu liquide.

**Modèle retenu** : **AR(5)-GARCH(1,1) avec loi de Student** (4,5 degrés de liberté). Tous les diagnostics des résidus sont validés. Les retards 4 et 5 révèlent une **structure hebdomadaire** dans la façon dont l'information se reflète dans les cours.

| Approche | VaR 95 % | Violations (sur 52) | Kupiec p-value | Verdict |
|---|---|---|---|---|
| RiskMetrics | 1,04 % | 0 | 0,021 | ❌ Rejeté (trop conservateur) |
| GARCH-EVT | 0,92 % | 1 | 0,247 | ✅ Validé |
| Monte Carlo | 0,88 % | 1 | 0,247 | ✅ Validé |

**Conclusion** : les approches **GARCH-EVT et Monte Carlo** sont validées, avec des performances statistiquement équivalentes. Le bootstrap confirme ce verdict : le taux de 5 % attendu se trouve dans l'intervalle de confiance [0 % ; 5,77 %].

Les tableaux complets (T1 à T18) sont dans le dossier C:\Users\HP\Documents\GitHub\var-brvm\Results\Table

## Application Shiny

- **Dashboard** : VaR, CVaR, volatilité et score de risque
- **Calcul VaR** : comparaison des trois approches
- **Backtesting** : validation des modèles
- **Sensibilité** : impact des paramètres
- **Comparaison** des modèles
- **Macro-Risque** : lien avec l'environnement macroéconomique
- **Prédiction ML** : approche par machine learning
- **Export** des résultats en PDF et CSV

### Lancer l'application

```bash
git clone https://github.com/Yasmine0701/var-brvm.git
```

Installer les packages R listés dans `requirements.txt`, puis dans R :

```r
shiny::runApp("var-brvm")
```

## Structure du projet

```text
var-brvm/
├── app.R, ui.R, server.R, global.R   # Application Shiny
├── modules/      # Onglets de l'application
├── functions/    # Fonctions de calcul (VaR, GARCH, EVT, ML)
├── www/          # Ressources graphiques
├── Data/         # Données BRVM
├── script/       # Scripts d'analyse du mémoire
└── Results/      # Tableaux (T1 à T18) et figures
```

## Outils

R · Shiny · rugarch · théorie des valeurs extrêmes · simulation Monte Carlo · plotly

## Auteure

**Yasmine Coulibaly**, Data Scientist, finance de marché et gestion des risques
[LinkedIn](https://www.linkedin.com/in/peteninta-lagnon-coulibaly) · coulyas74@gmail.com

Sous la direction de M. AKPOSSO Didier Martial (INSSEDS)
