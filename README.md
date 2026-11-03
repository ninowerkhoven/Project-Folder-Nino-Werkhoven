# Nino Werkhoven – Data Projects

Economics and Business Economics graduate (Utrecht University, minor Applied Data Science), starting the MSc Statistics and Data Science at Leiden University in February 2027. This repository collects my data analysis and prediction projects in R, SQL and Power BI.

Contact: ninowerkhoven@gmail.com

## Projects

| Project | What it does | Tools |
|---|---|---|
| [Flight Delay Analysis & Prediction](Flight_Insurance) | Analyses and predicts delays and cancellations of about 635,000 US domestic flights | SQL, Power BI, R, XGBoost, Shiny |
| Predicting Employment Loss (Bachelor Thesis) | Estimates which Dutch firms would lose extra jobs because of COVID-19, separately from jobs lost anyway | R, logistic regression |
| Spotify Popularity Prediction | Predicts the popularity of songs from their features | R, elastic net, Shiny |

## Flight Delay Analysis & Prediction

- Data Science Lab project analysing flight and weather data for approximately 635,000 US domestic flights (2010–2019). The project combines SQL-based data analysis, Power BI visualisation and machine learning to investigate flight delays and cancellations and explore their financial impact on travel insurance.

- 01_SQL_PowerBI_Analysis: SQL analysis of flight performance, delays and cancellations using aggregations, CTEs and views. Includes an interactive Power BI dashboard exploring patterns by airline, airport, month and departure time.

- 02_Machine_Learning_Insurance: XGBoost models to predict flight delays and cancellations, with model performance evaluation. Predictions are translated into expected financial outcomes and break-even insurance premiums. Includes an interactive R Shiny app that provides personalised advice, estimates a fair insurance premium and explains key risk factors using SHAP values.

## Predicting Employment Loss (Bachelor Thesis)

Can we estimate, using only information available at the time, how many jobs a firm would lose because of COVID-19, separately from jobs it would have lost anyway?

- Two logistic regression models on about 10 million firm-year observations from the Dutch LISA employment register (2000–2016): one for firm exit within three years, one for staff reductions within one year.
- Each prediction is split into a structural part (firm size, age, sector, province, workforce composition) and a crisis-induced part (reported revenue loss).
- Applied to firms that received support in the first round of the Dutch NOW wage subsidy programme.
- Out-of-sample accuracy is moderate (AUC 0.69 for exit, 0.65 for staff reductions). The estimates cannot be checked against what would have happened without COVID, so they are informative for groups of firms rather than individual firms.

## Spotify Popularity Prediction

An elastic net model that predicts song popularity, comparing different model settings, with results presented in an interactive R Shiny app.
