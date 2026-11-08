# Nino Werkhoven – Data Projects

Economics and Business Economics graduate from Utrecht University, with a minor in Applied Data Science. Starting the MSc Statistics and Data Science at Leiden University in February 2027.

This repository showcases my projects in data analysis, statistical modelling and machine learning, using R, SQL and Power BI. My work focuses on applying quantitative methods to real-world questions, with an interest in prediction, risk analysis and financial applications.

**Contact:** [ninowerkhoven@gmail.com](mailto:ninowerkhoven@gmail.com)

## Projects

| Project | Description | Tools |
|---|---|---|
| [Flight Delay Analysis & Prediction](Flight_Insurance) | Analyses and predicts delays and cancellations for approximately 635,000 US domestic flights, and explores the financial implications for travel insurance. | SQL, Power BI, R, XGBoost, Shiny |
| Predicting Employment Loss (Bachelor Thesis) | Estimates which Dutch firms would experience additional employment losses due to COVID-19, beyond expected employment losses. | R, logistic regression |
| Spotify Popularity Prediction | Predicts song popularity based on audio features and compares different model specifications. | R, elastic net, Shiny |

## Flight Delay Analysis & Prediction

Data Science Lab project analysing flight and weather data for approximately 635,000 US domestic flights (2010–2019). The project combines SQL-based data analysis, Power BI visualisation and machine learning to investigate flight delays and cancellations and explore their financial implications for travel insurance.

- **01_SQL_PowerBI_Analysis:** SQL analysis of flight performance, delays and cancellations using aggregations, CTEs and views. Includes an interactive Power BI dashboard exploring patterns by airline, airport, month and departure time.
- **02_Machine_Learning_Insurance:** Developed XGBoost models to predict flight delays and cancellations, comparing predictions based on historical weather information with predictions using actual departure-day weather. The use of actual weather information improved AUC from 0.65 to 0.69 for delay prediction and from 0.69 to 0.79 for cancellation prediction.
- **Insurance pricing and profitability analysis:** Applied cancellation predictions to a simulated travel insurance pricing case. Explored risk-based premiums across customer risk groups and compared the potential profitability of booking-time and more recent weather-based pricing strategies.
- **Interactive R Shiny app:** Translates model predictions into personalised travel insurance advice, estimates break-even premiums and explains key risk factors using SHAP values.

## Predicting Employment Loss (Bachelor Thesis)

Can we estimate, using only information available at the time, how many jobs a firm would lose because of COVID-19, separately from jobs it would have lost anyway?

- Developed two logistic regression models using approximately 10 million firm-year observations from the Dutch LISA employment register (2000–2016): one predicting firm exit within three years and one predicting staff reductions within one year.
- Decomposed predicted employment losses into a structural component (firm size, age, sector, province and workforce composition) and a crisis-induced component (reported revenue loss).
- Applied the models to firms that received support in the first round of the Dutch NOW wage subsidy programme.
- Out-of-sample performance was moderate (AUC 0.69 for firm exit and 0.65 for staff reductions). Since the counterfactual outcome without COVID-19 cannot be observed, the estimates are informative for groups of firms rather than individual firms.

## Spotify Popularity Prediction

Developed an elastic net model to predict song popularity based on audio features.

- Compared different model specifications and settings.
- Evaluated predictive performance and presented the results in an interactive R Shiny app.
