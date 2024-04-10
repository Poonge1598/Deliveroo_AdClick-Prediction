# Deliveroo_AdClick-Prediction

## Overview
Deliveroo, established in 2013, is a leading British online food delivery platform. Our project leverages machine learning to enhance the allocation of advertising resources, ensuring the most effective strategies are employed for ad placements and content.

## Project Description
We addressed the challenge of optimizing Deliveroo's ad campaigns through strategic data analysis and machine learning. Our goal was to predict ad click patterns, thereby refining marketing efforts for higher engagement and conversion rates.

## Data Description
The dataset includes customer interactions with Deliveroo's ad campaigns, featuring social media interactions, demographics, time spent on the website prior to clicking, and more. With 18,000 entries, it covers various features like region, daytime, carrier, and click conversion indicators.

### Length of the Dataset: 18,000 entries.
### Categorical Features: 
Includes 5 regions indicating the geographical locations of the customers in France, the customer's mobile service provider (4 carriers), the day of interaction (7 days), originating social platform (3 networks), and customer's choice of cuisine (5 cuisines).
### Numerical Features: 
Daytime (numerical representation of the time of day), duration of customer's engagement before clicking (in seconds), and the number of previous orders by the customer.
### Target Variable: 
Click Conversion, a binary indicator of whether an ad was clicked (1) or not clicked (0).

## Source of Data
Data was shared by Prof. Dr. Raoul Kübler, Associate Professor of Marketing, ESSEC Business School, Paris 

## Methodology
Our approach involved extensive EDA, preprocessing to address data imbalances and outliers, and the application of tree-based models like Decision Trees, Random Forest, and XGBoost due to their suitability for handling non-linear relationships within the data. We prioritized recall in our metrics to capture as many potential conversions as possible without significantly compromising accuracy. The end to end project was executed in R

## Results
The ensemble model combining Random Forest and XGBoost emerged as the best performer, achieving a significant improvement in recall (up to 82.85%) while maintaining high accuracy of 94.6%. This model effectively balances sensitivity and specificity, optimizing both recall and accuracy for ad click prediction.

## Dashboard
Additionally we also created a dashboard in Shiny where the user can explore the predictions made on a dataset of 2000 observations.

## Files in this repository
1. Projects Briefings.pdf - The PDF released by the professor, stating the problem statements made available to choose from, guidelines and expectations. We picked Topic 2 from this PDF. 
2. app.R - The dataset to be used for training and validating the model and then for making the final predictions
3. DSBA_Deliveroo_AdClick Prediction.ppt - The final presentation on detailing the approach and findings of the project. It includes sections on project objectives, data collection and description, methodology (with emphasis on machine learning models used), results, and conclusions. It highlights the dataset's characteristics, the analytical process for predicting ad click-through rates, and the performance of various predictive models, concluding with the project's impact on Deliveroo's marketing efficiency.
4. Deliveroo_Ad Clicks Final Predictions_XGB_Ensemble. csv - Final predictions made on prediction dataset
5. MA_Project.Rmd - The file to be uploaded on Rstudio (along with app. R dataset) to launch the dashboard
6. README - Readme guideline file to launch and use the Shiny dashboard.

## Help and Contributions
For help and issues, please open an issue in the repository. Contributions to the model or analysis are welcome via pull requests.
