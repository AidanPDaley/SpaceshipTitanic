# Spaceship Titanic — Passenger Transport Prediction

## Overview

This project analyzes the Kaggle **Spaceship Titanic** dataset to predict which passengers were transported to an alternate dimension following a spacecraft accident. The goal was to build a high-performing, interpretable classification model that improves upon a baseline Kaggle score (~78%) through careful feature engineering, leakage-aware preprocessing, and threshold tuning.

---

## Dataset

* **Source:** Kaggle – Spaceship Titanic
* **Observations:** ~8,700 passengers
* **Target:** `Transported` (True / False)
* **Features:** Demographics, travel details, cabin information, and onboard spending behavior

---

## Methodology

### Exploratory Data Analysis

* Analyzed categorical and continuous variables to understand relationships with the target
* Identified strong signals from:

  * CryoSleep status
  * Onboard spending behavior
  * Cabin location and group membership
* Assessed class balance and error tradeoffs to guide model evaluation

### Feature Engineering

* Parsed cabin identifiers into deck, side, and number
* Created aggregate spending features
* Engineered probabilistic features:

  * **CryoSleepScore** — predicted probability of CryoSleep using out-of-fold models
  * **VIPScore** — predicted probability of VIP status using Random Forests
* Incorporated group-based imputations while avoiding train/test leakage

### Modeling

* Trained a **Random Forest classifier** on engineered features
* Used:

  * Out-of-bag validation
  * Holdout validation
  * Confusion matrices and class-specific metrics
* Generated class probabilities and applied **custom classification thresholds** rather than relying on the default 0.5 cutoff

---

## Model Evaluation

Performance was evaluated using accuracy, precision, recall, and F1 score, with special emphasis on **recall for transported passengers** (minimizing false negatives).

Example metrics from the selected operating point:

* **Accuracy:** ~91–92%
* **Recall (Transported = True):** ~98–99%
* **F1 Score:** ~92%

Multiple thresholds were tested, and the final threshold was selected to balance recall and precision based on problem context.

---

## Results

* Achieved a **Kaggle public leaderboard score of ~80%**, outperforming the baseline
* Demonstrated that threshold tuning and probabilistic feature engineering significantly improved recall without sacrificing overall model quality
* Identified key drivers of passenger transport through feature importance analysis

---

## Key Takeaways

* Probability-based feature engineering (CryoSleepScore, VIPScore) added meaningful signal
* Careful handling of data leakage is critical when engineering features from related variables
* Adjusting classification thresholds is often more impactful than changing models

---

## Technologies Used

* **Language:** R
* **Libraries:** `randomForest`, `ggplot2`, `rpart`, `psych`
* **Techniques:** EDA, feature engineering, Random Forests, threshold tuning, confusion-matrix analysis

---

## Repository Contents

* `spaceship_titanic.R` — full analysis and modeling pipeline
* `submissionRF1.csv` — Kaggle submission file
* `README.md` — project overview (this file)

---

## Author

**Aidan Daley**
Master’s Student, Applied Data Science
GitHub: [https://github.com/AidanDaley](https://github.com/AidanDaley)
