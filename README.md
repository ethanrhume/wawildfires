# Predicting Wildfire Smoke-Associated Acute Care Utilization

## Background
Wildfires are increasing in frequency and severity across the Pacific Northwest,
and wildfire smoke (WFS) is the primary driver of rising PM2.5 concentrations in 
Washington State. WFS exposure is associated with increased cardiorespiratory acute care utilization, yet
Washington remains understudied compared to other heavily impacted regions. Climate
adaptation requires health systems to develop practical, real-time forecasting tools to anticipate
shifting acute care utilization patterns during WFS events.

This repo is attached to my thesis, "Predicting Wildfire Smoke-Associated Acute Care Utilization in Washington State Using
Observed PM2.5 Data". The study had two aims: (1) to assess whether a simple WFS exposure classifier
derived from real-time public air monitor data adequately approximates a validated satellite- and
ensemble model-based smoke-day classifier (Childs et al., 2022); (2) to determine whether this
classifier could support prediction of cardiorespiratory acute care encounters in the week following
WFS exposure among commercially insured Washington State residents.

## Methods
For Aim 1, I used daily PM2.5 data from air monitors across Washington State
(2010–2018) to construct a binary WFS exposure classifier. I validated classifier performance
against gold-standard model-derived estimates using standard classification metrics. For Aim 2, I used
longitudinal commercial claims data (2015–2018) to identify a study population of 83,350 enrollees with active asthma or COPD
diagnoses. I used the LightGBM gradient boosting framework to predict cardiorespiratory ED
visits and inpatient admissions within one week of smoke exposure.
