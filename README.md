# Detection of Respiratory Abnormalities among Patients with Amyotrophic Lateral Sclerosis (ALS) using Nocturnal PtCO2 Data Streams

## Aim
This project aims to determine if continuous PtCO2 data streams can be used as a potential method to facilitate the detection of respiratory abnormalities. In this study, we applied a two-stage framework to examine whether PtCO2-related features are associated with standard clinical respiratory function measurements (FVC and MIP).

## Method

### Stage I: Feature Extraction
In this stage, we extracted two sets of features. The first set comprises clinically meaningful features, including the mean PtCO2, the range of PtCO2, abnormal events, and so on. The second set consists of wavelet features, which are derived by applying a discrete wavelet transform to the data streams. While these wavelet features may have less direct clinical relevance, they are more effective in capturing PtCO2 changes across different frequency domains.

### Stage II: Hierachical Clustering
We applied a hierarchical clustering method to the set of wavelet features and identified two distinct clusters. Subsequent analysis revealed that these clusters significantly differentiate MIP levels, demonstrating that PtCO2-related features can aid in detecting respiratory abnormalities.

To enhance the visualization of the data and our methods, we created a Shiny app, which can be accessed through the following link: https://zren0723.shinyapps.io/co2_app/