# R-ASI-FactorySizeVSProductivity
Economics term paper examining factory size, capital intensity and output per worker in Indian registered manufacturing, 2021–22 to 2023–24; examining Smithian, Marxist and Marginalists view on the same.


# Factory Size, Machinery and Output per Worker


## Overview

This project examines the relationship between factory size, capital intensity and output per worker in Indian registered manufacturing. It connects the empirical patterns to three perspectives in the history of economic thought: Adam Smith's division of labour, Marx's analysis of production and distribution, and the marginalist approach to productive inputs.

## Research Question

How are factory size and machinery intensity associated with output per worker in Indian registered manufacturing, and how do the observed patterns relate to Smithian, Marxian and marginalist explanations?

## Data

The analysis uses factory-level data from India's Annual Survey of Industries (ASI) for 2021–22, 2022–23 and 2023–24.

Official data sources:

- [ASI 2021–22](https://microdata.gov.in/nada/index.php/catalog/188)
- [ASI 2022–23](https://microdata.gov.in/NADA/index.php/catalog/205)
- [ASI 2023–24](https://microdata.gov.in/NADA/index.php/catalog/256)

## Methods

- Data cleaning and construction of analysis variables in R
- Descriptive comparisons across factory-size groups
- Regression analysis using employment, capital intensity, year effects and industry effects
- Robustness and subgroup comparisons

## Main Findings

Larger and more capital-intensive factories tend to have higher output per worker in the sample. The estimated relationship between employment size and output per worker becomes smaller after accounting for capital intensity and industry and year differences. These findings are associations and should not be interpreted as causal effects.

## Repository Contents

- `paper/`: Final term paper in PDF format
- `scripts/`: R scripts for data cleaning, dataset construction and statistical analysis
- `outputs/`: Regression tables, descriptive statistics, figures and robustness-check results
- `README.md`: Project overview, data sources, methodology and key findings

## Limitations

The analysis uses repeated cross-sections and nominal monetary measures. It does not directly measure all relevant factors, including worker skills, management quality, hours worked or workplace relations. The results therefore support cautious interpretation rather than causal conclusions.

## Author

Shirshak Das  
M.A. Economics, IIT Madras

*This project was prepared as an academic term paper. Please refer to the paper for full methodology, results, citations and limitations.*
