# sds379r-polling-project
SDS379r independent research project - Replicating Multi-Year Polling Trends: Comparing 1992 and 2020 Presidential Elections

The purpose of this project was to attempt to replicate race and population density voting trends from the 1992 and 2020 presidential elections using polling conducted by ABC News and The Washington Post, collected from the Roper Center, for both years. The author cleaned and processed 12 datasets, merging them into a single dataset. Much of this data was legacy data, from defunct files that had to be modified to be forward compatible with modern data. The author used this data to run logistic regression models using R Studio IDE and the dplyr and rio packages. This model defined voting for a Democratic candidate as the success case, and used White voters from non-densely populated regions as the intercept. The findings from the polling data suggested the Democratic candidates in both election years won the Black vote within metropolitan and suburban areas.  This replicated similar results from other pre-existing research on these presidential races.