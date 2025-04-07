# Macro-Projections
Produces projections of macroeconomic and budgetary aggregates. Based on CBO's baseline. Used as input for Budget Lab models, both for the baseline economic outlook and for analysis of alternative economic scenarios. 

The current iteration of this component is an R script which takes as input raw data (stored in shared/raw_data) from CBO and SSA, as well as pulling CPI/CCPI data from FRED. The script compiles and, where necessary, transforms macro series -- both historical data and CBO projections.

This model creates two files. The first is **projections.csv**. This file contains the full set of economic and budgetary variables starting in the first year for which we do not have complete historical data (currently 2025). The second file is **historical.csv**. This file contains historical values as far bask as 1970 for a subset of projected variables. The main purpose of this file is to link historical and projected inflation data for indexing calculations in `Tax-Simulator`.
