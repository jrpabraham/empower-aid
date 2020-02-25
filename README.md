# How to deliver aid with narratives that empower: Experimental evidence and local forecasts from Kenya
_Catherine Thomas, Nicholas Otis, Justin Abraham, Hazel Markus, and Gregory Walton_

This is a public repository containing replication files for "How to deliver aid with narratives that empower: Experimental evidence and local forecasts from Kenya". Experimental data was collected using questionnaires programmed in Qualtrics. Results published in the manuscript and appendix were analyzed using R. To replicate data analysis, first open `START.Rproj` in the root directory. Run `K1_analysis.r` to produce results for Study 1 in Kenya, `Forecast.r` to produce results for Study 2, and `U1_analysis.r` for Study 3 in the US.

### Directories

+ `data/`: Folder containing individual data.
+ `doc/`: Folder containing tables published in the manuscript and appendix.
+ `graphics/`: Folder containing figures published in the manuscript and appendix.
+ `r/:` Folder containing source code for analyzing data and producing tables and figures.

### Data

+ `K1_Field_Survey.csv` contains de-identified individual data for the Kenyan experiment (Study 2).
+ `K1_Clean_Data.csv` contains all of the variables used for empirical analysis for Study 2.
+ `U1_MTurk_Eligible.csv` contains de-identified individual data for the US donor experiment (Study 3). This only includes subjects after screening for eligibility.
+ `U1_Clean_Data.csv` contains all of the variables used for empirical analysis for Study 3.

### Source Code

+ `K1_analysis.r` is the source code for Study 1 that takes the raw Kenyan data and outputs clean data, tables of results, and figures.
+ `Forecast.r` is the source code for Study 2 that takes uses the Kenyan data to produce forecasting results and figures.
+ `U1_analysis.r` is the source code for Study 3 that takes the raw US data and outputs clean data, tables of results, and figures.
