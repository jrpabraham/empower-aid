# Toward a science of delivering aid with dignity: Experimental evidence and local forecasts from Kenya
_Catherine Thomas, Nicholas Otis, Justin Abraham, Hazel Markus, and Gregory Walton_

This is a public repository containing replication files for "Toward a science of delivering aid with dignity: Experimental evidence and local forecasts from Kenya". Experimental data was collected using questionnaires programmed in Qualtrics. Results published in the manuscript and appendix were analyzed using R-3.6.2. To replicate data analysis, first open `empower-aid.Rproj` in the root directory. Run `Study1.r` to produce results for Study 1 in Kenya, `Study2.r` to produce forecasting results in Study 2, and `Study3.r` for Study 3 in the US.

### Contents

+ `data/`: Folder containing individual data.
	+ `KenyaData.Rdata` contains all of the variables used for empirical analysis for Study 2.
	+ `USData.Rdata` contains all of the variables used for empirical analysis for Study 3.
+ `doc/`: Folder containing tables published in the manuscript and appendix.
	+ `S1_appendix.docx` is an RTF file which includes all of the tables related to Study 1.
	+ `S2_appendix.docx` is an RTF file which includes the forecasting regression in Study 2.
	+ `S3_appendix.docx` is an RTF file which includes all of the tables related to Study 3.
+ `graphics/`: Folder containing figures published in the manuscript and appendix.
	+ `Figure1.png`
	+ `Figure2.png`
	+ `FigureS1.png`
	+ `FigureS2.png`
	+ `FigureS3.png`
+ `r/:` Folder containing source code for analyzing data and producing tables and figures.
	+ `Study1.r` is the source code for Study 1 that takes the Kenyan data and outputs clean data, tables of results, and figures.
	+ `Study2.r` is the source code for Study 2 that takes uses the Kenyan data to produce forecasting results and figures.
	+ `Study3.r` is the source code for Study 3 that takes the US data and outputs clean data, tables of results, and figures.
	+ `Funs.r` loads user-defined functions used in the data analysis.