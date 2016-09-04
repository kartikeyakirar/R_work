READ ME
============================================================================================================================================
* Directory structur
	 proj/
	├── R/
	├── data/
	├── doc/
	├── figs/
	└── output/


* The R directory contains various files with function definitions (but only function definitions - no code that actually runs).

* The data directory contains data used in the analysis. This is treated as read only; in paricular the R files are never allowed to write to the files in here. Depending on the project, these might be csv files, a database, and the directory itself may have subdirectories.

* The doc directory contains the white paper or doc. 

* The figs directory contains the figures. you should always be able to delete the contents and regenerate them.

* The output directory contains simuation output, processed datasets, logs, or other processed things.
=============================================================================================================================================

