# SAFETY-VAC Objective 2

This is the repository of the script implementing the Objective 2 of the SAFETY-VAC project. The study is registered in the EMA-HMA Catalogues [here](https://catalogues.ema.europa.eu/node/4134/administrative-details). 

The detailed documentation of the script can be browsed starting from [this link](https://vac4eu.github.io/ROC18_Objective_2/diagram.html). From the tree at this link, you can browse the codebooks of the intermediate dataset (represented by rectangles) and the steps of the R procedure (represented by circles).

To reproduce the script

- clone the repository in your R Studio
- open the folder i_input_subpop: this contains synthetic data where the script can be executed
- open the to_run.R file
- run the header (rows 1-150): this will load packages and set up parameters
- run the steps (rows 155 onwards): they correspond to the circles in the documentation above and each of them generates intermediate datasets; after each step is ru, the intermediate dataset can be opened in the folder g_intermediate
- at the end of the run open the folder g_export and find the produced tables and figures

Some steps also come with test mode, for example 03_T2_11_create_cohort_flowcharts.R. Such steps can be executed in test mode by setting TEST <- T and running them. In this case, the script points to a folder of dummy data that are easy to inspect
