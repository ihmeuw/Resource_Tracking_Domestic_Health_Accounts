
Pipeline for estimating ODA for harmful practices.  

First we run a keyword search across the entire CRS database to search for projects related to the harmful practices.  
This is done via a Jobmon workflow, in particular scripts 1_prep_inputs.R prepares data for the KWS, 2_jobmon_kws.R runs jobs that execute kws_main.R to run the KWS over batches of data, and 3_post_kws.R finalizes the keyword searched data.  
Use the control.yml file to adjust which aspects of these scripts will be rerun as needed.  

