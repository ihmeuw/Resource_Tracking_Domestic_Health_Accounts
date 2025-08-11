#' @Author: USERNAME
#' @Date_code_last_updated: 04/04/2025
#' @Date_data_last_updated: 04/04/2025
#' @Purpose: 
#' Running SFMA models on THEpc vs HALE or U5M by draw to create draws of inefficiency scores
#' @Methods: 
#' Apply MSCA SFMA model on retrospective THEpc vs HALE or U5M by draw; 
#' @Input_files: None
#' @Output_files: 
#' Individual draw files containing frontier fit, observed outcome values, and 
#' inefficiencies saved out to the following directory: 
#' `FILEPATH` 
#' Outcome and version names defined in this script.
#' @Dependencies: 
#' - This pipeline depends on the IHME SFMA package, available in a custom conda environment here: 
#' `FILEPATH`
#' @How_to_run: 
#' Run on cluster interactively in an RStudio session using latest SciComp image
#' @Approximate_runtime: 
#' Median: ~60 seconds per job. Min: 8 seconds, Max: 82 seconds (Linear Covs)
#' Median: ~100 seconds per job. Min: 45 seconds, Max: 162 seconds (PAF spline)

library(jobmonr)

#-- VARIABLES FOR CURRENT RUN -------------------------------------

# get the date
date <- gsub("-", "_", Sys.Date()) # replace the '-' in date with '_'

# set version name
version = "T"
# specify indicator to run, either "HALE" or "U5M"
# outcome <- "HALE"
outcome <- "U5M"

# set username
user <- Sys.info()[["user"]]

#-- PATHS FOR CURRENT RUN -------------------------------------

repo_path = file.path("FILEPATH", user, "bmgf_econometrics")
code_path <- file.path(repo_path, "FILEPATH")
root_fold <- file.path("FILEPATH", 
                       outcome, version)

# Create folders for current run
dir.create(root_fold)
dir.create(file.path(root_fold, "result_draws"))

#-- JOB INFO FOR CURRENT RUN -------------------------------------

log_dir <- paste0("FILEPATH", user)

cluster <- "slurm"
queue <- "long.q"
cluster_proj <- "proj_fgh"
r_shell <- "FILEPATH/execRscript.sh -s"
python_shell <- "FILEPATH/python_SFMA.sh"

#-- SET UP JOBMON WORKFLOW AND TEMPLATES -------------------------------------

tool <- tool(name=paste0(outcome, 'SFMA', date))
time_stamp = gsub(":", ".", format(Sys.time(), "%X")) # extracts just the time and replaces ':' with '.'
workflow <- jobmonr::workflow(tool = tool,
                              workflow_args=paste0(outcome, '_workflow_', date, "_", time_stamp),
                              name=paste0(outcome, "_SFMA_draws_", version)
)
jobmonr::set_default_workflow_resources(
  workflow=workflow,
  default_cluster_name=cluster,
  resources=list(
    "project"=cluster_proj,
    "working_dir"=getwd(),
    "stdout"=paste0(log_dir, "FILEPATH"),
    "stderr"=paste0(log_dir, "FILEPATH")
  )
)

#-- CREATE TEMPLATES ---------------------------------------------------------

resources_SFMA_frontier <- list(
  "cores"= as.integer(1), # threads
  "queue"= queue,
  "runtime"= 600, # in seconds 
  "memory"= "500M" # MB
)
template_SFMA_frontier <- jobmonr::task_template(
  tool=tool,
  template_name=paste0(outcome, "_SFMA_frontier"),
  command_template=(paste0("PYTHONPATH= OMP_NUM_THREADS=", resources_SFMA_frontier$cores,
                           " {python_shell} {scriptname} --draw_num {draw_num} --outcome {outcome} --version {version}")),
  node_args=list("draw_num"),
  task_args=list("outcome", "version"),
  op_args=list("python_shell", "scriptname")
)
set_default_template_resources(
  task_template=template_SFMA_frontier,
  default_cluster_name=cluster,
  resources=resources_SFMA_frontier
)

#-- CREATE TASKS -------------------------------------------------------------

# Run SFMA 250 times to get draws of frontier predictions and inefficiencies
SFMA_frontier_tasks <- list()
for (draw_num in c(0:249)) {
  SFMA_frontier_task <- jobmonr::task(
    task_template=template_SFMA_frontier,
    cluster_name=cluster,
    compute_resources=resources_SFMA_frontier,
    name=paste0(outcome, "_SFMA_frontier_", draw_num),
    python_shell=python_shell,
    scriptname=file.path(code_path, "2_SFMA_frontier.py"),
    draw_num=as.integer(draw_num),
    outcome=outcome,
    version=version
    )
  SFMA_frontier_tasks[[draw_num+1]] <- SFMA_frontier_task
}

#-- ADD TASKS AND RUN WORKFLOW --------------------------------------------------------

workflow <- jobmonr::add_tasks(workflow, 
                               unlist(SFMA_frontier_tasks, use.names=FALSE))

status <- jobmonr::run(
  workflow=workflow,
  resume=FALSE,
  seconds_until_timeout=36000
)

if (status != "D") {
  stop("The workflow failed")
} else {
  message("The workflow completed successfully")
}

