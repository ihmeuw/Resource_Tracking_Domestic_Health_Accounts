#
# Jobmon workflow for keyword searching across batched data.
#
# Steps:
# - instantiate tool & workflow
# - define task templates for each type of task needed
#   - partition task creates batches from a single data set
#   - keyword search tasks run the keyword search on each individual batch
# - generate specific instances of tasks by filling in template arguments
#   - control what tasks can be run by setting the control parameters
# - add tasks to workflow and run it
#
library(jobmonr)
source(here::here("src", "r", "params.R"))
source(here::here("src", "r", "logging.R"))


PATHS <- list(
    dirs = list(
        working = get_path("int", "data", c("hp_kws")),
        stdout  = get_path("int", "data", c("hp_kws", "out")),
        stderr  = get_path("int", "data", c("hp_kws", "err"))
    ),
    scripts = list(
        kws_main = here::here("analysis", "1_retro", "donor", "harmful_practices",
                              "kws_main.R")
    ),
    r_shell = "/FILEPATH/execRscript.sh"
)



PARAMS <- list(
    user     = Sys.info()["user"],
    dt_stamp = format(Sys.time(), "%Y-%m-%d_%H:%M:%S"),
    dt_id    = format(Sys.time(), "%Y%m%d%H%M%S"),
    cluster_proj = "proj_fgh",
    # need to be consistent with num cores so actual resources are in harmony
    # with what data.table believes
    cores = 4L
)


CONTROL <- yaml::read_yaml(
    here::here("analysis", "1_retro", "donor", "harmful_practices", "control.yml")
)[["run_kws"]]


# =============================================================================
# HELPERS
# =============================================================================
load_configs <- function(wdir) {
    configs <- list()
    files <- grep("_kws_config.json$", dir(wdir), value = TRUE)
    for (f in files) {
        cfg <- lapply(jsonlite::read_json(file.path(wdir, f)), unlist)
        configs[[cfg$name]] <- cfg
    }
    return(configs)
}

# =============================================================================
#               TOOL & WORKFLOW
# =============================================================================
#
# instantiate tool and workflow
#
JM <- new.env(parent = emptyenv())

JM$tool <- jobmonr::tool(name = "unfpa_hp_kws")

JM$workflow <- jobmonr::workflow(
    tool = JM$tool,
    name = paste0("unfpa_hp_kws_", PARAMS$dt_stamp)
)



#
# set default compute resources for workflow
#
. <- jobmonr::set_default_workflow_resources(
    workflow = JM$workflow,
    default_cluster_name = "slurm",
    resources = list(
        cores = PARAMS$cores,
        queue = "all.q",
        runtime = "00:20:00",
        memory = "4G",
        project = PARAMS$cluster_proj,
        stdout = PATHS$dirs$stdout,
        stderr = PATHS$dirs$stderr,
        constraints = "archive" # archive node has access to J drive
    )
)



# =============================================================================
#              TASK TEMPLATES 
# =============================================================================
#
# create task templates and define any particular resource deviations
#

# PARTITION
# this task partitions the data into chunks
kws_partition_task <-
    jobmonr::task_template(
        tool = JM$tool,
        template_name = "kws_partition_task",
        command_template = paste(
            # This line protects against hard-to-debug errors:
            # - `PYTHONPATH` prevents confusion between R, Reticulate, and Python
            # - `OMP_NUM_THREADS` is used by data.table for multi-threading
            "PYTHONPATH= OMP_NUM_THREADS={n_cores}",
            PATHS$r_shell,
            " -s", PATHS$scripts$kws_main,
            " --partition", # flag for script
            " --name {data_name}",
            " --wdir {wdir}",
            sep = " "
        ),
        node_args = list("data_name", "wdir"),
        task_args = list(),
        op_args = list("n_cores")
    )

. <- jobmonr::set_default_template_resources(
    task_template = kws_partition_task,
    default_cluster_name = "slurm",
    resources = list(
        cores = PARAMS$cores,
        memory = "20G",
        runtime = "00:10:00"
    )
)



# KEYWORD SEARCH
# this task runs the kws on a single batch of data
kws_main_task <-
    jobmonr::task_template(
        tool = JM$tool,
        template_name = "kws_main_task",
        command_template = paste(
            "PYTHONPATH= OMP_NUM_THREADS={n_cores}",
            PATHS$r_shell,
            " -s", PATHS$scripts$kws_main,
            " --name {data_name}",
            " --batchid {batch_id}",
            " --wdir {wdir}",
            sep = " "
        ),
        node_args = list("data_name", "batch_id", "wdir"),
        task_args = list(),
        op_args = list("n_cores")
    )

. <- jobmonr::set_default_template_resources(
    task_template = kws_main_task,
    default_cluster_name = "slurm",
    resources = list(
        cores = PARAMS$cores,
        memory = "5G",
        time = "00:15:00"
    )
)



# =============================================================================
#               GENERATE TASKS
# =============================================================================
log_info("")
log_info("***")
log_info("Keyword Search Jobmon Workflow")
log_info("***")

#
# generate a list of specific tasks to add to the workflow
#
JM$task_list <- list(
    partition = list(),
    kws = list()
)

configs <- load_configs(PATHS$dirs$working)
pre_files <- dir(file.path(PATHS$dirs$working, "pre"))
process_data <- names(configs)


# PARTITION
log_info("Adding partition tasks...")

for (nm in process_data) {
    done <- grep(paste0("^", nm, "_"), pre_files, value = TRUE)
    skip <- length(done) > 0 && !CONTROL[[nm]]$repartition
    if (skip) {
        log_info("- ", nm, ": skip - don't repartition")
        next
    }
    log_info("- ", nm, ": adding partition task")
    JM$task_list$partition[[nm]] <- 
        jobmonr::task(
            task_template = kws_partition_task,
            name = paste0(nm, "_part"),
            data_name = nm,
            wdir = PATHS$dirs$working,
            n_cores = PARAMS$cores
        )
}


# KEYWORD SEARCH
log_info("Adding kws tasks...")

post_files <- dir(file.path(PATHS$dirs$working, "post"))
for (nm in process_data) {
    done <- grep(paste0("^", nm, "_"), post_files, value = TRUE)
    skip <- length(done) > 0 && !CONTROL[[nm]]$rekws
    if (skip) {
        log_info("- ", nm, ": skip - don't re-run kws")
        next
    }

    nbatch <- configs[[nm]]$nbatch[[1]]
    log_info("- ", nm, ": adding ", nbatch, if (nbatch==1) " batch" else " batches")
    added_upstream <- FALSE
    for (id in seq(nbatch)) {
        taskid <- paste0(nm, "_", id)
        JM$task_list$kws[[taskid]] <- 
            jobmonr::task(
                task_template = kws_main_task,
                name = paste0(nm, "_kws_", id),
                batch_id = id,
                data_name = nm,
                wdir = PATHS$dirs$working,
                n_cores = PARAMS$cores
            )
        # DEPENDENCY
        # add partition task for this data set as dep if it does need to run
        if (!is.null( JM$task_list$partition[[nm]] )) {
            JM$task_list$kws[[taskid]]$add_upstreams(
                tasks = c(JM$task_list$partition[[nm]])
            )
            added_upstream <- TRUE
        }
    }
    if (added_upstream)
        log_info("  -- ", nm, ": added partition task upstream to all kws tasks")
}


# =============================================================================
#               RUN
# =============================================================================
for (dir in PATHS$dirs) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}

for (script in PATHS$scripts) {
    if (!file.exists(script))
        stop("Script not found: ", script)
}

#
# add tasks to workflow
JM$workflow$add_tasks(tasks = unname(unlist(JM$task_list)))
#
# run
log_info("Running jobmon workflow...")
# (36000s is default, in python api)
run <- jobmonr::run(JM$workflow,
                    resume = FALSE,
                    seconds_until_timeout = 36000)

log_info("Workflow ended.")

