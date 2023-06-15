# Important note:
# All paths defined in this configuration file must be either
# absolute or relative to the location of the Snakefile!

import os
from pathlib import Path
from snakemake.utils import min_version

# Snake Version
min_version("5.7.1")

# Configuration file
if len(config) == 0:
    if os.path.isfile("config/config.yaml"):

        configfile: "config/config.yaml"

    else:
        sys.exit(
            "Looks like there is no config.yaml file in "
            + os.getcwd()
            + " make sure there is one or at least specify one with the --configfile commandline parameter."
        )


## Sanitize provided input and output directories
import re


def getpath(str):
    if str in ["", ".", "./"]:
        return ""
    if str.startswith("./"):
        regex = re.compile("^\./?")
        str = regex.sub("", str)
    if not str.endswith("/"):
        str += "/"
    return str


print(f"Current directory: {Path.cwd()}")
print(f"Home directory: {Path.home()}")

prepdir = getpath(config["output_prep"])
brmsdir = getpath(config["output_brms"])
scriptsdir = getpath(config["scripts_dir"])

Renv = "workflows/envs/environment_R.yaml"


# Run all analyses
rule all:
    input:
        # os.path.join(prepdir, "groundhog_raw.RDS"),
        # config["ema_data_raw"],
        config["ema_data_clean"],


# Read individual EMA data and save an RDS file.
rule read_ema_data:
    input:
        dir_data=config["ema_data_dir"],
    output:
        rds=config["ema_data_raw"],
    log:
        "logs/read_data.log",
    script:
        "workflows/scripts/ema/import_ema_data.R"


# Initial data wrangling.
rule wrangling_ema_data:
    input:
        rds=config["ema_data_raw"],
    output:
        rds=config["ema_data_clean"],
    log:
        "logs/wangling_ema_data.log",
    script:
        "workflows/scripts/ema/data_wrangling.R"


rule clean_logs:
    output:
        touch("log/.cleanup_success"),


include: "workflows/rules/closing_messages.smk"
