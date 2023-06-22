# Important note:
# All paths defined in this configuration file must be either
# absolute or relative to the location of the Snakefile!

import os
from pathlib import Path
from snakemake.utils import min_version

# Sanitize provided input and output directories
import re

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


def getpath(str):
    if str in ["", ".", "./"]:
        return ""
    if str.startswith("./"):
        regex = re.compile("^\./?")
        str = regex.sub("", str)
    if not str.endswith("/"):
        str += "/"
    return str


# print(f"Current directory: {Path.cwd()}")
# print(f"Home directory: {Path.home()}")

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
        config["quest_data1_clean"],
        config["quest_data2_clean"],
        "data/prep/quest_scales/nates_scores.csv",
        "data/prep/quest_scales/dass21_scores.csv",
        "data/prep/quest_scales/ders_scores.csv",
        "data/prep/quest_scales/neoffi60_neuro_scores.csv",
        "data/prep/quest_scales/rosenberg_scores.csv",
        "data/prep/quest_scales/rscs_scores.csv",
        "data/prep/quest_scales/scl90_scores.csv",
        "data/prep/quest_scales/scs_scores.csv",


# Read individual EMA data and save an RDS file ---------------------
rule read_ema_data:
    input:
        dir_data=config["ema_data_dir"],
    output:
        rds=config["ema_data_raw"],
    log:
        "logs/read_data.log",
    script:
        "workflows/scripts/ema/import_ema_data.R"


# Initial EMA data wrangling.
rule wrangling_ema_data:
    input:
        rds=config["ema_data_raw"],
    output:
        rds=config["ema_data_clean"],
    log:
        "logs/wangling_ema_data.log",
    script:
        "workflows/scripts/ema/data_wrangling.R"


# Read questionnaire data and save two CSV files --------------------
rule read_quest_data:
    input:
        quest_data1=config["quest_data1_raw"],
        quest_data2=config["quest_data2_raw"],
    output:
        quest_data1=config["quest_data1_clean"],
        quest_data2=config["quest_data2_clean"],
    log:
        "logs/read_quest_data.log",
    script:
        "workflows/scripts/quest/import_quest_data.R"


# Select columns of the NATES questionnaire -------------------------
rule select_cols_nates:
    input:
        quest_data1=config["quest_data1_clean"],
    output:
        nates_cols="data/prep/quest_scales/nates_items.csv",
    log:
        "logs/select_cols_nates.log",
    script:
        "workflows/scripts/quest/select_cols_nates.R"


# Scoring of the NATES questionnaire.
rule scoring_nates:
    input:
        nates_cols="data/prep/quest_scales/nates_items.csv",
    output:
        nates_score="data/prep/quest_scales/nates_scores.csv",
    log:
        "logs/scoring_nates.log",
    script:
        "workflows/scripts/quest/scoring_nates.R"


include: "workflows/rules/closing_messages.smk"


# Select columns of the DASS-21 questionnaire -----------------------
rule select_cols_dass21:
    input:
        quest_data1=config["quest_data1_clean"],
    output:
        dass21_cols="data/prep/quest_scales/dass21_items.csv",
    log:
        "logs/select_cols_dass21.log",
    script:
        "workflows/scripts/quest/select_cols_dass21.R"


# Scoring of the DASS-21 questionnaire.
rule scoring_dass21:
    input:
        dass21_cols="data/prep/quest_scales/dass21_items.csv",
    output:
        dass21_scores="data/prep/quest_scales/dass21_scores.csv",
    log:
        "logs/scoring_dass21.log",
    script:
        "workflows/scripts/quest/scoring_dass21.R"


# Select columns of the DERS questionnaire --------------------------
rule select_cols_ders:
    input:
        quest_data1=config["quest_data1_clean"],
    output:
        ders_cols="data/prep/quest_scales/ders_items.csv",
    log:
        "logs/select_cols_ders.log",
    script:
        "workflows/scripts/quest/select_cols_ders.R"


# Scoring of the DERS questionnaire.
rule scoring_ders:
    input:
        ders_cols="data/prep/quest_scales/ders_items.csv",
    output:
        ders_scores="data/prep/quest_scales/ders_scores.csv",
    log:
        "logs/scoring_ders.log",
    script:
        "workflows/scripts/quest/scoring_ders.R"


# Select columns of the NEO-FFI-60-NEURO questionnaire --------------
rule select_cols_neoffi60neuro:
    input:
        quest_data1=config["quest_data1_clean"],
    output:
        neoffi60neuro_cols="data/prep/quest_scales/neoffi60_neuro_items.csv",
    log:
        "logs/select_cols_neoffi60neuro.log",
    script:
        "workflows/scripts/quest/select_cols_neoffi60_neuro.R"


# Scoring of the NEO-FFI-60-NEURO questionnaire.
rule scoring_neoffi60neuro:
    input:
        neoffi60neuro_cols="data/prep/quest_scales/neoffi60_neuro_items.csv",
    output:
        neoffi60neuro_scores="data/prep/quest_scales/neoffi60_neuro_scores.csv",
    log:
        "logs/scoring_neoffi60neuro.log",
    script:
        "workflows/scripts/quest/scoring_neoffi60neuro.R"


# Select columns of the Rosenberg questionnaire ---------------------
rule select_cols_rosenberg:
    input:
        quest_data1=config["quest_data1_clean"],
    output:
        rosenberg_cols="data/prep/quest_scales/rosenberg_items.csv",
    log:
        "logs/select_cols_rosenberg.log",
    script:
        "workflows/scripts/quest/select_cols_rosenberg.R"


# Scoring of the Rosenberg questionnaire.
rule scoring_rosenberg:
    input:
        rosenberg_cols="data/prep/quest_scales/rosenberg_items.csv",
    output:
        rosenberg_scores="data/prep/quest_scales/rosenberg_scores.csv",
    log:
        "logs/scoring_rosenberg.log",
    script:
        "workflows/scripts/quest/scoring_rosenberg.R"


# Select columns of the RSCS questionnaire --------------------------
rule select_cols_rscs:
    input:
        quest_data1=config["quest_data1_clean"],
    output:
        rscs_cols="data/prep/quest_scales/rscs_items.csv",
    log:
        "logs/select_cols_rscs.log",
    script:
        "workflows/scripts/quest/select_cols_rscs.R"


# Scoring of the RSCS questionnaire.
rule scoring_rscs:
    input:
        rscs_cols="data/prep/quest_scales/rscs_items.csv",
    output:
        rscs_scores="data/prep/quest_scales/rscs_scores.csv",
    log:
        "logs/scoring_rscs.log",
    script:
        "workflows/scripts/quest/scoring_rscs.R"


# Select columns of the SCL90 questionnaire -------------------------
rule select_cols_scl90:
    input:
        quest_data1=config["quest_data1_clean"],
    output:
        scl90_cols="data/prep/quest_scales/scl90_items.csv",
    log:
        "logs/select_cols_scl90.log",
    script:
        "workflows/scripts/quest/select_cols_scl90.R"


# Scoring of the SCL90 questionnaire.
rule scoring_scl90:
    input:
        scl90_cols="data/prep/quest_scales/scl90_items.csv",
    output:
        scl90_scores="data/prep/quest_scales/scl90_scores.csv",
    log:
        "logs/scoring_scl90.log",
    script:
        "workflows/scripts/quest/scoring_scl90.R"


# Select columns of the SCS questionnaire ---------------------------
rule select_cols_scs:
    input:
        quest_data1=config["quest_data1_clean"],
    output:
        scs_cols="data/prep/quest_scales/scs_items.csv",
    log:
        "logs/select_cols_scs.log",
    script:
        "workflows/scripts/quest/select_cols_scs.R"


# Scoring of the SCS questionnaire.
rule scoring_scs:
    input:
        scs_cols="data/prep/quest_scales/scs_items.csv",
    output:
        scs_scores="data/prep/quest_scales/scs_scores.csv",
    log:
        "logs/scoring_scs.log",
    script:
        "workflows/scripts/quest/scoring_scs.R"
