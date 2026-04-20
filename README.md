# dcmtobids

`dcmtobids` is an R package and CLI for planning and executing DICOM-to-BIDS
conversions. It can work in two modes:

- helper mode: start from precomputed NIfTI/JSON sidecars
- full mode: run `dcm2niix` first, then convert into a BIDS tree

The package focuses on dry-run planning, config validation, reproducible CLI
workflows, and machine-readable QA output.

## What It Does

- validate a `dcmtobids` JSON config
- inspect helper sidecars before conversion
- build a dry-run mapping from source files to BIDS destinations
- copy helper-style inputs into a BIDS directory
- run `dcm2niix` as part of an end-to-end DICOM-to-BIDS workflow
- populate `IntendedFor` and `Sources` after conversion
- emit JSON reports and per-run manifests for QA and audit trails

## Installation

### Install from a local checkout

```bash
git clone https://github.com/bbuchsbaum/dcmtobids.git
cd dcmtobids
R CMD INSTALL .
```

### Install from GitHub with `pak`

```bash
Rscript -e "install.packages('pak', repos='https://r-lib.github.io/p/pak/stable')"
Rscript -e "pak::pak('bbuchsbaum/dcmtobids')"
```

### Install the CLI launcher

```bash
Rscript -e "dcmtobids::install_cli('~/.local/bin')"
```

If `~/.local/bin` is not already on your `PATH`, add it:

```bash
echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.zshrc
source ~/.zshrc
dcmtobids help
```

## Requirements

- R with the package installed
- `dcm2niix` only if you want to start from raw DICOM data

You can validate the runtime environment with:

```bash
dcmtobids doctor --config ./config.json --bids-dir ./bids
```

If you are working from precomputed NIfTI/JSON helper files, use:

```bash
dcmtobids doctor --skip-dcm2niix --config ./config.json --bids-dir ./bids
```

## Two Workflows

### 1. Helper-sidecar workflow

Use this when you already have `.nii`, `.nii.gz`, and `.json` files from
`dcm2niix` or another preprocessing step.

Relevant commands:

- `inspect`
- `dry-run`
- `convert`
- `populate-intended-for`
- `run --skip-dcm2niix`

`convert` copies helper inputs into the BIDS output tree. It does not consume
or delete the source helper directory.

### 2. Full DICOM workflow

Use this when you want `dcmtobids` to run `dcm2niix` for you and then perform
the BIDS conversion.

Relevant command:

- `run`

In this mode, helper output is written under
`<bids-dir>/tmp_dcm2bids/sub-<participant>[_ses-<session>]` before conversion.

## CLI Commands

- `init-config`: write a starter config JSON
- `doctor`: check runtime, config, and output-directory readiness
- `inspect`: inventory helper sidecars and optionally write `sidecarinfo.tsv`
- `validate-config`: validate config structure
- `dry-run`: preview source-to-destination mappings without copying files
- `convert`: execute a helper-sidecar conversion plan
- `run`: run `dcm2niix` plus conversion in one command
- `populate-intended-for`: recompute `IntendedFor` and `Sources` in existing BIDS JSONs
- `version`: print the installed package version

## Minimal Config Example

`init-config` will generate a starter file, but the core shape looks like this:

```json
{
  "search_method": "fnmatch",
  "case_sensitive": true,
  "descriptions": [
    {
      "datatype": "anat",
      "suffix": "T1w",
      "criteria": {
        "SeriesDescription": "*T1*"
      }
    },
    {
      "id": "restbold",
      "datatype": "func",
      "suffix": "bold",
      "custom_entities": ["task-rest"],
      "criteria": {
        "SeriesDescription": "*rest*"
      }
    }
  ]
}
```

Notes:

- `criteria` are matched with `fnmatch` by default
- `custom_entities` can supply entities such as `task-rest` or `dir-AP`
- add an `id` when another description needs to reference this series in
  `sidecar_changes`

## Quick Start: Helper Inputs

```bash
dcmtobids init-config --output ./config.json
dcmtobids inspect --input-dir ./helper --out-tsv ./sidecarinfo.tsv
dcmtobids validate-config --config ./config.json

dcmtobids dry-run \
  --config ./config.json \
  --input-dir ./helper \
  --bids-dir ./bids \
  --participant 01

dcmtobids convert \
  --config ./config.json \
  --input-dir ./helper \
  --bids-dir ./bids \
  --participant 01 \
  --report ./report.json
```

Useful flags for this workflow:

- `--auto-extract-entities`
- `--do-not-reorder-entities`
- `--case-insensitive`
- `--clobber`
- `--fail-on-warning`

## Quick Start: Real DICOM Input

```bash
dcmtobids run \
  --dicom-dir /path/to/dicom_dir \
  --config /path/to/config.json \
  --bids-dir /path/to/bids \
  --participant 01 \
  --dcm2niix-bin dcm2niix \
  --dcm2niix-options "-b y -ba y -z y -f '%3s_%f_%p_%t'" \
  --report /path/to/report.json
```

Notes:

- `--dicom-dir` accepts a comma-separated list of DICOM directories or archives
- use `--force-dcm2bids` to regenerate helper output
- use `--clobber` to overwrite existing BIDS outputs
- use `--fail-on-warning` to make warning-level QA fail in CI

If you already have helper files and want to keep using the `run` command:

```bash
dcmtobids run \
  --skip-dcm2niix \
  --dicom-dir /path/to/helper \
  --config /path/to/config.json \
  --bids-dir /path/to/bids \
  --participant 01 \
  --report /path/to/report.json
```

## Rebuilding `IntendedFor` and `Sources`

If you changed `sidecar_changes` after conversion, you can update existing BIDS
JSON sidecars without moving files again:

```bash
dcmtobids populate-intended-for \
  --config ./config.json \
  --input-dir ./helper \
  --bids-dir ./bids \
  --participant 01 \
  --report ./populate-report.json
```

`--bids-uri` accepts `URI` or `relative`.

## Reports and Run Manifests

When `--report` is provided, `dcmtobids` writes a machine-readable JSON report.
The report includes:

- `qa.gate`: `pass`, `warn`, or `fail`
- `qa.codes`: aggregated warning and error codes with counts
- `warnings`: detailed structured events with message and context

The commands `dry-run`, `convert`, `run`, and `populate-intended-for` also
record run artifacts under:

- `.dcmtobids/runs/<run-id>/meta.json`
- optional companion files such as `config.json`, `plan.tsv`, `report.json`,
  `helper.json`, `conversion.json`, and `populate.json`

## R API

The CLI wraps the same core package functions you can call from R:

- `read_config()`
- `validate_config()`
- `inspect_sidecars()`
- `plan_conversion()`
- `convert_plan()`
- `run_dicom_conversion()`
- `populate_intended_for_plan()`
- `summarize_plan()`
- `write_conversion_report()`

## Further Reading

- Introductory vignette: [vignettes/introduction.Rmd](vignettes/introduction.Rmd)
- Installed documentation site: `docs/`

## Regenerating Python Golden Snapshots

To refresh the upstream Python file-path parity snapshots:

```bash
python tools/generate_python_golden.py
```

## License and Upstream Attribution

This package is distributed under GPL-3 and includes adapted behavior from the
GPL-3-licensed `dcm2bids` Python project. See:

- `LICENSE`
- `inst/NOTICE`
- `inst/licenses/dcm2bids-LICENSE.txt`
