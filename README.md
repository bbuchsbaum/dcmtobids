# dcmtobids (R)

R port scaffold of Dcm2Bids with a turnkey CLI.

## Install

```bash
git clone <repo-url> dcmtobids
cd dcmtobids

# install package
R CMD INSTALL .

# install CLI launcher
Rscript -e "dcmtobids::install_cli('~/.local/bin')"

# ensure launcher dir is on PATH (zsh)
echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.zshrc
source ~/.zshrc

# verify
dcmtobids help
```

## Alternative Install Methods

### Install with `pak` (GitHub)

```bash
Rscript -e "install.packages('pak', repos='https://r-lib.github.io/p/pak/stable')"
Rscript -e "pak::pak('<owner>/<repo>')"
Rscript -e "dcmtobids::install_cli('~/.local/bin')"
dcmtobids help
```

### Install with `pak` (local checkout)

```bash
cd /path/to/dcmtobids
Rscript -e "install.packages('pak', repos='https://r-lib.github.io/p/pak/stable')"
Rscript -e "pak::pak('local::.')"
Rscript -e "dcmtobids::install_cli('~/.local/bin')"
dcmtobids help
```

### Install with Homebrew (if you publish a formula)

```bash
brew tap <owner>/<tap>
brew install dcmtobids
dcmtobids help
```

## CLI Commands

- `init-config`: write a starter config JSON.
- `doctor`: check environment/config/output readiness.
- `inspect`: inventory sidecars and export a `sidecarinfo.tsv` table.
- `populate-intended-for`: recompute `IntendedFor`/`Sources` in existing BIDS JSONs.
- `validate-config`: validate JSON config structure.
- `dry-run`: preview source -> destination mappings.
- `convert`: convert from helper/NIfTI+JSON directory.
- `run`: one-command DICOM -> BIDS (`dcm2niix` + convert).

`convert` copies helper/NIfTI inputs into the BIDS output tree; it does not
consume the source helper directory.

## Turnkey Setup Flow

```bash
dcmtobids init-config --output ./config.json
dcmtobids inspect --input-dir ./helper --out-tsv ./sidecarinfo.tsv
dcmtobids doctor --config ./config.json --bids-dir ./bids
```

## Post-Conversion IntendedFor Repair

If you changed config `sidecar_changes` after a conversion, re-apply only
`IntendedFor`/`Sources` without moving files:

```bash
dcmtobids populate-intended-for \
  --config ./config.json \
  --input-dir ./helper \
  --bids-dir ./bids \
  --participant 01 \
  --report ./populate-report.json
```

Use `--skip-dcm2niix` in `doctor` if you are validating only a helper/NIfTI workflow:

```bash
dcmtobids doctor --skip-dcm2niix --config ./config.json --bids-dir ./bids
```

## Vignette

See the introductory vignette in [vignettes/introduction.Rmd](vignettes/introduction.Rmd)
for an end-to-end example using helper-style files (no real DICOM required).

## Quick Start Without Real DICOM

Use `--skip-dcm2niix` with precomputed NIfTI/JSON files.

```bash
dcmtobids run \
  --skip-dcm2niix \
  --dicom-dir /path/to/helper \
  --config /path/to/config.json \
  --bids-dir /path/to/bids \
  --participant 01 \
  --report /path/to/report.json
```

## Real DICOM Run

```bash
dcmtobids run \
  --dicom-dir /path/to/dicom_dir \
  --config /path/to/config.json \
  --bids-dir /path/to/bids \
  --participant 01 \
  --dcm2niix-bin dcm2niix \
  --dcm2niix-options "-b y -ba y -z y -f '%3s_%f_%p_%t'"
```

Use `--force-dcm2bids` to regenerate helper output and `--clobber` to overwrite BIDS outputs.
Use `--fail-on-warning` to fail CI if any warnings are emitted.

## QA Gating

When `--report` is provided, the report includes machine-readable QA fields:

- `qa.gate`: `pass`, `warn`, or `fail`
- `qa.codes`: aggregated warning/error codes with counts
- `warnings`: detailed events with `code`, `severity`, `message`, and `context`

## Run Manifests

`dry-run`, `convert`, `run`, and `populate-intended-for` automatically write
run records under:

- `.dcmtobids/runs/<run-id>/meta.json`
- plus available artifacts (`config.json`, `plan.tsv`, `report.json`, etc.)

## Regenerate Python Golden Snapshots

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
