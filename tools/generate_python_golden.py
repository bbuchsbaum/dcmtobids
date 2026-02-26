#!/usr/bin/env python3
"""Generate golden file-path snapshots from upstream Python Dcm2Bids.

Usage:
  python tools/generate_python_golden.py
"""

from __future__ import annotations

import shutil
import sys
import tempfile
from pathlib import Path


def main() -> int:
    repo_root = Path(__file__).resolve().parents[1]
    py_root = repo_root / "Dcm2Bids"
    sys.path.insert(0, str(py_root))

    from dcm2bids.dcm2bids_gen import Dcm2BidsGen  # type: ignore

    cases = [
        {"name": "config_test", "config": "config_test.json", "session": "", "auto": False, "noreorder": False},
        {"name": "config_test_auto_extract", "config": "config_test_auto_extract.json", "session": "", "auto": True, "noreorder": False},
        {"name": "config_test_complex", "config": "config_test_complex.json", "session": "", "auto": False, "noreorder": False},
        {"name": "config_test_dup", "config": "config_test_dup.json", "session": "", "auto": False, "noreorder": False},
        {"name": "config_test_float", "config": "config_test_float.json", "session": "", "auto": False, "noreorder": False},
        {"name": "config_test_key_absent", "config": "config_test_key_absent.json", "session": "", "auto": False, "noreorder": False},
        {"name": "config_test_multiple_intendedfor", "config": "config_test_multiple_intendedfor.json", "session": "", "auto": True, "noreorder": False},
        {"name": "config_test_multiple_intendedfor_uri_relative", "config": "config_test_multiple_intendedfor_uri_relative.json", "session": "", "auto": True, "noreorder": False},
        {"name": "config_test_no_reorder", "config": "config_test_no_reorder.json", "session": "", "auto": False, "noreorder": True},
        {"name": "config_test_not_case_sensitive_option", "config": "config_test_not_case_sensitive_option.json", "session": "", "auto": False, "noreorder": False},
        {"name": "config_test_sidecar", "config": "config_test_sidecar.json", "session": "dev", "auto": False, "noreorder": False},
    ]

    data_root = py_root / "tests" / "data"
    sidecars_root = data_root / "sidecars"
    out_dir = repo_root / "tests" / "fixtures" / "python-golden"
    out_dir.mkdir(parents=True, exist_ok=True)

    for case in cases:
        tmp_root = Path(tempfile.mkdtemp(prefix=f"py-golden-{case['name']}-"))
        try:
            input_dir = tmp_root / "input"
            bids_dir = tmp_root / "bids"
            input_dir.mkdir(parents=True, exist_ok=True)
            bids_dir.mkdir(parents=True, exist_ok=True)

            for src in sidecars_root.iterdir():
                shutil.copy2(src, input_dir / src.name)

            app = Dcm2BidsGen(
                dicom_dir=str(input_dir),
                participant="01",
                config=str(data_root / case["config"]),
                output_dir=str(bids_dir),
                session=case["session"],
                auto_extract_entities=case["auto"],
                do_not_reorder_entities=case["noreorder"],
                skip_dcm2niix=True,
            )
            app.run()

            files = []
            for fp in bids_dir.rglob("*"):
                if not fp.is_file():
                    continue
                rel = fp.relative_to(bids_dir).as_posix()
                if rel.startswith("tmp_dcm2bids/"):
                    continue
                files.append(rel)

            target = out_dir / f"{case['name']}.txt"
            target.write_text("\n".join(sorted(files)) + "\n", encoding="utf-8")
            print(f"{case['name']}: {len(files)} files")
        finally:
            shutil.rmtree(tmp_root, ignore_errors=True)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
