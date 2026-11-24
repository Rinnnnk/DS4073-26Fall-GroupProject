"""Download selected public NYC 2024 event fields for R preprocessing."""
import csv
import gzip
import io
import json
import sys
import time
from concurrent.futures import ThreadPoolExecutor
from datetime import date
from pathlib import Path
from urllib.parse import urlencode
from urllib.request import Request, urlopen

BASE = "https://data.cityofnewyork.us/resource/"
SOURCES = {
    "noise": ("erm2-nwe9", "created_date",
              "unique_key,created_date,complaint_type,borough,latitude,longitude",
              " AND starts_with(complaint_type,'Noise')"),
    "nypd": ("qgea-i56i", "cmplnt_fr_dt",
             "cmplnt_num,cmplnt_fr_dt,law_cat_cd,boro_nm,latitude,longitude", ""),
}


def download(dataset, month, out):
    asset, field, columns, extra = SOURCES[dataset]
    start = date(2024, month, 1).isoformat()
    end = (date(2024, month + 1, 1) if month < 12 else date(2025, 1, 1)).isoformat()
    where = f"{field} >= '{start}T00:00:00.000' AND {field} < '{end}T00:00:00.000'{extra}"
    path = out / f"{dataset}-2024-{month:02d}.csv"
    if path.is_file():
        with path.open(newline="") as stream:
            return path.name, sum(1 for _ in csv.reader(stream)) - 1
    params = {"$select": columns, "$where": where, "$limit": 100000}
    req = Request(BASE + asset + ".csv?" + urlencode(params), headers={"Accept-Encoding": "gzip"})
    for attempt in range(4):
        try:
            with urlopen(req, timeout=90) as response:
                payload = response.read()
                if response.headers.get("Content-Encoding") == "gzip":
                    payload = gzip.decompress(payload)
            stream = csv.reader(io.StringIO(payload.decode("utf-8-sig")))
            if next(stream) != columns.split(","):
                raise ValueError("Unexpected source columns")
            n = sum(1 for _ in stream)
            if not 0 < n < int(params["$limit"]):
                raise ValueError(f"{n} rows. Check filters or paginate.")
            path.write_bytes(payload)
            return path.name, n
        except Exception:
            if attempt == 3:
                raise
            time.sleep(2 ** attempt)


if __name__ == "__main__":
    root = Path(sys.argv[1])
    root.mkdir(parents=True, exist_ok=True)
    jobs = [(dataset, month, root) for month in range(1, 13) for dataset in SOURCES]
    with ThreadPoolExecutor(max_workers=6) as pool:
        futures = [pool.submit(download, *args) for args in jobs]
        results = {}
        for future in futures:
            name, rows = future.result()
            results[name] = rows
            print(name, rows, flush=True)
    (root / "event_counts.json").write_text(json.dumps(results, indent=2) + "\n")
