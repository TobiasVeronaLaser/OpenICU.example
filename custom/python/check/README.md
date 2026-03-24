# OpenICU-Example


## Check if OpenICU produces the same number of concept entries as ricu concepts have.

### ``Ricu`` / ``R``

Currently with ``Ricu`` we have following code in ``R`` to get the entries for concept e.g. `Creatinine`
```R
library(ricu)
x = miiv$labevents[miiv$labevents[["itemid"]] == 50912, ]
crea = x = load_concepts("crea", src="miiv", aggregate = FALSE, interval = secs(1), id_type = "patient")
```
with `50912` mapping to `Creatinine`

### ``OpenICU`` / ``Python``
e.g. for `Creatinine`

The extraction config  ``labevents.yml`` contains the event entry
```yml
...
event:
    ...
    -   name: labevent
        columns:
        time: col(charttime)
        code:
            - col(label)
            - col(valueuom)
        numeric_value: col(valuenum)
        text_value: col(value)
        filters:
        - drop_na(text_value)
```

and the concept config ``creatinine.yml``

```yml
name: creatinine
version: 1.0.0
unit: mg/dL

extension_columns:
  dataset: col("dataset")
  table: col("table")

mappings:
  - pattern:
      dataset: mimic-iv
      version: "3.1"
      table: labevents
      event: labevent
      code: .*?(Creatinine)
      unit: mg/dL
    columns:
      numeric_value: col(numeric_value)
      text_value: col(text_value)

```

in ``Python`` get the concept ``Creatinine``

```python
from pathlib import Path

import polars as pl

project_path = Path.cwd() / "output" / "project"

crea_directory = project_path / "workspace" /"concept" /"creatinine" / "1.0.0"

crea_paths = [str(p) for p in crea_directory.iterdir() if p.is_file()]

crea =  pl.scan_parquet(crea_paths[0])

crea.collect()
```

![alt text](image.png)

Currently the ``OpenICU`` concept contains 4 319 508 Entries and ``ricu`` contains 4 319 091 (removed 1 566 + removed 2 960 + concept 4 314 565 entries) entries. So 417 Missing entries.

### Next steps
bring ``ricu`` into a datetime format and compare with ``openicu``.

means bring the ``times`` into the same format datetime or seconds and with the same offset.
