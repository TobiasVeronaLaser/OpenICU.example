from pathlib import Path

import polars as pl

# from regex_generator import words_to_compact_regex


src_file_path = Path("/workspaces/example/data/projects/ricu/inst/extdata/config/concept-dict.json")
src_df = pl.read_json(src_file_path)
d_items_df = pl.scan_csv("/workspaces/example/data/physionet.org/files/mimiciv/3.1/icu/d_items.csv.gz")
d_items_df = d_items_df.with_columns((pl.col("itemid").cast(pl.Utf8) + "//" + pl.col("label")).alias("code"))
d_labitems_df = pl.scan_csv("/workspaces/example/data/physionet.org/files/mimiciv/3.1/hosp/d_labitems.csv.gz")
d_labitems_df = d_labitems_df.with_columns((pl.col("itemid").cast(pl.Utf8) + "//" + pl.col("label")).alias("code"))
base_path = Path("/workspaces/config/concept")

data = {}

def __main__():
    data = extract_concept_config()
    for 

def extract_concept_config() -> dict:
    for concept in src_df.columns:
        if (sources := src_df[concept][0].get("sources")) and sources.get("miiv"):
            if (ids := (miiv := sources["miiv"][0]).get("ids")) is None:
                continue
            cnpt_entry = src_df[concept][0]
            cnpt_category = cnpt_entry["category"]
            name = cnpt_entry["description"]
            table = miiv["table"]

            if not data.get(cnpt_category):
                data[cnpt_category] = {}
            # print(cnpt_entry)

            if isinstance(ids, str) or (isinstance(ids, list) and isinstance(ids[0], str)):
                continue
            ids: list[str] = ids if isinstance(ids, list) else [ids]
            data[cnpt_category][name] = {
                "name": cnpt_entry["description"].replace(" ", "_"),
                "unit" : cnpt_entry.get("unit"),
                "table" : table,
                "code" : None,
                "ids" : ids,
                "short" : concept,
                "min" : cnpt_entry.get("min"),
                "max" : cnpt_entry.get("max"), 
            }
            # print(data[cnpt_category][name])


            filtered_items = (d_labitems_df if table == "labevents" else d_items_df).filter(pl.col("itemid").is_in(ids)).select("code").collect()
            code_list = list(filtered_items.to_dict()["code"])
            regex = "|".join(code_list)
            # print(regex)
            # print(words_to_compact_regex(code_list))
            data[cnpt_category][name]["code"] = "(" + regex + ")"
    return data
    

def create(path: str, name: str, unit: str, table:str, cnpt_pattern: str) -> None:
    data = {
        "name": name,
        "version": "1.0.0",
        "unit": unit,
        "extension_columns": {
            "dataset": 'col("dataset")',
            "table": 'col("table")',
        },
        "mappings": [
            {
                "pattern": {
                    "dataset": "mimic-iv",
                    "version": "3.1",
                    "table": table,
                    "event": "result",
                    "code": cnpt_pattern,
                },
                "columns": {
                    "numeric_value": "col(numeric_value)",
                    "text_value": "col(text_value)",
                },
            }
        ],
    }

    Path(path).mkdir(parents=True, exist_ok=True)

    out_file = Path(path) / f"{name}.yml"

    with open(out_file, "w", encoding="utf8") as f:
        yaml.dump(data, f, sort_keys=False)

    print(f"YAML written to {out_file}")