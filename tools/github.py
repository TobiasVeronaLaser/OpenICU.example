import polars as pl


def make_issue_checklist():
    ricu = pl.read_json("/workspaces/example/data/projects/others/ricu/inst/extdata/config/concept-dict.json")
    categories = {}

    for col in ricu.columns:
        element = list(ricu[col])[0]
        if not categories.get(element["category"]):
            categories[element["category"]] = []
        categories[element["category"]] += [element["description"]]

    for key, value in categories.items():
        print(f"- [ ] {key}:")
    for v in value:
        print(f"  - [ ] {v}")
