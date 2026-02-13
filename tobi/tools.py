import polars as pl


def itemid_list_to_label_regex(df: pl.DataFrame, itemids: list[int], itemid_col_name: str = "itemid", label_col_name: str = "label") -> str:
    labels = df.filter(pl.col(itemid_col_name).is_in(itemids)).select(label_col_name).collect()[label_col_name].to_list()  # ty:ignore[not-subscriptable]
    if len(itemids) != len(labels):
        raise ValueError(f"Expected {len(itemids)} labels, but got {len(labels)}")
    return "(" + "|".join(labels) + ")"