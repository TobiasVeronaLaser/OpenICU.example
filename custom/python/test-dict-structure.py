from collections.abc import Mapping, Sequence


def get_type_name(value) -> str:
    return type(value).__name__


def is_primitive(value) -> bool:
    return isinstance(value, (str, int, float, bool, type(None)))


def extract_structure(obj):
    """
    Recursively extracts a structural schema from nested dict/list objects.
    """

    # ---- dict ----
    if isinstance(obj, Mapping):
        # check if leaf dict (only primitives)
        if all(is_primitive(v) for v in obj.values()):
            return {k: get_type_name(v) for k, v in obj.items()}

        # otherwise recurse
        return {k: extract_structure(v) for k, v in obj.items()}

    # ---- list ----
    elif isinstance(obj, Sequence) and not isinstance(obj, (str, bytes)):
        if not obj:
            return "list[unknown]"

        # assume homogeneous → take first element
        return [extract_structure(obj[0])]

    # ---- primitive ----
    else:
        return get_type_name(obj)