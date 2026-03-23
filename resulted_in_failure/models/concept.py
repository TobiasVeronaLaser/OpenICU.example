
from pydantic import BaseModel, Field


class SourceModel(BaseModel):
    ids: int | str | list[int] | list[str]

class ConceptModel(BaseModel):
    description: str
    category: str
    omopid: int | None = None
    sources: dict[str, SourceModel] | None = None
    min: float |str | None = None
    max: float | str | None = None
    unit: str | list[str] | None = None
    class_: str | list[str] | None = Field(alias="class")
    concepts: str | list[str] | None = None
    callback: str | None = None
    aggregate: str | list[str] | None = None
    target: str | None = None
    interval: str | None = None
    levels: list[str] | None = None
    keep_components: list[str] | None = None