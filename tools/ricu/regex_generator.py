from __future__ import annotations

import re
from dataclasses import dataclass, field
from typing import Dict, Iterable, List


@dataclass
class TrieNode:
    children: Dict[str, "TrieNode"] = field(default_factory=dict)
    terminal: bool = False  # True if a word ends here


def _polars_safe_escape_literal(s: str) -> str:
    """
    Escape a literal string for use inside Rust/Polars regex.

    Important: Python's re.escape() escapes spaces as "\\ ", which Polars (Rust regex)
    rejects. So we undo that specific escape.

    This keeps other escaping (., +, *, ?, [, ], (, ), {, }, |, ^, $, \\ ...) intact.
    """
    return re.escape(s).replace(r"\ ", " ")


def build_trie(words: Iterable[str]) -> TrieNode:
    root = TrieNode()
    for w in words:
        w_esc = _polars_safe_escape_literal(w)  # escape whole word (Polars-safe)
        node = root
        for ch in w_esc:
            node = node.children.setdefault(ch, TrieNode())
        node.terminal = True
    return root


def trie_to_regex(node: TrieNode) -> str:
    parts: List[str] = []

    for ch, child in sorted(node.children.items(), key=lambda x: x[0]):
        parts.append(ch + trie_to_regex(child))

    if node.terminal:
        # empty alternative -> word may end here
        parts.append("")

    if not parts:
        return ""

    if len(parts) == 1:
        return parts[0]

    return "(" + "|".join(parts) + ")"


def words_to_compact_regex(words: Iterable[str], anchor: bool = False) -> str:
    """
    Build a compact (prefix-factored) regex matching exactly one of the given words.
    - anchor=True wraps with ^(...)$ for full-string matches.
    """
    words_list = list(words)
    if not words_list:
        # matches nothing
        core = r"(?!)"
        return f"^{core}$" if anchor else core

    core = trie_to_regex(build_trie(words_list))
    core = "(" + core + ")"  # outer non-capturing group

    if anchor:
        core = "^" + core + "$"
    return core
