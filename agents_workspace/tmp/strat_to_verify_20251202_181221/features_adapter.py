from dataclasses import dataclass
from typing import Dict, List, Sequence, Tuple

import numpy as np
import pandas as pd

from stop_label_hgb_analysis import build_features


@dataclass
class FeatureSpace:
    num_cols: List[str]
    all_cat_cols: List[str]
    num_means: Dict[str, float]


def _align_onehot_columns(
    X_df: pd.DataFrame, num_cols: Sequence[str], all_cat_cols: Sequence[str]
) -> pd.DataFrame:
    """Align one-hot categorical columns between train/test splits."""
    missing = [c for c in all_cat_cols if c not in X_df.columns]
    for c in missing:
        X_df[c] = 0.0
    cols = list(num_cols) + list(all_cat_cols)
    return X_df[cols]


def build_aligned_features(
    train_df: pd.DataFrame,
    test_df: pd.DataFrame,
) -> Tuple[np.ndarray, np.ndarray, FeatureSpace]:
    """Build Brooks-style features and align one-hot encodings for train/test."""
    X_train_df, num_cols, train_cat_cols, num_means = build_features(train_df)
    X_test_df, _, test_cat_cols, _ = build_features(test_df, num_means=num_means)

    train_cat_set = set(train_cat_cols)
    test_cat_set = set(test_cat_cols)
    all_cat_cols = sorted(list(train_cat_set | test_cat_set))

    X_train_df = _align_onehot_columns(X_train_df, num_cols, all_cat_cols)
    X_test_df = _align_onehot_columns(X_test_df, num_cols, all_cat_cols)

    feature_space = FeatureSpace(
        num_cols=list(num_cols), all_cat_cols=all_cat_cols, num_means=num_means
    )

    X_train = X_train_df.values.astype(np.float32)
    X_test = X_test_df.values.astype(np.float32)
    return X_train, X_test, feature_space


def transform_full_test(
    full_df: pd.DataFrame, feature_space: FeatureSpace
) -> np.ndarray:
    """Build and align features for the full test universe using a fixed feature space."""
    X_full_df, _, cat_cols_full, _ = build_features(
        full_df, num_means=feature_space.num_means
    )
    _ = cat_cols_full  # unused, but kept for symmetry with build_features signature
    X_full_df = _align_onehot_columns(
        X_full_df, feature_space.num_cols, feature_space.all_cat_cols
    )
    return X_full_df.values.astype(np.float32)

