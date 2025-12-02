from typing import Dict, Tuple

import numpy as np
import pandas as pd
import xgboost as xgb
from sklearn.metrics import accuracy_score

from config import AmpModelConfig
from features_adapter import FeatureSpace, build_aligned_features
from results import AmpModelMetrics
from constants import AMP_COLS


def _compute_class_weights(
    y_train: np.ndarray,
) -> Tuple[np.ndarray, np.ndarray, np.ndarray]:
    """Replicate the original per-class weighting scheme for amplitude labels."""
    class_counts = np.bincount(y_train, minlength=3)
    baseline = class_counts[1]  # flat class as baseline
    raw_w = baseline / class_counts.astype(float)
    raw_w = raw_w / raw_w.mean()
    class_weights = np.clip(raw_w, 0.5, 3.0)
    sample_weight = class_weights[y_train]
    return class_counts, class_weights, sample_weight


def prepare_amp_targets(df: pd.DataFrame) -> pd.DataFrame:
    """Attach integer amplitude targets to a copy of the input frame."""
    label_map: Dict[str, int] = {"down": 0, "flat": 1, "up": 2}
    out = df.copy()
    out["y_amp"] = out["label_amp"].map(label_map).astype(int)
    return out


def train_amp_model(
    train_df: pd.DataFrame,
    test_df: pd.DataFrame,
    cfg: AmpModelConfig,
) -> Tuple[np.ndarray, np.ndarray, FeatureSpace, AmpModelMetrics]:
    """Train the 3-class amplitude XGB model and return probabilities and metrics."""
    train_df = prepare_amp_targets(train_df)
    test_df = prepare_amp_targets(test_df)

    X_train, X_test, feature_space = build_aligned_features(train_df, test_df)
    y_train = train_df["y_amp"].values
    y_test = test_df["y_amp"].values

    class_counts, class_weights, sample_weight = _compute_class_weights(y_train)

    clf_amp = xgb.XGBClassifier(
        n_estimators=cfg.n_estimators,
        max_depth=cfg.max_depth,
        learning_rate=cfg.learning_rate,
        subsample=cfg.subsample,
        colsample_bytree=cfg.colsample_bytree,
        objective="multi:softprob",
        num_class=3,
        eval_metric="mlogloss",
        tree_method="hist",
        n_jobs=-1,
        random_state=cfg.random_state,
    )
    clf_amp.fit(X_train, y_train, sample_weight=sample_weight)

    proba_train = clf_amp.predict_proba(X_train)
    proba_test = clf_amp.predict_proba(X_test)

    pred_train = np.argmax(proba_train, axis=1)
    pred_test = np.argmax(proba_test, axis=1)
    acc_train = accuracy_score(y_train, pred_train)
    acc_test = accuracy_score(y_test, pred_test)

    metrics = AmpModelMetrics(class_counts=class_counts, class_weights=class_weights, acc_train=acc_train, acc_test=acc_test)
    return proba_train, proba_test, feature_space, metrics
