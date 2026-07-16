(ns kami.mangaka.qa.score
  "スコア集約 (ADR-2607165100)。

  - rubric (0-1 軸) → 0-100 集約: aggregate-score。ai-gftd-mangaka の
    品質ループ (:panel/score) と同スケール。
  - v10 (0-10 軸) → 平均: mean10。v10 の total/heuristic_total は Python 側の
    重み付き合成で、重み定義は退役 runtime と共に失われている — 本 lib は
    **保存された値を正本として素通し** (axes/from-v10) し、重みを捏造しない。
    新規計算には mean10 / aggregate-score を使う。"
  )

(defn aggregate-score
  "0-1 軸 map の平均 × 100 (四捨五入, int) | nil (数値軸なし = no signal)。
  nil 値の軸は集約前に除外される (no signal は罰しない)。"
  [axes]
  (let [vals (filter number? (vals (or axes {})))]
    (when (seq vals)
      (int (+ 0.5 (* 100 (/ (reduce + 0.0 vals) (count vals))))))))

(defn mean10
  "0-10 軸 map の平均 (double, 1 桁丸め) | nil。:polarity :lower-better の軸を
  反転したい場合は呼び出し側で (- 10 v) してから渡す (重み・反転規則の正本は
  失われているため、本 lib は素直な平均だけを提供する)。"
  [axes]
  (let [vals (filter number? (vals (or axes {})))]
    (when (seq vals)
      (/ (Math/round (* 10.0 (/ (reduce + 0.0 vals) (count vals)))) 10.0))))

(defn merge-perception-axes
  "rubric axes に知覚軸を焼き足す。値が nil の知覚軸は足さない
  (offline = 軸欠落、罰しない)。"
  [axes perception-axes]
  (reduce-kv (fn [m k v] (if (some? v) (assoc m k v) m))
             (or axes {})
             (or perception-axes {})))
