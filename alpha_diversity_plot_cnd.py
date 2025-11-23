#アルファ多様性.py
#Feature ID,Sample1,Sample2,Sample3
#f__Lactobacillaceae,120,80,0
#f__Streptococcaceae,40,20,10
#f__Enterobacteriaceae,0,30,5

#pip install pandas numpy scikit-bio

#python3.10 calculate_alpha_diversity.py level7_table.csv

# analyze_alpha_diversity.py

#アルファ多様性.py
#Feature ID,Sample1,Sample2,Sample3
#f__Lactobacillaceae,120,80,0
#f__Streptococcaceae,40,20,10
#f__Enterobacteriaceae,0,30,5

#pip install pandas numpy scikit-bio statsmodels seaborn matplotlib

#アルファ多様性.py
#Feature ID,Sample1,Sample2,Sample3
#f__Lactobacillaceae,120,80,0
#f__Streptococcaceae,40,20,10
#f__Enterobacteriaceae,0,30,5

#pip install pandas numpy scikit-bio statsmodels seaborn matplotlib

#python3.10 calculate_alpha_diversity.py level7_table.csv

# analyze_alpha_diversity.py
# analyze_alpha_diversity.py
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import statsmodels.formula.api as smf
from statsmodels.stats.multitest import multipletests
import os
import sys


def main():
    # -----------------------------
    #  引数チェック（2つ必須）
    # -----------------------------
    if len(sys.argv) != 3:
        print("\n 引数が正しくありません。")
        print("   使い方: python3.10 analyze_alpha_diversity.py <alpha_diversity.csv> <metadata.csv>\n")
        sys.exit(1)

    diversity_file = sys.argv[1]
    metadata_file = sys.argv[2]

    print(f" 読み込みファイル：")
    print(f" - 多様性データ: {diversity_file}")
    print(f" - メタデータ   : {metadata_file}")

    # 出力フォルダ作成
    output_dir = "alpha_diversity_analysis_output_featureplot"
    os.makedirs(output_dir, exist_ok=True)

    try:
        diversity = pd.read_csv(diversity_file, index_col=0)
        metadata = pd.read_csv(metadata_file, index_col=0)

        diversity["SampleID"] = diversity.index
        metadata["SampleID"] = metadata.index
        df = pd.merge(diversity, metadata, on="SampleID")

        if "cnd" in df.columns:
            df["cnd"] = pd.Categorical(df["cnd"], categories=["seagrass", "non"], ordered=True)

        if "env" in df.columns:
            df["env"] = df["env"].astype("category")

    except Exception as e:
        print(f" CSV読込エラー: {e}")
        return

    # 多様性指標
    metrics = [
        "Observed_OTUs",
        "Shannon_Index",
        "Simpson_Index",
        "Inverse_Simpson_Index",
        "Pielou_Evenness"
    ]

    p_values = []
    models = {}

    # -----------------------------
    # 線形モデル
    # -----------------------------
    for metric in metrics:
        try:
            formula = f"{metric} ~ cnd + env"
            model = smf.ols(formula, data=df).fit()
            models[metric] = model

            p_val = model.pvalues.get("cnd[T.non]", None)
            p_values.append(p_val)

        except Exception as e:
            print(f"線形モデルエラー: {metric}: {e}")
            p_values.append(None)

    # FDR補正
    corrected_pvals = [None] * len(metrics)
    valid_indices = [i for i, p in enumerate(p_values) if p is not None]

    if valid_indices:
        raw_pvals = [p_values[i] for i in valid_indices]
        _, pvals_corrected, _, _ = multipletests(raw_pvals, method="fdr_bh")
        for idx, pval in zip(valid_indices, pvals_corrected):
            corrected_pvals[idx] = pval

    palette = {"seagrass": "forestgreen", "non": "royalblue"}

    # -----------------------------
    # グループ比較（dex）
    # -----------------------------
    for metric in metrics:
        try:
            sns.set(style="white")
            plt.figure(figsize=(8, 6))

            sns.boxplot(data=df, x="cnd", y=metric, hue="cnd",
                        palette=palette, showcaps=True, fliersize=0,
                        boxprops=dict(alpha=0.6), legend=False)

            sns.stripplot(data=df, x="cnd", y=metric, hue="cnd",
                          palette=palette, dodge=True, size=4,
                          jitter=True, alpha=0.5, legend=False)

            sns.despine()
            plt.title(f"{metric} by Group (cnd)")
            plt.tight_layout()

            plt.savefig(os.path.join(output_dir, f"{metric.lower()}_boxplot.png"), dpi=300)
            plt.savefig(os.path.join(output_dir, f"{metric.lower()}_boxplot.pdf"), dpi=300)
            plt.close()

        except Exception as e:
            print(f" グラフ描画エラー: {metric}: {e}")

        # 線形モデル出力
        try:
            model = models.get(metric)
            if model:
                with open(os.path.join(output_dir, f"lm_summary_{metric.lower()}.txt"), "w") as f:
                    f.write(model.summary().as_text())
        except Exception as e:
            print(f" モデル保存エラー: {metric}: {e}")

    # -----------------------------
    # env × dex の比較
    # -----------------------------
    for metric in metrics:
        try:
            sns.set(style="white")
            plt.figure(figsize=(12, 6))

            sns.boxplot(data=df, x="env", y=metric, hue="cnd",
                        palette=palette, showcaps=True, fliersize=0,
                        boxprops=dict(alpha=0.6))

            sns.stripplot(data=df, x="env", y=metric, hue="cnd",
                          palette=palette, dodge=True, size=3,
                          jitter=True, alpha=0.5, legend=False)

            plt.title(f"{metric} by Group within Each Environment")
            plt.tight_layout()

            plt.savefig(os.path.join(output_dir, f"{metric.lower()}_envgroup_boxplot.png"), dpi=300)
            plt.savefig(os.path.join(output_dir, f"{metric.lower()}_envgroup_boxplot.pdf"), dpi=300)
            plt.close()

        except Exception as e:
            print(f" 環境別描画エラー: {metric}: {e}")

    print("\n完了！結果は以下に保存：")
    print(f"📂 {output_dir}")


if __name__ == "__main__":
    main()
