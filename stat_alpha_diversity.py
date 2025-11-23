# python3.10 merge_pvalues_wide.py pvaluefileA.csv pvaluefileB.csv pvaluefileC.csv

#    ,Hakata,Kitsuki,Noto,...
#Hakata,1.0,0.005,0.001,...
#Kitsuki,0.005,1.0,0.02,...
#...


# 統計データ結合.py

import sys
import pandas as pd
from pathlib import Path

def extract_pairs(df):
    """p値マトリクスから比較ペアとp値のリストを抽出"""
    pairs = []
    cols = df.columns
    for i in range(len(cols)):
        for j in range(len(cols)):
            if i < j:  # 対称成分の重複を避ける
                val = df.iloc[i, j]
                pairs.append({
                    "Comparison": f"{cols[i]} vs {cols[j]}",
                    "p-value": val
                })
    return pd.DataFrame(pairs)

def main():
    if len(sys.argv) < 2:
        print("使い方: python3.10 merge_pvalues_combined.py pvaluefileA.csv pvaluefileB.csv ...")
        sys.exit(1)

    input_files = sys.argv[1:]
    merged_df = None

    for csv_path in input_files:
        name = Path(csv_path).stem  # ファイル名（拡張子なし）
        print(f"処理中: {csv_path}")

        # CSV読み込み
        df = pd.read_csv(csv_path, index_col=0)
        df.columns = df.columns.str.strip()
        df.index = df.index.str.strip()

        # 並び順を揃える（安全策）
        df = df.loc[df.columns, df.columns]

        # 長い形式へ変換
        long_df = extract_pairs(df)
        long_df = long_df.rename(columns={"p-value": name})

        if merged_df is None:
            merged_df = long_df
        else:
            merged_df = pd.merge(merged_df, long_df, on="Comparison", how="outer")

    # Excel出力
    output_path = Path("merged_pvalues_combined.xlsx")
    merged_df.to_excel(output_path, index=False)

    print(f"\n✅ 出力完了: {output_path.absolute()}")

if __name__ == "__main__":
    main()
