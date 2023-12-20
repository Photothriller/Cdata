#
# 関連2群の t検定
#

# (この授業の)万能検定ツールを読み込む
source("検定.R")

# ふたつの標本
sampleA <- c(3.2, 4.0, 3.5, 3.1, 3.2)
sampleB <- c(3.4, 4.3, 3.9, 3.5, 3.1)

# 対応する観測値の差を集めた新しい標本d
d <- sampleA - sampleB

# Student の t の計算に必要な数値群
dbar <- mean(d) # 標本平均
s <- sd(d) # 標本標準偏差
n <- length(d) # 標本サイズ
df <- n - 1 # 自由度

# 検定統計量 t を計算する
t <- dbar * sqrt(n) / s

print(sprintf("t = %f", t))

# 臨界値 t_{0.05}(df) を得る
print(sprintf("t_{0.05}(%d) = %f",
              df, abs(qt(0.025, df))))

# グラフでも示す (帰無仮説は mu = 0)
tdistg(df, x=dbar, mu=0, s=s/sqrt(n))

# 以上が教科書通りの解法です。以下も t検定を行います。
# ステップ実行で見てください。
t.test(sampleA, sampleB, paired=T)

# end of 練習問題N.R
