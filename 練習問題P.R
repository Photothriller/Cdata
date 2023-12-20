#
# 独立2群の t検定
#

# (この授業の)万能検定ツールを読み込む
source("検定.R")

# ふたつの標本
sampleA <- c(0.38, 0.41, 0.31, 0.33, 0.28)
sampleB <- c(0.41, 0.42, 0.49, 0.41, 0.57)

xbarA <- mean(sampleA) # 標本平均
xbarB <- mean(sampleB) # 標本平均

print(sprintf("xbar_A = %f", xbarA))
print(sprintf("xbar_B = %f", xbarB))

# Student の t の計算に必要な数値群
dbar <- xbarA - xbarB # 標本平均の差
nA <- length(sampleA) # 標本サイズ
nB <- length(sampleB) # 標本サイズ
SSA <- var(sampleA) * (nA - 1) # 偏差平方和
SSB <- var(sampleB) * (nB - 1) # 偏差平方和
dfp <- nA + nB - 2 # 自由度

# 合算標準偏差を計算する
sp <- sqrt((SSA + SSB) / dfp)

# 検定統計量 t を計算する
t <- dbar / (sp * sqrt(1/nA + 1/nB))

print(sprintf("t = %f", t))

# 臨界値 t_{0.05}(dfp) を得る
print(sprintf("t_{0.05}(%d) = %f",
              dfp, abs(qt(0.025, dfp))))

# グラフでも示す (帰無仮説は mu = 0)
tdistg(dfp, x=dbar, mu=0, s=sp * sqrt(1/nA + 1/nB))

# 以上が教科書通りの解法です。以下も t検定を行います。
# ステップ実行で見てください。
t.test(sampleA, sampleB, var.equal=T)

# end of 練習問題P.R
