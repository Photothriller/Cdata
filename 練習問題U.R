#
# 一元配置分散分析
#

# (この授業の)万能検定ツールを読み込む
source("検定.R")

# 4つの標本
sample1 <- c(3.10, 3.14, 3.07, 3.20, 2.84)
sample2 <- c(2.76, 2.88, 2.88, 3.08, 2.93)
sample3 <- c(3.19, 3.13, 3.45, 3.34, 3.00)
sample4 <- c(2.84, 2.72, 2.61, 2.65, 2.61)

k <- 4
samples <- c(sample1, sample2, sample3, sample4)

mx <- matrix(samples, nrow=length(sample1), ncol=k)

N <- length(samples) # 観測値の総数
print(sprintf("N = %d", N))
xdbar <- mean(samples) # 総平均
print(sprintf("x double bar = %f", xdbar))

# 誤差平均平方 MS_{within}
# 偏差平方和
SSw <- 0.0
for (j in 1:k) {
  xbar <- mean(mx[,j])
  for (i in 1:length(mx[,j])) {
    SSw <- SSw + (mx[i,j] - xbar)^2
  }
}
print(sprintf("SS_within = %f", SSw))

dfw <- N - k # 自由度
print(sprintf("df_within = %d", dfw))

MSw <- SSw / dfw # 平均平方
print(sprintf("MS_within = %f", MSw))

# 処理平均平方 MS_{between}
# 偏差平方和
SSb <- 0.0
for (j in 1:k) {
  SSb <- SSb + length(mx[,j]) * (mean(mx[,j]) - xdbar)^2
}
print(sprintf("SS_between = %f", SSb))

dfb <- k - 1 # 自由度
print(sprintf("df_between = %d", dfb))

MSb <- SSb / dfb # 平均平方
print(sprintf("MS_between = %f", MSb))

# 全平均平方
# 偏差平方和
SSt <- 0.0
for (j in 1:k) {
  for (i in 1:length(mx[,j])) {
    SSt <- SSt + (mx[i,j] - xdbar)^2
  }
}
print(sprintf("SS_total = %f", SSt))

dft <- N - 1 # 自由度
print(sprintf("df_total = %d", dft))

# 検算
print(sprintf("SS_between + SS_within = %f", SSb + SSw))
print(sprintf("df_between + df_within = %d", dfb + dfw))

# F値
f <- MSb / MSw

# 描画により示す
fdistg(dfb, dfw, f=f)

# 以上が教科書通りの解法です。以下も分散分析を行います。
# ステップ実行で見てください。
treatment_raw <- c(
  rep("1", 5), rep("2", 5), rep("3", 5), rep("4", 5)
)
treatment <- factor(treatment_raw)
anova(lm(samples ~ treatment))

# end of 練習問題U.R
