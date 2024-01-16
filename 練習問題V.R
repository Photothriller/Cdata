#
# 多重比較
#

# 4つの標本
sample1 <- c(3.10, 3.14, 3.07, 3.20, 2.84)
sample2 <- c(2.76, 2.88, 2.88, 3.08, 2.93)
sample3 <- c(3.19, 3.13, 3.45, 3.34)
sample4 <- c(2.84, 2.72, 2.61, 2.65, 2.61)

k <- 4 # 標本数

# 標本平均を求める
xbars <- c(
  mean(sample1), mean(sample2), mean(sample3), mean(sample4)
)
print(sprintf("標本平均 : %f, %f, %f, %f",
        xbars[1], xbars[2], xbars[3], xbars[4]))

# 標本サイズ
n1 <- length(sample1)
n2 <- length(sample2)
n3 <- length(sample3)
n4 <- length(sample4)
print(sprintf("標本サイズ : %d, %d, %d, %d", n1, n2, n3, n4))

# 観測値の総数
N <- n1 + n2 + n3 + n4
print(sprintf("N = %d", N))

# 1列にする
samples <- c(sample1, sample2, sample3, sample4)
sizes <- c(n1, n2, n3, n4)

# 群内分散の偏差平方和を求める
SSw <- 0
n <- 1
for (j in 1:k) {
  for (i in 1:sizes[j]) {
    SSw <- SSw + (samples[n] - xbars[j])^2
    n <- n + 1
  }
}
print(sprintf("SS_{within} = %f", SSw))

# 群内分散の自由度を求める
dfw <- N - k
print(sprintf("df_{within} = %d", dfw))

# 群内分散の平方根を求める
MSw <- SSw / dfw
print(sprintf("MS_{within} = %f", MSw))
MSwsqrt <- sqrt(MSw)
print(sprintf("√MS_{within} = %f", MSwsqrt))

# 検定統計量q の 5% 臨界値
q005 <- qtukey(0.05, k, dfw, lower.tail=F)
print(sprintf("q_{0.05}(%d, %d) = %f", k, dfw, q005))

# 総当たりの組み合わせ
pairs <- combn(x=c(1, 2, 3, 4), m=2)

# 総当たりで検定量q を計算し、臨界値と比較する
i <- 1
while (i < length(pairs)) {
  d <- xbars[pairs[i]] - xbars[pairs[i + 1]]
#  print(sprintf("%d-%d : 標本平均の差 = %f",
#        pairs[i], pairs[i + 1], d))
  q <- d / (MSwsqrt * sqrt(1 / 2 *
        (1 / sizes[pairs[i]] + 1 / sizes[pairs[i + 1]])))
  if (abs(q) > q005) {
    m <- sprintf("q_{%d-%d} = %f *", pairs[i], pairs[i + 1],  q)
  } else {
    m <- sprintf("q_{%d-%d} = %f", pairs[i], pairs[i + 1], q)
  }
  print(m)
  i <- i + 2
}

# 以上が教科書通りの解法です。以下も多重比較を行います。

#
# ライブラリを用いた多重比較
#
library(multcomp)

# 要因のラベル順にアルファベットが付けられるため工夫する
treatment_raw <- c(
  rep("2位-標本1", sizes[1]),
  rep("3位-標本2", sizes[2]),
  rep("1位-標本3", sizes[3]),
  rep("4位-標本4", sizes[4])
)
treatment <- factor(treatment_raw)

result <- glht(aov(samples ~ treatment),
                linfct=mcp(treatment="Tukey"))
res <- cld(result)
print(res)
plot(res)

# end of 練習問題V.R
