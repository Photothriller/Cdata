#
# 回帰分析
#

# 標本
sampleX <- c(1.0, 0.7, 0.1, 1.6, 0.9, 1.4, 1.2, 0.9,
             1.6, 0.6, 0.8, 2.2, 0.2, 0.3, 1.1, 1.5)
sampleY <- c(10,  15,  32,  6,   13,  4,   7,   14,
             7,   14,  14,  0,   19,  16,  5,   1)

# 分布を見ずに進めるのは危険なので、次を行うのを推奨
plot(sampleX, sampleY, xlab="農薬散布", ylab="ホタルの数")

# 標本サイズ
n <- length(sampleX)

# 相関係数を計算する
sx <- sd(sampleX)
sy <- sd(sampleY)
meanX <- mean(sampleX)
meanY <- mean(sampleY)
sxy <- sum((sampleX - meanX) * (sampleY - meanY)) / (n - 1)
print(sprintf("s_x = %f, s_y = %f", sx, sy))
print(sprintf("s_{xy} = %f", sxy))

r <- sxy / (sx * sy)
print(sprintf("r = %f", r))

# 回帰直線の傾き
b <- r * sy / sx
print(sprintf("b = %f", b))

# 切片
a <- meanY - b * meanX
print(sprintf("a = %f", a))

# 決定係数
print(sprintf("r^2 = %f", r^2))

# 以上が教科書通りの解法です。以下も回帰分析を行います。
# ステップ実行で見てください。
results <- cor.test(sampleX, sampleY)
print(sprintf("r^2 = %f", results$estimate^2))

m <- lm(sampleY ~ sampleX)

m

abline(m)

# end of 練習問題Y.R
