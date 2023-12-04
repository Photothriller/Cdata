肥満 <- c(4.6, 5.6, 3.2, 3.2, 3.7, 4.0, 5.0, 4.6)
肥満ではない <- c(4.6, 4.9, 7.1, 6.0, 5.2, 3.9, 5.3, 5.8)

# 両サンプルを結合
combined_samples <- c(肥満, 肥満ではない)
sample_group <- c(rep("肥満", length(肥満)), rep("肥満ではない", length(肥満ではない)))

# 順位を計算（タイの場合は平均順位を割り当て）
ranks <- rank(combined_samples)

# データフレームに変換
df <- data.frame(value = combined_samples, group = sample_group, rank = ranks)

# 各サンプルの順位和を計算
ra <- sum(df[df$group == "肥満", "rank"])
rb <- sum(df[df$group == "肥満ではない", "rank"])

# 検定統計量 U を計算する。
na <- length(肥満)
nb <- length(肥満ではない)

U <- min(na*nb + na / 2 * (na + 1) - ra, na*nb + nb / 2 * (nb + 1) - rb)

print(paste("U =", U))

# 元データのデータフレームを作成する。
sdf <- data.frame(肥満, 肥満ではない)

# 箱ひげ図を並べてデータの分布を確認する。
boxplot(sdf, ylim=c(0, 8.0), ylab="年収 (単位:百万円)")

# end of 練習問題D
