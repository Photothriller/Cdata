#
# 重回帰分析の、よりリアルなデモ用データの生成
#

# データは以下を参考にしました。
# 小須田, 和彦. (2007). ヒトの身長・体重における親子相関.
# 城西大学研究年報. 自然科学編, 30, 1-13.
# https://doi.org/10.20566/09149775_30_1
library(MASS)

# 仮定する統計量 - 標本サイズ
n = 119

# 身長の標準偏差
sd_daughter = 5.4 # (小須田, 2007) の 0.42 は小さすぎる
sd_father = 5.8 # 同 0.46 は小さすぎる
sd_mother = 5.4 # 同 0.45 は小さすぎる

# 身長の平均
mean_daughter = 158.4
mean_father = 168.8
mean_mother = 155.3

# 娘の身長との相関係数
r_father = 0.317
r_mother = 0.358

# 分散共分散行列を設定する。
vcmatrix <- matrix(c(
  sd_daughter^2,
  r_father * sd_daughter * sd_father,
  r_mother * sd_daughter * sd_mother,

  r_father * sd_daughter * sd_father,
  sd_father^2,
  0,

  r_mother * sd_daughter * sd_mother,
  0,
  sd_mother^2
), nrow=3)

# 多変量正規分布からデータを生成する。
# その際、母-娘相関が父-娘相関より大きくなるまで試行する。
# 特定の乱数シードを設定せず、毎回違った標本になるので、
# よい感じのが出るまで試してみてください。
# よいのができたらデータのファイル名を変更するかコピーします。
r_f <- 0
r_m <- 0

while (r_m <= r_f) {
    mv <- mvrnorm(n=n,
            mu=c(mean_daughter, mean_father, mean_mother),
            Sigma=vcmatrix)

    娘身長 <- round(mv[,1], digits=1)
    父身長 <- round(mv[,2], digits=1)
    母身長 <- round(mv[,3], digits=1)

    r_f <- cor.test(父身長, 娘身長)$estimate
    r_m <- cor.test(母身長, 娘身長)$estimate
}

print(sprintf("%f > %f", r_m, r_f))

g <- data.frame(娘身長, 父身長, 母身長)

plot(g)

write.table(g, "真の親子身長.txt", row.names=F)

# end of 真の親子身長ジェネレータ.R
