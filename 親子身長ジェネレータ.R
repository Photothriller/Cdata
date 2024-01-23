#
# 重回帰分析のデモ用データの生成
#

# データは以下を参考にしました。
# 小須田, 和彦. (2007). ヒトの身長・体重における親子相関.
# 城西大学研究年報. 自然科学編, 30, 1-13.
# https://doi.org/10.20566/09149775_30_1

set.seed(316227)
n <- 50 # 標本サイズ

a <- 49.4

r_father <- 0.306
mean_father <- 168.78
sd_father <- 3.2

r_mother <- 0.37
mean_mother <- 155.32
sd_mother <- 2.45

父身長 <- round(rnorm(n=n, mean=mean_father, sd=sd_father),
             digits=1)
母身長 <- round(rnorm(n=n, mean=mean_mother, sd=sd_mother),
             digits=1)
e <- rnorm(n=n, mean=0, sd=2.8)

娘身長 <- round(a + r_father * 父身長 + r_mother * 母身長
             + e, digits=1)

g <- data.frame(娘身長, 父身長, 母身長)

write.table(g, "親子身長.txt", row.names=F)

# end of 親子身長ジェネレータ.R
