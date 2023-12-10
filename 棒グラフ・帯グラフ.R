# このサンプルは、1コマンドずつ実行してみてください。

# 棒グラフ編
表1 <- read.table("英語数学成績.txt", header=T)

barplot(表1$英語[1:10], names.arg=c(1:10),
        xlab="出席番号", ylab="得点",
        main="出席番号1〜10の英語の試験結果")

## t(table) は table の行と列を反転させた行列を返す。
## 行列 (matrix) は同じ型のデータの 2次元の並び。
行列 <- t(
  data.frame(英語=表1$英語[1:10], 数学=表1$数学[1:10]))

barplot(行列, beside=T, names.arg=c(1:10),
        legend.text=T, ylim=c(0, 100),
        xlab="出席番号", ylab="得点",
        main="出席番号1〜10の英語・数学の試験結果")

# 帯グラフ編
data1 <- c(
  "A社製 "=51,
  "B社製 "=21,
  "C社製 "=20,
  "D社製 "=8)

data2 <- c(
  "A社製 "=33,
  "B社製 "=35,
  "C社製 "=20,
  "D社製 "=12)

data <- matrix(c(data1, data2), length(data1), 2)
colnames(data) <- c("X地域", "Y地域")

## 横向き帯グラフ
barplot(data, horiz=T, col=cm.colors(4),
        xlab="シェア (%)",
        legend.text=names(data1), main="地域別シェア")

## 積み上げグラフ
barplot(data, col=cm.colors(4), ylab="シェア (%)",
        legend.text=names(data1), main="地域別シェア")

## 棒グラフ (割合よりも量に注目する)
barplot(data, beside=T, col=cm.colors(4),
        ylab="シェア (%)",
        legend.text=names(data1), main="地域別シェア")

## 色に依存せず、斜線の密度で区別する。
## col との併用も可。
## density は円グラフ等でも有効。
density <- c(50, 25, 13, 7)

barplot(data, beside=T, density=density,
        ylab="シェア (%)",
        legend.text=names(data1), main="地域別シェア")

# 円グラフ編 (要注意)
## 人間は角度を比較する能力が弱い上、
## 円グラフでは割合以外を表せない
## (であるのに色々な情報を表すのに使われがち)。
pie(data1, col=cm.colors(4), main="X地域でのシェア")
pie(data2, col=cm.colors(4), main="Y地域でのシェア")

# end of 棒グラフ・帯グラフ.R
