# このサンプルは、1コマンドずつ実行してみてください。

# 正規分布を描こう
x <- seq(-4, 4, length=100) # -4〜4 を 100等分
y <- dnorm(x) # 標準正規分布

plot(x, y)
plot(x, y, type="l")

## curve 関数で簡単に描く
curve(dnorm(x), from=-4, to=4)

## μ-σからμ+σの範囲に入る確率は？
print(1 - pnorm(-1)*2)

## μ-2σからμ+2σの範囲に入る確率は？
print(1 - pnorm(-2)*2)

## μ-3σからμ+3σの範囲に入る確率は？
print(1 - pnorm(-3)*2)

## 臨界値z_{0.05} は？
print(abs(qnorm(0.025)))

# t分布を描こう
## まず正規分布を描く
curve(dnorm(x), from=-5.5, to=5.5, lty=1, col=1,
      ylim=c(0, 0.4),
      xlab="x", ylab="確率密度", main="正規分布とt分布")
par(new=T)

## curve 関数で描く
for (k in 2:5) {
  curve(dt(x, df=2^k), from=-5.5, to=5.5, lty=k, col=k,
        ylim=c(0, 0.4), axes=F, ann=F)
  par(new=T)
}

legend <- c("N(0, 1^2)")
for (k in 2:5) {
  legend <- c(legend, sprintf("t(%d)", 2^k))
}
legend("topleft", legend=legend, lty=1:5, col=1:5)

## 臨界値 t_{0.05}(df) を調べる
for (k in 2:5) {
  str <- sprintf("t_{0.05}(%d) = %f",
                 2^k, abs(qt(0.025, df=2^k)))
  print(str)
}

# end of 正規分布とt分布.R
