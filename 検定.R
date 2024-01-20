options(digits=3) # 有効桁数を 3とする

# 関数 psd(x) : 母標準偏差を返す (population SD)
# x : 母集団のベクトル
psd <- function(x) {
  sqrt(sd(x)^2*(length(x) - 1)/length(x))
}

# 関数 ndistg() : 標準正規分布を描画し棄却域を赤で示す
# x : 観測値(の差)(の平均) (NULL)
# mu : 母平均(期待値) (NULL)
# sigma : 母標準偏差 (NULL)
ndistg <- function(x=NULL, mu=NULL, sigma=NULL) {
  # 標準正規分布を描く
  curve(dnorm(x), from=-4, to=4, ylim=c(0, 0.4),
        main=expression(N(0, 1^2)))

  # 臨界値z_{0.05} の線を描く
  z_005 <- qnorm(0.025)
  arrows(z_005, 0, z_005, dnorm(z_005), length=0)
  arrows(-z_005, 0, -z_005, dnorm(-z_005), length=0)

  # 臨界値を超える範囲の x を求め、2列の行列にする
  xrange1 <- seq(-4, z_005, length=100) # 100個の点
  xrange2 <- seq(-z_005, 4, length=100) # 100個の点
  xranges <- matrix(c(xrange1, xrange2),
                    nrow=100, ncol=2)

  # 左右それぞれxの範囲のyを求め、多角形として赤で塗る
  for (k in 1:2) {
    yrange <- dnorm(xranges[,k])
    polygon(c(xranges[,k], rev(xranges[,k])),
            c(rep(0, 100), rev(yrange)), col="red")
  }

  # 凡例を用意する (臨界値のみ)
  legend <- c(str2lang(paste0("z[0.05]==", -z_005)))

  # 観測値が指定されていれば、標準化して青い線を描く
  if (is.null(x) == F) {
    if (is.null(mu)) {
      print("母平均mu が指定されていません。")
      return()
    }
    if (is.null(sigma)) {
      print("母標準偏差sigma が指定されていません。")
      return()
    }
    z <- (x - mu) / sigma
    arrows(z, 0, z, dnorm(z), length=0, col="blue")
    
    # 凡例にz値を付け足す
    legend <- c(legend, str2lang(paste0("z==", z)))
  }

  # 凡例を描く
  legend("topright", legend=legend)
}

# 関数 tdistg(df) : t分布を描画し棄却域を赤で示す
# df : 自由度 (1の場合、棄却域は正しく描画されません)
# x : 観測値(の差)(の平均) (NULL)
# mu : 母平均(期待値) (NULL)
# s : 標本標準偏差 (NULL)
# t : t値 (NULL)
tdistg <- function(df, x=NULL, mu=NULL, s=NULL, t=NULL) {
  # 参考の標準正規分布をグレイの点線で描く
  curve(dnorm(x), from=-5, to=5, ylim=c(0, 0.4),
        lty=3, col="gray", axes=F, ann=F)
  par(new=T)

  # t分布を描く
  curve(dt(x, df), from=-5, to=5, ylim=c(0, 0.4),
        main=str2lang(paste0("t(", df, ")")))

  # 参考の臨界値z_{0.05}をグレイの点線で描く
  z_005 <- qnorm(0.025)
  arrows(z_005, 0, z_005, dnorm(z_005), length=0,
         lty=3, col="gray")
  arrows(-z_005, 0, -z_005, dnorm(-z_005), length=0,
         lty=3, col="gray")

  # 臨界値t_{0.05}(df)の線を描く
  t_005 <- qt(0.025, df)
  arrows(t_005, 0, t_005, dt(t_005, df), length=0)
  arrows(-t_005, 0, -t_005, dt(-t_005, df), length=0)

  # 臨界値を超える範囲の x を求め、2列の行列にする
  xrange1 <- seq(-5, t_005, length=100) # 100個の点
  xrange2 <- seq(-t_005, 5, length=100) # 100個の点
  xranges <- matrix(c(xrange1, xrange2),
                    nrow=100, ncol=2)

  # 左右それぞれxの範囲のyを求め、多角形として赤で塗る
  for (k in 1:2) {
    yrange <- dt(xranges[,k], df)
    polygon(c(xranges[,k], rev(xranges[,k])),
            c(rep(0, 100), rev(yrange)), col="red")
  }

  # 凡例を用意する (自由度と臨界値のみ)
  legend <- c(
    str2lang(paste0("df==", df)),
    str2lang(paste0("t[0.05](", df, ")==", -t_005)))

  # 観測値が指定されていれば、student化して青い線を描く
  if (is.null(x) == F) {
    if (is.null(mu)) {
      print("母平均(期待値)mu が指定されていません。")
      return()
    }
    if (is.null(s)) {
      print("標本標準偏差s が指定されていません。")
      return()
    }
    t <- (x - mu) / s
  }
  if (is.null(t) == F) {
    arrows(t, 0, t, dt(t, df), length=0, col="blue")

    # 凡例にt値を付け足す
    legend <- c(legend, str2lang(paste0("t==", t)))
  }

  # 凡例を描く
  legend("topright", legend=legend)
}

# 関数 fdistg(dfb, dfw) : F分布を描画し棄却域を赤で示す
# dfb : 群間分散の自由度 df_{between}
# dfw : 群内分散の自由度 df_{within}
# f : F値 (NULL)
fdistg <- function(dfb, dfw, f=NULL) {
  curve(df(x, dfb, dfw), 0, 5,
       main=str2lang(paste0("F(", dfb, ",", dfw, ")")))

  # 臨界値F_{0.05}(dfb, dfw)の線を描く
  F_005 <- qf(0.05, dfb, dfw, lower.tail=F)
  arrows(F_005, 0, F_005, df(F_005, dfb, dfw), length=0)

  # 臨界値を超える範囲の x を求め、2列の行列にする
  xrange <- seq(F_005, 5, length=100) # 100個の点

  # 右範囲のyを求め、多角形として赤で塗る
  yrange <- df(xrange, dfb, dfw)
  polygon(c(xrange, rev(xrange)),
      c(rep(0, 100), rev(yrange)), col="red")

  # 凡例を用意する (自由度と臨界値のみ)
  legend <- c(
    str2lang(paste0("F[0.05](", dfb, ",", dfw, ")==", F_005)))

  # F値が指定されていれば、青い線を描く
  if (is.null(f) == F) {
    arrows(f, 0, f, df(f, dfb, dfw), length=0, col="blue")
  
    # 凡例にF値を付け足す
    legend <- c(legend, str2lang(paste0("F==", f)))
  }

  # 凡例を描く
  legend("topright", legend=legend)
}

# end of 検定.R
