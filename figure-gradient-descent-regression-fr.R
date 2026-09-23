library(animint2)
library(data.table)
true.f.list <- list(
  constante=function(x)3,
  lin.haut=function(x)2*x + 5,
  lin.bas=function(x)3-x,
  quad.=function(x)x^2,
  sin=function(x)5*sin(2*x)+5)
set.seed(1)
N <- 200
x <- runif(N, -3, 3)
max.pente <- 4
min.pente <- -3
min.ordonnée <- -1
max.ordonnée <- 7
grid.dt <- CJ(
  pente=seq(min.pente, max.pente, by=0.1),
  ordonnée=seq(min.ordonnée, max.ordonnée, by=0.1))
sim.dt.list <- list()
weight.dt.list <- list()
erreur.dt.list <- list()
overview.dt.list <- list()
pred.dt.list <- list()
for(tendence in names(true.f.list)){
  true.f <- true.f.list[[tendence]]
  set.seed(1)
  y <- true.f(x) + rnorm(N, 0, 2)
  expr <- sub("[1] ", "", capture.output(print(body(true.f))), fixed=TRUE)
  tendence_expr <- sprintf("%s y = %s", tendence, expr)
  erreur.fun <- function(ordonnée, pente){
    mean((0.5*(x*pente + ordonnée - y))^2)
  }
  grid.dt[, erreur := erreur.fun(ordonnée, pente), by=.(pente, ordonnée)]
  ##overview.dt.list[[tendence]] <- data.table()
  ## grad desc
  feature.mat <- cbind(1, x)
  for(taux in 10^seq(-3, 0)){
    sim.dt.list[[paste(tendence, taux)]] <- data.table(
      tendence, taux, tendence_expr, x, y)
    erreur.dt.list[[paste(tendence, taux)]] <- data.table(
      tendence, taux, grid.dt)
    weight.vec <- c(ordonnée=0,pente=0)
    for(itération in 0:40){
      pred.vec <- as.numeric(feature.mat %*% weight.vec)
      resid.vec <- pred.vec-y
      pred.dt.list[[paste(tendence, taux, itération)]] <- data.table(
        tendence, taux, itération,
        x, y, prediction=pred.vec, residual=resid.vec)
      grad.vec <- colMeans(resid.vec * feature.mat)
      dir.vec <- -grad.vec * taux
      names(dir.vec) <- names(weight.vec)
      after.weight <- weight.vec + dir.vec
      weight.dt.list[[paste(tendence, taux, itération)]] <- data.table(
        tendence, taux, itération, t(weight.vec),
        orig=t(weight.vec),
        dir=t(dir.vec),
        after=t(after.weight),
        erreur=do.call(erreur.fun, as.list(weight.vec)))
      weight.vec <- after.weight
    }
  }
}
sim.dt <- do.call(rbind, sim.dt.list)
ggplot()+
  facet_grid(. ~ tendence_expr)+
  geom_point(aes(
    x, y),
    data=sim.dt)

erreur.dt <- do.call(rbind, erreur.dt.list)
erreur.dt[, erreur.relative := (erreur-min(erreur))/(max(erreur)-min(erreur)), by=tendence]
erreur.dt[, .(min.erreur=min(erreur)), by=tendence]
weight.dt <- do.call(rbind, weight.dt.list)
for(axis.name in names(weight.vec)){
  for(dir.or.not in c("", "after.")){
    var.name <- paste0(dir.or.not, axis.name)
    value.vec <- weight.dt[[var.name]]
    rep.list <- list(
      list(comp=`<`, extreme="min", replace=-Inf),
      list(comp=`>`, extreme="max", replace=Inf))
    for(info in rep.list){
      min.or.max <- get(paste0(info$extreme, ".", axis.name))
      set(
        weight.dt,
        i=which(info$comp(value.vec, min.or.max)),
        j=var.name,
        value=info$replace)
    }
  }
}
ggplot()+
  facet_grid(taux ~ tendence)+
  geom_tile(aes(
    pente, ordonnée, fill=erreur.relative),
    data=erreur.dt)+
  scale_fill_gradient(low="white", high="red")+
  theme_bw()+
  geom_point(aes(
    pente, ordonnée),
    data=weight.dt)+
  geom_segment(aes(
    pente, ordonnée,
    xend=after.pente, yend=after.ordonnée),
    data=weight.dt)

some.erreur <- erreur.dt[, .(
  max.erreur=max(erreur)
), by=tendence
][
  weight.dt, on="tendence"
][
  erreur<max.erreur
]
some.erreur <- weight.dt[, .(
  max.erreur=erreur[1]
), by=tendence
][
  weight.dt, on="tendence"
][
  erreur > max.erreur*1.5, erreur := Inf
]
ggplot()+
  facet_grid(tendence ~ taux, scales="free_y")+
  geom_line(aes(
    itération, erreur),
    size=2,
    data=some.erreur)+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))

dput(RColorBrewer::brewer.pal(Inf, "Blues"))
taux.colors <- c(
  "1"="#08306B",
  "0.1"="#2171B5",
  "0.01"="#6BAED6",
  "0.001"="#C6DBEF")
some.erreur[, taux := factor(taux)]
ggplot()+
  facet_grid(tendence ~ ., scales="free_y")+
  geom_line(aes(
    itération, erreur, group=taux, color=taux),
    size=2,
    data=some.erreur)+
  scale_color_manual(values=taux.colors, breaks=names(taux.colors))+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))

weight.dt[, taux := factor(taux)]
ggplot()+
  scale_color_manual(values=taux.colors, breaks=names(taux.colors))+
  facet_grid(. ~ tendence)+
  geom_tile(aes(
    pente, ordonnée, fill=erreur.relative),
    data=erreur.dt)+
  scale_fill_gradient(low="white", high="red")+
  theme_bw()+
  geom_point(aes(
    pente, ordonnée, color=taux),
    data=weight.dt)+
  geom_segment(aes(
    pente, ordonnée,
    color=taux,
    xend=after.pente, yend=after.ordonnée),
    data=weight.dt)

pred.dt <- do.call(rbind, pred.dt.list)
some.erreur[, tendence.taux := paste(tendence, taux)]
erreur.dt[, tendence.taux := paste(tendence, taux)]
weight.dt[, tendence.taux := paste(tendence, taux)]
sim.dt[, tendence.taux := paste(tendence, taux)]
pred.dt[, tendence.taux := paste(tendence, taux)]
pred.dt[, inf.pred := ifelse(
  prediction>max(y), Inf, ifelse(
    prediction<min(y), -Inf, prediction))]
viz <- animint(
  title="Descente de gradient pour régression linéaire",
  source="https://github.com/tdhock/2026-08-apprentissage/blob/master/figure-gradient-descent-regression-fr.R",
  data=ggplot()+
    ggtitle("Données et fonction de prédiction")+
    theme_bw()+
    theme_animint(height=250, width=1000, colspan=2, last_in_row=TRUE)+
    geom_point(aes(
      x, y, key=x),
      data=sim.dt,
      showSelected="tendence.taux")+
    scale_x_continuous(
      "entrée x")+
    scale_y_continuous(
      "sortie y")+
    geom_segment(aes(
      x, y,
      key=x,
      color=ligne,
      xend=x, yend=inf.pred),
      data=data.table(pred.dt, ligne="erreur"),
      chunk_vars="tendence.taux",
      size=1,
      showSelected=c("tendence.taux", "itération"))+
    geom_line(aes(
      x, inf.pred, key=1, color=ligne),
      chunk_vars="tendence.taux",
      data=data.table(pred.dt, ligne="prédiction"),
      showSelected=c("tendence.taux","itération")),
  erreurIterations=ggplot()+
    ggtitle("Erreur, choisir itération/données/taux")+
    theme_bw()+
    theme_animint(width=400, height=500)+
    facet_grid(tendence ~ ., scales="free_y")+
    make_tallrect(some.erreur, "itération")+
    geom_line(aes(
      itération, erreur, group=taux, color=taux),
      size=4,
      clickSelects="tendence.taux",
      data=some.erreur)+
    geom_point(aes(
      itération, erreur, key=1),
      color="black",
      fill="white",
      size=5,
      showSelected=c("tendence.taux", "itération"),
      data=some.erreur)+
    scale_color_manual(values=taux.colors, breaks=names(taux.colors)),
  erreurGrid=ggplot()+
    ggtitle("Espace des paramètres, choisir itération")+
    theme_bw()+
    theme_animint(width=400, height=500)+
    geom_tile(aes(
      pente, ordonnée,
      fill=erreur.relative,
      color=erreur.relative,
      key=paste(pente, ordonnée)),
      showSelected="tendence.taux",
      data=erreur.dt)+
    scale_fill_gradient(low="white", high="red")+
    scale_color_gradient(low="white", high="red")+
    geom_point(aes(
      pente, ordonnée, key=itération),
      showSelected="tendence.taux",
      clickSelects="itération",
      size=5,
      alpha=0.55,
      data=weight.dt)+
    geom_point(aes(
      pente, ordonnée, key=1),
      showSelected=c("tendence.taux", "itération"),
      fill="white",
      color="black",
      size=5,
      data=weight.dt)+
    geom_segment(aes(
      pente, ordonnée,
      key=1,
      xend=after.pente, yend=after.ordonnée),
      showSelected=c("tendence.taux", "itération"),
      color="deepskyblue",
      data=weight.dt)+
    geom_point(aes(
      after.pente, after.ordonnée,
      key=1),
      showSelected=c("tendence.taux", "itération"),
      color="deepskyblue",
      data=weight.dt),
  first=list(
    tendence.taux="lin.haut 0.1"),
  time=list(
    variable="itération",
    ms=500),
  duration=list(
    tendence.taux=500,
    itération=500),
  out.dir="figure-gradient-descent-regression-fr")
viz
if(FALSE){
  animint2pages(viz, "2026-09-23-gradient-descent-regression-fr", chromote_sleep_seconds=3)
  animint2::update_gallery("~/R/gallery-fr")
}
