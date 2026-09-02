library(animint2)
library(data.table)
max.x <- 5
min.x <- -max.x
lfun <- function(expr)eval(substitute(list(function(x)expr)))
tendence.dt <- rowwiseDT(
  tendence=, fonction=, graine=,
  "constante", lfun(1), 6,
  "cubique", lfun(x^3/(max.x^3)), 9,#1?
  "quadratique", lfun(x^2/(max.x^2)), 11,#6?
  "linéaire", lfun(x/max.x), 14)
grid.x.vec <- seq(min.x, max.x, l=401)
set.seed(7)#4?
N.train <- 10
max.degree <- N.train-1
N.total <- 100
x <- runif(N.total, min.x, max.x)
ensemble <- rep("validation", N.total)
subtrain <- "sous-entraînement"
ensemble[1:N.train] <- subtrain
model.dt.list <- list()
lm.name <- "degré de base polynome, modèle linéaire"
nn.name <- "nombre de plus proches voisins"
for(tendence.i in 1:nrow(tendence.dt)){
  tendence.row <- tendence.dt[tendence.i]
  set.seed(tendence.row$graine)
  f <- tendence.row$fonction[[1]]
  y <- f(x) + rnorm(N.total, sd=0.1)
  all.sets <- rbind(
    data.table(ensemble, x, y),
    data.table(ensemble="grid", x=grid.x.vec, y=NA_real_))
  yrange <- all.sets[ensemble!="grid", range(y)]
  all.sets[, ynorm := (y-yrange[1])/diff(yrange)]
  degree.vec <- 0:max.degree
  train.set <- all.sets[ensemble==subtrain]
  for(degree in degree.vec){
    pred.y <- if(degree==0){
      train.set[, mean(ynorm)]
    }else{
      right.side.vec <- paste0("I(x^", 1:degree, ")")
      right.side.str <- paste(right.side.vec, collapse="+")
      model.str <- paste("ynorm ~", right.side.str)
      model.formula <- as.formula(model.str)
      model.fit <- lm(model.formula, train.set)
      predict(model.fit, all.sets)
    }
    model.dt.list[[paste(tendence.i, degree, "lm")]] <- data.table(
      tendence.row,
      all.sets,
      pred.y,
      paramètre=degree,
      regularisation=lm.name)
  }
  for(nombre.voisins in 1:N.train){
    ## kfit <- kknn::kknn(
    ##   ynorm~x,
    ##   train.set,
    ##   all.sets[, .(x)],
    ##   k=nombre.voisins,
    ##   scale=FALSE,
    ##   kernel="rectangular")
    kfit <- FNN::knn.reg(
      train.set[, .(x)],
      all.sets[, .(x)],
      train.set$ynorm,
      nombre.voisins)
    model.dt.list[[paste(tendence.i, nombre.voisins, "nn")]] <- data.table(
      tendence.row,
      all.sets,
      pred.y=kfit[["pred"]],
      paramètre=nombre.voisins,
      regularisation=nn.name)
  }
}
model.dt <- do.call(rbind, model.dt.list)

error.dt <- model.dt[
  ensemble != "grid", .(
    mse=mean((ynorm - pred.y)^2)
  ), by=.(tendence, regularisation, paramètre, ensemble)
][
, mse.thresh := ifelse(mse<1e-10, 0, mse)
]
best.err <- error.dt[ensemble=="validation"][, .SD[mse==min(mse)], by=.(tendence, regularisation)]
(set.colors <- rowwiseDT(
  ensemble=, color=,
  subtrain, "black",
  "validation", "red"
)[, setNames(color, ensemble)])
model.info <- rowwiseDT(
  regularisation=, color=, size=,
  lm.name, "blue", 3,
  nn.name, "green", 2)
(model.colors <- model.info[, setNames(color, regularisation)])
(model.sizes <- model.info[, setNames(size, regularisation)])
expand <- 0.1
not.grid <- model.dt[ensemble!="grid"]
model.dt[, pred.thresh := ifelse(
  pred.y < min(not.grid$ynorm)-expand, -Inf,
  ifelse(pred.y > max(not.grid$ynorm)+expand, Inf, pred.y))]
tallrect.dt <- unique(error.dt[, .(regularisation, paramètre)])
test.err <- error.dt[ensemble=="validation"]
text.dt <- rowwiseDT(
  regularisation=, paramètre=, hjust=,
  lm.name, max.degree, 0,
  nn.name, 1, 1
)[test.err, on=.(regularisation, paramètre), nomatch=0L]
duration.list <- list(tendence=1000)
for(regularisation in names(model.colors)){
  duration.list[[regularisation]] <- 1000
}
height.pixels <- 700
(viz <- animint(
  error=ggplot()+
    ggtitle("Choisir tendence et paramètres")+
    theme(legend.position="none")+
    theme_animint(height=height.pixels, width=500)+
    scale_y_continuous("log10(erreur carrée moyenne)")+
    scale_x_continuous(
      "hyper-paramètre de regularisation",
      limits=range(tallrect.dt$paramètre)+c(-1,1),
      breaks=unique(tallrect.dt$paramètre))+
    scale_color_manual(values=set.colors)+
    scale_fill_manual(values=model.colors)+
    facet_grid(regularisation ~ ., scales="free")+
    geom_tallrect(aes(
      xmin=paramètre-0.5,
      xmax=paramètre+0.5,
      fill=regularisation),
      alpha=0.5,
      color=NA,
      data=tallrect.dt,
      showSelected="regularisation",
      clickSelects=c(regularisation="paramètre"))+
    geom_line(aes(
      paramètre, log10(mse.thresh), color=ensemble, group=paste(tendence, ensemble)),
      clickSelects="tendence",
      showSelected=c("regularisation", "ensemble"),
      size=5,
      alpha_off=0.1,
      data=error.dt)+
    geom_point(aes(
      paramètre, log10(mse.thresh), color=ensemble),
      shape=1,
      fill="white",
      alpha_off=0.1,
      size=4,
      clickSelects="tendence",
      showSelected=c("regularisation", "ensemble"),
      data=best.err)+
    geom_text(aes(
      (0.5-hjust)*0.5+paramètre, log10(mse.thresh),
      hjust=hjust,
      label=tendence,
      color=ensemble),
      clickSelects="tendence",
      showSelected=c("regularisation", "ensemble"),
      data=text.dt),
  fonctions=ggplot()+
    ggtitle("Tendence (points) et modèles (courbes) pour la sélection")+
    xlab("entrée x")+
    ylab("sortie y")+
    theme_animint(height=height.pixels, width=600, last_in_row=TRUE)+
    scale_fill_manual(values=set.colors)+
    scale_color_manual(values=model.colors)+
    scale_size_manual(values=model.sizes)+
    geom_point(aes(
      x, ynorm, fill=ensemble, key=x),
      size=4,
      showSelected=c("ensemble","tendence"),
      data=not.grid)+
    geom_line(aes(
      x, pred.thresh,
      size=regularisation,
      key=regularisation,
      group=regularisation,
      color=regularisation),
      data=model.dt[ensemble=="grid"],
      showSelected=c("tendence", regularisation="paramètre")),
  duration=duration.list,
  out.dir="figure-sur-sous-apprentissage",
  title="Surapprentissage et sous-apprentissage avec modèle linéaire et plus proches voisins",
  first=setNames(list(10), nn.name),
  source="https://github.com/tdhock/2026-08-apprentissage/blob/master/figure-sur-sous-apprentissage.R"))
if(FALSE){
  animint2pages(viz, "2026-09-02-sur-sous-apprentissage")
}

quad.err <- error.dt[
  tendence=="quadratic"
][
, Set := ifelse(
  set=="subtrain",
  "sous-entraînement (subtrain)",
  "validation")
][
, modèle := ifelse(
  regularisation=="linear model polynomial degree",
  "linéaire", "plus proches voisins")
][
, "hyper-paramètre" := ifelse(
  regularisation=="linear model polynomial degree",
  "ordre de polynome", "nombre de voisins")
][]
Set.colors <- structure(
  c("black","red"),
  names=unique(quad.err$Set))
Set.linetypes <- structure(
  c("dotted","solid"),
  names=unique(quad.err$Set))
ggplot()+
  theme_bw()+
  geom_line(aes(
    paramètre, mse, linetype=Set),
    size=1,
    data=quad.err)+
  scale_color_manual(
    "ensemble",
    values=Set.colors)+
  scale_linetype_manual(
    "ensemble",
    values=Set.linetypes)+
  ##facet_wrap(~ regularisation, scales="free")+
  facet_grid(~ modèle + `hyper-paramètre`, labeller=label_both, scales="free")+
  scale_y_log10("Erreur L2")+
  scale_x_continuous(
    "valeur de hyper-paramètre",
    breaks=seq(0,10))+
  coord_cartesian(ylim=c(1e0,1e-4))
