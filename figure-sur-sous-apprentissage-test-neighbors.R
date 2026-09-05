library(animint2)
library(data.table)
max.x <- 5
min.x <- -max.x
lfun <- function(expr)eval(substitute(list(function(x)expr)))
tendence.dt <- rowwiseDT(
  tendence=, fonction=, graine=,
  "constante", lfun(1), 6,
  "linéaire", lfun(x/max.x), 14,
  "quadratique", lfun(x^2/(max.x^2)), 11,#6?
  "cubique", lfun(x^3/(max.x^3)), 9
)[, tendence := factor(tendence, tendence)]#1?
grid.x.vec <- seq(min.x, max.x, l=401)
set.seed(1)#4?
N.total <- 100
x <- runif(N.total, min.x, max.x)
n.blocs <- 5
bloc.uniq <- 1:n.blocs
bloc.vec <- rep(bloc.uniq, length.out = N.total)
subtrain <- "sous-entraînement"
model.dt.list <- list()
lm.name <- "degré de base polynome, modèle linéaire"
nn.name <- "nombre de plus proches voisins"
for(tendence.i in 1:nrow(tendence.dt)){
  tendence.row <- tendence.dt[tendence.i]
  set.seed(tendence.row$graine)
  f <- tendence.row$fonction[[1]]
  y <- f(x) + rnorm(N.total, sd=0.1)
  for(test.bloc in bloc.uniq){
    ensemble <- ifelse(test.bloc==bloc.vec, "test", "entraînement")
    ensemble[ensemble=="entraînement"] <- c(subtrain, "validation")
    #ensemble[!(x %between% (c(-1,1)*test.max))] <- "test" #extrapolation
    #ensemble[x %between% (c(-1,1)*2)] <- "test" #interpolation
    plot(y ~ x, col=c(test=1, validation=2, "sous-entraînement"=3)[ensemble])
    all.sets <- rbind(
      data.table(ensemble, x, y),
      data.table(ensemble="grid", x=grid.x.vec, y=NA_real_)
    )[, row.i := .I]
    yrange <- all.sets[ensemble!="grid", range(y)]
    all.sets[, ynorm := (y-yrange[1])/diff(yrange)]
    subtrain.set <- all.sets[ensemble==subtrain]
    N.subtrain <- nrow(subtrain.set)
    max.degree <- N.subtrain-1
    degree.vec <- 0:5
    for(nombre.voisins in 1:N.subtrain){
      kfit <- FNN::knn.reg(
        subtrain.set[, .(x)],
        all.sets[, .(x)],
        subtrain.set$ynorm,
        nombre.voisins)
      model.dt.list[[paste(
        tendence.i, test.bloc, nombre.voisins, "nn"
      )]] <- data.table(
        tendence.row,
        test.bloc,
        all.sets,
        pred.y=kfit[["pred"]],
        voisins=nombre.voisins
      )
    }
  }
}
model.dt <- do.call(rbind, model.dt.list)[
, tendence.bloc := sprintf("%s.%d", tendence, test.bloc)
][]

(error.dt <- model.dt[
  ensemble != "grid", .(
    RMSE=sqrt(mean((ynorm - pred.y)^2))
  ), by=.(tendence, test.bloc, tendence.bloc, voisins, ensemble)
][
, RMSE.thresh := ifelse(RMSE<1e-10, 0, RMSE)
][])
(min.valid.err <- error.dt[ensemble=="validation"][
, .SD[which.min(RMSE)]
, by=.(tendence, test.bloc, tendence.bloc)])
pfac <- function(x)factor(x, c("sans caractères", "meilleur sur validation", sprintf("fixe(%s)", unique(error.dt$voisins))))
test.err <- error.dt[ensemble=="test"][
, param := pfac(sprintf("fixe(%d)", voisins))
][]
best.err <- test.err[
  min.valid.err[, .(tendence, test.bloc, tendence.bloc, voisins)],
  on=.NATURAL
][
, param := pfac("meilleur sur validation")
][]
all.test.err <- rbind(
  best.err, test.err,
  test.err[param=="fixe(40)"][, let(
    param = pfac("sans caractères")
  )]
)

ggplot()+
  geom_point(aes(
    RMSE, param),
    data=all.test.err)+
  facet_grid(. ~ tendence, scales="free", space="free")

yord <- c(
  "sans caractères",
  "sélection-sans",
  "sélection",
  "sélection-meilleur",
  "meilleur sur validation")
Pfac <- function(x)factor(x,yord)
show.err <- rbind(
  all.test.err[, let(
    Paramètre = Pfac(ifelse(
      grepl("fixe", param),
      "sélection",
      paste(param)
    ))
  )]
)

ggplot()+
  scale_x_log10()+
  geom_point(aes(
    RMSE, Paramètre),
    data=show.err)+
  facet_grid(. ~ tendence, scales="free", space="free")

show.err.wide <- dcast(
  show.err[Paramètre=="meilleur sur validation", voisins := NA],
  tendence + Paramètre + voisins ~ .,
  list(mean, sd, length),
  value.var="RMSE")
show.err.compare <- show.err[Paramètre == "sélection"][
  show.err[Paramètre != "sélection", .(
    tendence, test.bloc, tendence.bloc, compare_RMSE=RMSE, compare_param=Paramètre
  )], on=.NATURAL, allow.cartesian=TRUE]
show.err.p <- show.err.compare[, {
  L <- t.test(RMSE, compare_RMSE, paired=TRUE)
  p=L$p.value
  if(is.nan(p))p <- 1
  data.table(
    Paramètre=Pfac(paste0("sélection-", sub(" .*", "", compare_param))),
    RMSE=mean(RMSE),
    compare_RMSE=mean(compare_RMSE),
    p)
}, by=.(tendence, voisins, param, compare_param)]

text.color <- "black"
text.size <- 12
data.color <- "red"
best.err <- error.dt[ensemble=="validation"][, .SD[RMSE==min(RMSE)], by=tendence.bloc]
(set.colors <- rowwiseDT(
  ensemble=, color=,
  subtrain, "black", #subtrain without quotes!
  "validation", "deepskyblue",
  "test", "red"
)[, setNames(color, ensemble)])
expand <- 0.1
not.grid <- model.dt[ensemble!="grid"]
model.dt[, pred.thresh := ifelse(
  pred.y < min(not.grid$ynorm)-expand, -Inf,
  ifelse(pred.y > max(not.grid$ynorm)+expand, Inf, pred.y))]
tallrect.dt <- unique(error.dt[, .(voisins)])
#test.err <- error.dt[ensemble=="validation"]
height.pixels <- 500
tf.dt <- unique(model.dt[, .(tendence, test.bloc, tendence.bloc)])
viz <- animint(
  title="Test error p-values for nearest neighbors, simulated regression",
  duration=list(
    tendence.bloc=1000,
    voisins=1000),
  test=ggplot()+
    ggtitle("Erreur sur l’ensemble test, choisir tendence et bloc")+
    theme_animint(
      width=1000, height=250,
      colspan=2, last_in_row=TRUE)+
    geom_segment(aes(
      RMSE, Paramètre,
      key=Paramètre,
      xend=compare_RMSE, yend=Paramètre),
      size=1,
      showSelected="voisins",
      color=data.color,
      data=show.err.p)+
    geom_segment(aes(
      RMSE_mean+RMSE_sd, Paramètre,
      key=Paramètre,
      xend=RMSE_mean-RMSE_sd, yend=Paramètre),
      showSelected="voisins",
      color=data.color,
      data=show.err.wide[Paramètre=="sélection"])+
    geom_point(aes(
      RMSE_mean, Paramètre,
      key=Paramètre),
      showSelected="voisins",
      color=data.color,
      data=show.err.wide[Paramètre=="sélection"])+
    geom_segment(aes(
      RMSE_mean+RMSE_sd, Paramètre,
      xend=RMSE_mean-RMSE_sd, yend=Paramètre),
      color=data.color,
      data=show.err.wide[Paramètre!="sélection"])+
    geom_point(aes(
      RMSE_mean, Paramètre),
      color=data.color,
      data=show.err.wide[Paramètre!="sélection"])+
    geom_text(aes(
      RMSE_mean, Paramètre,
      label=sprintf("%.3f±%.3f", RMSE_mean, RMSE_sd)),
      color=text.color,
      size=text.size,
      data=show.err.wide[Paramètre!="sélection"])+
    geom_text(aes(
      RMSE_mean, Paramètre,
      key=Paramètre,
      label=sprintf("%.3f±%.3f", RMSE_mean, RMSE_sd)),
      color=text.color,
      size=text.size,
      showSelected="voisins",
      data=show.err.wide[Paramètre=="sélection"])+
    geom_text(aes(
      RMSE_mid, Paramètre,
      key=Paramètre,
      label=label),
      showSelected="voisins",
      color=text.color,
      size=text.size,
      data=show.err.p[, let(
        RMSE_mid = (RMSE+compare_RMSE)/2,
        label=ifelse(
          p<0.001,
          "p<0.001",
          sprintf("p=%.3f", p))
      )])+
    geom_point(aes(
      RMSE, Paramètre,
      key=test.bloc),
      showSelected="voisins",
      clickSelects="tendence.bloc",
      color="blue",
      size=5,
      fill_off="transparent",
      alpha=0.5,
      alpha_off=0.5,
      fill=data.color,
      data=all.test.err[Paramètre=="sélection"])+
    geom_point(aes(
      RMSE, Paramètre,
      key=paste(Paramètre, test.bloc)),
      clickSelects="tendence.bloc",
      color="blue",
      size=5,
      fill_off="transparent",
      alpha=0.5,
      alpha_off=0.5,
      fill=data.color,
      data=all.test.err[Paramètre != "sélection"])+
    scale_y_discrete(drop=FALSE)+
    scale_x_continuous(
      "Racine de l’erreur carrée moyenne",
      breaks=seq(0, 1, by=0.1))+
    facet_grid(. ~ tendence, scales="free", space="free", labeller=label_both),
  error=ggplot()+
    ggtitle("Choisir nombre de voisins")+
    theme(legend.position="none")+
    theme_animint(height=height.pixels, width=300)+
    geom_text(aes(
      20, 0.4,
      key=1,
      label=sprintf(
        "tendence=%s, bloc=%d",
        tendence, test.bloc)),
      showSelected="tendence.bloc",
      data=tf.dt)+
    scale_y_continuous("Racine de l’erreur carrée moyenne")+
    scale_x_continuous(
      "nombre de voisins",
      breaks=c(1, seq(10, 40, by=10)))+
    scale_color_manual(values=set.colors)+
    geom_line(aes(
      voisins, RMSE.thresh,
      color=ensemble,
      group=ensemble,
      key=ensemble),
      showSelected=c("tendence.bloc", "ensemble"),
      size=5,
      data=error.dt[ensemble != "test"])+
    geom_point(aes(
      voisins, RMSE.thresh,
      color=ensemble,
      key=ensemble),
      fill="white",
      size=4,
      showSelected=c("tendence.bloc", "ensemble"),
      data=best.err)+
    geom_tallrect(aes(
      xmin=voisins-0.5,
      xmax=voisins+0.5),
      alpha=0.5,
      color=NA,
      data=tallrect.dt,
      clickSelects="voisins"),
  fonctions=ggplot()+
    ggtitle("Tendence (points) et modèle (courbe) pour la sélection")+
    xlab("entrée x")+
    ylab("sortie y")+
    geom_text(aes(
      0, 1,
      key=1,
      label=sprintf(
        "tendence=%s, bloc=%d",
        tendence, test.bloc)),
      showSelected="tendence.bloc",
      data=tf.dt)+
    theme_animint(height=height.pixels, width=600)+
    scale_fill_manual(values=set.colors)+
    geom_point(aes(
      x, ynorm, fill=ensemble, key=row.i),
      size=4,
      showSelected=c("ensemble","tendence.bloc"),
      data=not.grid)+
    geom_line(aes(
      x, pred.thresh,
      key=1),
      data=model.dt[ensemble=="grid"],
      showSelected=c("tendence.bloc", "voisins")),
  out.dir="figure-sur-sous-apprentissage-test-neighbors",
  source="https://github.com/tdhock/2026-08-apprentissage/blob/master/figure-sur-sous-apprentissage-test-neighbors.R"
)
#viz
animint2dir(viz, viz$out.dir, open.browser = FALSE)

if(FALSE){
  animint2pages(viz, "2026-09-04-sur-sous-apprentissage-test-neighbors", chromote_sleep_seconds=3)
}
