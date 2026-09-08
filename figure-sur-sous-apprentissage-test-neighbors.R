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
pfac <- function(x)factor(x, c("max (sans caractères)", "meilleur sur validation", sprintf("fixe(%s)", unique(error.dt$voisins))))
test.err <- error.dt[ensemble=="test"][
, param := pfac(sprintf("fixe(%d)", voisins))
][]
best.err <- test.err[
  min.valid.err[, .(tendence, test.bloc, tendence.bloc, voisins)],
  on=.NATURAL
][
, param := pfac("meilleur sur validation")
][]
chaque.bloc <- "chaque bloc de VC"
moyenne.SD <- "moyenne ± écart type"
erreur.info <- rowwiseDT(
  erreur=, size=, color=,
  chaque.bloc, 5, "blue",
  moyenne.SD, 3, "red")
all.test.err <- rbind(
  best.err, test.err,
  test.err[param=="fixe(40)"][, let(
    param = pfac("max (sans caractères)")
  )]
)[, erreur := chaque.bloc][]

ggplot()+
  geom_point(aes(
    RMSE, param),
    data=all.test.err)+
  facet_grid(. ~ tendence, scales="free", space="free")

yord <- c(
  "max (sans caractères)",
  "sélection-max",
  "sélection",
  "sélection-meilleur",
  "meilleur sur validation")
Pfac <- function(x)factor(x,yord)
show.err <- rbind(
  all.test.err[, let(
    Voisins = Pfac(ifelse(
      grepl("fixe", param),
      "sélection",
      paste(param)
    ))
  )]
)

ggplot()+
  scale_x_log10()+
  geom_point(aes(
    RMSE, Voisins),
    data=show.err)+
  facet_grid(. ~ tendence, scales="free", space="free")

show.err.wide <- dcast(
  show.err[Voisins=="meilleur sur validation", voisins := NA],
  tendence + Voisins + voisins ~ .,
  list(mean, sd, length),
  value.var="RMSE"
)[, erreur := moyenne.SD]
show.err.compare <- show.err[Voisins == "sélection"][
  show.err[Voisins != "sélection", .(
    tendence, test.bloc, tendence.bloc, compare_RMSE=RMSE, compare_param=Voisins
  )], on=.NATURAL, allow.cartesian=TRUE]
show.err.p <- show.err.compare[, {
  L <- t.test(RMSE, compare_RMSE, paired=TRUE)
  p=L$p.value
  if(is.nan(p))p <- 1
  data.table(
    Voisins=Pfac(paste0("sélection-", sub(" .*", "", compare_param))),
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
  title="Valeurs-p pour l’erreur sur test, plus proches voisins, régression",
  duration=list(
    tendence.bloc=1000,
    voisins=1000),
  test=ggplot()+
    ggtitle("Erreur sur l’ensemble test, choisir tendence dans les données, et bloc de VC")+
    theme_animint(
      width=1000, height=250,
      colspan=2, last_in_row=TRUE)+
    scale_color_manual(values=erreur.info[, setNames(color, erreur)])+
    scale_size_manual(values=erreur.info[, setNames(size, erreur)])+
    geom_point(aes(
      RMSE_mean, Voisins,
      key=Voisins),
      showSelected=c("erreur","voisins"),
      color=data.color,
      help="Point rouge pour l’erreur moyenne du nombre de voisins sélectionné",
      data=show.err.wide[Voisins=="sélection"])+
    geom_point(aes(
      RMSE_mean, Voisins,
      size=erreur,
      color=erreur),
      help="Point rouge pour l’erreur moyenne du meilleur sur validation et max (sans caractères)",
      data=show.err.wide[Voisins!="sélection"])+
    geom_segment(aes(
      RMSE, Voisins,
      key=Voisins,
      xend=compare_RMSE, yend=Voisins),
      size=1,
      help="Segment rouge entre moyennes",
      showSelected="voisins",
      color=data.color,
      data=show.err.p)+
    geom_segment(aes(
      RMSE_mean+RMSE_sd, Voisins,
      key=Voisins,
      xend=RMSE_mean-RMSE_sd, yend=Voisins),
      showSelected=c("voisins","erreur"),
      color=data.color,
      help="Segment rouge pour l’écart type",
      data=show.err.wide[Voisins=="sélection"])+
    geom_segment(aes(
      RMSE_mean+RMSE_sd, Voisins,
      xend=RMSE_mean-RMSE_sd, yend=Voisins),
      color=data.color,
      showSelected="erreur",
      help="Segment rouge pour l’écart type",
      data=show.err.wide[Voisins!="sélection"])+
    geom_text(aes(
      RMSE_mean, Voisins,
      label=sprintf("%.3f±%.3f", RMSE_mean, RMSE_sd)),
      color=text.color,
      size=text.size,
      showSelected="erreur",
      help="Texte pour moyenne ± écart type",
      data=show.err.wide[Voisins!="sélection"])+
    geom_text(aes(
      RMSE_mean, Voisins,
      key=Voisins,
      label=sprintf("%.3f±%.3f", RMSE_mean, RMSE_sd)),
      color=text.color,
      size=text.size,
      showSelected=c("erreur","voisins"),
      help="Texte pour moyenne ± écart type",
      data=show.err.wide[Voisins=="sélection"])+
    geom_text(aes(
      RMSE_mid, Voisins,
      key=Voisins,
      label=label),
      showSelected="voisins",
      color=text.color,
      size=text.size,
      help="Texte pour probabilité critique (valeur-p), différence entre sélection et meilleur/max nombre de voisins",
      data=show.err.p[, let(
        RMSE_mid = (RMSE+compare_RMSE)/2,
        label=ifelse(
          p<0.001,
          "p<0.001",
          sprintf("p=%.3f", p))
      )])+
    geom_point(aes(
      RMSE, Voisins,
      size=erreur,
      color=erreur,
      key=test.bloc),
      showSelected="voisins",
      clickSelects="tendence.bloc",
      fill_off="transparent",
      alpha=0.5,
      alpha_off=0.5,
      fill=data.color,
      help="Point entouré en bleu pour l’erreur de chaque bloc de validation croisée", 
      data=all.test.err[Voisins=="sélection"])+
    geom_point(aes(
      RMSE, Voisins,
      size=erreur,
      color=erreur,
      key=paste(Voisins, test.bloc)),
      clickSelects="tendence.bloc",
      fill_off="transparent",
      alpha=0.5,
      alpha_off=0.5,
      fill=data.color,
      help="Point entouré en bleu pour l’erreur de chaque bloc de validation croisée", 
      data=all.test.err[Voisins != "sélection"])+
    scale_y_discrete(drop=FALSE)+
    scale_x_continuous(
      "Racine de l’erreur carrée moyenne (test)",
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
      help="Texte pour la sélection de tendence et bloc",
      data=tf.dt)+
    scale_y_continuous("Racine de l’erreur carrée moyenne (sous-ent. ou validation)")+
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
      help="Courbes pour l’erreur sur sous-entraînement et validation",
      data=error.dt[ensemble != "test"])+
    geom_point(aes(
      voisins, RMSE.thresh,
      color=ensemble,
      key=ensemble),
      fill="white",
      size=4,
      showSelected=c("tendence.bloc", "ensemble"),
      help="Point pour la meilleure erreur sur validation",
      data=best.err)+
    geom_tallrect(aes(
      xmin=voisins-0.5,
      xmax=voisins+0.5),
      alpha=0.5,
      color=NA,
      data=tallrect.dt,
      help="Rectange pour la sélection du nombre de voisins",
      clickSelects="voisins"),
  fonctions=ggplot()+
    ggtitle("Tendence (points) et modèle (courbe) pour la sélection")+
    theme_animint(height=height.pixels, width=700)+
    xlab("entrée x")+
    ylab("sortie y")+
    geom_text(aes(
      0, 1.05,
      key=1,
      label=sprintf(
        "tendence=%s, bloc=%d",
        tendence, test.bloc)),
      showSelected="tendence.bloc",
      help="Texte pour la sélection de tendence et bloc",
      data=tf.dt)+
    scale_fill_manual(values=set.colors)+
    geom_point(aes(
      x, ynorm, fill=ensemble, key=row.i),
      size=4,
      showSelected=c("ensemble","tendence.bloc"),
      help="Points pour les données",
      data=not.grid)+
    geom_line(aes(
      x, pred.thresh,
      key=1),
      data=model.dt[ensemble=="grid"],
      help="Courbe noir pour la fonction de prédiction pour la sélection du nombre de voisins",
      showSelected=c("tendence.bloc", "voisins")),
  out.dir="figure-sur-sous-apprentissage-test-neighbors",
  source="https://github.com/tdhock/2026-08-apprentissage/blob/master/figure-sur-sous-apprentissage-test-neighbors.R"
)

if(FALSE){
  animint2dir(viz, viz$out.dir, open.browser = FALSE)
  viz
  animint2pages(viz, "2026-09-04-sur-sous-apprentissage-test-neighbors", chromote_sleep_seconds=3)
  animint2::update_gallery("~/R/gallery-fr/")
}
