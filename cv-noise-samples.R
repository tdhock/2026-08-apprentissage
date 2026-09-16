library(data.table)
n.folds <- 6
max.N <- 1000
N <- max.N*n.folds/(n.folds-1)
abs.x <- 3*pi
set.seed(2)
norm01 <- function(z,ref=z)(z-min(ref))/(max(ref)-min(ref))
(grid.dt <- data.table(
  raw=seq(-abs.x,abs.x, l=201),
  y=0 #for mlr3
)[, x := norm01(raw)][])
x.vec <- runif(N, -abs.x, abs.x)
str(x.vec)
reg.pattern.list <- list(
  sin=sin,
  constante=function(x)0)
standard.deviation.vec <- c(
  facile=0.4,
  dificile=1.1,
  impossible=5)
dfac <- function(x)factor(x, names(standard.deviation.vec))
reg.task.list <- list()
reg.data.list <- list()
grid.tendence.dt.list <- list()
for(tendence in names(reg.pattern.list)){
  f <- reg.pattern.list[[tendence]]
  for(difficulté.chr in names(standard.deviation.vec)){
    standard.deviation <- standard.deviation.vec[[difficulté.chr]]
    difficulté <- dfac(difficulté.chr)
    task_id <- paste(tendence, difficulté)
    tendence.vec <- f(x.vec)
    y <- tendence.vec+rnorm(N,sd=standard.deviation)
    task.dt <- data.table(
      x=norm01(x.vec,grid.dt$raw),
      y = norm01(y))
    reg.data.list[[paste(difficulté, task_id)]] <- data.table(
      difficulté,
      tendence,
      task_id,
      task.dt)
    reg.task.list[[paste(difficulté, task_id)]] <- mlr3::TaskRegr$new(
      task_id, task.dt, target="y"
    )
    grid.tendence.dt.list[[paste(difficulté, task_id)]] <- data.table(
      difficulté,
      tendence,
      task_id,
      algorithm="ideal",
      x=grid.dt$x,
      y=norm01(f(grid.dt$raw),y)
    )      
  }
}
(reg.data <- rbindlist(reg.data.list))
(grid.tendence.dt <- rbindlist(grid.tendence.dt.list))
if(require(animint2)){
  ggplot()+
    geom_point(aes(
      x, y),
      data=reg.data)+
    geom_line(aes(
      x, y),
      color="red",
      size=2,
      data=grid.tendence.dt)+
    facet_grid(tendence ~ difficulté, labeller=label_both)
}

reg_size_cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()
n.entraînement.sizes <- 8
reg_size_cv$param_set$values$sizes <- n.entraînement.sizes
reg_size_cv$param_set$values$ratio <- 563/1000 # 0.01^(1/8)
reg_size_cv$param_set$values$folds <- n.folds
reg_size_cv$instantiate(reg.task.list[[1]])#required for consistent folds across tasks.
u.train.groups <- unique(reg_size_cv$instance$iteration.dt$n.train.groups)
rect.x <- log10(u.train.groups)

(reg.learner.list <- list(
  if(requireNamespace("rpart"))mlr3::LearnerRegrRpart$new(),
  mlr3::LearnerRegrFeatureless$new()))
(reg.bench.grid <- mlr3::benchmark_grid(
  reg.task.list,
  reg.learner.list,
  reg_size_cv))

if(FALSE){
  if(require(future))plan("multisession")
}
if(require(lgr))get_logger("mlr3")$set_threshold("warn")
(reg.bench.result <- mlr3::benchmark(
  reg.bench.grid, store_models = TRUE))

algo.info <- rowwiseDT(
  Algorithme=, algorithm=, color=, size=,
  "sans caractères", "featureless", "deepskyblue", 4,
  "arbre de décision", "rpart", "red", 2,
  "idéal", "ideal", "black", 1)
algo.colors <- algo.info[, setNames(color, Algorithme)]
algo.sizes <- algo.info[, setNames(size, Algorithme)]

reg.bench.score <- nc::capture_first_df(
  mlr3resampling::score(reg.bench.result),
  task_id=list(
    tendence=".*?",
    " ",
    difficulté=".*", dfac
  )
)[, let(
  Nentraînement = n.train.groups,
  tendence_difficulté_Nentraînement = paste(tendence,difficulté,n.train.groups),
  bloc.test = test.fold
)][algo.info, on="algorithm", nomatch=0L]
Nentraînement_vec <- unique(reg.bench.score$Nentraînement)

grid.task <- mlr3::TaskRegr$new("grid", grid.dt, target="y")
pred.dt.list <- list()
point.dt.list <- list()
for(score.i in 1:nrow(reg.bench.score)){
  reg.bench.row <- reg.bench.score[
    score.i
  ][, entraînement := train][]
  task.dt <- data.table(
    reg.bench.row$task[[1]]$data(),
    reg.bench.row$resampling[[1]]$instance$fold.dt)
  set.ids <- data.table(
    Ensemble=c("test","entraînement")
  )[
  , data.table(row_id=reg.bench.row[[Ensemble]][[1]])
  , by=Ensemble]
  i.points <- set.ids[
    task.dt, on="row_id"
  ][
    is.na(Ensemble), Ensemble := "ignoré"
  ]
  point.id <- reg.bench.row[, paste(tendence_difficulté_Nentraînement, bloc.test, Algorithme)]
  point.dt.list[[point.id]] <- data.table(
    reg.bench.row[, .(tendence_difficulté_Nentraînement, bloc.test, Algorithme)],
    i.points)
  i.learner <- reg.bench.row$learner[[1]]
  pred.dt.list[[score.i]] <- data.table(
    reg.bench.row[, .(
      tendence_difficulté_Nentraînement, tendence, difficulté, Nentraînement, bloc.test, Algorithme
    )],
    as.data.table(
      i.learner$predict(grid.task)
    )[, .(x=grid.dt$x, y=response)]
  )
}
(pred.dt <- rbindlist(pred.dt.list))
(point.dt <- rbindlist(point.dt.list)[Algorithme=="sans caractères"])
(upred <- unique(pred.dt[, .(tendence_difficulté_Nentraînement, tendence, difficulté, Nentraînement)]))
tendence.dt <- upred[
  grid.tendence.dt, on=.(tendence,difficulté), allow.cartesian=TRUE
][
  algo.info, on="algorithm", nomatch=0L
]

(reg.bench.wide <- dcast(
  reg.bench.score,
  tendence + difficulté + Nentraînement + Algorithme + tendence_difficulté_Nentraînement ~ .,
  list(mean, sd, length, min, max),
  value.var=c("regr.mse")))
reg.bench.test <- dcast(
  reg.bench.score[, log10.mse := log10(regr.mse)],
  tendence + difficulté + Nentraînement + bloc.test + tendence_difficulté_Nentraînement ~ algorithm,
  value.var=c("log10.mse"))
seq.diff <- diff(rect.x)[1]/2
test.proposed <- reg.bench.test[, {
  paired <- t.test(rpart, featureless, alternative="two.sided", paired=TRUE)
  unpaired <- t.test(rpart, featureless, alternative="two.sided", paired=FALSE)
  data.table(
    mean.of.diff=paired$estimate, p.value=paired$p.value, p.unpaired=unpaired$p.value,
    mean.rpart=unpaired$estimate[1], mean.featureless=unpaired$estimate[2], p.unpaired=unpaired$p.value)
}, keyby=.(tendence,difficulté,Nentraînement,tendence_difficulté_Nentraînement)
][, `:=`(
  difference=ifelse(
    is.nan(p.value) | p.value>0.05, "pas significative", "significative"),
  xmin=10^(rect.x-seq.diff),
  xmax=10^(rect.x+seq.diff)
), by=.(tendence,difficulté)][]
reg.bench.join <- reg.bench.wide[
  test.proposed[, .(tendence_difficulté_Nentraînement,tendence,difficulté,Nentraînement,difference)],
  on=.NATURAL]
mid.x <- 10^((max(rect.x)+min(rect.x))/2)
data.color <- "grey50"

mse.limits <- c(0.01, 0.05)
mse.breaks <- c(0.01,0.02,0.04)
Toff <- 1.2
Tbrk <- c(0,0.5,1)
Tbreaks <- c(Tbrk,Tbrk+Toff)
Tlabels <- c(Tbrk,Tbrk)
ignoré.x <- 1.1
ignoré.y.point <- 0.1
ignoré.y.text <- 0
Tpred <- function(DT){
  if(! "Ensemble" %in% names(DT)){
    DT <- data.table(Ensemble=c("entraînement","test"))[, data.table(DT), by=Ensemble]
  }
  data.table(DT)[
  , x := ifelse(Ensemble=="test",0,Toff)+x
  ][
    Ensemble=="ignoré", `:=`(x=ignoré.x, y=ignoré.y.point)
  ][]
}
(data.sizes <- point.dt[, .(N=.N), by=.(tendence_difficulté_Nentraînement, bloc.test, Ensemble)])

viz <- animint(
  title="Échantillons pour apprendre une fonction de régression",
  overview=ggplot()+
    ggtitle("Choisir tendence, difficulté, Nentraînement")+
    theme_bw()+
    theme_animint(width=600, height=300)+
    geom_ribbon(aes(
      Nentraînement,
      ymin=regr.mse_mean-regr.mse_sd,
      ymax=regr.mse_mean+regr.mse_sd,
      group=Algorithme,
      fill=Algorithme),
      help=paste("Moyenne ± écart type sur", n.folds, "blocs dans la validation croisée"),
      color=NA,
      alpha=0.5,
      data=reg.bench.wide)+
    geom_line(aes(
      Nentraînement, regr.mse_mean,
      group=Algorithme),
      help=paste("Moyenne sur", n.folds, "blocs dans la validation croisée"),
      color="grey",
      showSelected="Algorithme",
      data=reg.bench.wide)+
    scale_size_manual(values=algo.sizes)+
    scale_fill_manual(values=algo.colors)+
    scale_color_manual(values=c(
      significative="black",
      "pas significative"=NA))+
    geom_point(aes(
      Nentraînement, regr.mse_mean,
      color=difference,
      size=Algorithme,
      fill=Algorithme),
      help=paste("Moyenne sur", n.folds, "blocs dans la validation croisée"),
      data=reg.bench.join)+
    geom_segment(aes(
      Nentraînement, 10^mean.rpart,
      key=1,
      xend=Nentraînement, yend=10^mean.featureless),
      showSelected="tendence_difficulté_Nentraînement",
      help="Segment gris pour la différence entre rpart et sans caractères",
      size=3,
      alpha=0.5,
      data=test.proposed)+
    geom_text(aes(
      Nentraînement, 10^pmax(mean.rpart,mean.featureless)*1.1,
      key=1,
      hjust=ifelse(Nentraînement<mid.x, 0, 1),
      label=fcase(
        p.value<0.0001, "p<0.0001",
        is.nan(p.value), "Diff=0",
        default=sprintf("p=%.4f", p.value))),
      showSelected="tendence_difficulté_Nentraînement",
      help="Probabilité critique dans un test de Student, différence entre rpart et sans caractères",
      data=test.proposed)+
    geom_rect(aes(
      xmin=xmin, xmax=xmax,
      ymin=0, ymax=Inf),
      alpha=0.1,
      help="Rectangle gris pour la sélection de tendence, difficulté, nombre d’échantillons d’entraînement",
      fill="black",
      color=NA,
      clickSelects="tendence_difficulté_Nentraînement",
      data=test.proposed)+
    scale_y_log10(
      "Erreur carrée moyenne",
      limits=mse.limits,
      breaks=mse.breaks
    )+
    scale_x_log10(
      "Nentraînement = Nombre d’échantillons d’entraînement")+
    facet_grid(tendence ~ difficulté),
  scatter=ggplot()+
    ggtitle("Erreur carrée pour la séléction")+
    theme_bw()+
    theme_animint(width=300, height=300, last_in_row=TRUE)+
    theme(legend.position="none")+
    coord_equal(xlim=mse.limits, ylim=mse.limits)+
    scale_x_log10(
      "sans caractères",
      breaks=mse.breaks)+
    scale_y_log10(
      "arbre de décision",
      breaks=mse.breaks)+
    geom_abline(aes(
      slope=slope, intercept=intercept),
      help="Ligne diagonale pour l’égalité des taux d’erreur",
      color="grey50",
      data=data.table(slope=1, intercept=0))+
    geom_segment(aes(
      x, y, xend=xend, yend=yend, color=Algorithme),
      data=rbind(
        data.table(x=0, y=0, xend=0, yend=Inf, Algorithme="arbre de décision"),
        data.table(x=0, y=0, xend=Inf, yend=0, Algorithme="sans caractères")),
      help="Segments pour les couleurs de chaque Algorithme",
      alpha=0.5,
      showSelected="Algorithme",
      size=5)+
    scale_color_manual(values=algo.colors)+
    geom_point(aes(
      10^featureless, 10^rpart,
      key=bloc.test,
      tooltip=sprintf(
        "fold %d featureless=%.3f rpart=%.3f", bloc.test, featureless, rpart)),
      showSelected="tendence_difficulté_Nentraînement",
      help="Un cercle pour chaque bloc dans la validation croisée",
      clickSelects="bloc.test",
      size=5,
      alpha=0.7,
      data=reg.bench.test),
  details=ggplot()+
    ggtitle("Erreur pour la sélection")+
    theme_bw()+
    theme_animint(width=1000, height=150, colspan=2, last_in_row=TRUE)+
    theme(legend.position="none")+
    scale_y_discrete("Algo")+
    scale_x_log10(
      "Erreur carrée moyenne",
      limits=mse.limits,
      breaks=mse.breaks)+
    scale_color_manual(values=algo.colors)+
    geom_point(aes(
      regr.mse, Algorithme,
      key=paste0(Algorithme, bloc.test),
      color=Algorithme,
      tooltip=sprintf(
        "%s bloc %d MSE=%.3f", Algorithme, bloc.test, regr.mse)),
      showSelected=c("Algorithme","tendence_difficulté_Nentraînement"),
      help="Un cercle pour chaque division et Algorithmee",
      clickSelects="bloc.test",
      alpha=0.7,
      size=5,
      data=reg.bench.score),
  pred=ggplot()+
    ggtitle("Prédictions pour la division choisie")+
    theme_bw()+
    theme_animint(height=300, width=900, colspan=2)+
    geom_point(aes(
      x, y,
      key=row_id),
      showSelected=c("tendence_difficulté_Nentraînement","bloc.test"),
      size=3,
      help="Un cercle pour chaque échantillon test (gauche) et entraînement (droite).",
      fill="white",
      color=data.color,
      data=Tpred(point.dt))+
    scale_x_continuous(
      "x = entrée",
      labels=Tlabels,
      breaks=Tbreaks)+
    scale_y_continuous("y = sortie")+
    geom_line(aes(
      x, y,
      key=Ensemble,
      group=Ensemble,
      color=Algorithme,
      size=Algorithme),
      help="Courbe noir pour la fonction de prédiction idéale, utilisée pour la création de ces données",
      showSelected=c("tendence_difficulté_Nentraînement"),
      data=Tpred(tendence.dt))+
    geom_line(aes(
      x, y,
      key=paste(Algorithme,Ensemble),
      color=Algorithme,
      size=Algorithme,
      group=paste(Algorithme,Ensemble)),
      help="Courbes en bleue et rouge pour les fonctions de prédiction",
      showSelected=c("tendence_difficulté_Nentraînement","bloc.test"),
      data=Tpred(pred.dt))+
    geom_text(aes(
      x, ifelse(Ensemble=="ignoré", ignoré.y.text, 0.98),
      hjust=ifelse(Ensemble=="ignoré", 0.5, 0),
      key=Ensemble,
      label=sprintf("N=%d données %s", N, Ensemble)),
      data=Tpred(data.sizes[, x := 0]),
      help="Texte pour nombre d’échantillons dans chaque ensemble",
      color=data.color,
      showSelected="tendence_difficulté_Nentraînement")+
    scale_size_manual(values=algo.sizes)+
    scale_color_manual(values=algo.colors),
  out.dir="cv-noise-samples",
  source="https://github.com/tdhock/2026-08-apprentissage/blob/main/cv-noise-samples.R",
  duration=list(
    bloc.test=1000,
    tendence_difficulté_Nentraînement=1000),
  first=list(
    tendence_difficulté_Nentraînement="sin facile 1000")
)
viz

if(FALSE){
  animint2pages(viz, "2026-09-16--chantillons-pour-r-gression", chromote_sleep_seconds = 5)
}



