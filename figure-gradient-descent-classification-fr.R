set.seed(1)
sim.col <- 2
sim.row <- 100
data.i <- seq_len(sim.row)
features.hidden <- matrix(runif(sim.row*sim.col), sim.row, sim.col)
library(data.table)
hidden.dt <- data.table(type="hidden", features.hidden)
bayes <- function(DT)DT[, (V1>0.2 & V2<0.8)]
label.vec <- ifelse(bayes(hidden.dt), 1, -1)
features.noisy <- features.hidden+rnorm(sim.row*sim.col, sd=0.05)
label.fac <- factor(label.vec)
table(label.vec)
subtrain <- "sous-entraînement"
is.ensemble.list <- list(
  validation=rep(c(TRUE,FALSE), l=nrow(features.noisy)))
ensemble.vec <- ifelse(is.ensemble.list$validation, "validation", subtrain)
is.ensemble.list[[subtrain]] <- !is.ensemble.list$validation
n.subtrain <- sum(is.ensemble.list[[subtrain]])
(sim.dt <- data.table(
  rbind(
    data.table(type="noisy", features.noisy),
    hidden.dt),
  data.i,
  ensemble=ensemble.vec, 
  label.fac
))
library(animint2)
ggplot()+
  facet_grid(ensemble ~ type, labeller=label_both)+
  geom_point(aes(
    V1, V2, color=label.fac),
    data=sim.dt)+
  coord_equal()
n.grid <- 30
V1.seq <- sim.dt[, seq(min(V1), max(V1), l=n.grid)]
V2.seq <- sim.dt[, seq(min(V2), max(V2), l=n.grid)]
grid.dt <- CJ(V1=V1.seq, V2=V2.seq)[, tile.i := 1:.N][]
grid.dt[, bayes.num := ifelse(bayes(grid.dt), 1, -1)]
grid.dt[, bayes.fac := factor(bayes.num)]
ggplot()+
  facet_grid(. ~ type)+
  geom_tile(aes(
    V1, V2, fill=bayes.fac),
    data=grid.dt)+
  geom_point(aes(
    V1, V2, fill=label.fac),
    color="black",
    shape=21,
    data=sim.dt)+
  coord_equal()
get_contours <- function(score){
  contour.list <- contourLines(
    V1.seq, V2.seq, 
    matrix(score, length(V1.seq), length(V2.seq), byrow=TRUE),
    levels=0)
  if(length(contour.list)){
    data.table(contour.i=seq_along(contour.list))[, {
      with(contour.list[[contour.i]], data.table(level, x, y))
    }, by=contour.i]
  }
}
bayes.contour.dt <- get_contours(grid.dt$bayes.num)
ggplot()+
  facet_grid(. ~ type)+
  geom_path(aes(
    x, y, group=contour.i),
    data=bayes.contour.dt)+
  geom_point(aes(
    V1, V2, fill=label.fac),
    color="black",
    shape=21,
    data=sim.dt)+
  coord_equal()
new_node <- function(value, gradient=NULL, ...){
  node <- new.env()
  node$value <- value
  node$parent.list <- list(...)
  node$backward <- function(){
    grad.list <- gradient(node)
    for(parent.name in names(grad.list)){
      parent.node <- node$parent.list[[parent.name]]
      parent.node$grad <- grad.list[[parent.name]]
      parent.node$backward()
    }
  }
  node
}
initial_node <- function(mat){
  new_node(mat, gradient=function(...)list())
}
mm <- function(feature.node, weight.node)new_node(
  cbind(1, feature.node$value) %*% weight.node$value,
  features=feature.node, 
  weights=weight.node,
  gradient=function(node)list(
    features=node$grad %*% t(weight.node$value),
    weights=t(cbind(1, feature.node$value)) %*% node$grad))
relu <- function(before.node)new_node(
  ifelse(before.node$value < 0, 0, before.node$value),
  before=before.node,
  gradient=function(node)list(
    before=ifelse(before.node$value < 0, 0, node$grad)))
sigmoid <- function(before.node)new_node(
  1/(1+exp(before.node$value)),
  before=before.node,
  gradient=function(node)list(
    before=node$value*(1-node$value)))
log_loss <- function(pred.node, label.node)new_node(
  mean(log(1+exp(-label.node$value*pred.node$value))),
  pred=pred.node,
  label=label.node,
  gradient=function(...)list(
    pred=-label.node$value/(
      1+exp(label.node$value*pred.node$value)
    )/length(label.node$value)))
pred_node <- function(ensemble.features){
  feature.node <- initial_node(ensemble.features)
  for(layer.i in seq_along(weight.node.list)){
    weight.node <- weight.node.list[[layer.i]]
    before.node <- mm(feature.node, weight.node)
    feature.node <- if(layer.i < length(weight.node.list)){
      relu(before.node)
    }else{
      before.node
    }
  }
  feature.node
}
step.size <- 0.5
max.iterations <- 1000
units.per.layer.list <- list(
  "réseau de neurones"=c(ncol(features.noisy), 50, 1),
  "modèle linéaire"=c(ncol(features.noisy), 1))
grid.mat <- grid.dt[, cbind(V1,V2)]
loss.dt.list <- list()
err.dt.list <- list()
pred.dt.list <- list()
for(model.name in names(units.per.layer.list)){
  units.per.layer <- units.per.layer.list[[model.name]]
  weight.node.list <- list()
  set.seed(10)
  for(layer.i in seq(1, length(units.per.layer)-1)){
    input.units <- units.per.layer[[layer.i]]+1
    output.units <- units.per.layer[[layer.i+1]]
    weight.mat <- matrix(
      rnorm(input.units*output.units), input.units, output.units)
    weight.node.list[[layer.i]] <- initial_node(weight.mat)
  }
  for(iteration in 1:max.iterations){
    loss.node.list <- list()
    for(ensemble.name in names(is.ensemble.list)){
      is.ensemble <- is.ensemble.list[[ensemble.name]]
      ensemble.label.node <- initial_node(label.vec[is.ensemble])
      ensemble.features <- features.noisy[is.ensemble,]
      ensemble.pred.node <- pred_node(ensemble.features)
      ensemble.loss.node <- log_loss(ensemble.pred.node, ensemble.label.node)
      loss.node.list[[ensemble.name]] <- ensemble.loss.node
      ensemble.pred.num <- ifelse(ensemble.pred.node$value<0, -1, 1)
      is.error <- ensemble.pred.num != ensemble.label.node$value
      err.dt.list[[paste(model.name, iteration, ensemble.name)]] <- data.table(
        model.name, 
        iteration, ensemble=ensemble.name,
        data.i,
        ensemble.features,
        label=ensemble.label.node$value,
        pred.num=as.numeric(ensemble.pred.num))
      loss.dt.list[[paste(model.name, iteration, ensemble.name)]] <- data.table(
        model.name, 
        iteration, ensemble.name, 
        logistique=ensemble.loss.node$value,
        `pourcent erroné`=100*mean(is.error))
    }
    grid.node <- pred_node(grid.mat)
    pred.dt.list[[paste(model.name, iteration)]] <- data.table(
      model.name, 
      iteration,
      grid.dt,
      pred=as.numeric(grid.node$value))
    loss.node.list[[subtrain]]$backward()
    for(layer.i in seq_along(weight.node.list)){
      weight.node <- weight.node.list[[layer.i]]
      weight.node$value <- weight.node$value-step.size*weight.node$grad
    }  
  }
}
(loss.dt <- rbindlist(loss.dt.list))
(err.dt <- rbindlist(err.dt.list))
(pred.dt <- rbindlist(pred.dt.list))
loss.tall <- melt(loss.dt, measure=c("logistique", "pourcent erroné"))
loss.tall[, log10.iteration := log10(iteration)]
min.dt <- loss.tall[
, .SD[which.min(value)], by=.(model.name, ensemble.name, variable)]
ggplot()+
  facet_grid(variable ~ model.name, scales="free")+
  scale_y_continuous("")+
  geom_line(aes(
    iteration, value, color=ensemble.name),
    data=loss.tall)+
  geom_point(aes(
    iteration, value, fill=ensemble.name),
    shape=21,
    color="black",
    data=min.dt)
iteration.contours <- pred.dt[
, get_contours(pred), by=.(model.name, iteration)]

some <- function(DT)DT[iteration%in%c(1,5,10,50,100, 500, 1000)]
some.loss <- some(loss.dt)
pred.dt[, norm.pred := pred/max(abs(pred)), by=.(model.name, iteration)]
some.pred <- some(pred.dt)
some.err <- some(err.dt)
some.contours <- some.pred[
, get_contours(pred), by=.(model.name, iteration)]
ggplot()+
  facet_grid(model.name + ensemble ~ iteration, labeller="label_both")+
  geom_tile(aes(
    V1, V2, fill=norm.pred),
    data=some.pred)+
  geom_path(aes(
    x, y, group=contour.i),
    color="grey50",
    data=some.contours)+
  scale_fill_gradient2()+
  geom_point(aes(
    V1, V2),
    size=2,
    data=some.err[label!=pred.num])+
  geom_point(aes(
    V1, V2, color=label.fac),
    size=1,
    data=sim.dt[type=="noisy"])+
  coord_equal()
loss.tall[, modèle.itération := paste(model.name, iteration)]
err.dt[, modèle.itération := paste(model.name, iteration)]
pred.dt[, modèle.itération := paste(model.name, iteration)]
iteration.contours[, modèle.itération := paste(model.name, iteration)]
loss.tall[, ensemble := ensemble.name]
loss.dt[, ensemble := ensemble.name]
loss.dt[, modèle.itération := paste(model.name, iteration)]
min.dt[, ensemble := ensemble.name]
sim.dt[, label.num := as.numeric(paste(label.fac))]
err.dt[, exactitude := ifelse(label==pred.num, "correcte", "erronée")]
loss.dt[, n.ensemble := ifelse(ensemble==subtrain, n.subtrain, sim.row-n.subtrain)]
loss.dt[, error.count := n.ensemble*`pourcent erroné`/100]
mi.sub <- function(DT)DT[iteration%in%as.integer(seq(1,max.iterations,l=100))]
(mi.levs <- unique(mi.sub(loss.dt)$modèle.itération))
some <- function(DT){
  mi.sub(DT)[, modèle.itération := factor(modèle.itération, mi.levs)][]
}
viz <- animint(
  title="Descente de gradient pour classification avec modèle linéaire et réseau de neurones",
  source="https://github.com/tdhock/2026-08-apprentissage/blob/master/figure-gradient-descent-classification-fr.R",
  loss=ggplot()+
    ggtitle("Erreur, choisir modèle et itération")+
    theme_bw()+
    theme_animint(width=500, height=800)+
    theme(panel.margin=grid::unit(1, "lines"))+
    facet_grid(variable ~ model.name, scales="free")+
    scale_y_log10("")+
    scale_x_continuous(
      "Itération/epoque de la descente de gradient")+
    geom_line(aes(
      iteration, value, color=ensemble, group=ensemble),
      data=loss.tall)+
    geom_point(aes(
      iteration, value, fill=ensemble),
      shape=21,
      color="black",
      data=min.dt)+
    geom_tallrect(aes(
      xmin=iteration-0.5, 
      xmax=iteration+0.5),
      alpha=0.5,
      clickSelects="modèle.itération",
      data=some(loss.tall[ensemble.name==subtrain])),
  data=ggplot()+
    ggtitle("Fonction apprise pour la sélection")+
    theme_bw()+
    theme_animint(width=500, height=800, last_in_row=TRUE)+
    facet_grid(ensemble ~ ., labeller="label_both")+
    geom_tile(aes(
      V1, V2,
      key=tile.i,
      group=tile.i,
      fill=norm.pred), color = NA,
      showSelected="modèle.itération",
      data=some(pred.dt))+
    geom_text(aes(
      0.5, 1.1,
      key=1,
      label=sprintf(
        "logistique=%.4f, %d/%d erreurs=%.0f%%",
        logistique,
        as.integer(error.count),
        as.integer(n.ensemble),
        `pourcent erroné`)),
      showSelected="modèle.itération",
      data=some(loss.dt))+
    geom_path(aes(
      x, y, group=contour.i, key=contour.i),
      showSelected="modèle.itération",
      color="grey50",
      data=some(iteration.contours))+
    geom_point(aes(
      V1, V2,
      key=data.i,
      group=data.i,
      fill=label/2,
      color=exactitude),
      showSelected=c("modèle.itération", "ensemble"),
      size=4,
      data=some(err.dt))+
    scale_fill_gradient2(
      "Classe y, Préd. Prob.",
      low="red")+
    scale_color_manual(
      values=c(
        correcte="white",
        erronée="black"))+
    scale_x_continuous(
      "Entrée x1")+
    scale_y_continuous(
      "Entrée x2")+
    coord_equal(),
  first=list(
    modèle.itération="modèle linéaire 1"),
  time=list(
    variable="modèle.itération",
    ms=500),
  duration=list(
    modèle.itération=500),
  out.dir="figure-gradient-descent-classification-fr"
)
viz
if(FALSE){
  (info <- animint2pages(viz, "2026-09-23-gradient-descent-classification-fr", chromote_sleep_seconds=3))
  system(paste("echo", info$owner_repo, ">> ~/R/gallery-fr/repos.txt"))
  animint2::update_gallery("~/R/gallery-fr")
}

