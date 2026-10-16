fun.list <- list(
  constante=function(x)1,
  linéaire=function(x)x,
  quadratique=function(x)x*x,
  escalier=function(x)ifelse(x<1, 4, 1))
N <- 100
set.seed(2)
min.x <- -2
max.x <- 3
input.vec <- runif(N, min.x, max.x)
library(data.table)
subtrain <- "sous-entraînement"
in.out.dt <- data.table(tendance=names(fun.list))[, {
  f <- fun.list[[tendance]]
  true.vec <- f(input.vec)
  list(
    input=input.vec,
    true=true.vec,
    output=true.vec+rnorm(N),
    ensemble=rep(c(subtrain, "validation"),l=length(input.vec)))
}, by=list(tendance)]

voisins.dt <- data.table(voisins=1:(N/2))
pred.dt <- voisins.dt[, {
  in.out.dt[, {
    is.subtrain <- ensemble==subtrain
    in.mat <- cbind(input[is.subtrain])
    fit <- caret::knnreg(in.mat, output[is.subtrain], k=voisins)
    data.table(
      voisins,
      input,
      output,
      pred=predict(fit, input),
      ensemble)
  }, by=list(tendance)]
}, by=list(voisins)]

grid.dt <- voisins.dt[, {
  in.out.dt[, {
    is.subtrain <- ensemble==subtrain
    in.mat <- cbind(input[is.subtrain])
    fit <- caret::knnreg(in.mat, output[is.subtrain], k=voisins)
    x <- seq(min.x, max.x, l=200)
    f <- fun.list[[tendance]]
    data.table(
      input=x,
      idéal=f(x),
      prédiction=predict(fit, x))
  }, by=list(tendance)]
}, by=list(voisins)]

library(animint2)
rss.dt <- pred.dt[, {
  res.vec <- pred-output
  rss <- sum(res.vec*res.vec)
  mse <- rss/length(res.vec)
  data.table(RMSE=sqrt(mse))
  }, by=list(voisins, tendance, ensemble)]
min.dt <- rss.dt[
, .SD[which.min(RMSE)]
, by=.(ensemble, tendance)]
ggplot()+
  facet_grid(. ~ tendance)+
  geom_line(aes(
    voisins, RMSE, color=ensemble),
    data=rss.dt)+
  geom_point(aes(
    voisins, RMSE, color=ensemble),
    fill="white",
    shape=21,
    data=min.dt)+
  scale_x_continuous()

pred.colors <- c(prédiction="red", idéal="blue")
pred.dt[, fonction := "prédiction"]
grid.tall <- melt(
  grid.dt,
  measure.vars=names(pred.colors),
  variable.name="fonction")
in.out.dt[, fonction := "idéal"]
ensemble.colors <- rowwiseDT(
  ensemble=, color=,
  subtrain, "grey50",#NOT QUOTED
  "validation", "black"
)[, setNames(color, ensemble)]

(viz <- animint(
  title="Sous-entraînement et validation pour régression avec plus proches voisins",
  funs=ggplot()+
    ggtitle("Données et prédictions")+
    theme_bw()+
    theme_animint(width=1000, last_in_row=TRUE)+
    scale_fill_manual(values=ensemble.colors)+
    scale_color_manual(values=pred.colors)+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(ensemble ~ tendance)+
    ylab("y = sortie")+
    xlab("x = entrée")+
    geom_point(aes(
      input, output, fill=ensemble),
      size=4,
      data=in.out.dt)+
    geom_line(aes(
      input, value, color=fonction),
      data=grid.tall[fonction=="idéal" & voisins==1])+
    geom_line(aes(
      input, value,
      key="pred",
      color=fonction),
      showSelected="voisins",
      data=grid.tall[fonction=="prédiction"]),
  select=ggplot()+
    ggtitle("Erreurs sur sous-entraînement et validation, choisir nombre de voisins")+
    theme_bw()+
    theme_animint(width=1000, height=300)+
    facet_grid(. ~ tendance)+
    geom_line(aes(
      voisins, RMSE, color=ensemble, group=ensemble),
      data=rss.dt)+
    geom_point(aes(
      voisins, RMSE, color=ensemble, fill=erreur),
      data=data.table(erreur="min", min.dt))+    
    geom_text(aes(
      N/2, 0.3,
      key="voisins",
      label=sprintf(
        "%d plus proche%s voisin%s",
        voisins, s, s)),
      showSelected=c("fonction","voisins"),
      color=pred.colors[["prédiction"]],
      hjust=1,
      data=data.table(fonction="prédiction", voisins.dt)[, s := ifelse(voisins==1, "", "s")])+
    geom_text(aes(
      N/2, ifelse(ensemble==subtrain, 0.15, 0),
      color=ensemble,
      key=ensemble,
      label=sprintf(
        "Erreur %s : %.4f", ensemble, RMSE)),
      showSelected="voisins",
      hjust=1,
      data=rss.dt)+
    ylab("Racine de l’erreur carrée moyenne")+
    geom_tallrect(aes(
      xmin=voisins-0.5,
      xmax=voisins+0.5),
      alpha=0.5,
      clickSelects="voisins",
      data=voisins.dt)+
    scale_x_continuous(
      breaks=c(1, seq(10, 50, by=10)))+
    coord_cartesian(ylim=c(0,2))+
    scale_fill_manual(values=c(min="white"))+
    scale_color_manual(values=ensemble.colors),
  out.dir="figure-sur-sous-apprentissage-validation",
  duration=list(voisins=1000),
  source="https://github.com/tdhock/2026-08-apprentissage/blob/master/figure-sur-sous-apprentissage-validation.R"))
if(FALSE){
  animint2pages(viz, "2026-09-08-sur-sous-apprentissage-validation", chromote_sleep_seconds=3)
  animint2::update_gallery("~/R/gallery-fr")
}
