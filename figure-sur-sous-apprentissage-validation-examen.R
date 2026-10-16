library(data.table)
library(ggplot2)
fun.list <- list(
  constante=function(x)1,
  linéaire=function(x)x,
  quadratique=function(x)x*x,
  escalier=function(x)ifelse(x<1, 4, 1))
N <- 100
set.seed(2)
set.seed(7)
min.x <- -2
max.x <- 3
input.vec <- runif(N, min.x, max.x)
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

rss.dt <- pred.dt[, {
  res.vec <- pred-output
  rss <- sum(res.vec*res.vec)
  mse <- rss/length(res.vec)
  data.table(RMSE=sqrt(mse))
  }, by=list(voisins, tendance, ensemble)]
min.dt <- rss.dt[
, .SD[which.min(RMSE)]
, by=.(ensemble, tendance)]
pred.colors <- c(prédiction="red", idéal="blue")
pred.dt[, fonction := "prédiction"]
grid.tall <- melt(
  grid.dt,
  measure.vars=names(pred.colors),
  variable.name="fonction")
in.out.dt[, fonction := "idéal"]
ensemble.colors <- rowwiseDT(
  ensemble=, color=,
  subtrain, "red",#NOT QUOTED
  "validation", "black"
)[, setNames(color, ensemble)]
rss.dt[, données := as.integer(factor(tendance))]
gg <- ggplot()+
  theme_bw()+
  #theme(legend.position="left")+
  theme(legend.position="none")+
  facet_grid(. ~ données, labeller=label_both)+
  geom_line(aes(
    voisins, RMSE,
    linetype=ensemble,
    color=ensemble, group=ensemble),
    data=rss.dt)+
  directlabels::geom_dl(aes(
    voisins, RMSE,
    label=sub("-", "\n", ensemble),
    color=ensemble, group=ensemble),
    method=list(cex=0.7, "first.qp", directlabels::dl.add(x=-0.1)),
    data=rss.dt)+
  ylab("Racine de l’erreur carrée moyenne")+
  scale_x_continuous(
    limits=c(-20, 50),
    breaks=c(1, seq(10, 50, by=10)))+
  coord_cartesian(ylim=c(-0.1,2))+
  scale_color_manual(values=ensemble.colors)
png("figure-sur-sous-apprentissage-validation-examen.png", width=10, height=3, units="in", res=200)
print(gg)
dev.off()
