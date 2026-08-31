import numpy as np
import pandas as pd
zip_df = pd.read_csv("~/teaching/2023-08-deep-learning/data/zip.test.gz",sep=" ")
zip_X = zip_df.iloc[:,1:].to_numpy()
nrow_total, ncol_total = zip_X.shape
zip_y = zip_df.iloc[:,0].to_numpy()
num_classes = 10
weight_mat = np.random.randn(ncol_total, num_classes)
weight_mat.shape
class Initial:
    def __init__(self, value):
        self.valeur = value
    def backward(self):
        pass
weight_node = Initial(weight_mat)
input_node = Initial(zip_X)
weight_node.valeur
input_node.valeur
class Node:
    def __init__(self,**kwargs):
        self.parents=kwargs
        self.valeur = self.prop_avant()
    def backward(self):
        self.gradient()#affecter grad dans les parents
        for parent in self.parents.values():
            parent.backward()

class Linear(Node):
    def prop_avant(self):
        return np.matmul(
            self.parents["features"].valeur,
            self.parents["weights"].valeur)
    def gradient(self):
        self.parents["features"].grad = np.matmul(
            self.grad,
            self.parents["weights"].valeur.T)
        self.parents["weights"].grad = np.matmul(
            self.parents["features"].valeur.T,
            self.grad)

class CrossEntropyLoss(Node):
    def prop_avant(self):
        self.A_mat = self.parents["predictions"].valeur
        nrow_batch, ncol_batch = self.A_mat.shape
        self.y_vec = self.parents["cibles"].valeur
        M_vec = self.A_mat.max(axis=1)
        exp_mat = np.exp(self.A_mat - M_vec.reshape(nrow_batch, 1))
        log_vec = np.log(exp_mat.sum(axis=1))
        A_vec = self.A_mat[np.arange(nrow_batch), self.y_vec]
        self.L_vec = M_vec + log_vec
        J_vec = self.L_vec-A_vec
        return J_vec.mean()
    def gradient(self):
        nrow_batch, ncol_batch = self.A_mat.shape
        prob_mat = np.exp(
            perte_node.A_mat-perte_node.L_vec.reshape(nrow_batch,1))
        I_mat = np.zeros((nrow_batch,num_classes))
        I_mat[np.arange(nrow_batch), self.y_vec] = 1
        self.parents['predictions'].grad = (prob_mat-I_mat)/nrow_batch
        
weight_node.grad
taux_apprentissage = 0.001
batch_size = 10
n_iterations = math.ceil(nrow_total / batch_size)
for iteration in range(n_iterations):
    first = iteration*batch_size
    after = first+batch_size
    batch_node = Initial(zip_X[first:after,])
    y_node = Initial(zip_y[first:after])
    A_node = Linear(features=batch_node, weights=weight_node)
    perte_node = CrossEntropyLoss(predictions=A_node, cibles=y_node)
    print(perte_node.valeur)
    perte_node.backward()
    weight_node.valeur -= weight_node.grad * taux_apprentissage
