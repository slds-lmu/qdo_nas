from naszilla.meta_neural_net import MetaNeuralnet
from naszilla.params import meta_neuralnet_params

metann_params = meta_neuralnet_params("standard")
num_ensemble = 5

def train_ensemble(xtrain, ytrain):
    train_error = 0
    ensemble = []

    for e in range(num_ensemble):
        meta_neuralnet = MetaNeuralnet()
        net_params = metann_params['ensemble_params'][e]
        train_error += meta_neuralnet.fit(xtrain, ytrain, **net_params)
        ensemble.append(meta_neuralnet)

    train_error /= num_ensemble
    return ensemble

