# Adapted from https://github.com/microsoft/nni/tree/master/examples/model_compress
# Copyright (c) Microsoft Corporation.
# Licensed under the MIT license.

import os
import torch
import torch.nn as nn
from torch.utils.data import DataLoader
from tqdm import tqdm
import numpy as np

from utils import *
from utils import create_model, EvalDataset, count_flops


device = torch.device("cuda" if torch.cuda.is_available() else "cpu")

model_type = 'mobilenet_v2_torchhub'    # 'mobilenet_v1' 'mobilenet_v2' 'mobilenet_v2_torchhub'
checkpoint_dir = './pretrained_{}/'.format(model_type)
checkpoint = checkpoint_dir + '/checkpoint_best.pt'    # model checkpoint produced by pretrain.py




def load_model(model_type):
    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    input_size = 224
    n_classes = 120
    model = create_model(model_type=model_type, pretrained=True, n_classes=n_classes,
                         input_size=input_size)
    model = model.to(device)
    experiment_dir = 'pretrained_{}'.format(model_type)
    if os.path.isfile(experiment_dir + '/checkpoint_best.pt'):
        print('Loading checkpoint from {}'.format(experiment_dir + '/checkpoint_best.pt'))
        model.load_state_dict(torch.load(experiment_dir + '/checkpoint_best.pt', map_location = device))
    
    return model 

def test_model(model, device = torch.device("cuda" if torch.cuda.is_available() else "cpu")):
    test_dataset = EvalDataset('./data/stanford-dogs/Processed/test')
    test_dataloader = DataLoader(test_dataset, batch_size=64, shuffle=False)
    model.eval()
    loss_func = nn.CrossEntropyLoss()
    acc_list, loss_list = [], []
    with torch.no_grad():
        for i, (inputs, labels) in enumerate(tqdm(test_dataloader)):
            inputs, labels = inputs.float().to(device), labels.to(device)
            preds= model(inputs)
            pred_idx = preds.max(1).indices
            acc = (pred_idx == labels).sum().item() / labels.size(0)
            acc_list.append(acc)
            loss = loss_func(preds, labels).item()
            loss_list.append(loss)

    final_loss = np.array(loss_list).mean()
    final_acc = np.array(acc_list).mean()
    print('Test loss: {}\nTest accuracy: {}'.format(final_loss, final_acc))

    dummy_input = torch.rand([1, 3, 224, 224]).to(device)
    flops, params, results = count_flops_params(model, dummy_input)
    print(f"FLOPs: {flops}, params: {params}")

    return model_type, final_loss, final_acc, flops, params


def run_test(model_type):
    model = load_model(model_type)
    return test_model(model)

if __name__ == '__main__':
    run_test('resnet18')
    
