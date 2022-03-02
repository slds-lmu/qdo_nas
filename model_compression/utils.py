# Copyright (c) Microsoft Corporation.
# Licensed under the MIT license.

import os
import sys
from dataclasses import dataclass
import torch
from torch.utils.data import Dataset, DataLoader
import torchvision.transforms as transforms
import numpy as np
from nni.compression.pytorch.utils.counter import count_flops_params

from pathlib import Path
sys.path.append(str(Path(__file__).absolute().parents[0] / 'models'))
from mobilenet import MobileNet
from mobilenet_v2 import MobileNetV2


def create_model(model_type=None, n_classes=120, input_size=224, checkpoint=None, pretrained=False, width_mult=1.):
    if model_type == 'mobilenet_v1':
        model = MobileNet(n_class=n_classes, profile='normal')
    elif model_type == 'mobilenet_v2':
        model = MobileNetV2(n_class=n_classes, input_size=input_size, width_mult=width_mult)
    elif model_type == 'mobilenet_v2_torchhub':
        model = torch.hub.load('pytorch/vision:v0.8.1', 'mobilenet_v2', pretrained=pretrained)
        # model = torch.hub.load('pytorch/vision:v0.10.0', 'mobilenet_v2', pretrained=pretrained)
        feature_size = model.classifier[1].weight.data.size()[1]
        replace_classifier = torch.nn.Linear(feature_size, n_classes)
        model.classifier[1] = replace_classifier
    elif model_type == 'resnet18':
        model = torch.hub.load('pytorch/vision:v0.8.1', 'resnet18', pretrained=pretrained)
        feature_size = model.fc.weight.data.size()[1]
        replace_classifier = torch.nn.Linear(feature_size, n_classes)
        model.fc = replace_classifier
    elif model_type == 'efficientnet_b0':
        model = torch.hub.load('NVIDIA/DeepLearningExamples:torchhub', 'nvidia_efficientnet_b0', pretrained=pretrained)
        feature_size = model.classifier.fc.weight.data.size()[1]
        replace_classifier = torch.nn.Linear(feature_size, n_classes)
        model.classifier.fc  = replace_classifier
    elif model_type == 'efficientnet_se_b0':
        model = torch.hub.load('NVIDIA/DeepLearningExamples:torchhub', 'nvidia_efficientnet_widese_b0', pretrained=pretrained)
        feature_size = model.classifier.fc.weight.data.size()[1]
        replace_classifier = torch.nn.Linear(feature_size, n_classes)
        model.classifier.fc = replace_classifier  
    elif model_type is None:
        model = None
    else:
        raise RuntimeError('Unknown model_type.')

    if checkpoint is not None:
        model.load_state_dict(torch.load(checkpoint, map_location = torch.device("cuda" if torch.cuda.is_available() else "cpu")))

    return model


def get_dataloader(dataset_type, data_path, batch_size=32, shuffle=True):
    assert dataset_type in ['train', 'eval']
    if dataset_type == 'train':
        ds = TrainDataset(data_path)
    else:
        ds = EvalDataset(data_path)
    return DataLoader(ds, batch_size, shuffle=shuffle)


class TrainDataset(Dataset):
    def __init__(self, npy_dir):
        self.root_dir = npy_dir
        self.case_names = [self.root_dir + '/' + x for x in os.listdir(self.root_dir)]
        
        transform_set = [transforms.Lambda(lambda x: x),
                         transforms.RandomRotation(30),
                         transforms.ColorJitter(),
                         transforms.RandomHorizontalFlip(p=1)]
        self.transform = transforms.RandomChoice(transform_set)
        
    def __len__(self):
        return len(self.case_names)

    def __getitem__(self, index):
        instance = np.load(self.case_names[index], allow_pickle=True).item()
        x = instance['input'].transpose(2, 0, 1)     # (C, H, W)
        x = torch.from_numpy(x).type(torch.float)    # convert to Tensor to use torchvision.transforms
        x = self.transform(x)
        return x, instance['label']


class EvalDataset(Dataset):
    def __init__(self, npy_dir):
        self.root_dir = npy_dir
        self.case_names = [self.root_dir + '/' + x for x in os.listdir(self.root_dir)]

    def __len__(self):
        return len(self.case_names)

    def __getitem__(self, index):
        instance = np.load(self.case_names[index], allow_pickle=True).item()
        x = instance['input'].transpose(2, 0, 1)
        x = torch.from_numpy(x).type(torch.float)
        return x, instance['label']


def count_flops(model, log=None, device=None):
    dummy_input = torch.rand([1, 3, 256, 256])
    if device is not None:
        dummy_input = dummy_input.to(device)
    flops, params, results = count_flops_params(model, dummy_input)
    print(f"FLOPs: {flops}, params: {params}")
    if log is not None:
        log.write(f"FLOPs: {flops}, params: {params}\n")
    return flops, params


def str2bool(value):
    if value.lower() == 'true':
        return True
    elif value.lower() == 'false':
        return False
    else:
        return value

def parse_bool(dct):
    for k, v in dct.items():
        if isinstance(v, str):
            dct[k] = str2bool(v)
    return dct


@dataclass
class Args:
    checkpoint_name: str = 'checkpoint_best.pt'
    model_type: str = 'mobilenet_v2_torchhub'
    pruning_mode: str = 'conv1andconv2'
    sparsity: str = 0.5
    pruner_name: str = 'l1'
    agp_pruning_alg: str = 'l1'
    agp_n_iters: int = 64
    agp_n_epochs_per_iter: int = 1
    speed_up: bool = False
    n_workers: int = 16
    finetune_epochs: int = 180
    learning_rate: float = 1e-4
    weight_decay: float = 0.0
    batch_size: int = 32
    kd: bool = False
    alpha: float = 0.99
    temp: float = 8.
    slim_sparsifying_epochs:int = 10
    experiment_dir: str = 'experiments/'
