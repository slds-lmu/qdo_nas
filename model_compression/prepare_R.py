import os
import sys
import xml
from pathlib import Path
from dataclasses import dataclass
from PIL import Image
import matplotlib.pyplot as plt

import torch
import torch.nn as nn
import torch.nn.functional as F
import numpy as np

sys.path.insert(1, os.path.join(os.getcwd(), ""))

from nni.compression.pytorch import ModelSpeedup
from nni.compression.pytorch.utils.counter import count_flops_params

from utils import create_model, get_dataloader, parse_bool, Args
from pruning_experiments import run_pruning

import gc

device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")

num_workers = 16
torch.set_num_threads(num_workers)

def run_pruning_from_R(cfg):
    args = Args(**cfg)
    args.n_workers = 16
    test_loss, test_acc, val_loss, val_acc, flops, params, init_dict = run_pruning(args)
    torch.cuda.empty_cache()
    gc.collect()
    return [test_loss, test_acc, val_loss, val_acc, flops, params]

