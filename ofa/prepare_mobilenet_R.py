from ofa.model_zoo import ofa_net
from ofa.utils import download_url
from ofa.tutorial import AccuracyPredictor, FLOPsTable
import random
from copy import deepcopy

import yaml
from ofa.utils import download_url

class LatencyEstimator(object):
    def __init__(
        self,
        local_dir="latency_tools",
        url="https://hanlab.mit.edu/files/proxylessNAS/LatencyTools/mobile_trim.yaml",
    ):
        if url.startswith("http"):
            fname = download_url(url, local_dir, overwrite=True)
        else:
            fname = url

        with open(fname, "r") as fp:
            self.lut = yaml.load(fp)

    @staticmethod
    def repr_shape(shape):
        if isinstance(shape, (list, tuple)):
            return "x".join(str(_) for _ in shape)
        elif isinstance(shape, str):
            return shape
        else:
            return TypeError

    def query(
        self,
        l_type: str,
        input_shape,
        output_shape,
        mid=None,
        ks=None,
        stride=None,
        id_skip=None,
        se=None,
        h_swish=None,
    ):
        infos = [
            l_type,
            "input:%s" % self.repr_shape(input_shape),
            "output:%s" % self.repr_shape(output_shape),
        ]

        if l_type in ("expanded_conv",):
            assert None not in (mid, ks, stride, id_skip, se, h_swish)
            infos += [
                "expand:%d" % mid,
                "kernel:%d" % ks,
                "stride:%d" % stride,
                "idskip:%d" % id_skip,
                "se:%d" % se,
                "hs:%d" % h_swish,
            ]
        key = "-".join(infos)
        return self.lut[key]["mean"]

    def predict_network_latency(self, net, image_size=224):
        predicted_latency = 0
        # first conv
        predicted_latency += self.query(
            "Conv",
            [image_size, image_size, 3],
            [(image_size + 1) // 2, (image_size + 1) // 2, net.first_conv.out_channels],
        )
        # blocks
        fsize = (image_size + 1) // 2
        for block in net.blocks:
            mb_conv = block.mobile_inverted_conv
            shortcut = block.shortcut

            if mb_conv is None:
                continue
            if shortcut is None:
                idskip = 0
            else:
                idskip = 1
            out_fz = int((fsize - 1) / mb_conv.stride + 1)
            block_latency = self.query(
                "expanded_conv",
                [fsize, fsize, mb_conv.in_channels],
                [out_fz, out_fz, mb_conv.out_channels],
                mid=mb_conv.depth_conv.conv.in_channels,
                ks=mb_conv.kernel_size,
                stride=mb_conv.stride,
                id_skip=idskip,
                se=1 if mb_conv.use_se else 0,
                h_swish=1 if mb_conv.act_func == "h_swish" else 0,
            )
            predicted_latency += block_latency
            fsize = out_fz
        # final expand layer
        predicted_latency += self.query(
            "Conv_1",
            [fsize, fsize, net.final_expand_layer.in_channels],
            [fsize, fsize, net.final_expand_layer.out_channels],
        )
        # global average pooling
        predicted_latency += self.query(
            "AvgPool2D",
            [fsize, fsize, net.final_expand_layer.out_channels],
            [1, 1, net.final_expand_layer.out_channels],
        )
        # feature mix layer
        predicted_latency += self.query(
            "Conv_2",
            [1, 1, net.feature_mix_layer.in_channels],
            [1, 1, net.feature_mix_layer.out_channels],
        )
        # classifier
        predicted_latency += self.query(
            "Logits", [1, 1, net.classifier.in_features], [net.classifier.out_features]
        )
        return predicted_latency

    def predict_network_latency_given_spec(self, spec):
        image_size = spec["r"][0]
        predicted_latency = 0
        # first conv
        predicted_latency += self.query(
            "Conv",
            [image_size, image_size, 3],
            [(image_size + 1) // 2, (image_size + 1) // 2, 24],
        )
        # blocks
        fsize = (image_size + 1) // 2
        # first block
        predicted_latency += self.query(
            "expanded_conv",
            [fsize, fsize, 24],
            [fsize, fsize, 24],
            mid=24,
            ks=3,
            stride=1,
            id_skip=1,
            se=0,
            h_swish=0,
        )
        in_channel = 24
        stride_stages = [2, 2, 2, 1, 2]
        width_stages = [32, 48, 96, 136, 192]
        act_stages = ["relu", "relu", "h_swish", "h_swish", "h_swish"]
        se_stages = [False, True, False, True, True]
        for i in range(20):
            stage = i // 4
            depth_max = spec["d"][stage]
            depth = i % 4 + 1
            if depth > depth_max:
                continue
            ks, e = spec["ks"][i], spec["e"][i]
            if i % 4 == 0:
                stride = stride_stages[stage]
                idskip = 0
            else:
                stride = 1
                idskip = 1
            out_channel = width_stages[stage]
            out_fz = int((fsize - 1) / stride + 1)

            mid_channel = round(in_channel * e)
            block_latency = self.query(
                "expanded_conv",
                [fsize, fsize, in_channel],
                [out_fz, out_fz, out_channel],
                mid=mid_channel,
                ks=ks,
                stride=stride,
                id_skip=idskip,
                se=1 if se_stages[stage] else 0,
                h_swish=1 if act_stages[stage] == "h_swish" else 0,
            )
            predicted_latency += block_latency
            fsize = out_fz
            in_channel = out_channel
        # final expand layer
        predicted_latency += self.query(
            "Conv_1",
            [fsize, fsize, 192],
            [fsize, fsize, 1152],
        )
        # global average pooling
        predicted_latency += self.query(
            "AvgPool2D",
            [fsize, fsize, 1152],
            [1, 1, 1152],
        )
        # feature mix layer
        predicted_latency += self.query("Conv_2", [1, 1, 1152], [1, 1, 1536])
        # classifier
        predicted_latency += self.query("Logits", [1, 1, 1536], [1000])
        return predicted_latency

class ArchManager:
    def __init__(self):
        self.num_blocks = 20
        self.num_stages = 5
        self.kernel_sizes = [3, 5, 7]
        self.expand_ratios = [3, 4, 6]
        self.depths = [2, 3, 4]
        self.resolutions = [160, 176, 192, 208, 224]

    def random_sample(self):
        sample = {}
        d = []
        e = []
        ks = []
        for i in range(self.num_stages):
            d.append(random.choice(self.depths))

        for i in range(self.num_blocks):
            e.append(random.choice(self.expand_ratios))
            ks.append(random.choice(self.kernel_sizes))

        sample = {
            "wid": None,
            "ks": ks,
            "e": e,
            "d": d,
            "r": [random.choice(self.resolutions)],
        }

        return sample

    def random_resample(self, sample, i):
        assert i >= 0 and i < self.num_blocks
        sample["ks"][i] = random.choice(self.kernel_sizes)
        sample["e"][i] = random.choice(self.expand_ratios)

    def random_resample_depth(self, sample, i):
        assert i >= 0 and i < self.num_stages
        sample["d"][i] = random.choice(self.depths)

    def random_resample_resolution(self, sample):
        sample["r"][0] = random.choice(self.resolutions)

class LatencyTable:
    def __init__(self, device="note10", resolutions=(160, 176, 192, 208, 224)):
        self.latency_tables = {}

        for image_size in resolutions:
            self.latency_tables[image_size] = LatencyEstimator(
                url="latency_tools/%s/%d_lookup_table.yaml" % (device, image_size)
            )

    def predict_efficiency(self, spec: dict):
        return self.latency_tables[spec["r"][0]].predict_network_latency_given_spec(
            spec
        )

arch_encoder = ArchManager()
ofa_network = ofa_net("ofa_mbv3_d234_e346_k357_w1.2", pretrained=True)

acc_predictor = AccuracyPredictor(
    pretrained=True,
    device="cpu"
)

efficiency_predictor = LatencyTable()  # "note10"
flops_predictor = FLOPsTable(device="cpu", batch_size=1)

def arch2feature(arch):
    return acc_predictor.spec2feats(ks_list=arch["ks"], ex_list=arch["e"], d_list=arch["d"], r=arch["r"]).numpy()

def predict_acc(arch):
    arch_ = deepcopy(arch)
    arch_.update({"r":[arch_.get("r")]})
    return acc_predictor.predict_accuracy([arch_]).item() * 100

def predict_eff(arch):
    arch_ = deepcopy(arch)
    arch_.update({"r":[arch_.get("r")]})
    return efficiency_predictor.predict_efficiency(arch_)

def predict_flops(arch):
    arch_ = deepcopy(arch)
    arch_.update({"r":[arch_.get("r")]})
    return flops_predictor.predict_efficiency(arch_)

def model_size(arch, ofa_network):
    ofa_network.set_active_subnet(ks=arch["ks"], e=arch["e"], d=arch["d"])
    model = ofa_network.get_active_subnet(preserve_weight=True)
    param_size = 0
    for param in model.parameters():
        param_size += param.nelement() * param.element_size()
    buffer_size = 0
    for buffer in model.buffers():
        buffer_size += buffer.nelement() * buffer.element_size()

    size_all_mb = (param_size + buffer_size) / 1024**2
    return size_all_mb
