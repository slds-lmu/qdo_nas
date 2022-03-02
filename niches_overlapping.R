# nb101
# num_params
#import nasbench.api
#import numpy as np
#ss = nasbench.api.NASBench("naszilla/nas_benchmark_datasets/nasbench_only108.tfrecord")
#num_params = []
#hashes = ss.hash_iterator()
#for hash in hashes:
#  tmp = ss.get_metrics_from_hash(hash)
#  num_params.append(tmp[0]["trainable_parameters"])
#
#np.quantile(num_params, 0.5)
#np.quantile(num_params, [0.01, 0.05, 0.1, 0.3])
#np.quantile(num_params, [0.01, 0.02, 0.5, 0.1, 0.2, 0.3, 0.4, 0.5, 0.7])
#np.max(num_params) # 49979274
nb101_small_n1 = NicheBoundaries$new("niche1", niche_boundaries = list(num_params = c(0, 5356682)))
nb101_small_n2 = NicheBoundaries$new("niche2", niche_boundaries = list(num_params = c(0, Inf)))
nb101_small_nb = NichesBoundaries$new("nb101_small", niches_boundaries = list(niche1 = nb101_small_n1, niche2 = nb101_small_n2))

nb101_medium_n1 = NicheBoundaries$new("niche1", niche_boundaries = list(num_params = c(0, 650520)))
nb101_medium_n2 = NicheBoundaries$new("niche2", niche_boundaries = list(num_params = c(0, 1227914)))
nb101_medium_n3 = NicheBoundaries$new("niche3", niche_boundaries = list(num_params = c(0, 1664778)))
nb101_medium_n4 = NicheBoundaries$new("niche4", niche_boundaries = list(num_params = c(0, 3468426)))
nb101_medium_n5 = NicheBoundaries$new("niche5", niche_boundaries = list(num_params = c(0, Inf)))
nb101_medium_nb = NichesBoundaries$new("nb101_medium", niches_boundaries = list(niche1 = nb101_medium_n1, niche2 = nb101_medium_n2, niche3 = nb101_medium_n3, niche4 = nb101_medium_n4, niche5 = nb101_medium_n5))

nb101_large_n1 = NicheBoundaries$new("niche1", niche_boundaries = list(num_params = c(0, 650520)))
nb101_large_n2 = NicheBoundaries$new("niche2", niche_boundaries = list(num_params = c(0, 824848)))
nb101_large_n3 = NicheBoundaries$new("niche3", niche_boundaries = list(num_params = c(0, 1227914)))
nb101_large_n4 = NicheBoundaries$new("niche4", niche_boundaries = list(num_params = c(0, 1664778)))
nb101_large_n5 = NicheBoundaries$new("niche5", niche_boundaries = list(num_params = c(0, 2538506)))
nb101_large_n6 = NicheBoundaries$new("niche6", niche_boundaries = list(num_params = c(0, 3468426)))
nb101_large_n7 = NicheBoundaries$new("niche7", niche_boundaries = list(num_params = c(0, 3989898)))
nb101_large_n8 = NicheBoundaries$new("niche8", niche_boundaries = list(num_params = c(0, 5356682)))
nb101_large_n9 = NicheBoundaries$new("niche9", niche_boundaries = list(num_params = c(0, 8118666)))
nb101_large_n10 = NicheBoundaries$new("niche10", niche_boundaries = list(num_params = c(0, Inf)))
nb101_large_nb = NichesBoundaries$new("nb101_large", niches_boundaries = list(niche1 = nb101_large_n1, niche2 = nb101_large_n2, niche3 = nb101_large_n3, niche4 = nb101_large_n4, niche5 = nb101_large_n5, niche6 = nb101_large_n6, niche7 = nb101_large_n7, niche8 = nb101_large_n8, niche9 = nb101_large_n9, niche10 = nb101_large_n10))

# nb201
# latency
#from nas_201_api import NASBench201API as API
#import numpy as np
#api = API("naszilla/nas_benchmark_datasets/NAS-Bench-201-v1_0-e61699.pth")
#num = len(api)
#latency_cifar10 = []
#latency_cifar100 = []
#latency_imagenet = []
#for i, arch_str in enumerate(api):
#  info = api.query_meta_info_by_index(i, hp="200")
#  latency_cifar10.append(info.get_latency("cifar10-valid"))
#  latency_cifar100.append(info.get_latency("cifar100"))
#  latency_imagenet.append(info.get_latency("ImageNet16-120"))
#
#np.quantile(latency_cifar10, 0.5)
#np.quantile(latency_cifar10, [0.01, 0.05, 0.1, 0.3])
#np.quantile(latency_cifar10, [0.01, 0.02, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.7])
#np.max(latency_cifar10) # 0.02722082639995374
#
#np.quantile(latency_cifar100, 0.5)
#np.quantile(latency_cifar100, [0.01, 0.05, 0.1, 0.3])
#np.quantile(latency_cifar100, [0.01, 0.02, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.7]) 
#np.max(latency_cifar100) # 0.02614096800486247
#
#np.quantile(latency_imagenet, 0.5)
#np.quantile(latency_imagenet, [0.01, 0.05, 0.1, 0.3])
#np.quantile(latency_imagenet, [0.01, 0.02, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.7]) 
#np.max(latency_imagenet) # 0.02822377681732178

nb201_cifar10_small_n1 = NicheBoundaries$new("niche1", niche_boundaries = list(latency = c(0, 0.01596731888620477)))
nb201_cifar10_small_n2 = NicheBoundaries$new("niche2", niche_boundaries = list(latency = c(0, Inf)))
nb201_cifar10_small_nb = NichesBoundaries$new("nb201_small", niches_boundaries = list(niche1 = nb201_cifar10_small_n1, niche2 = nb201_cifar10_small_n2))

nb201_cifar10_medium_n1 = NicheBoundaries$new("niche1", niche_boundaries = list(latency = c(0, 0.00919228)))
nb201_cifar10_medium_n2 = NicheBoundaries$new("niche2", niche_boundaries = list(latency = c(0, 0.01138714)))
nb201_cifar10_medium_n3 = NicheBoundaries$new("niche3", niche_boundaries = list(latency = c(0, 0.01232998)))
nb201_cifar10_medium_n4 = NicheBoundaries$new("niche4", niche_boundaries = list(latency = c(0, 0.01475572)))
nb201_cifar10_medium_n5 = NicheBoundaries$new("niche5", niche_boundaries = list(latency = c(0, Inf)))
nb201_cifar10_medium_nb = NichesBoundaries$new("nb201_medium", niches_boundaries = list(niche1 = nb201_cifar10_medium_n1, niche2 = nb201_cifar10_medium_n2, niche3 = nb201_cifar10_medium_n3, niche4 = nb201_cifar10_medium_n4, niche5 = nb201_cifar10_medium_n5))

nb201_cifar10_large_n1 = NicheBoundaries$new("niche1", niche_boundaries = list(latency = c(0, 0.00919228)))
nb201_cifar10_large_n2 = NicheBoundaries$new("niche2", niche_boundaries = list(latency = c(0, 0.00957457)))
nb201_cifar10_large_n3 = NicheBoundaries$new("niche3", niche_boundaries = list(latency = c(0, 0.01138714)))
nb201_cifar10_large_n4 = NicheBoundaries$new("niche4", niche_boundaries = list(latency = c(0, 0.01232998)))
nb201_cifar10_large_n5 = NicheBoundaries$new("niche5", niche_boundaries = list(latency = c(0, 0.01327515)))
nb201_cifar10_large_n6 = NicheBoundaries$new("niche6", niche_boundaries = list(latency = c(0, 0.01475572)))
nb201_cifar10_large_n7 = NicheBoundaries$new("niche7", niche_boundaries = list(latency = c(0, 0.01534633)))
nb201_cifar10_large_n8 = NicheBoundaries$new("niche8", niche_boundaries = list(latency = c(0, 0.01596732)))
nb201_cifar10_large_n9 = NicheBoundaries$new("niche9", niche_boundaries = list(latency = c(0, 0.01768237)))
nb201_cifar10_large_n10 = NicheBoundaries$new("niche10", niche_boundaries = list(latency = c(0, Inf)))
nb201_cifar10_large_nb = NichesBoundaries$new("nb201_large", niches_boundaries = list(niche1 = nb201_cifar10_large_n1, niche2 = nb201_cifar10_large_n2, niche3 = nb201_cifar10_large_n3, niche4 = nb201_cifar10_large_n4, niche5 = nb201_cifar10_large_n5, niche6 = nb201_cifar10_large_n6, niche7 = nb201_cifar10_large_n7, niche8 = nb201_cifar10_large_n8, niche9 = nb201_cifar10_large_n9, niche10 = nb201_cifar10_large_n10))

nb201_cifar100_small_n1 = NicheBoundaries$new("niche1", niche_boundaries = list(latency = c(0, 0.015000444871407967)))
nb201_cifar100_small_n2 = NicheBoundaries$new("niche2", niche_boundaries = list(latency = c(0, Inf)))
nb201_cifar100_small_nb = NichesBoundaries$new("nb201_small", niches_boundaries = list(niche1 = nb201_cifar100_small_n1, niche2 = nb201_cifar100_small_n2))

nb201_cifar100_medium_n1 = NicheBoundaries$new("niche1", niche_boundaries = list(latency = c(0, 0.00856115)))
nb201_cifar100_medium_n2 = NicheBoundaries$new("niche2", niche_boundaries = list(latency = c(0, 0.01030767)))
nb201_cifar100_medium_n3 = NicheBoundaries$new("niche3", niche_boundaries = list(latency = c(0, 0.01143533)))
nb201_cifar100_medium_n4 = NicheBoundaries$new("niche4", niche_boundaries = list(latency = c(0, 0.01363741)))
nb201_cifar100_medium_n5 = NicheBoundaries$new("niche5", niche_boundaries = list(latency = c(0, Inf)))
nb201_cifar100_medium_nb = NichesBoundaries$new("nb201_medium", niches_boundaries = list(niche1 = nb201_cifar100_medium_n1, niche2 = nb201_cifar100_medium_n2, niche3 = nb201_cifar100_medium_n3, niche4 = nb201_cifar100_medium_n4, niche5 = nb201_cifar100_medium_n5))

nb201_cifar100_large_n1 = NicheBoundaries$new("niche1", niche_boundaries = list(latency = c(0, 0.00856115)))
nb201_cifar100_large_n2 = NicheBoundaries$new("niche2", niche_boundaries = list(latency = c(0, 0.00893427)))
nb201_cifar100_large_n3 = NicheBoundaries$new("niche3", niche_boundaries = list(latency = c(0, 0.01030767)))
nb201_cifar100_large_n4 = NicheBoundaries$new("niche4", niche_boundaries = list(latency = c(0, 0.01143533)))
nb201_cifar100_large_n5 = NicheBoundaries$new("niche5", niche_boundaries = list(latency = c(0, 0.01250159)))
nb201_cifar100_large_n6 = NicheBoundaries$new("niche6", niche_boundaries = list(latency = c(0, 0.01363741)))
nb201_cifar100_large_n7 = NicheBoundaries$new("niche7", niche_boundaries = list(latency = c(0, 0.01429903)))
nb201_cifar100_large_n8 = NicheBoundaries$new("niche8", niche_boundaries = list(latency = c(0, 0.01500044)))
nb201_cifar100_large_n9 = NicheBoundaries$new("niche9", niche_boundaries = list(latency = c(0, 0.01660615)))
nb201_cifar100_large_n10 = NicheBoundaries$new("niche10", niche_boundaries = list(latency = c(0, Inf)))
nb201_cifar100_large_nb = NichesBoundaries$new("nb201_large", niches_boundaries = list(niche1 = nb201_cifar100_large_n1, niche2 = nb201_cifar100_large_n2, niche3 = nb201_cifar100_large_n3, niche4 = nb201_cifar100_large_n4, niche5 = nb201_cifar100_large_n5, niche6 = nb201_cifar100_large_n6, niche7 = nb201_cifar100_large_n7, niche8 = nb201_cifar100_large_n8, niche9 = nb201_cifar100_large_n9, niche10 = nb201_cifar100_large_n10))

nb201_imagenet_small_n1 = NicheBoundaries$new("niche1", niche_boundaries = list(latency = c(0, 0.014301609992980958)))
nb201_imagenet_small_n2 = NicheBoundaries$new("niche2", niche_boundaries = list(latency = c(0, Inf)))
nb201_imagenet_small_nb = NichesBoundaries$new("nb201_small", niches_boundaries = list(niche1 = nb201_imagenet_small_n1, niche2 = nb201_imagenet_small_n2))

nb201_imagenet_medium_n1 = NicheBoundaries$new("niche1", niche_boundaries = list(latency = c(0, 0.00767465)))
nb201_imagenet_medium_n2 = NicheBoundaries$new("niche2", niche_boundaries = list(latency = c(0, 0.0094483)))
nb201_imagenet_medium_n3 = NicheBoundaries$new("niche3", niche_boundaries = list(latency = c(0, 0.01054566)))
nb201_imagenet_medium_n4 = NicheBoundaries$new("niche4", niche_boundaries = list(latency = c(0, 0.01271056)))
nb201_imagenet_medium_n5 = NicheBoundaries$new("niche5", niche_boundaries = list(latency = c(0, Inf)))
nb201_imagenet_medium_nb = NichesBoundaries$new("nb201_medium", niches_boundaries = list(niche1 = nb201_imagenet_medium_n1, niche2 = nb201_imagenet_medium_n2, niche3 = nb201_imagenet_medium_n3, niche4 = nb201_imagenet_medium_n4, niche5 = nb201_imagenet_medium_n5))

nb201_imagenet_large_n1 = NicheBoundaries$new("niche1", niche_boundaries = list(latency = c(0, 0.00767465)))
nb201_imagenet_large_n2 = NicheBoundaries$new("niche2", niche_boundaries = list(latency = c(0, 0.00826192)))
nb201_imagenet_large_n3 = NicheBoundaries$new("niche3", niche_boundaries = list(latency = c(0, 0.0094483)))
nb201_imagenet_large_n4 = NicheBoundaries$new("niche4", niche_boundaries = list(latency = c(0, 0.01054566)))
nb201_imagenet_large_n5 = NicheBoundaries$new("niche5", niche_boundaries = list(latency = c(0, 0.01173623)))
nb201_imagenet_large_n6 = NicheBoundaries$new("niche6", niche_boundaries = list(latency = c(0, 0.01271056)))
nb201_imagenet_large_n7 = NicheBoundaries$new("niche7", niche_boundaries = list(latency = c(0, 0.01352221)))
nb201_imagenet_large_n8 = NicheBoundaries$new("niche8", niche_boundaries = list(latency = c(0, 0.01430161)))
nb201_imagenet_large_n9 = NicheBoundaries$new("niche9", niche_boundaries = list(latency = c(0, 0.01595311)))
nb201_imagenet_large_n10 = NicheBoundaries$new("niche10", niche_boundaries = list(latency = c(0, Inf)))
nb201_imagenet_large_nb = NichesBoundaries$new("nb201_large", niches_boundaries = list(niche1 = nb201_imagenet_large_n1, niche2 = nb201_imagenet_large_n2, niche3 = nb201_imagenet_large_n3, niche4 = nb201_imagenet_large_n4, niche5 = nb201_imagenet_large_n5, niche6 = nb201_imagenet_large_n6, niche7 = nb201_imagenet_large_n7, niche8 = nb201_imagenet_large_n8, niche9 = nb201_imagenet_large_n9, niche10 = nb201_imagenet_large_n10))

