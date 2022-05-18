domain = ps(
  pruning_mode = p_fct(c("conv0", "conv1", "conv2", "conv1andconv2", "all")),
  pruner_name = p_fct(c("l1", "l2", "slim", "agp", "fpgm", "mean_activation", "apoz", "taylorfo")),
  sparsity = p_dbl(0.4, 0.7),
  agp_pruning_alg = p_fct(c("l1", "l2", "slim", "fpgm", "mean_activation", "apoz", "taylorfo"), depends = pruner_name == "agp"),
  agp_n_iters = p_int(1, 500, depends = pruner_name == "agp"),
  agp_n_epochs_per_iter = p_int(1, 10, depends = pruner_name == "agp"),
  slim_sparsifying_epochs = p_int(1, 30, depends = pruner_name == "slim"),
  speed_up = p_lgl(),
  finetune_epochs = p_int(1, 27, tags = "budget"),
  learning_rate = p_dbl(1e-6, 1e-2),
  weight_decay = p_dbl(0, 0.1),
  kd = p_lgl(),
  alpha = p_dbl(0, 1, depends = kd == TRUE),
  temp = p_dbl(0, 100, depends = kd == TRUE)
)

# Table 8
search_space = ps(
  pruning_mode = p_fct(c("conv0", "conv1", "conv2", "conv1andconv2", "all")),
  pruner_name = p_fct(c("l1", "l2", "slim", "agp", "fpgm", "mean_activation", "apoz", "taylorfo")),
  sparsity = p_dbl(0.4, 0.7),
  agp_pruning_alg = p_fct(c("l1", "l2", "slim", "fpgm", "mean_activation", "apoz", "taylorfo"), depends = pruner_name == "agp"),
  agp_n_iters = p_int(1, 100, depends = pruner_name == "agp"),
  agp_n_epochs_per_iter = p_int(1, 10, depends = pruner_name == "agp"),
  slim_sparsifying_epochs = p_int(1, 30, depends = pruner_name == "slim"),
  speed_up = p_lgl(),
  finetune_epochs = p_int(1, 27, tags = "budget"),
  learning_rate = p_dbl(1e-6, 1e-2, logscale = TRUE, tags = "log"),
  weight_decay = p_dbl(0, 0.1),
  kd = p_lgl(),
  alpha = p_dbl(0, 1, depends = kd == TRUE),
  temp = p_dbl(0, 100, depends = kd == TRUE)
)

