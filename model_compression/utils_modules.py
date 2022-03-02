def get_config_list_mobilenet(args):
    config_list = []
    if 'conv0' in args.pruning_mode or args.pruning_mode == 'all':
        if args.pruner_name == 'slim' or (args.pruner_name == 'agp' and args.agp_pruning_alg == 'slim'):
            config_list.append({
                'op_names': ['features.{}.conv.0.1'.format(x) for x in range(2, 18)],
                'sparsity': args.sparsity
            })
        else:
            config_list.append({
                'op_names': ['features.{}.conv.0.0'.format(x) for x in range(2, 18)],
                'sparsity': args.sparsity
            })
    if 'conv1' in args.pruning_mode or args.pruning_mode == 'all':
        if args.pruner_name == 'slim' or (args.pruner_name == 'agp' and args.agp_pruning_alg == 'slim'):
            config_list.append({
                'op_names': ['features.{}.conv.1.1'.format(x) for x in range(2, 18)],
                'sparsity': args.sparsity
            })
        else:
            config_list.append({
                'op_names': ['features.{}.conv.1.0'.format(x) for x in range(2, 18)],
                'sparsity': args.sparsity
            })
    if 'conv2' in args.pruning_mode or args.pruning_mode == 'all':
        if args.pruner_name == 'slim' or (args.pruner_name == 'agp' and args.agp_pruning_alg == 'slim'):
            config_list.append({
                'op_names': ['features.{}.conv.3'.format(x) for x in range(2, 18)],
                'sparsity': args.sparsity
            })
        else:
            config_list.append({
                'op_names': ['features.{}.conv.2'.format(x) for x in range(2, 18)],
                'sparsity': args.sparsity
            })
    return config_list


def get_config_list_default(args):
    if args.pruner_name == 'level' or (args.pruner_name == 'agp' and args.agp_pruning_alg == 'level'):
        op_types = ['default']
    elif args.pruner_name == 'slim' or (args.pruner_name == 'agp' and args.agp_pruning_alg == 'slim'):
        op_types = ['BatchNorm2d']
    else:
        op_types = ['Conv2d']
    config_list = [{
        'sparsity': args.sparsity,
        'op_types': op_types
    }] 
    return config_list
