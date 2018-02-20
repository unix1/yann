{application, 'yann', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['yann_app','yann_map_server','yann_map_sup','yann_neuron','yann_neuron_sup','yann_sup']},
	{registered, [yann_sup]},
	{applications, [kernel,stdlib]},
	{mod, {yann_app, []}},
	{env, []}
]}.