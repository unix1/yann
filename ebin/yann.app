{application, 'yann', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['yann','yann_app','yann_layout','yann_layout_server','yann_layout_sup','yann_neuron','yann_neuron_sup','yann_sup','yann_util']},
	{registered, [yann_sup]},
	{applications, [kernel,stdlib]},
	{mod, {yann_app, []}},
	{env, []}
]}.