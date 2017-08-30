{application, 'yann', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['yann_app','yann_server','yann_sup']},
	{registered, [yann_sup]},
	{applications, [kernel,stdlib]},
	{mod, {yann_app, []}},
	{env, []}
]}.