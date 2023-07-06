{application, 'pgsqlp', [
	{description, "PostgreSQL Parser"},
	{vsn, "0.1.0"},
	{modules, ['pgsqlp','pgsqlp_copy','pgsqlp_keyword','pgsqlp_replication','pgsqlp_scalar','pgsqlp_select','pgsqlp_tx']},
	{registered, []},
	{applications, [kernel,stdlib,envy,phrase,scran]},
	{optional_applications, []},
	{env, []}
]}.