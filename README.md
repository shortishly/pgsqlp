# pgsqlp, a Postgres SQL parser using scran

pgsqlp, uses the [scran][github-com-scran] parser combinators to
implement a partial Postgres SQL parser. It currently implements only
the SQL parsing necessary for the *server* side of logical
replication.

The test cases are currently the best place to look at functional examples
of the combinators.

[github-com-scran]: https://github.com/shortishly/scran
