<br>

<p align="center">
    <a href="https://shortishly.github.io/pgsqlp/cover/">
      <img alt="Test Coverage" src="https://img.shields.io/badge/dynamic/json?url=https%3A%2F%2Fshortishly.github.io%2Fpgsqlp%2Fcover%2Fcoverage.json&query=%24.total&suffix=%25&style=flat-square&label=Test%20Coverage&color=green">
    </a>
    <a href="https://shortishly.github.io/pgsqlp/edoc/">
      <img alt="edoc" src="https://img.shields.io/badge/Documentation-edoc-green?style=flat-square">
    </a>
    <a href="https://erlang.org/">
      <img alt="Erlang/OTP 25+" src="https://img.shields.io/badge/Erlang%2FOTP-25%2B-green?style=flat-square">
    </a>
    <a href="https://www.apache.org/licenses/LICENSE-2.0">
      <img alt="Apache-2.0" src="https://img.shields.io/github/license/shortishly/pgsqlp?style=flat-square">
    </a>
</p>

# What is pgsqlp?

pgsqlp, uses the [scran][github-com-scran] parser combinators to
implement a partial Postgres SQL parser. It currently implements only
the SQL parsing necessary for the *server* side of logical
replication.

The test cases are currently the best place to look at functional examples
of the combinators.

[github-com-scran]: https://github.com/shortishly/scran
