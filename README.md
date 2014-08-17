# Basilica

A basilica is like a forum, but for a few ill-defined differences. For more detail please consult the table below, adapted from a crude sketch I made while drunk.

Forum | Basilica
----- | --------
PHP | Haskell
90s | 2010s
trolls | friends
"rich formatting" | markdown
paging | lazy tree
threads ↑ comments ↓ | uniform hierarchy
`<form>` | HTTP API
inline CSS | bots, webhooks, extensions
F5 | websockets

# Status

Basilica is still *remarkably unfinished*.

Small bits and pieces might happen to work here and there, but such behavior should be considered unusual.

# Development

Right now it uses SQLite. You need to create the database.

    $ sqlite3 basilica.db ".read schema.sql"

And install the dependencies.

    $ cabal install --only-dependencies -j

Then you can run it.

    $ cabal run
