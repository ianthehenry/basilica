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

# API

Basilica exposes a simple CRUD API, and is designed to be easy for computers to speak to. There are a few models that it speaks, always in JSON:

## Models

### Thread

```json
{
  "id": 1,
  "idParent": 0,
  "by": "ian",
  "at": "2014-08-17T01:19:15.139Z",
  "count": 0,
  "content": "any string",
  "children": [...]
}
```

- `id` is a monotonically increasing identifier, and *it is the only field that should be used for sorting threads*.
- `by` is currently just a string. Later it may be something else.
- `at` is a string representing the date that the thread was created, in ISO 8601 format. This field exists to be displayed to the user; it should not be used for sorting or paging.
- `count` is the *total number of children that this thread has*, regardless of the number of children returned in any response.
- `children` some responses may include children. This is *not necessarily an exhaustive list*. Comparing the number of elements in this field to the `count` field can tell you if there are more children to load.

## Routes

### `POST /threads/:idParent`

- for: creating a new thread as a child of the specified `idParent`
- arguments: an `x-www-form-urlencoded` body is expected with
    - `by` (any string)
        - required
    - `content` (any string)
- response: the newly created thread, JSON-encoded
    - if the thread has a `count` other than `0`, that's a bug
    - the thread will not have `children`

### `GET /threads/:id`

- for: loading threads and thread children
- arguments: query parameters
    - `depth`: how deeply to recursively load `children`
        - *not currently implemented*
        - default: `1`
        - if `0`, the response will not include `children` at all
        - valid values: just `0` and `1` right now
    - `after`: the `id` of a thread
        - *not currently implemented*
        - optional
        - ignored if `depth` is `0`
        - the response will not include any threads created before this in the `children` list (recursively, if multiple depths are ever supported)
    - `limit`: the maximum number of `children` to load
        - *not currently implemented*
        - default: `50`
        - ignored if `depth` is `0`
        - valid values: `1` to `500`
        - applies recursively, if multiple depths are ever supported
- response: a JSON-encoded thread
    - if `depth` is greater than `0`, it will include `children`
    - remember that `count` is always the *total* number of children, regardless of the `limit`

### `GET /threads`

- for: loading every single thread in the entire database
- response: a JSON array of threads, with no `children`

## Known clients

- [browser client](https://github.com/ianthehenry/basilica-client), nowhere near finished

# Development

Right now it uses SQLite. You need to create the database.

    $ sqlite3 basilica.db ".read schema.sql"

And install the dependencies.

    $ cabal install --only-dependencies -j

Then you can run it.

    $ cabal run
