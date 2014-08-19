# Basilica

A basilica is like a forum, but for a few ill-defined differences. For more detail please consult the table below, adapted from a crude sketch I made while drunk.

Forum | Basilica
----: | :-------
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

### Post

```json
{
  "id": 1,
  "idParent": 0,
  "by": "ian",
  "at": "2014-08-17T01:19:15.139Z",
  "count": 0,
  "content": "any string",
  "children": []
}
```

- `id` is a monotonically increasing identifier, and *it is the only field that should be used for sorting posts*.
- `idParent` *might be `null`*. Root posts have no parents.
- `by` is currently just a string. Later it may be something else.
- `at` is a string representing the date that the post was created, in ISO 8601 format. This field exists to be displayed to the user; it should not be used for sorting or paging. Use `id` for that.
- `count` is the *total number of children that this post has*, regardless of the number of children returned in any response.
- `children` is a list of posts whose `idParent` is equal to this post's `id`. This is *not necessarily an exhaustive list*. Comparing the number of elements in this field to the `count` field can tell you if there are more children to load.

## Routes

### `POST /posts/:idParent`

- for: creating a new post as a child of the specified `idParent`
- `idParent` is optional. If ommitted, this will create a post with `idParent` set to `null`.
- arguments: an `x-www-form-urlencoded` body is expected with
    - `by` (any string)
        - required
        - when accounts are implemented, this will be restricted
    - `content` (any string)
        - required
        - must not be the empty string
- response: the newly created post, JSON-encoded
    - if the post has a `count` other than `0`, that's a bug
    - the post will not have `children`

### `GET /posts/:id`

- for: loading posts and post children
- arguments: query parameters
    - `depth`: how deeply to recursively load `children`
        - **not currently implemented**
        - default: `1`
        - if `0`, the response will not include `children` at all
        - valid values: just `0` and `1` right now
    - `after`: the `id` of a post
        - **not currently implemented**
        - optional
        - ignored if `depth` is `0`
        - the response will not include any posts created before this in the `children` list (recursively, if multiple depths are ever supported)
    - `limit`: the maximum number of `children` to load
        - **not currently implemented**
        - default: `50`
        - ignored if `depth` is `0`
        - valid values: `1` to `500`
        - applies recursively, if multiple depths are ever supported
- response: a JSON-encoded post
    - if `depth` is greater than `0`, it will include `children`
    - remember that `count` is always the *total* number of children, regardless of the `limit`

### `GET /posts`

- for: loading every single post in the entire database
- response: a JSON array of posts, with no `children`

## Known clients

- [browser client](https://github.com/ianthehenry/basilica-client), nowhere near finished

# Development

Right now it uses SQLite. You need to create the database.

    $ sqlite3 basilica.db ".read schema.sql"

And install the dependencies.

    $ cabal install --only-dependencies -j

Then you can run it.

    $ cabal run
