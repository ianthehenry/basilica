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

Small bits and pieces might happen to work here and there, but such behavior should be considered exceptional.

# API

## Resources

Basilica defines a few resources, which are always communicated in JSON.

Sometimes the API will send *resolved* data, which means that it will turn:

    "idResource": 123

Into:

    "resource": { "id": 123, ... }

When it does so will be documented in the route response.

Unless otherwise specified, no value will be `null`.

### Post

```json
{ "id": 49
, "idParent": 14
, "idUser": 43
, "at": "2014-08-17T01:19:15.139Z"
, "count": 0
, "content": "any string"
, "children": []
}
```

- `id` is a monotonically increasing identifier, and *it is the only field that should be used for sorting posts*.
- `idParent` *can be `null`*. Root posts have no parents.
- `idUser` is the `id` of the user who created the post.
- `at` is a string representing the date that the post was created, in ISO 8601 format. This field exists to be displayed to the user; it should not be used for sorting or paging. Use `id` for that.
- `count` is the *total number of children that this post has*, regardless of the number of children returned in any response.
- `children` is a list of posts whose `idParent` is equal to this post's `id`. This is *not necessarily an exhaustive list*. Comparing the number of elements in this field to the `count` field can tell you if there are more children to load.
    - `children` will *always* be sorted by `id`, with newer posts (larger `id`s) in the front of the list

### User

```json
{ "id": 32
, "email": "name@example.com"
, "name": "ian"
, "face": {}
}
```

- `email` will be omitted unless otherwise specified in the route documentation
- `face` is an object that indicates how to render a thumbnail of the user. Currently the only valid options are:
    - `{ "gravatar": "a130ced3f36ffd4604f4dae04b2b3bcd" }`
    - `{ "string": "☃" }`
        - **not implemented**

### Code

Codes are never communicated via JSON, so it doesn't make sense to show their format. Publicly, they can be considered strings. They happen to currently be hexadecimal strings, but that's an implementation detail that may change.

### Token

```json
{ "id": 91
, "token": "a long string"
, "idUser": 32
}
```

## Authentication

There's a goofy hand-rolled auth scheme.

There are no passwords. Authentication is done purely through email. The process looks this:

- request a code (see `POST /codes`)
- Basilica emails it to you
- you trade in the code for a token (see `POST /tokens`)
- you use that token to authenticate all future requests (by setting the `X-Token` header)

I'm gonna repeat that last thing because it's important: you need to set an `X-Token` header to make an authenticated request. No cookies, query parameters, nothing like that. That header is the only thing that counts.

This is similar to the "forgot my password" flow found in most apps, except that you don't have to pretend to remember anything.

## Routes

### Postal Routes

#### `POST /posts/:idParent`

- **requires a valid `token`**
- for: creating a new post as a child of the specified `idParent`
- `idParent` is optional. If ommitted, this will create a post with `idParent` set to `null`.
- arguments: an `x-www-form-urlencoded` body is expected with
    - `content` (any string)
        - required
        - must not be the empty string
- response: the newly created post, JSON-encoded
    - `idUser` will be resolved
    - if the post has a `count` other than `0`, that's a bug
    - the post will not have `children`

```sh
$ curl -i                             # show response headers (otherwise a 401 is very confusing)
       -X POST                        # set the HTTP verb
       --data "content=hello%20world" # escape your string!
       -H "X-Token: asdf"             # requires authentication
       "http://localhost:3000/posts"  # the actual route
```

#### `GET /posts/:id`

- for: loading posts and post children
- arguments: query parameters
    - `depth`: how deeply to recursively load `children`
        - **not implemented**
        - default: `1`
        - if `0`, the response will not include `children` at all
        - valid values: just `0` and `1` right now
    - `after`: the `id` of a post
        - **not implemented**
        - optional
        - ignored if `depth` is `0`
        - the response will not include any posts created before this in the `children` list (recursively, if multiple depths are ever supported)
    - `limit`: the maximum number of `children` to load
        - **not implemented**
        - default: `50`
        - ignored if `depth` is `0`
        - valid values: `1` to `500`
        - applies recursively, if multiple depths are ever supported
- response: a JSON-encoded post
    - if `depth` is greater than `0`, it will include `children`
    - `idUser` will be resolved for the root post and all children, recursively
    - remember that `count` is always the *total* number of children, regardless of the `limit`

#### `GET /posts`

- for: loading every single post in the entire database, catching up after a disconnect (with `after`)
- arguments: query parameters
    - `after`: the `id` of a post
        - optional
        - the response will only contain posts created after the specified post
    - `limit`: the maximum number of posts to return
        - **not implemented**
        - default: `50`
        - valid values: `1` to `500`
- response:
    - if `after` is specified, and there were more than `limit` posts to return, this returns... some error code. I'm not sure what though. `410`, maybe?
        - **not implemented**
    - otherwise, a JSON array of posts with no `children` fields, sorted by `id` from newest to oldest
    - `idUser` will be resolved

### User Routes

#### `POST /users`

- for: signing up for a new account
- arguments: `x-www-form-urlencoded`
    - `email`: the email address for the user.
    - `name`: the username. Must contain only alphanumeric characters.
- response:
    - `200` with the newly created `user`
    - `400` if the username contains non-alphanumeric characters
    - `409` if an account already exists with the specified username or email address, with no response body
- side effect: automatically invokes `POST /codes` with the given email address

### Auth Routes

#### `POST /codes`

- for: creating a new code, which can be used to obtain a token
- arguments: `x-www-form-urlencoded`
    - `email`: the email address of the user for which you would like to create a code
- response: this route will always return an empty response body with a `200` status code, regardless of whether `email` corresponds to a valid email address
    - a timing attack can absolutely be used to determine if the email corresponds to a valid account or not; knock yourself out
- side effect: if the email address specified matches a user account, Basilica will send an email containing the newly created code.

#### `DELETE /codes/:code`

- for: revoking a code, in case it was sent in error
- **not implemented**
- or documented

#### `POST /tokens`

- for: creating a new token
- arguments: `x-www-form-urlencoded`
    - `code`: a code obtained from a call to `POST /codes`
        - required
- note: auth tokens don't do anything yet
- response:
    - if the code is valid, a JSON-encoded token with `idUser` resolved into `user`
    - otherwise, `401`
- side effect: invalidates the code specified

#### `GET /tokens`

- for: listing tokens
- response: an array of JSON-encoded token objects with only `id` specified
    - probably other stuff later
- **not implemented**

#### `DELETE /tokens/:id`

- for: revoking a token ("logging out")
- arguments:
    - `id`: the `id` of the token to revoke
        - required
- response: `200`, `404`, or `401`
- **not implemented**

# Websockets

There is currently one websocket route, a single firehose stream of all new posts created, in JSON, with `idUser` resolved. The route is just `/`, with the `ws` or `wss` protocol.

When connected, Basilica will periodically send ping frames. If the client doesn't respond in a timely manner, that client will be closed with either a friendly or slightly hostile message.

Currently this is set to ping every 20 seconds and to disconnect clients if more than 40 seconds passes without receiving a pong. Don't rely on those values, though. Just pong the pings as quickly as you can. All websocket libraries should do this for you automatically.

## Notes

- When a new post is created, clients should update their cached `count` value for its parent. It's important that this value stays up-to-date for accurate paging.
- When a disconnect occurs, and it will, reconnect the socket and then call `GET /posts?after=id`, where `id` is the latest post that you knew about. It's important that you reconnect the socket before filling the gap, otherwise any post created in the brief moment after the response and before the socket comes back will be lost.

## Basiliclients

- The official [browser client](https://github.com/ianthehenry/basilica-client), with some implemented features.

# Development

Basilica uses SQLite. You need to create the database.

    $ sqlite3 basilica.db ".read schema.sql"

And install the dependencies.

    $ cabal install --only-dependencies -j

Then modify the `conf` file. The `client-origin` field is optional, and mainly useful for development, so that you can serve your client from something like [Brunch](http://brunch.io/). When specified, it will set the `Access-Control-Allow-Origin` header and respond to `OPTIONS` requests appropriately.

Then you basilican run it.

    $ cabal run

Now you're ready to *basilicate*.
