# countdown-to

Very early, experimental example integrating the Haskell graphql-api library and Elm. Data is stored in postgres to show integrating scotty, graphql-api and a database.

## Instructions

### Server

The build is managed by `stack`. I think that `stack run` might be a third-party thing, probably worth a search if it doesn't work for you.

```
cd server
stack run
```

### Client

Elm & other node dev dependencies are managed by `yarn`. Due to the use of Elm's `Navigation` library, this is using `elm-live` for serving the client.

```
cd client
yarn
yarn start
```

### Database

The database is postgres. Create a database with `createdb countdown-to_dev`. You _should_ then be able to use the `create_timers.sql` script in the root to create the table. I used `pgAdmin 4` on MacOS to generate that script, and `Postico` to load it in. I'm not a database admin, and haven't done backend in a long time, so YMMV.
