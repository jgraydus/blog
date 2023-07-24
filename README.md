This is a simple blogging app. It supports writing entries with Github-flavored markdown.

## production set up

First create the database:

```
sqlite3 blog.db < model.sql
```

Then create your user:

```
cabal run make-user -- -c <CONFIG FILE PATH> -e <EMAIL ADDRESS> -p <PASSWORD>
```

Build the client:

```
cd client && npx esbuild src/index.jsx --bundle --minify --outfile=build/bundle.js
```

Finally, run the server:

```
cabal run blog -- -c <CONFIG FILE PATH>
```

## dev set up

Create the database:

```
sqlite3 blog.db < model.sql
```

Then create your user:

```
cabal run make-user -- -e <EMAIL ADDRESS> -p <PASSWORD>     # uses the included config.json file
```

Start the dev server:

```
cabal run dev-server       # builds the client and server. automatically rebuilds and restarts
                           # when files change
```

