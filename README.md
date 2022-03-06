# 1XD3 Project

See the current deployment here:
https://err53.github.io/1xd3/

## Development

Run `elm reactor` to start a local web server for testing.

Alternatively, if you want hot-reloading, `npm install -g elm-live` and run `elm-live src/Main.elm`.

## Deployment

Run `elm make src/Main.elm --optimize --output=public/index.html` to build the optimized Elm app.