# 1XD3 Project

[![CI](https://github.com/err53/1xd3/actions/workflows/main.yml/badge.svg?branch=main)](https://github.com/err53/1xd3/actions/workflows/main.yml)

See the current deployment here:
https://err53.github.io/1xd3/

## Development

Run `elm reactor` to start a local web server for testing.

Alternatively, if you want hot-reloading, `npm install` and `npm start`.

## Deployment

Run `elm make src/Main.elm --optimize --output=dist/index.html` to build the optimized Elm app.