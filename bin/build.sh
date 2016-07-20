#!/bin/sh

rm -rf dist/
mkdir -p dist/

cp ./public/index.html dist/
NODE_ENV=production webpack -p --config webpack.production.config.js
