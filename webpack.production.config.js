var webpack = require("webpack");

module.exports = {
  entry: __dirname + "/src" + "/index.js",

  output: {
    path: __dirname + "/dist",
    filename: "bundle.js",
  },

  module: {
    loaders: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loaders: ["elm-hot", "elm-webpack"]
      },
      {
        test: /\.scss$/,
        loaders: ["style", "css", "sass"]
      },
    ],
  },
};
