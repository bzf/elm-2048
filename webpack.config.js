var webpack = require("webpack");

module.exports = {
  entry: {
    app: ["webpack/hot/dev-server", "./src/index.js"],
  },

  output: {
    path: "./dist",
    filename: "bundle.js",
    publicPath: "http://localhost:8080/dist/"
  },

  devServer: {
    contentBase: "./public",
  },

  module: {
    loaders: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loaders: ["elm-hot", "elm-webpack"]
      },
      {
        test: /\.(png|jpg|gif)$/,
        loaders: ["file-loader"]
      },
      {
        test: /\.scss$/,
        loaders: ["style", "css", "sass"]
      },
    ],
  },
};
