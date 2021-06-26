const path = require('path');
const webpack = require('webpack');

module.exports = (env, argv) => {
  return {
    plugins: [
      new webpack.HotModuleReplacementPlugin()
    ],
    entry: './src/index.js',
    output: {
      filename: 'main.js',
      path: path.resolve(__dirname),
    },
    devServer: {
      hot: true
    },
    module: {
      rules: [
        {
          test: /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          use: [
            { loader: 'elm-hot-webpack-loader' },
            {
              loader: 'elm-webpack-loader',
              options: {
                cwd: __dirname,
                optimize: (argv.mode == 'production'),
                debug: (argv.mode == 'development')
              }
            }
          ]
        },
        {
          test: /\.js$/,
          exclude: [/elm-stuff/, /node_modules/],
          loader: 'babel-loader'
        }
      ]
    }
  }
};