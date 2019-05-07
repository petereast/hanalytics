const path = require('path');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const ForkTsCheckerWebpackPlugin = require('fork-ts-checker-webpack-plugin');
const ManifestPlugin = require('webpack-manifest-plugin');

const devMode = process.env.NODE_ENV !== 'production';

module.exports = {
  entry: ['./src/server/index.tsx'],
  output: {
    filename: 'server.js',
    path: __dirname + '/dist',
    publicPath: `${basePath}/assets/`
  },

  target: 'node',

  resolve: {
    extensions: ['.ts', '.tsx', '.js', '.json', '.less']
  },

  module: {
    rules: [
      {
        test: /\.(png|jpg|gif|jpeg|svg)$/i,
        use: [
          {
            loader: 'url-loader',
            options: {
              name: devMode ? '[name].[ext]' : '[name].[hash].[ext]',
              limit: 8192,
              fallback: 'file-loader'
            }
          }
        ]
      },

      {
        test: /\.tsx?$/,
        exclude: /(node_modules|assets)/,
        use: {
          loader: "babel-loader",
          options: {
            cacheDirectory: true
          }
        }
      },
    ]
  },
};
