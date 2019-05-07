const path = require('path');
const webpack = require('webpack');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const ForkTsCheckerWebpackPlugin = require('fork-ts-checker-webpack-plugin');
const ManifestPlugin = require('webpack-manifest-plugin');
const OptimizeCSSAssetsPlugin = require("optimize-css-assets-webpack-plugin");
const TerserPlugin = require("terser-webpack-plugin");

const devMode = process.env.NODE_ENV !== 'production';
const basePath = process.env.BASE_PATH || '';

module.exports = {
  entry: ['./src/index.tsx'],

  output: {
    filename: devMode ? 'main.js' : 'main.[hash].js',
    chunkFilename: '[name].chunk.js',
    path: path.resolve(__dirname, 'dist/assets'),
    publicPath: `${basePath}/assets/`
  },

  mode: devMode ? 'development' : 'production',

  devtool: devMode ? 'cheap-module-source-map' : 'source-map',

  resolve: {
    extensions: ['.ts', '.tsx', '.js', '.json', '.less']
  },

  module: {
    rules: [
      {
        test: /\.(png|jpg|gif|jpeg|svg|woff|woff2)$/i,
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
            cacheDirectory: true,
            plugins: devMode ? ["react-hot-loader/babel"] : []
          }
        }
      },

      {
        test: /\.less$/,
        use: [
          devMode ? 'style-loader' : MiniCssExtractPlugin.loader,
          {
            loader: 'css-loader'
          },
          {
            loader: 'less-loader'
          }
        ]
      },

      {
        test: /\.sass$/,
        use: [
          devMode ? 'style-loader' : MiniCssExtractPlugin.loader,
          {
            loader: 'css-loader'
          },
          {
            loader: 'sass-loader'
          }
        ]
      }
    ]
  },
  plugins: [
    new webpack.EnvironmentPlugin({ NODE_ENV: 'development', BASE_PATH: '' }),
    new MiniCssExtractPlugin({
      filename: devMode ? '[name].css' : '[name].[hash].css',
      chunkFilename: devMode ? '[id].css' : '[id].[hash].css',
    }),
    new ForkTsCheckerWebpackPlugin({
      workers: ForkTsCheckerWebpackPlugin.TWO_CPUS_FREE,
      measureCompilationTime: true,
    }),
    new ManifestPlugin({
      fileName: '../asset-manifest.json',
    }),
  ],
  optimization: {
    splitChunks: {
      cacheGroups: {
        styles: {
          name: 'styles',
          test: /\.css$/,
          chunks: 'all',
          enforce: true
        }
      }
    }
  }
};
