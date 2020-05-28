const path = require('path');
const webpack = require('webpack');
const CopyWebpackPlugin = require('copy-webpack-plugin');

module.exports = env => {
  const production = env && env.production || false;

  function debuggerEnabled(prod) {
    return prod ? false : true
  };

  return {
    module: {
        rules: [
            {
                test: /\.html$/,
                exclude: /node_modules/,
                loader: 'file-loader?name=[name].[ext]'
            },
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                use: [
                    { loader: 'elm-hot-webpack-loader'},
                    { loader: 'elm-webpack-loader',
                        options: {
                            cwd: __dirname,
                            debug: debuggerEnabled(production)
                        }
                    }
                ]
            }
        ]
    },


    plugins: [
        new webpack.HotModuleReplacementPlugin(),
        new CopyWebpackPlugin([ { from: 'assets', to: 'assets' } ])
    ],

    //output: {
      //// Tweak this to match your GitHub project name
      //publicPath: "snap",
    //},

    mode: 'development',

    devServer: {
      inline: true,
      hot: true,
      compress: true,
      stats: 'errors-only',
      historyApiFallback: true,
    }
  };
};
