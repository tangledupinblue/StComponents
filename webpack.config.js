// Note this only includes basic configuration for development mode.
// For a more comprehensive configuration check:
// https://github.com/fable-compiler/webpack-config-template

var path = require("path");

// If we're running the webpack-dev-server, assume we're in development mode
var isProduction = !process.argv.find(v => v.indexOf('webpack-dev-server') !== -1);
console.log("Bundling for " + (isProduction ? "production" : "development") + "...");




module.exports = {
    mode: isProduction ? "production" : "development",
    entry: "./src/StComponents.fsproj",
    output: {
        path: path.join(__dirname, "./public"),
        filename: "bundle.js" //isProduction ? "bundle.js" : "bundle-dev.js",
    },
    devtool: isProduction ? "source-map" : "eval-source-map",
    devServer: {
        contentBase: "./public",
        port: 8081,
    },
    module: {
        rules: [{
            test: /\.fs(x|proj)?$/,
            use: "fable-loader"
        }]
    }
}