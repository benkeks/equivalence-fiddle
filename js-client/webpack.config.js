const path = require("path");
const webpack = require("webpack");

const config = require("./scalajs.webpack.config.js");

const entryKey = Object.keys(config.entry)[0];
const preludePath = path.resolve(__dirname, "webpack-prelude.js");

config.entry[entryKey] = [preludePath];

// Use source-map to generate separate .map files with full source mapping chain
// This preserves the chain: bundle map -> Scala.js map -> Scala sources
config.devtool = "source-map";

// Configure output to use proper source map references
config.output = config.output || {};
config.output.sourceMapFilename = "[file].map";

// Keep source-map-loader enabled but make it non-fatal for missing sources
// It will read the Scala.js source map and include it in the bundle map
if (config.module && config.module.rules) {
  config.module.rules = config.module.rules.map(rule => {
    if (rule.enforce === "pre" && rule.use && 
        (typeof rule.use === "string" ? rule.use : rule.use[0]).includes("source-map-loader")) {
      return {
        ...rule,
        use: [
          {
            loader: "source-map-loader",
            options: {
              filterSourceMappingUrl: (url, resourcePath) => {
                // Only load source maps for Scala.js output, not webjar modules
                return resourcePath.includes("eqfiddle-client-fastopt");
              }
            }
          }
        ]
      };
    }
    return rule;
  });
}

config.plugins = (config.plugins || []).concat([
  new webpack.ProvidePlugin({
    $: "jquery",
    jQuery: "jquery",
    "window.jQuery": "jquery",
    CodeMirror: "codemirror",
    "window.CodeMirror": "codemirror",
    d3: "d3"
  }),
  // Plugin to rewrite source map references to use HTTP paths that DevTools can load
  new webpack.DefinePlugin({
    // This helps ensure source maps reference accessible paths
  })
]);

// Custom webpack plugin to fix source map source paths
class SourceMapPathFixerPlugin {
  apply(compiler) {
    compiler.hooks.compilation.tap('SourceMapPathFixerPlugin', (compilation) => {
      compilation.hooks.processAssets.tap(
        {
          name: 'SourceMapPathFixerPlugin',
          stage: compiler.webpack.Compilation.PROCESS_ASSETS_STAGE_REPORT
        },
        () => {
          // Find and fix the bundle source map
          const mapAssetName = Object.keys(compilation.assets).find(name => 
            name.endsWith('-bundle.js.map')
          );
          
          if (mapAssetName) {
            const mapAsset = compilation.assets[mapAssetName];
            let mapContent;
            try {
              mapContent = mapAsset.source().toString();
            } catch (e) {
              return;
            }
            
            let mapObj;
            try {
              mapObj = JSON.parse(mapContent);
            } catch (e) {
              return;
            }
            
            // Rewrite sources that reference the intermediate map to use relative paths
            // DevTools will resolve these relative to the bundle location
            if (mapObj.sources) {
              mapObj.sources = mapObj.sources.map(src => {
                // Convert webpack:///../scala/HASH/path to ./scala/HASH/path
                // so it becomes resolvable as HTTP URLs from the bundle location
                if (src && src.includes('/../scala/')) {
                  return src.replace(/^webpack:\/\/\/\.\.\//,'./');
                }
                return src;
              });
            }
            
            compilation.assets[mapAssetName] = {
              source: () => JSON.stringify(mapObj, null, 2),
              size: () => JSON.stringify(mapObj, null, 2).length
            };
          }
        }
      );
    });
  }
}

config.plugins.push(new SourceMapPathFixerPlugin());

module.exports = config;
