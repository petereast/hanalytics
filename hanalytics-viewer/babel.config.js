module.exports = function (api) {
  api.cache(true);

  return {
    "presets": [
      [
        "@babel/preset-env",
        {
          "useBuiltIns": false,
          "targets": {
            "node": true
          }
        }
      ],
      "@babel/preset-typescript",
      "@babel/preset-react"
    ],
    "plugins": [
      [
        "@babel/plugin-proposal-class-properties",
        {
          "loose": true
        }
      ]
    ]
  };
}
