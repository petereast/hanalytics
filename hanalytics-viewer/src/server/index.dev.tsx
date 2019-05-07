import { addHook } from "pirates";
import { Configuration } from "webpack";

// Convert image file imports into empty strings
addHook(() => "", {
  exts: [".jpg", ".jpeg", ".png", ".gif", ".png", ".svg"],
  matcher: () => true,
});

import * as Koa from "koa";
import * as koaWebpack from "koa-webpack";
import * as Webpack from "webpack";

const webpackConfig: Configuration = require("../../webpack.config.js");
import logger from "./logger";
import { init } from "./app";

const app = new Koa();

const compiler = Webpack(webpackConfig);

const port = parseInt(process.env.PORT || "7175", 10);

koaWebpack({
  compiler,
  devMiddleware: {
    serverSideRender: true,
    publicPath: "/assets/",
  },
  hotClient: {
    port: port + 1,
  },
}).then((middleware) => {
  app.use(middleware);

  init(app);

  logger.info("serverStarted", { port });

  app.listen(port);
});
