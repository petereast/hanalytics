import * as React from "react";
import * as Router from "koa-router";
import { Context } from "koa";
import { Helmet } from "react-helmet";
import * as ReactDOMServer from "react-dom/server";
import { StaticRouter, matchPath } from "react-router";

import { StaticContext } from "./types";
import Container from "./Container";
import App, { routes, AppRoute } from "../App";
import { createStore } from "../store";

const basePath = process.env.BASE_PATH || "";

const router = new Router().prefix(basePath);

router.get("*", async (ctx: Context) => {
  const store = createStore();

  // Dispatch actions here to initialise the store

  let dataFetches: {}[] = [];

  routes.some(
    (route: AppRoute): boolean => {
      const match = matchPath(ctx.request.path, route);

      if (match && route.component && typeof route.component.fetchData === "function") {
        dataFetches.push(
          route.component.fetchData(store.dispatch, match, {
            search: ctx.request.search,
          })
        );
      }

      return !!match;
    }
  );

  await Promise.all(dataFetches);

  const context: StaticContext = {};

  const page = ReactDOMServer.renderToString(
    <StaticRouter basename={basePath} location={ctx.request.url} context={context}>
      <App store={store} />
    </StaticRouter>
  );

  const helmet = Helmet.renderStatic();

  const markup = ReactDOMServer.renderToString(
    <Container store={store} helmet={helmet}>
      {page}
    </Container>
  );

  if (context.status) {
    ctx.status = context.status;
  }

  ctx.body = markup;
});

export default router;
