import * as React from "react";
import { HelmetData } from "react-helmet";
import { Store } from "redux";

import asset from "./utils/asset";

const Container = ({
  children,
  store,
  helmet,
}: {
  children: string;
  store: Store;
  helmet: HelmetData;
}) => (
  <html>
    <head>
      {helmet.title.toComponent()}
      {helmet.meta.toComponent()}
      {helmet.link.toComponent()}
      <link rel="stylesheet" href={asset("main.css")} />
      <script src={asset("main.js")} defer />
    </head>

    <body>
      <main id="app" dangerouslySetInnerHTML={{ __html: children }} />

      <div id="initial_state" data-state={JSON.stringify(store.getState())} />
    </body>
  </html>
);

export default Container;
