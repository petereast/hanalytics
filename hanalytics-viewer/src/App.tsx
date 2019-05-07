import * as React from "react";
import { Store } from "redux";
import { Switch, Route, RouteProps } from "react-router";
import { Provider } from "react-redux";
import { hot } from "react-hot-loader";

import { StaticContext } from "./server/types";
import Home from "./pages/Home";
import Auth from "./pages/Auth";
import NotFound from "./pages/NotFound";

export interface AppRoute {
  path: string;
  exact?: boolean;
  component: RouteProps["component"] & { fetchData?: Function };
}

export const routes: AppRoute[] = [
  { path: "/", exact: true, component: Home },
  { path: "/auth", component: Auth },
];

function Status({ code, children }: { code: number; children: React.ReactNode }) {
  return (
    <Route
      render={({ staticContext }: { staticContext?: StaticContext }) => {
        if (staticContext) {
          staticContext.status = code;
        }

        return children;
      }}
    />
  );
}

const App = ({ store }: { store: Store }) => (
  <Provider store={store}>
    <div>
      <Switch>
        {routes.map((route: AppRoute) => (
          <Route key={route.path} {...route} />
        ))}

        <Status code={404}>
          <NotFound />
        </Status>
      </Switch>
    </div>
  </Provider>
);

export default hot(module)(App);
