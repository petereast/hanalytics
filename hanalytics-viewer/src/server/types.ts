import { StaticRouterContext } from "react-router";

export interface StaticContext extends StaticRouterContext {
  status?: number;
}
