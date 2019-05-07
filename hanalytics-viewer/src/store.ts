import { createStore as reduxCreateStore, applyMiddleware, compose } from "redux";
import thunk from "redux-thunk";

import reducers from "./reducers";

const composeEnhancers =
  typeof window !== "undefined"
    ? (window as { __REDUX_DEVTOOLS_EXTENSION_COMPOSE__?: Function })
        .__REDUX_DEVTOOLS_EXTENSION_COMPOSE__ || compose
    : compose;

export function createStore(initialState = undefined) {
  return reduxCreateStore(reducers, initialState, composeEnhancers(applyMiddleware(thunk)));
}
