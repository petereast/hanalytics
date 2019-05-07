import * as React from "react";
import * as ReactDOM from "react-dom";
import { BrowserRouter } from "react-router-dom";

import App from "./App";
import { createStore } from "./store";

console.log("Entry point");

const stateEl: any = document.getElementById("initial_state");

const initialState = stateEl ? JSON.parse(stateEl.dataset.state) : {};

const store = createStore(initialState);

const container = document.getElementById("app");

const basePath = process.env.BASE_PATH || "";

ReactDOM.render(
  <BrowserRouter basename={basePath}>
    <App store={store} />
  </BrowserRouter>,
  container
);

import "bulma/bulma.sass";
import "./styles/style.less";
