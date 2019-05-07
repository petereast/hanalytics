import { combineReducers } from "redux";

import auth from "./auth";
import { AuthState } from "./auth";

export interface AppState {
  auth: AuthState;
}

export default combineReducers({
  auth,
});
