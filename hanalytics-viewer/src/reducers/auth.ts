import { TEST_AUTH_ACTION, TestAuthAction } from "../actions/auth";

export interface AuthState {
  something: number;
  loading?: boolean;
}

export const defaultState: AuthState = { something: 0 };

export default function auth(state = defaultState, action: TestAuthAction): AuthState {
  switch (action.type) {
    case TEST_AUTH_ACTION:
      return {
        ...state,
        loading: false,
        something: state.something + action.something,
      };

    default:
      return state;
  }
}
