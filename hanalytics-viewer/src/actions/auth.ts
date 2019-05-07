export const TEST_AUTH_ACTION = "TEST_AUTH_ACTION";

export interface TestAuthAction {
  type: typeof TEST_AUTH_ACTION;
  something: number;
}

export function testAuthAction(something: number): TestAuthAction {
  return {
    type: TEST_AUTH_ACTION,
    something,
  };
}
