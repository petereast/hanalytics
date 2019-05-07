import { expect } from "chai";
import * as React from "react";
import { shallow } from "enzyme";

import ExampleComponent from "./ExampleComponent";

describe(".tsx component test file", () => {
  it("Should render", () => {
    const wrapper = shallow(<ExampleComponent />);

    expect(wrapper.text()).to.equal("Hello");
  });
});
