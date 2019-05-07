import * as React from "react";
import { ReactNode } from "react";
import { connect, DispatchProp } from "react-redux";
import { Helmet } from "react-helmet";

import { testAuthAction } from "../actions/auth";
const small = require("../../assets/small.jpg");
const large = require("../../assets/large.jpg");

interface AuthProps extends DispatchProp {
  auth: { something: number };
}

class Auth extends React.PureComponent<AuthProps, {}> {
  public static fetchData(): Promise<void> {
    console.log("Auth page fetch data");

    return Promise.resolve();
  }

  public handleClick = (): void => {
    this.props.dispatch(testAuthAction(100));
  };

  public render(): ReactNode {
    return (
      <div>
        <Helmet>
          <title>Auth</title>
        </Helmet>

        <div className="auth-page">Auth here</div>

        <p>Something: {this.props.auth.something}</p>

        <button onClick={this.handleClick}>Click me</button>

        <img src={small} />
        <img src={large} />
      </div>
    );
  }
}

export default connect((s) => s)(Auth);
