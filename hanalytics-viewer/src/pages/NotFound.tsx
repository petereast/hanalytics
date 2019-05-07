import * as React from "react";
import { ReactNode } from "react";
import { Helmet } from "react-helmet";

export default class NotFound extends React.PureComponent<{}, {}> {
  public render(): ReactNode {
    return (
      <div>
        <Helmet>
          <title>Page Not Found</title>
        </Helmet>

        <h1>Not Found</h1>
      </div>
    );
  }
}
