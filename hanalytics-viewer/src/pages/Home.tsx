import * as React from "react";
import { ReactNode } from "react";
import { Button } from "react-bulma-components";
import { Helmet } from "react-helmet";

export default class Home extends React.PureComponent<{}, {}> {
  public static fetchData(): Promise<void> {
    console.log("Homepage fetch data");

    return Promise.resolve();
  }

  public render(): ReactNode {
    return (
      <div>
        <Helmet>
          <title>Homepage</title>
        </Helmet>

        <h1>Homepage</h1>
        <p>
          Hanalytics is a thing to do a thing
        </p>

        <Button color="primary">My Bulma button</Button>
      </div>
    );
  }
}
