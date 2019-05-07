# Typescript SSR Webpack boilerplate

```bash
yarn install

# Optional
export BASE_PATH=/path-to-app

yarn build:dev
yarn start:dev
```

## Env vars

* `BASE_PATH`: set the base path for the app, like `BASE_PATH=/myawesomething`. Build in CI like this:

  ```bash
  BASE_PATH=/foo yarn build
  BASE_PATH=/foo yarn build:server
  ```
