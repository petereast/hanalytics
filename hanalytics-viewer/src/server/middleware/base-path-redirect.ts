import { Context } from "koa";

const basePath = process.env.BASE_PATH || "";

// Redirect paths above $BASE_PATH (if set) to $BASE_PATH
export async function basePathRedirect(ctx: Context, next: Function) {
  if (basePath && !ctx.path.includes(basePath)) {
    return ctx.redirect(`${basePath}${ctx.path}`);
  }

  await next();
}
