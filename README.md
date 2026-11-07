# Time counter

An Elm app that counts time since an event, or down to a future milestone.
Timers live entirely in shareable URLs; there is no backend or account.

## Run

Requires Node.js 22.12+ and npm. Elm and all build tools install locally.

```sh
npm ci
npm run dev
```

Open http://127.0.0.1:4243. If that port is busy:

```sh
npm run dev -- --port 4347
```

## Checks

```sh
npm run format:check
npm test
npm run build
npx playwright install chromium
npm run test:e2e
```

Browser checks run against the production build in isolated Chromium contexts,
with desktop and mobile layouts. They never reuse an existing server or browser.
Use `E2E_PORT=4347 npm run test:e2e` if port 4243 is occupied.
GitHub Actions runs the same checks on pushes and pull requests.

## Behavior

- Enter a title and valid date; clock fields are optional and default to midnight.
- Dates and calendar durations use **UTC**, matching the original app.
- Duration consumes whole calendar years, then months, then weeks, days and time.
  Month-end anniversaries clamp to the last day of the destination month.
- Future events show “Time remaining”; past events show “Time elapsed”.
- Copy the address bar URL to share. The title and timestamp are encoded, not
  encrypted: anyone with the URL can read them.
- New links use URL-safe Base64. Original `/t/<base64>` links still work,
  including padding, embedded slashes, and percent-encoded payloads.
- No database, local storage, remote fonts, or service worker is required.

## Build and hosting

```sh
npm run build
npm run preview
```

Deploy `build/` to a static host. Configure an SPA fallback: requests for `/new`
and `/t/*` must serve `index.html`, while real assets remain directly accessible.
For nginx, use `try_files $uri $uri/ /index.html;` in the site location.

`bin/deploy.sh` retains the original SSH deployment convention. It requires
an explicitly configured `DEPLOY_HOST` and `DEPLOY_PATH`; review those values
and the server directory layout before running it. Local checks do not deploy.

## Review of the 2019 version

Reviewed original commit `79b551d` (2019-11-01).

| Finding | Original source | Fix |
| --- | --- | --- |
| Build depends on global `elm-app`, with no npm manifest or lockfile | `bin/deploy.sh:8`, `src/index.js:2` | Pinned Elm 0.19.2, Vite, npm lockfile and CI |
| Calendar totals are subtracted twice; one year plus one month produces negative weeks | `src/Timer.elm:205–239` | Consume calendar units in order, then split the remainder |
| Editing any field marks the entire form valid; missing or impossible dates are normalized silently | `src/Main.elm:170–176`, `src/Timer.elm:242–277` | Explicit validation with visible errors and calendar round-trip checks |
| Invalid timestamp silently becomes epoch | `src/Timer.elm:200–202` | Reject malformed timestamps |
| Standard Base64 can contain `/`, breaking the single-segment route; delimiter in title breaks decoding | `src/Main.elm:122,150–151`, `src/Timer.elm:185–197` | URL-safe encoding and backward-compatible parsing |
| Tests exercise unused `timerTimeLeft`, not screen's `timeLeft`; week expectation also includes an extra day | `tests/Tests.elm:14–55` | Tests target the production calculation and user-visible routes |
| Date conversion wraps after 2038 because Elm integer division uses a signed 32-bit result | `src/Timer.elm:276` | Float division followed by `floor`, with a 2100 regression test |
| Fixed 100px padding and large horizontal rows constrain narrow screens | `src/Main.elm:220–278,332,362–372` | Bounded layout, smaller padding, wrapping duration, mobile overflow checks |

Removed unused `Event.elm` and obsolete create-elm-app service-worker boilerplate.
Preserved the Elm architecture, elm-ui, yellow background, original home copy,
and existing link format compatibility.

## Dependency audit

`cross-spawn` under `node-elm-compiler` is pinned to patched 6.0.6 via npm overrides.
At review time, `npm audit` still reports three high-severity development-tool
entries: `braces`, and its dependents `chokidar` and `elm-test`.
The available latest `braces` release is affected by
[GHSA-vfj7-8cjw-p6xm](https://github.com/advisories/GHSA-vfj7-8cjw-p6xm), with no
patched version available. These tools do not ship in the static app;
`npm audit --omit=dev` reports zero vulnerabilities. Recheck when upgrading tools.
