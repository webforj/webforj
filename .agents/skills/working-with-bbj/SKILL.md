---
name: working-with-bbj
description: Builds, installs, tests and diagnoses webforJ work that runs in BBjServices, through BBjShip and the Maven plugin goals. Use for any change to webforj-bbjship, the BBj goals of the build plugins or the plugin foundation code behind them, for any app or archetype that targets BBj, and for any failure seen in BBjServices.
---

# Working with BBj

BBj is the engine of webforJ.

## BBj code

- BBj is a thin relay. A BBj program or servlet calls the BBj API Java cannot reach (servlet request and response, admin session, terminal) and hands the result to Java in one call. Every rule, decision, model, message and format lives in Java, unit tested.
- Only the documented BBx servlet API and BBj admin API exist. Never Jetty, a BBjServices internal class or a lookup by class name. What the API does not expose is not available. The BBj source is read to understand a failure, never to design a fix, and no design names a BBj internal.
- HTTP is never handled by hand in Java: no router, header parser or status table. The servlet API gives method, path, headers and body, standard libraries give JSON, Base64 and problem details.
- The BBjShip CLI is the administrator's tool on the BBj machine. It authenticates with the administration user name and password only and never manages users, passwords or tokens. Tokens from Enterprise Manager are accepted by the deployment endpoint only. A caller judges a command by its exit code, never by its output.
- BBj code is indented with two spaces. A syntax check never writes compiled output beside the sources.

## Layout on the BBj machine

| Path | Holds |
|------|-------|
| `<BBj home>/plugins/webforj-bbjship` | Shipped files only: `bbjship`, `bbjship.cmd`, `bbjship.ps1`, `lib`, `servlets`, `version.properties`, `bbj-plugin.json` |
| `<BBj home>/plugins/.webforj-bbjship-deployments` | Everything BBjShip writes: deployments, static files, assets, the development switch, the properties backup, install transactions |
| `<BBj home>/cfg/BBj.properties` | The Java arguments of BBjServices |

Locked names: contexts `webforj-bbjship` and `webforj-bbjship-assets` (path `/webforj`), classpaths `_webforj_bbjship`, `_webforj_bbjship_cli` and `webforj_bbjship_<app>`.

## A BBj project

A missing item fails in BBjServices, not in the build.

1. `jar` packaging and the `webforj-maven-plugin` with `<extensions>true</extensions>`.
2. A `<bbj>` block with the publish name and a deployment URL ending in `/webforj-bbjship`.
3. The `webforj` dependency excludes `webforj-engine` in the pom, and `webforj-engine` is declared again in `provided` scope. Nothing BBj ships in its `lib` folder ever enters an archive. Without the provided engine the IDE compiler writes stub classes that throw "Unresolved compilation problem" in BBjServices.
4. `webforj.assetsSource = packaged` in the dev and prod configuration, or `icons://` and static assets fail.
5. For development runs, the `<hotswap>` block and `compile webforj:watch` as the default goal.

## Development runs

- The developer adds the hotswap block and runs the watch, nothing else. The watch installs BBjShip, enables development runs, adds the hotswap Java argument to the BBjServices properties file and restarts BBjServices through the BBjShip CLI, each step only when not already true. Deploys never touch BBjServices, and stopping an app never stops it.
- BBj keeps its Java and every setting except that one Java arguments line. Without enhanced class redefinition the run gets method body changes and a warning naming the JetBrains Runtime, never a failure. Locked.
- Every step runs on every operating system.
- A deployment URL naming another machine is refused.
- Everything of the run, devtools included, ends with the build run.

## Install after every change

A change to `webforj-bbjship` or the foundation code it uses is installed on the maintainer's BBj before the turn ends. The maintainer tests the installed build only. One background script:

1. `mvn -o -pl <changed modules> clean install`.
2. From a BBj project folder, `mvn -o webforj:bbj-setup -Dwebforj.bbj.force=true`.
3. `bbjship apps list --username <admin> --password-stdin` exits 0.

If the old servlet code still answers, run `bbjship remote enable --port <HTTP port>` again to attach the new jar. BBjServices is never restarted for an install.

## Diagnosing a failure in BBjServices

In order, before any theory:

1. Stub classes in `target/classes` from an IDE build. Run `mvn clean compile` first.
2. The app itself, for example a duplicate `@Route` path.
3. An old BBjShip. Compare `version.properties` and the jar checksum with the build.
4. The project list above.

A failure after a class swap and a page reload is reproduced on the same build tool, one variable at a time, before a cause is named, with the reproduction count reported.

## Tests and proofs

- BBjShip tests mock the BBj admin API and servlet types, following the fixtures in `webforj-plugins/webforj-bbjship/webforj-bbjship-plugin/src/test`. Test names spell Bbj.
- A live proof runs only after the install steps, with an app the maintainer named or a generated archetype project, on a port the maintainer is not using.
