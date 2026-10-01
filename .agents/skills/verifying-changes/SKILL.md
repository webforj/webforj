---
name: verifying-changes
description: Builds and verifies a webforJ change, then proves it in a running app the way a developer works, with single file compiles and no page reloads. Use when building, running the verify, testing a change live or before reporting any change as working.
---

# Verifying changes

A green suite, a clean build and a matching checksum prove that the code compiles. They prove nothing about whether the change works. No change that alters behaviour is done until it was driven in a running app and the app was measured. A change that alters no behaviour (member order, formatting, private names) needs the Build section only. The rules of AGENTS.md about the maintainer's machine apply.

Copy this checklist and tick it off:

```
Verification:
- [ ] Formatter applied
- [ ] Module verify green, exit code read from the command itself
- [ ] Jar installed, FeatureHandler services file present
- [ ] Private app copy on its own ports, pom on the version under test
- [ ] Failure reproduced with the old jar
- [ ] Window marker set
- [ ] Actions driven with single file compiles, no reload
- [ ] Two tabs checked
- [ ] App copy stopped by its own port, tabs closed
```

## Build

1. Run the formatter: `mvn -o -q -pl <module> spotless:apply`.
2. Verify the module: `mvn -o -pl <module> verify > verify.log 2>&1`, then capture `$?` and read the summary lines of the log. Spotless check and checkstyle run there.
3. On failure, fix and go back to step 1. Continue only when the log says `BUILD SUCCESS`.

Rules:
- Never trust an exit code after a pipe. A background run is judged by its log (`BUILD SUCCESS` or `[ERROR]`), never by the notification's exit code.
- Checkstyle warnings of `google_checks` are read and fixed in every touched file, except what the formatter itself recreates. The IDE panel shows warnings the Maven build does not fail on.
- After any edit, the verification counts only if it ran after that edit.
- An installed devtools jar must contain `META-INF/services/...FeatureHandler` with its entries. An IDE build that shares `target/classes` can drop it. Rebuild clean when it is missing.
- Before a new branch or a comparison, fetch and compare the local main with the remote main.

## Live proof

1. **Copy the test app** the maintainer names into a private folder. When none is named, ask. Copy it (rsync, excluding `target` and `node_modules`). Pick a free port for the server and a free port for live reload.
2. **Set the version.** `webforj.version` in the copy's `pom.xml` is the SNAPSHOT under test. No command line override.
3. **Install the jar** in the background, then check the `FeatureHandler` services file in it.
4. **Prepare the classpath:**

   ```bash
   mvn -o -q dependency:build-classpath -Dmdep.outputFile=cp.txt
   ```

5. **Launch** the copy in the background with the browser auto open off, its own server port and its own live reload port. Wait by reading the log, never with a foreground loop.
6. **Reproduce first.** With the jar from before the fix, drive the reported failure and record what happened. A fix never seen failing is not verified.
7. **Mark the page.** Set a marker on `window` before the first action and read it back after every action to prove the page never reloaded.
8. **Drive the change** the way a developer works. The tool writes a source file. Only that file is compiled, by the agent standing in for the IDE. The hotswap sends a class update. The view rebuilds in place. The next action happens on the same page.

   ```bash
   javac -g -parameters --release 21 -cp "target/classes:$(cat cp.txt)" -d target/classes <changed files>
   ```

   The app log must show a class update for that one class and no page reload line. If it shows a reload, find out why before continuing.
9. **Chain actions** across files, in the layout and in the routed view, without reloading.
10. **Two tabs.** Open a second tab of the same app and check that shared state shows in both.
11. **Stop** the copy by its own port only and close the browser tabs that were opened.

Rules while driving:
- Use the browser tools the session provides, never a scripted browser runner. Never resize the window.
- Batch related cases, read compact results, few round trips. Verbose output goes to a log.
- Never `mvn compile` the module between steps and never reload the page.

## Report

- What was run and what was seen at each step: marker values, class update lines, file content.
- What was not run.
- The failure on the old jar next to the success on the new one.

"Verified live" without all of this is a false claim.
