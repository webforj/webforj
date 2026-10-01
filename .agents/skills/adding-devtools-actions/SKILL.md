---
name: adding-devtools-actions
description: Adds a new craftforJ server action with its response, model classes, registration and tests, following the existing actions of webforj-devtools. Use when the panel needs a new request on the action channel, or when a CraftforjActionHandler or a registerDefaultActions entry is to be added.
---

# Adding devtools actions

The API shape is approved by the maintainer before code is written. It adds one action and nothing beside it.

## Reference

Before writing, read these in full:

- `webforj-devtools/src/main/java/com/webforj/devtools/craftforj/action/CraftforjActionHandler.java`
- `webforj-devtools/src/main/java/com/webforj/devtools/craftforj/capabilities/action/GetCapabilitiesAction.java`
- `webforj-devtools/src/main/java/com/webforj/devtools/craftforj/CraftforjLifecycleListener.java`, method `registerDefaultActions`
- one sibling action of the same feature package and its test

The siblings teach the layout only. Where one breaks a rule of `writing-java` or `testing-java` (verbs off the list, more than five constructor parameters, a mutable model returned as the response, a test without `@Nested`), the rule wins.

## Steps

1. **Check for reuse.** Check that no existing action can return the data. Extending an existing response beats a second action. Data the application declares is read from the framework object that already holds it, never from a scan of the source tree.
2. **Place the class.** The action goes in `<feature>/action/` and is named `<Verb><Thing>Action`, verb from the closed list of `writing-java`.
3. **Test it first.** Write `<Action>Test` in the mirrored test package following `testing-java`: one file per class, `@Nested` per aspect. An action that writes source is tested whole file in, whole file out. See it fail.
4. **Write the action.**
   - It implements `CraftforjActionHandler<Response>`.
   - It holds `public static final String ACTION = "<feature>.<verbThing>"`, `getAction()` and `handle(JsonObject params)`.
   - Its collaborators come in through the constructor.
5. **Write the response.** The `Response` is a nested public static final class with a package private constructor and getters only. With more than three fields it uses the nested builder of `writing-java`.
6. **Put the rest in model.** Any other data class the action returns goes in `<feature>/model/`, data only. Logic goes in a service of the feature package, not in the action and not in the model.
7. **Handle bad input.** A missing or wrong parameter fails with `CraftforjActionException` and a message naming the parameter.
8. **Register it.** Register in `registerDefaultActions` next to its family. If the action writes, register it under the capability that gates it.
9. **Check the client contract.** The client lives in a separate repository. Its types must match the Java model one to one, including enum names. When no client change was asked for, the report says the client was not touched.
10. **Review.** Run `reviewing-java`, then the build steps of `verifying-changes`.

## Never

- Public API on a webforJ component, a servlet or filter, or a new loading path.
- A second action where an existing one can return the data.
