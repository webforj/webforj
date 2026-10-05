---
name: changing-devtools
description: Keeps changes to the craftforJ server inside its boundaries for extension, transport, source writing, product scope and history. Use before proposing or making any change in webforj-devtools, and when a task touches contributions, the action channel, servlets or filters, source rewriting, compiling, AI tools, Kotlin support or undo history.
---

# Changing devtools

Never crossed, in any shape or form.

## Extension

- craftforJ never adds public API to webforJ components. Not an annotation, not a method, not a manifest. Per component data comes only from contribution classes, discovered the way `FeatureHandler` contributions are.
- craftforJ never searches or scans for declarations. Declarations are pushed through contributions.

## Transport

- No servlets, no filters, no proxy, no request intercepting or HTTP serving code of any kind. What must reach the browser goes through what exists: the action channel, inline page scripts, classpath resources the containers already serve. If none fits, stop and ask.
- The server never relays or authenticates outbound calls for the browser.
- The server defines no markup and no drawing instructions. Contributions return data only. Pictures are shipped image resources or a placeholder.
- One loading path for the panel modules. Extend the loader or the manifest, never add another.

## Source writing

- craftforJ never writes class output. The in memory check of `CompileValidator` stays as it is. A feature writes the source file and stops. The IDE compiles.
- Every feature that changes a file goes through the one source pass of the engine. Features depend on the engine, never on each other.
- Line numbers are a hint. The anchor is AST identity: type, variable, enclosing class. Every declaration lookup is type checked. When identity is unclear the write is refused, never guessed.
- Setter updates are scoped to the block the write belongs to, never the whole file. `this.field` counts as the field.
- A save never reports success for a write the running app cannot show.
- Generated lines are wrapped to 100 columns in the printed text layer. Lines the user wrote stay byte identical.
- Writes of files are atomic, temp file and move, with a hash compare before the swap.
- Stylesheet writes keep one top level block per prelude. Edits are exact text matches, never a CSS round trip.
- A fix strengthens the shared layer it belongs to. Public signatures and supported behaviour stay.

## Product scope

- An AI tool takes the exact path the manual action takes. Never a parallel implementation, never a domain object built by hand.
- The AI assistant is for short debugging sessions. Heavy workflows are deterministic UI flows, not AI tools.
- A capability that is not answered is unknown, not off. Only a real server answer changes it.
- No hardcoded snapshot of external data: icons, DWC internals, catalogs. Resolve live from the app or the framework.
- Kotlin sources are read only. Kotlin is detected per class, write surfaces are hidden for it, never shown disabled.

## History and undo

- One user action is one step. One request is one step. Apply is one step. A user action is never split into several requests to get finer steps.
- The server records a step around one action handler, through one project file writer. Write actions themselves carry no history code.

## Public surface

- Nothing in the devtools packages is public API. Classes, type hierarchies, names and methods can change, move or be removed in any release without prior notice. The contracts documented in the public webforJ documentation are the only supported surface.
- Every devtools package carries a `package-info.java` that states this. A new package gets one.

## Hotswap tools

- The devtools exist on the developer's machine only. The build plugin is the sole channel that puts them on a development classpath, in every container, and a deployment never carries them. An application never declares them to get the development loop.
- webforJ never bundles, mirrors or redistributes a hotswap tool. The build plugin downloads it from its publisher at the developer's opt in. No pom names a tool in any scope and no tool type appears in webforJ source.
- A tool is addressed only through public documented entry points, by name, from its own receiver in the MIT devtools jar, and never from BBj or any proprietary artifact. Everything works when the tool is absent.
