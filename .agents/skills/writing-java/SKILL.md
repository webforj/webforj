---
name: writing-java
description: Applies the webforJ Java patterns for naming, visibility, data classes in model, member order, formatting, lifecycle and framework use. Use before writing or changing any Java code in this repository.
---

# Java patterns

New code copies the layout of the reference classes of its module. Where a reference or a sibling class breaks a rule of this skill, the rule wins. Existing public names stay as they are.

Reference classes of the craftforJ server:

| Kind | Reference |
|------|-----------|
| service | `webforj-devtools/src/main/java/com/webforj/devtools/craftforj/source/staging/SourceStagingArea.java` |
| data class | `webforj-devtools/src/main/java/com/webforj/devtools/craftforj/source/staging/model/StagedFile.java` |
| store on disk | `webforj-devtools/src/main/java/com/webforj/devtools/craftforj/keys/CraftforjKeyStore.java` |

## Naming

A name says what it is. Reading a symbol anywhere, in a file, a diff or a message, must be enough to know what it does and which part of the code owns it.

### Methods

Every method name starts with a verb from this closed list, then the noun it acts on. Public or private, the list is the same. Test methods follow the `shouldX` naming of the module, test helpers follow the list.

| Group | Verbs |
|-------|-------|
| Values and predicates | `get` `set` `is` `has` `can` |
| Lifecycle | `create` `build` `init` `reset` `destroy` |
| Data | `load` `fetch` `read` `write` `save` `parse` `format` `resolve` `find` `merge` |
| Mutation | `add` `remove` `clear` `update` `apply` `toggle` |
| Flow | `start` `stop` `open` `close` `show` `hide` `send` `emit` |
| Events | `handle` |
| Conversion | `to` |

Nothing else. `ensureX`, `compactX`, `makeX`, `doX`, `processX`, `checkX`, `computeX` are rejected. `makeX` is `createX`. A check that answers yes or no is `isX` or `hasX`.

Overrides of JDK, framework and library contracts are exempt, because the name belongs to someone else (`close`, `run`, `equals`, `hashCode`, `toString`, `computeValue`). The names of a reference API the maintainer named as the model are exempt too.

### Accessors

- `getX()` and `isX()`, never `x()`. Setters are `setX(...)`.
- An immutable class exposes no setters. No `withX` copies.

### Classes

- A class name says its role in the owning domain: `<Domain><Thing>`, the way the module already names its classes. Look at the package and copy its pattern.
- A data class is a noun. A service is a noun of what it owns (`SourceStagingArea`, `CraftforjKeyStore`). An action is `<Verb><Thing>Action`.
- No name is declared twice in a module. A bare `Helper`, `Util`, `Manager` or `Handler` without its domain is rejected.

### Words webforJ already owns

Never use a webforJ word for something else: layout, slot, route, outlet, scope, shape, template, theme, component, composite, element, frame, window, item, concern. If the class is not about that webforJ thing, pick the plain word for what it does (whitespace, indentation, text, source).

### Standard words only

Never invent a word where a standard one exists. Use the term the Java language, the JLS, the library in use or webforJ already has: varargs, receiver, scope of a call, unconditional, side effect, declaration, initializer. A coined word is acceptable only when no standard term exists, and then it is explained once in the class JavaDoc.

### Names from other frameworks

Generic industry words are fine even when another framework uses them (`first`, `all`, `count`, `filter`, `of`, `within`). A name another framework invented for its own API is never used, nor its class names, file names or method families.

### craftforJ

- The public name is craftforJ. It replaces "DevTools" in every user visible string.
- Java code sits under `com.webforj.devtools.craftforj`. Classes are `Craftforj*`, never `CraftForJ*`.
- Configuration keys are `webforj.devtools.craftforj.*`.
- The Maven artifact `webforj-devtools`, wire protocol strings and livereload keep their names.

## Visibility

- Public only when another package calls it. A method, class or listener used by one class in one package is package private or private.
- No speculative API. Nothing is public "for later" or "for tests".
- A class that is not meant to be extended is `final`.
- Constructors used only inside the package are package private.

## Data and logic

- Data carriers live in a `model` subpackage beside the services that use them. Never next to the services.
- A model class holds data only: fields, constructors, getters, setters where it is mutable. No rules, no lookups, no id generation, no validation flows, no helpers.
- The logic lives in services in the feature package.
- An action's `Response` class stays nested in its action handler.
- No Java records, ever. A data carrier is a class with private fields and `getX()` and `isX()` accessors. Immutable means no setters.
- More than about three constructor arguments means a nested `Builder` with `create(...)`, `setX(...)`, `build()`. Never more than five constructor parameters.
- Lists handed out by an immutable class are unmodifiable copies.
- A field that selects a kind or an operation is an enum, never `static final String` constants. An enum that qualifies one class is nested inside it. Plain uppercase constants, no serialization annotations.
- Identity is strongly typed. No string ids with reserved values, no string concatenation as a key. Typed factories carry identity. Strings are allowed only for real data.
- Several variants of one operation are one method with an enum parameter, never one method per variant.

## Member order

Never interleaved. A private method between two public ones is rejected. Static methods sit with the methods of their visibility.

1. static fields, constants first
2. instance fields
3. constructors
4. public methods
5. package private methods
6. protected methods
7. private methods, instance and static
8. nested classes, private ones last

## Formatting

- Two space indent, 100 columns, Google Java style as the formatter writes it. The formatter is right where it disagrees with checkstyle. Never hand edit around it.
- No wildcard imports. No fully qualified type names in code or JavaDoc, import the type. Exceptions: a class name string for reflection, and two types with the same simple name in one file.
- A blank line before `return` only when the return closes a block of three or more statements. A body of one or two statements has the return directly under the line above.
- Multiline or structured strings (embedded JavaScript, JSON, source fixtures) are text blocks with `.formatted(...)`, never `+` chains.
- JavaDoc and comments follow the `writing-prose` skill.

## Null safety

- Changes to annotations never change behaviour. No new exceptions, no fallbacks, no logic.
- Setters never accept null unless the maintainer asked for it.

## Lifecycle

- No static cache that outlives the restart class loader. Class keyed caches use `ClassValue`. A static holder in a webforJ jar is reset on every Spring restart while the old instance stays alive.
- Anything that owns a port or a thread starts and stops with the application context. State that must cross a restart goes into a system property.
- A new `webforj.*` configuration key under Spring needs its typed property and the config builder entry, or it never reaches the app.

## Libraries and shared services

- Text and HTML escaping, stripping and sanitizing go through jsoup only. Never a hand written escaper.
- Every Java parse in the devtools module goes through `SourceParserService`. No other class constructs a parser. The language level is resolved from the runtime, never hardcoded, and never below Java 21.
- Bean reading goes through webforJ's own `BeanIntrospection`. Missing behaviour is added there, with tests.

## Components and the framework

- Study sibling components and the `concern` interfaces before changing a component. Reuse the shared infrastructure (concerns, slots, `ElementComposite`) instead of copying a neighbour.
- Components extend `DwcComponent` or `Composite`. Understand the difference before touching either hierarchy.
- A new component ships its Kotlin DSL update in the same change.
- webforJ public API is frozen from feature modules. A feature needs a change to foundation API only with the maintainer's approval.
