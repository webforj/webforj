---
name: testing-java
description: Applies the webforJ test rules for layout, assertions, whole file source transformation tests and coverage, and keeps every existing test. Use before writing or changing any Java test.
---

# Java tests

JUnit 5 and Mockito.

## Layout

- One test file per production class, named after it, in the same package under `src/test/java`. Several aspects of one class are `@Nested` classes inside that one file.
- No themed scenario files across classes, no second test file for the same class with another suffix.
- A test file over about 500 lines or with eight or more `@Nested` groups is split into a subpackage named after the class, one file per group, `<Class><Concern>Test`.
- Shared fixtures are plain classes named for what they hold, never after a production class.
- Test classes and methods are package private and carry no JavaDoc.

## Assertions

- A call that must not throw is `assertDoesNotThrow(...)`, never try, catch and `fail`.
- The lambda of `assertThrows` holds the one call under test. Its arguments are typed locals declared right before it.
- Several cases of one shape are one `@ParameterizedTest` with a private static `@MethodSource`.

## Source transformation tests

Any test of code that rewrites Java source has exactly one form.

1. A text block with the whole source file going in.
2. The operation.
3. `assertEquals` of a text block with the whole expected file against what was written.

- A refusal asserts the exact message and that the file is byte identical.
- An operation over two files shows and compares both files.
- A dry run compares the whole patched text and that the disk is untouched.
- Never `contains`, `indexOf`, `endsWith`, a returned name or a boolean on a package private helper. Helpers of a rewriting feature are tested through the public entry point with whole file cases.
- Embedded code is a text block. Tabs are written as `\t` escapes.
- A long line inside a sample is fixed by a shorter name in the sample, never by wrapping the sample line.
- Code generation tests are one case per input: the input as it is, the whole generated file as it is.

## Tests are accumulated fixtures

- Never delete or weaken an existing test. Each one is a case that happened. A change that makes an existing test fail is wrong, not the test.
- The exception is a test of code that was removed because it lost its last caller. It goes with that code.
- Fixing a finding never changes the test count or a single character of an existing text block.

## Coverage

- New code at 80 percent of lines, branches and methods, tests written first.
- A bug fix starts with a failing test that was seen failing.
