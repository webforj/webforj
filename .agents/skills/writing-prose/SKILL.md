---
name: writing-prose
description: Applies the writing rules for JavaDoc, comments, user facing messages, Markdown and replies, including what never leaks into the repository. Use before writing any prose, JavaDoc or message.
---

# Writing

Applies to JavaDoc, comments, strings, messages, Markdown and replies. Commits, PRs and issues are in the `describing-changes` skill and follow these rules too.

## Characters and punctuation

- No em dash and no en dash, anywhere. ASCII hyphen, comma, parentheses or two sentences instead.
- No semicolons in prose, comments, JavaDoc, strings or shell commands written for a reader. Semicolons appear only as statement terminators in code.
- No colons inside JavaDoc sentences. Split the sentence.
- No ellipsis character.
- No hyphens in compound words: open source, long term, read only, round trip.
- Ranges are written "0 to 1".

## Voice

- Plain, flat, factual. Third person. No "you", no "we" in documentation and PRs.
- Banned words and phrases: stands as a testament, plays a vital role, underscores, robust, comprehensive, facilitates, leverages, not only but also, it is worth noting, in summary, overall, in conclusion, seamless, powerful, elegant, out of the box, for free, in practice, drop in, battle tested.
- No blog voice, no grand framing, no symmetric flourish endings.

## JavaDoc

- On every class and every public member.
- A package private or private member carries JavaDoc only when what it does is complex enough that its name and signature cannot say it. Same rules, what, never how.
- Says what, never how. No implementation detail, no mechanism, no reasoning, no history, no comparison with another design.
- Terse, in the register of the surrounding code. A one line summary sentence ending with a period.
- Further paragraphs are wrapped as `<p>` on its own line, the text, `</p>` on its own line. A blank `*` line between paragraphs and before the block tags.
- Every thrown exception is documented with `@throws`.
- `{@link Type}` with the simple name, never a fully qualified name.
- Every file's class JavaDoc ends with `@author`, copied from the neighbouring classes of the module, and `@since <next release>`, the version of the current SNAPSHOT in `pom.xml` without the suffix (26.03 for 26.03-SNAPSHOT), never copied from neighbouring classes. `@since` goes on the class and nested classes, not on each method.
- Test classes carry no JavaDoc.
- A JavaDoc paragraph the maintainer deleted stays deleted. Read the file fresh before editing any comment and edit only what is there.

## Comments

- No line comments and no block comments in Java. The code and the commit message carry the reasoning.
- No comment that explains another system's internals.
- No phrase coined during a conversation ever reaches the source: no prompt wording, no design talk, no "intentionally", "by design", "to match", "for consistency".
- Never "mirrors X" or any coupling claim in source.
- Never name a foreign framework in webforJ source, JavaDoc, comments, error text or tests, not as a comparison, an inspiration or a parallel. Describe the behaviour in webforJ's own words.

## Never leak

- No machine paths, user names, home folders, ports of someone's machine, session or tool details in any file of the repository.
- No internal cross references a reader has no context for, no process notes, no test plans in the artifact itself.
- No internal ids in any text a user sees. Resolve the object and show its display name.
- Nothing that says or hints that an assistant wrote the code or the text, anywhere.

## Messages to users

- A user facing error states what happened and what to do, calmly. The raw detail goes to the log.
- Split messages per cause only when the user can act differently on each cause, and only on errors the code really produces.

## Documentation files

- No em dashes, no AI tells. Settings are tables with Setting, Default, Description.
