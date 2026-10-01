---
name: reviewing-java
description: Audits new or changed Java classes against the writing-java, testing-java and writing-prose skills before the work counts. Use after writing or moving Java code, after a subagent hands work back, and before reporting any Java change as done.
---

# Reviewing Java

Every class touched by the change is read in full and checked against the owning skills. A finding on a line the change touched is fixed in the same change. A finding on an untouched line is listed in the report and left alone, because public signatures and unrelated code stay as they are. This skill never changes behaviour.

## Steps

1. List the touched files with `git -C <repo> status --short`, `git -C <repo> diff --name-only` and `git -C <repo> diff --cached --name-only`. Read each Java file fully, fresh from disk.
2. Read `writing-java`, `testing-java` and `writing-prose` if they are not loaded yet.
3. For each class, go through every section of `writing-java` (naming, visibility, data and logic, member order, formatting, null safety, lifecycle, libraries), the JavaDoc, Comments and Never leak sections of `writing-prose`, and for tests every section of `testing-java`. Write every finding down as `file:line` and the rule it breaks.
4. Fix the findings on touched lines. Run the build steps of `verifying-changes`.
5. Run the audits below. The first five must print nothing on touched lines. The sixth prints the member list of one class, read it top to bottom against the member order of `writing-java`.
6. If anything is left on a touched line, go back to step 4.

## Audits

Run over the touched files only.

```bash
grep -n -E '^\s*//|/\*[^*]' <files>
grep -n '—\|–' <files>
grep -n -E '\brecord\b' <files>
grep -n -E '[^"./]\b([a-z]+\.){2,}[A-Z][A-Za-z]+' <files> | grep -v -E '^\S+:\s*(import|package) |@author|@since|Class\.forName'
grep -n -E '\{@link [a-z][a-z0-9.]+\.' <files>
grep -n -E '^  [A-Za-z<@]' <file> | grep -v -E '^[0-9]+:  (return|throw|if|for|while|else|try|\}|\*)'
```

## Report

A short list: each finding with `file:line`, the rule, and the fix made, then the findings on untouched lines that were left alone, then the verify result with its exit code.
