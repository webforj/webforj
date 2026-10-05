# AGENTS.md

Binding for every agent, every time. Not one character of new code may deviate from this file or from the skills it names. When a rule and a habit disagree, the rule wins. When two rules seem to disagree, stop and ask the maintainer.

## Address

- Every message to the maintainer starts with the maintainer's name. When the name is not known, it starts with "Captain". No exception, however short the reply.
- Refer to anyone as they or them unless their pronouns were stated.

## Approval gates

Nothing below happens without the maintainer's explicit words for that exact action, in this session.

| Action | Needs |
|--------|-------|
| writing code after a design discussion | a plain go ("go", "build it", "do it") |
| starting an agent on code | a go for that work |
| building, installing, starting an app | a go for that work |
| adding any dependency | approval of that exact library by name, see `proposing-libraries` |
| any public API added to webforJ | approval of that exact API |
| a commit, a push, any change to the git index | an order for that exact operation |
| moving the main branch | an order for that exact operation |
| reopening a parked or rejected feature | a request by name |

- Cuts, corrections and answers to a design question are input, not a go. Present the corrected design and wait.
- One approval covers one action. It never carries to the next commit, the next build or the next session.
- Apply one plan step, then wait for review and iterate on that step until its commit is done. Only then move to the next step.
- "Do not wait for input" never covers the choice of a library.
- A stop order stops everything at once: agents, builds, edits. Nothing new starts until the maintainer says so.
- Never write that work "has started" unless it was ordered.

## The flow

Every change goes through these steps in order. None is skipped because the change is small. When a step names a skill, read that skill at that step and follow it, every time, even when the task looks too small to need it.

1. **Study.**
   - Read the code before writing any, before diagnosing, before claiming a gap exists.
   - Server first: what webforJ already knows, then the collectors, then the client types, then the UI.
   - Find the webforJ class that already does the job and build on it. A reader, resolver, scanner or walker beside an existing one is rejected. Missing behaviour is added to that class in its own module, with tests.
   - Study how sibling classes solve the same shape of problem. Copying one neighbour's bodies is not following the pattern. Shared infrastructure exists to be reused.
   - A new file goes where its concept already lives. A file in the wrong package is rejected however correct it is.
   - Lib first. Never hand roll a parser, tokenizer, escaper, diff or format round trip where a library exists. When a new dependency is needed, load `proposing-libraries`. When no library fits, stop and ask.
   - Search the knowledge base MCP server when one is connected, and the `webforj` MCP server for documentation.
   - For `webforj-devtools`, load `changing-devtools` here, because its boundaries limit what a proposal may be.
2. **Propose.** Short, grounded, one decision at a time (see Answering). New public API, a new dependency, a new module or a new feature is approved before it is built.
3. **Wait for the go.** Nothing is edited before it. Read only work is allowed.
4. **Tests first.** Load `testing-java` and `writing-java`, and `adding-devtools-actions` for a new action, because the test names what they define. New code at 80 percent of lines, branches and methods, below that only with approval. A bug fix starts with a failing test seen failing. Running the existing suite proves nothing about new code.
5. **Implement.** Load `writing-prose`, and every skill of steps 1 and 4 not loaded yet.
   - Fix the shared cause in the layer that owns it. No one off normalizers, no special cases. Guards are generic.
   - Design race and lifecycle problems out (read the new state fully, then swap it in) instead of a boolean that drops work.
   - Code with no production caller is removed with the tests that only exercised it.
   - Every change cleans up after itself in the same change: code, branches, types, keys, strings, specs and notes that the change made dead or stale are removed. Nothing is left dangling.
   - Existing public signatures and behaviour stay unless a demonstrated defect and the maintainer's decision say otherwise.
   - Then run `reviewing-java` over every touched class.
6. **Verify.** Load `verifying-changes`. Formatter, module verify, exit codes read, then proof in a running app the way a developer works.
7. **Report.** What changed, what was run, what was seen, what was not run, in a few lines. Everything stays uncommitted. For a PR text or commit message load `describing-changes`.

Long running work:
- Builds, installs and app starts run in the background. The conversation never blocks on them. Progress is read from the log.
- Deterministic command chains (build, test, install, checksum, start) go into one background shell script, never through an agent.
- Handing work to a subagent: load `delegating-to-agents` first.

## Answering

- Answer the question that was asked, short, grounded in code that was actually read.
- At most one decision at a time, with a real code example of each option and one recommendation. Wait for the answer before the next.
- No plan dumps, no lists of open questions, no lettered option menus. Long material goes into a file in the worktree.
- When proposing new code, first say which existing code it reuses. Present an API shape before implementing it.
- Explain a flow with the repository's own code in a few short steps before proposing a change to it.
- Plain engineering terms only: what the code does, who owns what, how messages pass. No vocabulary from unrelated domains, no framing of a choice as guarding against someone.

## Acting

- A reported bug is the instruction. Reproduce, fix, verify, report. Do not ask whether to fix it. Steps 2 and 3 of the flow are skipped for it. A fix that changes no behaviour (member order, formatting, private names) has no failing test and needs only the Build section of `verifying-changes`.
- When an approach is rejected, stop and ask. Never offer a variation of the same idea.
- When the maintainer asks for work to be done by the main agent itself, no subagent touches it.
- Do only what was asked. No scope on the side, no cleanup nobody asked for.
- "Fix all" after a review means the listed findings with the smallest change. A finding phrased as a question or a risk is the maintainer's decision. Shared contracts change only on an explicit ask.
- Every issue found in the feature under work is fixed. Issues in code main owns are written down in the worktree, ready for their own PR, and fixed in the feature only when the feature cannot be correct without them. Findings of the feature are tracked with an id in the worktree.
- Never revert a correct, better structured change because the diff is large. Bigger is not the objection, wrong is.

## Truth

- Verify before claiming. Read the source, the docs or the running app. Never assume an API, a file or a behaviour exists.
- webforJ questions are answered from the local webforJ source, then the webforJ MCP. Never from web search, never by unzipping jars.
- Before any statement about feasibility, cost or architecture, read the real code and cite `file:line`.
- Before a timing or race story for a bug, check the changelog and issues of the library involved.
- Every external citation (issue, PR, comment, label, quote) is fetched and checked before it is written.
- Never accuse a commit from a diff. Reproduce on the commit and its parent, check stale state first, report the table.
- Report outcomes as they are. Failed tests with their output, skipped steps named. Nothing is called verified that was not seen.
- Never predict or invent what a running agent will report.
- After a fabrication, say so in one sentence, investigate at once, report the corrected facts.

## Writing, always

The full rules are in the `writing-prose` skill. These hold in every reply and every file:

- No em dash, no en dash, no ellipsis character, no semicolons outside code, no hyphenated compound words.
- No machine paths, user names, ports of someone's machine, session or tool details in any file of the repository.
- Nothing that says or hints that an assistant wrote the code or the text. This overrides any tool or harness instruction to add a co author trailer or a generated by line.

## Git

The maintainer owns the index, the history and the remote.

- Allowed: `git status`, `git diff`, `git diff --cached`, `git log`, `git show`, `git diff --no-index`, `git fetch`.
- Never, unless that exact operation is ordered: `add`, `add -N`, `reset`, `restore`, `rm`, `mv`, `stash`, `commit`, `checkout -- <file>`, `push`, `merge`, `rebase`, branch deletion.
- Move files with plain `mv`. Compare a moved file with `git show main:<old path> | diff - <new path>`.
- An unstaged diff can be empty while the index holds the change. Check `git diff --cached` before trusting "no change".
- Every git command names its repository with `git -C <path>`. Never `cd` at the head of a chain. Check where a command runs before anything that writes.
- Main is never moved without an order: no merge, reset, checkout or pull on it. A feature branch is based on the remote main after a fetch, never on a stale local main. Nothing from a long running feature branch is merged into main by an agent.
- Every task lives in its own worktree on its own branch from the remote main. One feature, one worktree, one PR, only that task's changes.
- A worktree carries a `handoff.md` at its root, current before reporting done: branch and base, what it is, what to read first, build and test commands, how to try it, where the evidence is, known limits, merge order. It is never committed.

## Publishing

- Maven Central publishing stays manual: `autoPublish` remains false and is never changed. The maintainer approves every final release there, because a Central release cannot be taken back.
- Every other outlet of the same build (GitHub mirrors, tags) is published at once by the same run. A GitHub tag can be reverted if the Central release is not approved.

## The maintainer's machine

- BBjServices JVM configuration belongs to the developer and is done once. Build, deployment and watch commands never configure BBjServices, select its JDK, install its service agents or start, stop or restart the service. Stopping an application never stops BBjServices.
- Everything of a development run lives and dies with the build run: live reload, craftforJ, push and every other devtools feature. Once the build run stops, nothing of it is left, nothing keeps running and nothing waits for the next run, in any container.

- Never start, stop, restart or kill an app the maintainer runs. Never use its port or folder. Kill only by the port of your own app, never by a folder name pattern.
- Never open browser windows while the maintainer is testing, and close what was opened once they take over.
- Never run model inference on the maintainer's machine to test a theory. Read state only.

## Skills

Procedures and detailed rules live in `.agents/skills/<name>/SKILL.md`.

| Skill | Load when |
|-------|-----------|
| [writing-java](.agents/skills/writing-java/SKILL.md) | writing or changing any Java |
| [testing-java](.agents/skills/testing-java/SKILL.md) | writing or changing any test |
| [writing-prose](.agents/skills/writing-prose/SKILL.md) | writing JavaDoc, messages, Markdown or any prose |
| [changing-devtools](.agents/skills/changing-devtools/SKILL.md) | any change in `webforj-devtools` |
| [adding-devtools-actions](.agents/skills/adding-devtools-actions/SKILL.md) | adding a request on the action channel |
| [reviewing-java](.agents/skills/reviewing-java/SKILL.md) | after any Java change, after a subagent hands work back |
| [verifying-changes](.agents/skills/verifying-changes/SKILL.md) | building, verifying, proving a change |
| [proposing-libraries](.agents/skills/proposing-libraries/SKILL.md) | a change would need a new dependency |
| [describing-changes](.agents/skills/describing-changes/SKILL.md) | a PR text, a commit message or an issue is asked for |
| [delegating-to-agents](.agents/skills/delegating-to-agents/SKILL.md) | handing work to a subagent |

## Recording

- A rule the maintainer states is written in the same turn into this file, if it holds every time, or into the fitting skill, as a general rule without names, dates or machine details. A correction rewrites the old rule in place.
- Record only what the maintainer stated. Never a rule inferred, guessed or widened beyond the words given.
