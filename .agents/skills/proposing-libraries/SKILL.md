---
name: proposing-libraries
description: Builds the evidence table for a dependency the maintainer must approve, then stops. Use whenever a change would need a library that is not already a dependency.
---

# Library proposal

No dependency is installed and no code is written against one until the maintainer names it. This skill only gathers facts and recommends.

## Steps

1. **State the need.** Write one sentence on what the library must do. First check that no existing dependency and no webforJ class already does it.
2. **Find candidates.** Find two to four, widely used and actively maintained.
3. **Fetch the facts.** For each candidate, fetch every fact from its registry page and its repository. Never from memory.
4. **Fill the table.**

   | Library | Weekly downloads | Stars | Last release | Maintainers | Licence | Size | Types or Java version | Fits the code |
   |---------|------------------|-------|--------------|-------------|---------|------|-----------------------|---------------|

   "Fits the code" says whether the library needs anything the project does not use, and whether its licence works inside a shipped jar.
5. **Recommend.** Give one recommendation in one sentence.
6. **Stop and wait.** If no candidate is widely used and maintained, say so and stop. Never fall back to a hand written version.

## Rules

- Archived, unmaintained or untyped libraries are named as such in the table.
- A library the maintainer rejected never comes back in a later proposal.
- "Do not wait for input" never covers this choice.
