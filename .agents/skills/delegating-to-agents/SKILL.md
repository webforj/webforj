---
name: delegating-to-agents
description: Hands approved execution work to subagents with a complete brief, an explicit model and a full review of their output. Use before spawning any subagent.
---

# Agent briefs

## Who does what

- The main agent plans, decides and does the hard work. Planning, discovery and design are never delegated.
- Subagents get concrete, approved execution work only.
- When the maintainer says the main agent does the work itself, no subagent is used.
- Builds, installs, deploys and app restarts are never delegated. They run as one background shell script.

## The brief

No subagent ever works without the rules. Every brief contains:

1. The instruction to read `AGENTS.md` and to load every skill it names for the task (`writing-java`, `testing-java`, `writing-prose`, `changing-devtools` as they apply) before touching anything, and to follow them to the character.
2. The reference classes to copy the style from, named by path inside the repository.
3. The exact scope: files to touch, files not to touch, the expected result.
4. The checks to run for what it touched only, and that it never builds the whole project.
5. That it never commits, stages, stashes or pushes.

## Models

- Every spawn names its model explicitly. A fast model for mechanical work with a fully specified direction, a strong reasoning model only for work that needs judgment. Never an agent that inherits the main agent's full context.
- Every agent runs in the background. The main agent keeps answering the maintainer while it runs and relays its result when it arrives.

## Review

- The main agent reads every line a subagent wrote before it counts. Code that was not read in full is not done.
- Every new class goes through the `reviewing-java` skill: visibility, member order, data in `model`, verbs, JavaDoc, no comments that explain.
- What the subagent got wrong is fixed by the main agent or sent back with a precise correction.

## Councils

- When a design has been rejected more than once, a council of up to three strong reasoning agents with distinct lenses (reference products, correctness against the contract, real use) judges the next one before it is shown. The main agent converges their findings into one design.
- A considerable chunk of finished work is judged the same way against the goal and the scale the maintainer stated, after it is built and driven.
