<div align="center">

![webforJ: the full-stack Java framework for building business web apps and modernizing Swing apps in Java](https://raw.githubusercontent.com/webforj/.github/main/profile/cover-v3.png)

**Java is all you need.**

webforJ is the [full-stack Java framework](https://docs.webforj.com/blog/java-full-stack-framework) for business web apps: you build the whole app in Java, screens included, with no JavaScript and no REST layer to maintain. It works with any backend, standalone or with Spring Boot, it's AI-ready, and your Swing apps come along.

[![Maven Central](https://img.shields.io/maven-central/v/com.webforj/webforj-parent.svg?color=022495)](https://central.sonatype.com/artifact/com.webforj/webforj)
[![License: MIT](https://img.shields.io/badge/License-MIT-022495)](LICENSE)
[![Verify](https://github.com/webforj/webforj/actions/workflows/verify.yml/badge.svg)](https://github.com/webforj/webforj/actions/workflows/verify.yml)
[![Quality Gate Status](https://sonarcloud.io/api/project_badges/measure?project=webforj&metric=alert_status)](https://sonarcloud.io/summary/new_code?id=webforj)
[![OpenSSF Best Practices](https://www.bestpractices.dev/projects/8775/badge)](https://www.bestpractices.dev/projects/8775)

**[Start your first app](https://docs.webforj.com/docs/introduction/getting-started)** · [Documentation](https://docs.webforj.com) · [Blog](https://docs.webforj.com/blog) · [Book a call](https://webforj.com/schedule-discovery/)

</div>

## Just Java, from screen to data

A Java web framework and a Java UI framework in one: components, routing, forms with binding and validation, tables, security, translation and tests. Your views are Java classes, and your Spring services go straight into them. Data comes from Spring Data, JPA or any source through a [repository](https://docs.webforj.com/docs/advanced/repository/overview).

```java
@Route("/orders")
public class OrdersView extends Composite<FlexLayout> {
  private final FlexLayout self = getBoundComponent();
  private final Table<Order> table = new Table<>();

  public OrdersView(OrderService orders) {
    table.addColumn("Order", Order::number);
    table.addColumn("Customer", Order::customer);
    table.addColumn("Total", Order::total);
    table.setItems(orders.findOpen());

    Button refresh = new Button("Refresh", ButtonTheme.PRIMARY,
        e -> table.setItems(orders.findOpen()));

    self.setDirection(FlexDirection.COLUMN).add(refresh, table);
  }
}
```

Generate a project in the browser with [startforJ](https://docs.webforj.com/startforj).

## AI that writes real webforJ

One install of the [webforJ AI plugin](https://docs.webforj.com/docs/ai-tooling) teaches Claude Code, GitHub Copilot, Cursor, Gemini CLI and Codex the real API.

```bash
claude plugin marketplace add webforj/webforj-ai
claude plugin install webforj@webforj-ai
```

- **[MCP server](https://docs.webforj.com/docs/ai-tooling/mcp) and [Agent Skills](https://docs.webforj.com/docs/ai-tooling/agent-skills):** answers from current docs for your version, and step-by-step workflows for forms, components, styling, security, async work, localization and upgrades.
- **[Browserless testing](https://docs.webforj.com/docs/testing/testbed/overview):** views run in plain JUnit tests, so your assistant gets a pass or a fail after every change.
- **Agents that use your app:** an agent reads the page, finds components and clicks like a user, with exactly the user's permissions. [MCP Apps](https://docs.webforj.com/docs/integrations/mcp-apps/overview) open a webforJ view inside an AI conversation.

```java
String page = Snapshot.render(view);
var export = ComponentLocator.of(Button.class).byText("Export").locate();
UserInteraction.click(export);
```

## Visual editing that writes Java

[craftforJ](https://docs.webforj.com/docs/craftforj) edits your running app in the browser. Click a component, change it, and write the edits you keep back into your Java source. The [craftforJ assistant](https://docs.webforj.com/docs/ai-tooling/craftforj-assistant) takes a goal, writes Java, compiles and checks it, and nothing reaches your source without your approval.

## Swing to web, one screen at a time

Run your Swing, JavaFX or SWT app in the browser with [Webswing](https://docs.webforj.com/docs/integrations/webswing/overview), then move it to webforJ one screen at a time while old and new screens [talk to each other](https://docs.webforj.com/docs/integrations/webswing/communication). Read the [Swing to web migration guide](https://docs.webforj.com/blog/swing-to-web-migration). Backed by BASIS International's almost 40 years of modernizing mission-critical systems, including 20+ years of delivering technology built on Java.

## One design system

Every component comes from one design system with themes, light and dark modes and design tokens, plus the [DWC Design Kit for Figma](https://www.figma.com/community/file/1682060886525639971/) for your designers.

## Talk to us

Ask a question, report a bug or plan a project. We speak English, German, French and Spanish. To contribute code, read the [contributing guide](CONTRIBUTING.md).

[![Ask the docs](https://img.shields.io/badge/Ask%20the%20docs-022495?style=for-the-badge&logo=algolia&logoColor=white)](https://docs.webforj.com) [![Report a bug](https://img.shields.io/badge/Report%20a%20bug-24292f?style=for-the-badge&logo=github&logoColor=white)](https://github.com/webforj/webforj/issues/new/choose) [![Book a call](https://img.shields.io/badge/Book%20a%20call-022495?style=for-the-badge&logo=data:image/svg%2Bxml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCAyNCAyNCI+PHBhdGggZmlsbD0id2hpdGUiIGQ9Ik03IDJoMnYyaDZWMmgydjJoMmEyIDIgMCAwIDEgMiAydjE0YTIgMiAwIDAgMS0yIDJINWEyIDIgMCAwIDEtMi0yVjZhMiAyIDAgMCAxIDItMmgyVjJ6bTEyIDhINXYxMGgxNFYxMHoiLz48L3N2Zz4=&logoColor=white)](https://webforj.com/schedule-discovery/)

[![Follow webforJ on LinkedIn](https://img.shields.io/badge/LinkedIn-0A66C2?style=for-the-badge&logo=data:image/svg%2Bxml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCAyNCAyNCI+PHBhdGggZmlsbD0id2hpdGUiIGQ9Ik0yMC40NSAyMC40NWgtMy41NnYtNS41N2MwLTEuMzMtLjAyLTMuMDQtMS44NS0zLjA0LTEuODUgMC0yLjE0IDEuNDUtMi4xNCAyLjk0djUuNjdIOS4zNVY5aDMuNDF2MS41NmguMDVjLjQ4LS45IDEuNjQtMS44NSAzLjM3LTEuODUgMy42IDAgNC4yNyAyLjM3IDQuMjcgNS40NnY2LjI4ek01LjM0IDcuNDNhMi4wNiAyLjA2IDAgMSAxIDAtNC4xMyAyLjA2IDIuMDYgMCAwIDEgMCA0LjEzek03LjEyIDIwLjQ1SDMuNTZWOWgzLjU2djExLjQ1eiIvPjwvc3ZnPg==&logoColor=white)](https://www.linkedin.com/company/webforj) [![webforJ on YouTube](https://img.shields.io/badge/YouTube-FF0000?style=for-the-badge&logo=youtube&logoColor=white)](https://www.youtube.com/@webforJ) [![webforJ blog](https://img.shields.io/badge/Blog-F26522?style=for-the-badge&logo=rss&logoColor=white)](https://docs.webforj.com/blog)

## License

webforJ is open source under the [MIT License](LICENSE).

---

<div align="center">

**Do it all in Java.**

[webforj.com](https://webforj.com) · [JavaDocs](https://javadoc.io/doc/com.webforj)

</div>
