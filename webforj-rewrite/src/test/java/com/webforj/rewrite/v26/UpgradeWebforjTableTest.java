package com.webforj.rewrite.v26;

import static org.openrewrite.java.Assertions.java;

import org.junit.jupiter.api.Test;
import org.openrewrite.test.RecipeSpec;
import org.openrewrite.test.RewriteTest;

@SuppressWarnings("java:S2699")
class UpgradeWebforjTableTest implements RewriteTest {

  @Override
  public void defaults(RecipeSpec spec) {
    spec.recipeFromResources("com.webforj.rewrite.v26.UpgradeWebforjTable");
  }

  @Test
  void flagsIconRendererStringConstructor() {
    rewriteRun(java("""
        import com.webforj.component.table.renderer.IconRenderer;

        class MyView {
          IconRenderer<String> renderer = new IconRenderer<>("heart");
        }
        """,
        """
            import com.webforj.component.table.renderer.IconRenderer;

            class MyView {
              IconRenderer<String> renderer = /* TODO webforJ 26: Removed. Use new IconRenderer(new Icon(name, pool)) instead. */ /*~~>*/new IconRenderer<>("heart");
            }
            """));
  }

  @Test
  void flagsIconRendererStringPoolConstructor() {
    rewriteRun(java("""
        import com.webforj.component.table.renderer.IconRenderer;

        class MyView {
          IconRenderer<String> renderer = new IconRenderer<>("star", "custom");
        }
        """,
        """
            import com.webforj.component.table.renderer.IconRenderer;

            class MyView {
              IconRenderer<String> renderer = /* TODO webforJ 26: Removed. Use new IconRenderer(new Icon(name, pool)) instead. */ /*~~>*/new IconRenderer<>("star", "custom");
            }
            """));
  }
}
