package com.webforj.rewrite.v26;

import static org.openrewrite.java.Assertions.java;

import org.junit.jupiter.api.Test;
import org.openrewrite.test.RecipeSpec;
import org.openrewrite.test.RewriteTest;

@SuppressWarnings("java:S2699")
class UpgradeWebforjWebswingTest implements RewriteTest {

  @Override
  public void defaults(RecipeSpec spec) {
    spec.recipeFromResources("com.webforj.rewrite.v26.UpgradeWebforjWebswing");
  }

  @Test
  void flagsSetAutoReconnect() {
    rewriteRun(java("""
        import com.webforj.component.webswing.WebswingOptions;

        class MyApp {
          void configure() {
            new WebswingOptions().setAutoReconnect(5000);
          }
        }
        """, """
        import com.webforj.component.webswing.WebswingOptions;

        class MyApp {
          void configure() {
            /* TODO webforJ 26: Removed. Configure directly in the Admin Console. */
            /*~~>*/new WebswingOptions().setAutoReconnect(5000);
          }
        }
        """));
  }

  @Test
  void flagsSetPingParams() {
    rewriteRun(java("""
        import com.webforj.component.webswing.WebswingOptions;

        class MyApp {
          void configure() {
            WebswingOptions opts = new WebswingOptions();
            opts.setPingParams(new WebswingOptions.PingParams());
          }
        }
        """, """
        import com.webforj.component.webswing.WebswingOptions;

        class MyApp {
          void configure() {
            WebswingOptions opts = new WebswingOptions();
            /* TODO webforJ 26: Removed. Configure directly in the Admin Console. */
            /*~~>*/opts.setPingParams(new /*~~>*/WebswingOptions.PingParams());
          }
        }
        """));
  }
}
