package com.webforj.rewrite.v26;

import static org.openrewrite.java.Assertions.java;

import org.junit.jupiter.api.Test;
import org.openrewrite.test.RecipeSpec;
import org.openrewrite.test.RewriteTest;

@SuppressWarnings("java:S2699")
class MigrateHtmlInSetTextTest implements RewriteTest {

  @Override
  public void defaults(RecipeSpec spec) {
    spec.recipe(new MigrateHtmlInSetText());
  }

  @Test
  void shouldMigrateHtmlWrappedSetTextToSetHtml() {
    rewriteRun(java("""
        import com.webforj.component.button.Button;

        class MyView {
          void render() {
            Button button = new Button();
            button.setText("<html><b>hi</b></html>");
          }
        }
        """, """
        import com.webforj.component.button.Button;

        class MyView {
          void render() {
            Button button = new Button();
            button.setHtml("<html><b>hi</b></html>");
          }
        }
        """));
  }

  @Test
  void shouldFlagHtmlWrappedSetTextWhenReceiverHasNoSetHtml() {
    rewriteRun(java("""
        import com.webforj.BusyIndicator;

        class MyView {
          void render(BusyIndicator indicator) {
            indicator.setText("<html><b>hi</b></html>");
          }
        }
        """,
        """
            import com.webforj.BusyIndicator;

            class MyView {
              void render(BusyIndicator indicator) {
                /*~~(webforJ 26: setText with <html> is deprecated and this type has no setHtml, handle manually. Removed in webforJ 27.)~~>*/indicator.setText("<html><b>hi</b></html>");
              }
            }
            """));
  }

  @Test
  void shouldIgnorePlainTextSetText() {
    rewriteRun(java("""
        import com.webforj.component.button.Button;

        class MyView {
          void render() {
            Button button = new Button();
            button.setText("Click Me");
          }
        }
        """));
  }

  @Test
  void shouldIgnoreNonLiteralSetText() {
    rewriteRun(java("""
        import com.webforj.component.button.Button;

        class MyView {
          void render(Button button, String message) {
            button.setText(message);
          }
        }
        """));
  }
}
