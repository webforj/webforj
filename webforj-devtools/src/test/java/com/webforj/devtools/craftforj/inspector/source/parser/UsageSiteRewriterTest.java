package com.webforj.devtools.craftforj.inspector.source.parser;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.github.javaparser.JavaParser;
import com.github.javaparser.ParserConfiguration;
import com.github.javaparser.ast.CompilationUnit;
import com.webforj.devtools.craftforj.inspector.source.SourceModificationException;
import com.webforj.devtools.craftforj.inspector.source.model.TargetContext;
import java.util.List;
import java.util.Set;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

@DisplayName("UsageSiteRewriter")
class UsageSiteRewriterTest {

  private static final String EXPLORE_SOURCE = """
      package com.example;

      public class Explore {
        public Explore(String message, String iconName, String ctaLabel) {
          Paragraph messageLabel = new Paragraph(message);
          Button cta = new Button(ctaLabel)
              .setTheme(ButtonTheme.PRIMARY);
          add(messageLabel, cta);
        }
      }
      """;

  private static final String DASHBOARD_SOURCE = """
      package com.example;

      public class DashboardView {
        public DashboardView() {
          add(new Explore("Your dashboard is empty", "layout-dashboard", "Create widget"));
        }
      }
      """;

  private static final JavaParser PARSER = new JavaParser(
      new ParserConfiguration().setLanguageLevel(ParserConfiguration.LanguageLevel.JAVA_17));

  private CompilationUnit parse(String source) {
    return PARSER.parse(source).getResult().orElseThrow();
  }

  private TargetContext target(int line, String typeName) {
    TargetContext target = new TargetContext(line, typeName);
    target.setAcceptableTypes(Set.of(typeName));

    return target;
  }

  @Nested
  class Trace {

    @Test
    @DisplayName("traces a creation argument to the enclosing constructor parameter")
    void shouldTraceCreationArgument() {
      List<UsageSiteRewriter.Trace> traces =
          UsageSiteRewriter.trace(parse(EXPLORE_SOURCE), target(6, "Button"), "setText");

      assertEquals(1, traces.size());
      assertEquals("Explore", traces.get(0).className());
      assertEquals(2, traces.get(0).parameterIndex());
      assertEquals(3, traces.get(0).parameterCount());
      assertFalse(traces.get(0).fromSetter());
    }

    @Test
    @DisplayName("traces a setter call passing a constructor parameter")
    void shouldTraceSetterParameter() {
      String source = """
          package com.example;

          public class Card {
            public Card(String title) {
              Button action = new Button();
              action.setText(title);
            }
          }
          """;

      List<UsageSiteRewriter.Trace> traces =
          UsageSiteRewriter.trace(parse(source), target(5, "Button"), "setText");

      assertEquals(1, traces.size());
      assertEquals(0, traces.get(0).parameterIndex());
      assertEquals(1, traces.get(0).parameterCount());
      assertTrue(traces.get(0).fromSetter());
    }

    @Test
    @DisplayName("traces a chained setter on the creation expression")
    void shouldTraceChainedSetter() {
      String source = """
          package com.example;

          public class Card {
            public Card(String title) {
              Button action = new Button()
                  .setTheme(ButtonTheme.PRIMARY)
                  .setText(title);
            }
          }
          """;

      List<UsageSiteRewriter.Trace> traces =
          UsageSiteRewriter.trace(parse(source), target(5, "Button"), "setText");

      assertEquals(1, traces.size());
      assertTrue(traces.get(0).fromSetter());
    }

    @Test
    @DisplayName("returns empty when a setter carries a hardcoded value")
    void shouldRejectHardcodedSetter() {
      String source = """
          package com.example;

          public class Card {
            public Card(String title) {
              Button action = new Button(title);
              action.setText("hardcoded");
            }
          }
          """;

      assertTrue(UsageSiteRewriter.trace(parse(source), target(5, "Button"), "setText").isEmpty());
    }

    @Test
    @DisplayName("returns empty for a field initializer no constructor wires up")
    void shouldRejectFieldInitializer() {
      String source = """
          package com.example;

          public class Card {
            private Button action = new Button("fixed");
          }
          """;

      assertTrue(UsageSiteRewriter.trace(parse(source), target(4, "Button"), "setText").isEmpty());
    }

    @Test
    @DisplayName("traces a field-initialized component through the constructor's setter")
    void shouldTraceFieldInitializedComponent() {
      String source = """
          package com.example;

          public class SignalCard {
            private final Span value = new Span();

            public SignalCard(String label, String value) {
              this.value.setText(value);
            }
          }
          """;

      List<UsageSiteRewriter.Trace> traces =
          UsageSiteRewriter.trace(parse(source), target(4, "Span"), "setText");

      assertEquals(1, traces.size());
      assertEquals("SignalCard", traces.get(0).className());
      assertEquals(1, traces.get(0).parameterIndex());
      assertEquals(2, traces.get(0).parameterCount());
      assertTrue(traces.get(0).fromSetter());
    }

    @Test
    @DisplayName("returns empty when a field-initialized component's setter is hardcoded")
    void shouldRejectFieldInitializedHardcodedSetter() {
      String source = """
          package com.example;

          public class SignalCard {
            private final Span value = new Span();

            public SignalCard(String label) {
              this.value.setText("fixed");
            }
          }
          """;

      assertTrue(UsageSiteRewriter.trace(parse(source), target(4, "Span"), "setText").isEmpty());
    }

    @Test
    @DisplayName("returns empty when no creation of the type exists at the line")
    void shouldRejectMissingCreation() {
      assertTrue(
          UsageSiteRewriter.trace(parse(EXPLORE_SOURCE), target(6, "Slider"), "setText").isEmpty());
      assertTrue(UsageSiteRewriter.trace(parse(EXPLORE_SOURCE), target(99, "Button"), "setText")
          .isEmpty());
    }

    @Test
    @DisplayName("traces a component assigned to a field in the constructor")
    void shouldTraceFieldAssignment() {
      String source = """
          package com.example;

          public class Card {
            private Button action;

            public Card(String title) {
              action = new Button(title);
            }
          }
          """;

      List<UsageSiteRewriter.Trace> traces =
          UsageSiteRewriter.trace(parse(source), target(7, "Button"), "setText");

      assertEquals(1, traces.size());
      assertEquals(0, traces.get(0).parameterIndex());
    }

    @Test
    @DisplayName("matches the creation without acceptable types configured")
    void shouldMatchWithoutAcceptableTypes() {
      TargetContext bare = new TargetContext(6, "");

      assertFalse(UsageSiteRewriter.trace(parse(EXPLORE_SOURCE), bare, "setText").isEmpty());
    }

    @Test
    @DisplayName("ignores creation arguments that are not parameter names")
    void shouldIgnoreNonParameterArguments() {
      String source = """
          package com.example;

          public class Card {
            public Card(String title) {
              Button action = new Button(title.trim(), FIXED);
            }
          }
          """;

      assertTrue(UsageSiteRewriter.trace(parse(source), target(5, "Button"), "setText").isEmpty());
    }

    @Test
    @DisplayName("ignores setters on other variables")
    void shouldIgnoreOtherVariables() {
      String source = """
          package com.example;

          public class Card {
            public Card(String title) {
              Button other = new Button();
              Button action = new Button(title);
              other.setText("something");
            }
          }
          """;

      List<UsageSiteRewriter.Trace> traces =
          UsageSiteRewriter.trace(parse(source), target(6, "Button"), "setText");

      assertEquals(1, traces.size());
      assertFalse(traces.get(0).fromSetter());
    }
  }

  @Nested
  class Rewrite {

    @Test
    @DisplayName("replaces the traced argument at the usage site")
    void shouldReplaceArgument() {
      CompilationUnit usage = parse(DASHBOARD_SOURCE);
      List<UsageSiteRewriter.Trace> traces =
          UsageSiteRewriter.trace(parse(EXPLORE_SOURCE), target(6, "Button"), "setText");

      UsageSiteRewriter.rewrite(usage, 5, traces, "Create widget", "New label", String.class);

      assertTrue(usage.toString().contains("\"New label\""));
      assertFalse(usage.toString().contains("\"Create widget\""));
      assertTrue(usage.toString().contains("\"Your dashboard is empty\""));
    }

    @Test
    @DisplayName("replaces a computed argument on a definite setter trace and reports it")
    void shouldReplaceComputedArgumentOnDefiniteTrace() {
      CompilationUnit usage = parse("""
          package com.example;

          public class DashboardView {
            public DashboardView() {
              add(new SignalCard("Fleet utilisation", utilisation + "%"));
            }
          }
          """);
      List<UsageSiteRewriter.Trace> traces =
          List.of(new UsageSiteRewriter.Trace("SignalCard", 1, 2, true));

      String replaced = UsageSiteRewriter.rewrite(usage, 5, traces, "64%", "65%", String.class);

      assertEquals("utilisation + \"%\"", replaced);
      assertTrue(usage.toString().contains("new SignalCard(\"Fleet utilisation\", \"65%\")"));
    }

    @Test
    @DisplayName("throws when a definite trace meets a stale literal")
    void shouldThrowOnStaleLiteralWithDefiniteTrace() {
      CompilationUnit usage = parse("""
          package com.example;

          public class DashboardView {
            public DashboardView() {
              add(new SignalCard("Fleet utilisation", "something else"));
            }
          }
          """);
      List<UsageSiteRewriter.Trace> traces =
          List.of(new UsageSiteRewriter.Trace("SignalCard", 1, 2, true));

      assertThrows(SourceModificationException.class,
          () -> UsageSiteRewriter.rewrite(usage, 5, traces, "64%", "65%", String.class));
    }

    @Test
    @DisplayName("returns null when the replaced argument was the matching literal")
    void shouldReturnNullForLiteralReplacement() {
      CompilationUnit usage = parse(DASHBOARD_SOURCE);
      List<UsageSiteRewriter.Trace> traces =
          UsageSiteRewriter.trace(parse(EXPLORE_SOURCE), target(6, "Button"), "setText");

      String replaced =
          UsageSiteRewriter.rewrite(usage, 5, traces, "Create widget", "New label", String.class);

      assertNull(replaced);
    }

    @Test
    @DisplayName("throws when no creation exists at the usage line")
    void shouldThrowWhenCreationMissing() {
      CompilationUnit usage = parse(DASHBOARD_SOURCE);
      List<UsageSiteRewriter.Trace> traces =
          List.of(new UsageSiteRewriter.Trace("Explore", 2, 3, false));

      assertThrows(SourceModificationException.class,
          () -> UsageSiteRewriter.rewrite(usage, 99, traces, "Create widget", "New", null));
    }

    @Test
    @DisplayName("throws when the argument no longer matches the original value")
    void shouldThrowOnValueMismatch() {
      CompilationUnit usage = parse(DASHBOARD_SOURCE);
      List<UsageSiteRewriter.Trace> traces =
          List.of(new UsageSiteRewriter.Trace("Explore", 2, 3, false));

      assertThrows(SourceModificationException.class,
          () -> UsageSiteRewriter.rewrite(usage, 5, traces, "Something else", "New", null));
    }

    @Test
    @DisplayName("throws when the usage calls a different overload")
    void shouldThrowOnArgumentCountMismatch() {
      CompilationUnit usage = parse("""
          package com.example;

          public class DashboardView {
            public DashboardView() {
              add(new Explore("Create widget"));
            }
          }
          """);
      List<UsageSiteRewriter.Trace> traces =
          List.of(new UsageSiteRewriter.Trace("Explore", 2, 3, false));

      assertThrows(SourceModificationException.class,
          () -> UsageSiteRewriter.rewrite(usage, 5, traces, "Create widget", "New", null));
    }

    @Test
    @DisplayName("throws when several arguments match the original value")
    void shouldThrowOnAmbiguousMatch() {
      CompilationUnit usage = parse("""
          package com.example;

          public class DashboardView {
            public DashboardView() {
              add(new Explore("same", "same"));
            }
          }
          """);
      List<UsageSiteRewriter.Trace> traces =
          List.of(new UsageSiteRewriter.Trace("Explore", 0, 2, false),
              new UsageSiteRewriter.Trace("Explore", 1, 2, false));

      assertThrows(SourceModificationException.class,
          () -> UsageSiteRewriter.rewrite(usage, 5, traces, "same", "New", null));
    }

    @Test
    @DisplayName("matches numeric and boolean literals against client values")
    void shouldMatchScalarLiterals() {
      CompilationUnit usage = parse("""
          package com.example;

          public class View {
            public View() {
              add(new Meter(42, true));
            }
          }
          """);

      UsageSiteRewriter.rewrite(usage, 5,
          List.of(new UsageSiteRewriter.Trace("Meter", 0, 2, false)), 42.0, 7, Integer.class);
      UsageSiteRewriter.rewrite(usage, 5,
          List.of(new UsageSiteRewriter.Trace("Meter", 1, 2, false)), true, false, Boolean.class);

      assertTrue(usage.toString().contains("new Meter(7, false)"));
    }

    @Test
    @DisplayName("matches a null literal against a null original value")
    void shouldMatchNullLiteral() {
      CompilationUnit usage = parse("""
          package com.example;

          public class View {
            public View() {
              add(new Card(null));
            }
          }
          """);

      UsageSiteRewriter.rewrite(usage, 5, List.of(new UsageSiteRewriter.Trace("Card", 0, 1, false)),
          null, "Hello", String.class);

      assertTrue(usage.toString().contains("new Card(\"Hello\")"));
    }

    @Test
    @DisplayName("matches a text block argument")
    void shouldMatchTextBlock() {
      CompilationUnit usage = parse("""
          package com.example;

          public class View {
            public View() {
              add(new Card(\"""
                  hello\"""));
            }
          }
          """);

      UsageSiteRewriter.rewrite(usage, 5, List.of(new UsageSiteRewriter.Trace("Card", 0, 1, false)),
          "hello", "bye", String.class);

      assertTrue(usage.toString().contains("\"bye\""));
    }

    @Test
    @DisplayName("does not match mismatched booleans or non-literal arguments")
    void shouldRejectMismatchedArguments() {
      CompilationUnit usage = parse("""
          package com.example;

          public class View {
            public View() {
              add(new Card(false, compute()));
            }
          }
          """);

      assertThrows(SourceModificationException.class, () -> UsageSiteRewriter.rewrite(usage, 5,
          List.of(new UsageSiteRewriter.Trace("Card", 0, 2, false)), true, false, null));
      assertThrows(SourceModificationException.class, () -> UsageSiteRewriter.rewrite(usage, 5,
          List.of(new UsageSiteRewriter.Trace("Card", 1, 2, false)), "x", "y", null));
    }

    @Test
    @DisplayName("throws when the trace list is empty")
    void shouldThrowOnEmptyTraces() {
      CompilationUnit usage = parse(DASHBOARD_SOURCE);

      assertThrows(SourceModificationException.class,
          () -> UsageSiteRewriter.rewrite(usage, 5, List.of(), "a", "b", null));
    }

    @Test
    @DisplayName("matches negative numeric literals")
    void shouldMatchNegativeLiterals() {
      CompilationUnit usage = parse("""
          package com.example;

          public class View {
            public View() {
              add(new Meter(-5));
            }
          }
          """);

      UsageSiteRewriter.rewrite(usage, 5,
          List.of(new UsageSiteRewriter.Trace("Meter", 0, 1, false)), -5.0, 3, Integer.class);

      assertTrue(usage.toString().contains("new Meter(3)"));
    }
  }
}
