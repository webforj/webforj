package com.webforj.devtools.craftforj.inspector.source.parser;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class StatementWrapperTest {

  private static final String ORIGINAL = """
      package com.example;

      class Test {
        Test() {
        }
      }
      """;

  private static final String LONG_CALL = "    columnsLayout.setBreakpoints(List.of("
      + "new Breakpoint(\"default\", 0, 1), new Breakpoint(\"small\", \"20em\", 1), "
      + "new Breakpoint(\"medium\", \"40em\", 3), new Breakpoint(\"large\", \"60em\", 4)));";

  private String modifiedWith(String line) {
    return """
        package com.example;

        class Test {
          Test() {
        %s
          }
        }
        """.formatted(line);
  }

  @Nested
  class Wrap {

    @Test
    void shouldWrapLongGeneratedCallAtArgumentBoundaries() {
      String result = StatementWrapper.wrap(ORIGINAL, modifiedWith(LONG_CALL));

      String expected = """
              columnsLayout.setBreakpoints(List.of(
                  new Breakpoint("default", 0, 1),
                  new Breakpoint("small", "20em", 1),
                  new Breakpoint("medium", "40em", 3),
                  new Breakpoint("large", "60em", 4)));\
          """;
      assertEquals(modifiedWith(expected), result);
    }

    @Test
    void shouldLeaveShortGeneratedLinesUntouched() {
      String line = "    columnsLayout.setSpan(email, 3);";
      String result = StatementWrapper.wrap(ORIGINAL, modifiedWith(line));

      assertEquals(modifiedWith(line), result);
    }

    @Test
    void shouldLeaveLongUserLinesUntouched() {
      String modified = modifiedWith(LONG_CALL);

      assertEquals(modified, StatementWrapper.wrap(modified, modified));
    }

    @Test
    void shouldWrapMultiArgumentCallOnePerLine() {
      String line = "    layout.configure(new Breakpoint(\"extra-small-viewport\", \"10em\", 1), "
          + "new Breakpoint(\"extra-large-viewport\", \"90em\", 6), someOtherArgument);";
      String result = StatementWrapper.wrap(ORIGINAL, modifiedWith(line));

      String expected = """
              layout.configure(
                  new Breakpoint("extra-small-viewport", "10em", 1),
                  new Breakpoint("extra-large-viewport", "90em", 6),
                  someOtherArgument);\
          """;
      assertEquals(modifiedWith(expected), result);
    }

    @Test
    void shouldUseTheFileIndentUnitForContinuationLines() {
      String original = """
          package com.example;

          class Test {
              Test() {
              }
          }
          """;
      String line = "        columnsLayout.setBreakpoints(List.of("
          + "new Breakpoint(\"small\", \"20em\", 1), new Breakpoint(\"medium\", \"40em\", 3), "
          + "new Breakpoint(\"large\", \"60em\", 4)));";
      String modified = """
          package com.example;

          class Test {
              Test() {
          %s
              }
          }
          """.formatted(line);

      String result = StatementWrapper.wrap(original, modified);

      String expected = """
                  columnsLayout.setBreakpoints(List.of(
                          new Breakpoint("small", "20em", 1),
                          new Breakpoint("medium", "40em", 3),
                          new Breakpoint("large", "60em", 4)));\
          """;
      assertEquals(modified.replace(line, expected), result);
    }

    @Test
    void shouldLeaveNonStatementLongLinesUntouched() {
      String line = "  // a very long comment line that exceeds the limit "
          + "and keeps going and going and going and going and going";
      String result = StatementWrapper.wrap(ORIGINAL, modifiedWith(line));

      assertEquals(modifiedWith(line), result);
    }

    @Test
    void shouldLeaveUnparsableLongLinesUntouched() {
      String line = "    new Breakpoint(\"medium\", \"40em\", 3), "
          + "new Breakpoint(\"large\", \"60em\", 4), new Breakpoint(\"huge\", \"80em\", 5)));";
      String result = StatementWrapper.wrap(ORIGINAL, modifiedWith(line));

      assertEquals(modifiedWith(line), result);
    }
  }
}
