package com.webforj.devtools.craftforj.source.parser;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Named;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

@DisplayName("WhitespaceWriter")
class WhitespaceWriterTest {

  private static final String ORIGINAL = """
      public class View {
        private Button save;

        public View() {
          Button save = new Button("Save");
          add(save);
        }
      }
      """;

  @Test
  @DisplayName("should pass untouched content through byte-identical")
  void shouldPassUntouchedContentThrough() {
    assertEquals(ORIGINAL, WhitespaceWriter.repair(ORIGINAL, ORIGINAL));
  }

  @Test
  @DisplayName("should return content that does not parse as it is")
  void shouldReturnUnparseableContentAsIs() {
    String broken = """
        public class View {
          public View( {
        }
        """;

    assertEquals(broken, WhitespaceWriter.repair(ORIGINAL, broken));
  }

  @Test
  @DisplayName("should give an argument back the indentation the printer ate")
  void shouldRestoreIndentationOfArgument() {
    String original = """
        public class View {
          public View() {
            add(
                new Button("First"),
                new Button("Second"),
                new Button("Third"));
            add(
              new Button("Other"),
              new Button("Third"));
          }
        }
        """;
    String printed = """
        public class View {
          public View() {
            add(
                new Button("First"),
               new Button("Third"));
            add(
              new Button("Other"),
              new Button("Third"));
          }
        }
        """;

    assertEquals("""
        public class View {
          public View() {
            add(
                new Button("First"),
                new Button("Third"));
            add(
              new Button("Other"),
              new Button("Third"));
          }
        }
        """, WhitespaceWriter.repair(original, printed));
  }

  @Test
  @DisplayName("should indent the statements the printer pushed out at the top of a block")
  void shouldIndentStatementsAtTopOfBlock() {
    String printed = """
        public class View {
          private Button save;

          public View() {
          Button button = new Button("Button");
          Button save = new Button("Save");
            add(save);
          }
        }
        """;

    assertEquals("""
        public class View {
          private Button save;

          public View() {
            Button button = new Button("Button");
            Button save = new Button("Save");
            add(save);
          }
        }
        """, WhitespaceWriter.repair(ORIGINAL, printed));
  }

  @Test
  @DisplayName("should indent the inside of a new block with the unit of the file")
  void shouldIndentNewBlockWithFileUnit() {
    String original = """
        public class View {
          private Button save;
        }
        """;
    String printed = """
        public class View {
          private Button save;
        \s\s
          public View() {
              add(save);
          }
        }
        """;

    assertEquals("""
        public class View {
          private Button save;

          public View() {
            add(save);
          }
        }
        """, WhitespaceWriter.repair(original, printed));
  }

  @Test
  @DisplayName("should follow a file that indents with tabs")
  void shouldFollowTabs() {
    String original = """
        public class View {
        \tpublic View() {
        \t\tadd(save);
        \t}
        }
        """;
    String printed = """
        public class View {
        \tpublic View() {
        \tButton b = null;
        \tadd(save);
        \t}
        }
        """;

    assertEquals("""
        public class View {
        \tpublic View() {
        \t\tButton b = null;
        \t\tadd(save);
        \t}
        }
        """, WhitespaceWriter.repair(original, printed));
  }

  @Test
  @DisplayName("should put a new field right under the field before it")
  void shouldJoinNewFieldToFields() {
    String printed = """
        public class View {
          private Button save;

        private Button button;

          public View() {
            Button save = new Button("Save");
            add(save);
          }
        }
        """;

    assertEquals("""
        public class View {
          private Button save;
          private Button button;

          public View() {
            Button save = new Button("Save");
            add(save);
          }
        }
        """, WhitespaceWriter.repair(ORIGINAL, printed));
  }

  @Test
  @DisplayName("should drop the whitespace line the printer leaves between two fields")
  void shouldDropStrayWhitespaceLine() {
    String original = """
        public class View {
          private Button save;
          private String title;
        }
        """;
    String printed = """
        public class View {
          private Button save;
          private Button button;
        \s\s
          private String title;
        }
        """;

    assertEquals("""
        public class View {
          private Button save;
          private Button button;
          private String title;
        }
        """, WhitespaceWriter.repair(original, printed));
  }

  @Test
  @DisplayName("should keep a new first member right under the class line")
  void shouldKeepFirstMemberUnderClassLine() {
    String original = """
        public class View extends Composite<Div> {
        }
        """;
    String printed = """
        public class View extends Composite<Div> {

          public View() {
              getBoundComponent().add(save);
          }
        }
        """;

    assertEquals("""
        public class View extends Composite<Div> {
          public View() {
            getBoundComponent().add(save);
          }
        }
        """, WhitespaceWriter.repair(original, printed));
  }

  @Test
  @DisplayName("should give back the blank line above a member after a removal ate it")
  void shouldRestoreBlankLineAboveMember() {
    String original = """
        public class View {
          private FlexLayout layout;
          private Button save;

          public View() {
            add(layout);
          }
        }
        """;
    String printed = """
        public class View {
          private FlexLayout layout;
          public View() {
            add(layout);
          }
        }
        """;

    assertEquals("""
        public class View {
          private FlexLayout layout;

          public View() {
            add(layout);
          }
        }
        """, WhitespaceWriter.repair(original, printed));
  }

  @Test
  @DisplayName("should align the brace that closes a block the edit emptied")
  void shouldAlignBraceOfEmptiedBlock() {
    String printed = """
        public class View {
          private Button save;

          public View() {
        }
        }
        """;

    assertEquals("""
        public class View {
          private Button save;

          public View() {
          }
        }
        """, WhitespaceWriter.repair(ORIGINAL, printed));
  }

  @Test
  @DisplayName("should join the closing of a call the printer left alone on a line")
  void shouldJoinDanglingClosing() {
    String original = """
        public class View {
          public View() {
            add(save,
                cancel);
          }
        }
        """;
    String printed = """
        public class View {
          public View() {
            add(save
            );
          }
        }
        """;

    assertEquals("""
        public class View {
          public View() {
            add(save);
          }
        }
        """, WhitespaceWriter.repair(original, printed));
  }

  @ParameterizedTest
  @MethodSource("unchangedRepairs")
  void shouldKeepPrintedContentUnchanged(String original, String printed) {
    assertEquals(printed, WhitespaceWriter.repair(original, printed));
  }

  @Test
  @DisplayName("should drop the blank line a removal left at the edge of a block")
  void shouldTrimBlockEdges() {
    String original = """
        public class View {
          public View() {
            add(layout);
            // The main action
            add(save);
          }
        }
        """;
    String printed = """
        public class View {
          public View() {
            add(layout);

          }
        }
        """;

    assertEquals("""
        public class View {
          public View() {
            add(layout);
          }
        }
        """, WhitespaceWriter.repair(original, printed));
  }

  @Test
  @DisplayName("should rebase code that moved in with the indentation of its old place")
  void shouldRebaseMovedCode() {
    String original = """
        public class View {
          public View(boolean wide) {
            if (wide) {
              add(layout);
            }
          }
        }
        """;
    String printed = """
        public class View {
          public View(boolean wide) {
            if (wide) {
              add(layout);
              save.onClick(e -> {
              if (e != null) {
                save.setEnabled(false);
              }
            });
              layout.add(save,
                cancel);
            }
          }
        }
        """;

    assertEquals("""
        public class View {
          public View(boolean wide) {
            if (wide) {
              add(layout);
              save.onClick(e -> {
                if (e != null) {
                  save.setEnabled(false);
                }
              });
              layout.add(save,
                  cancel);
            }
          }
        }
        """, WhitespaceWriter.repair(original, printed));
  }

  private static Stream<Arguments> unchangedRepairs() {
    return Stream.of(
        Arguments.of(
            Named.of("should leave the closing of a call below a line that ends in a comment", """
                public class View {
                  public View() {
                    add(save, // kept
                        cancel);
                  }
                }
                """), """
                public class View {
                  public View() {
                    add(save // kept
                    );
                  }
                }
                """),
        Arguments.of(Named
            .of("should indent a changed statement from the first line of a wrapped signature", """
                public class View {
                  public View(String first,
                      String second) {
                    save.setText("A");
                  }
                }
                """), """
                public class View {
                  public View(String first,
                      String second) {
                    save.setText("B");
                  }
                }
                """),
        Arguments
            .of(Named.of("should keep a blank line the developer wrote at the edge of a block", """
                public class View {
                  public View() {

                    add(layout);
                  }

                  void build() {
                    add(save);
                  }
                }
                """), """
                public class View {
                  public View() {

                    add(layout);
                  }

                  void build() {
                    add(save);
                    add(help);
                  }
                }
                """));
  }
}
