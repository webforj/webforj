package com.webforj.devtools.craftforj.inspector.source.parser;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import java.util.Set;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class ImportWriterTest {

  private static final String BUTTON = "com.webforj.component.Button";
  private static final String ICON = "com.webforj.component.Icon";

  private String importLine(String qualifiedName) {
    return "import " + qualifiedName + ";";
  }

  @Nested
  class Sync {

    @Test
    void shouldAddNewImportAfterExistingImports() {
      String source = """
          package com.example;

          import com.example.Other;

          class Test {
          }
          """;

      String result = ImportWriter.sync(source, List.of(BUTTON), Set.of(BUTTON));

      assertTrue(result.contains(importLine("com.example.Other") + "\n" + importLine(BUTTON)));
    }

    @Test
    void shouldNotDuplicateExistingImport() {
      String source = """
          package com.example;

          %s

          class Test {
          }
          """.formatted(importLine(BUTTON));

      String result = ImportWriter.sync(source, List.of(BUTTON), Set.of(BUTTON));

      assertEquals(source, result);
      assertEquals(1, result.split(importLine(BUTTON), -1).length - 1);
    }

    @Test
    void shouldRemoveUnusedCandidateImport() {
      String source = """
          package com.example;

          %s

          class Test {
          }
          """.formatted(importLine(BUTTON));

      String result = ImportWriter.sync(source, List.of(BUTTON), Set.of());

      assertFalse(result.contains(importLine(BUTTON)));
      assertTrue(result.contains("class Test"));
    }

    @Test
    void shouldKeepUsedAndRemoveUnusedTogether() {
      String source = """
          package com.example;

          %s
          %s

          class Test {
          }
          """.formatted(importLine(BUTTON), importLine(ICON));

      String result = ImportWriter.sync(source, List.of(BUTTON, ICON), Set.of(BUTTON));

      assertTrue(result.contains(importLine(BUTTON)));
      assertFalse(result.contains(importLine(ICON)));
    }

    @Test
    void shouldNotTouchImportsOutsideCandidates() {
      String source = """
          package com.example;

          import com.example.Other;

          class Test {
          }
          """;

      String result = ImportWriter.sync(source, List.of(BUTTON), Set.of());

      assertEquals(source, result);
    }

    @Test
    void shouldInsertNewImportInSortedPositionAmongExistingImports() {
      String source = """
          package com.example;

          import com.example.Apple;
          import com.example.Zebra;

          class Test {
          }
          """;

      String result =
          ImportWriter.sync(source, List.of("com.example.Middle"), Set.of("com.example.Middle"));

      assertTrue(result.contains(importLine("com.example.Apple") + "\n"
          + importLine("com.example.Middle") + "\n" + importLine("com.example.Zebra")));
    }

    @Test
    void shouldAppendAfterLastImportWhenNewImportSortsLast() {
      String source = """
          package com.example;

          import com.example.Apple;
          import com.example.Middle;

          class Test {
          }
          """;

      String result =
          ImportWriter.sync(source, List.of("com.example.Zebra"), Set.of("com.example.Zebra"));

      assertTrue(result
          .contains(importLine("com.example.Middle") + "\n" + importLine("com.example.Zebra")));
    }

    @Test
    void shouldInsertAfterPackageWhenNoImportsExist() {
      String source = """
          package com.example;

          class Test {
          }
          """;

      String result = ImportWriter.sync(source, List.of(BUTTON), Set.of(BUTTON));

      assertTrue(result.contains("package com.example;\n\n" + importLine(BUTTON)));
      assertTrue(result.contains("\n\nclass Test"));
    }

    @Test
    void shouldInsertAtTopWithoutPackage() {
      String source = """
          class Test {
          }
          """;

      String result = ImportWriter.sync(source, List.of(BUTTON), Set.of(BUTTON));

      assertTrue(result.startsWith(importLine(BUTTON)));
    }

    @Test
    void shouldPreserveBlankLinesAroundImportBlock() {
      String source = """
          package com.example;

          import com.example.Other;
          %s

          class Test {
          }
          """.formatted(importLine(BUTTON));

      String result = ImportWriter.sync(source, List.of(BUTTON), Set.of());

      assertTrue(result.contains(importLine("com.example.Other") + "\n\nclass Test"));
    }
  }
}
