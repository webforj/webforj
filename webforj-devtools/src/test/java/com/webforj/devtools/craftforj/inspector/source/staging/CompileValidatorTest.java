package com.webforj.devtools.craftforj.inspector.source.staging;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.devtools.craftforj.inspector.source.staging.model.CompileDiagnostic;
import com.webforj.devtools.craftforj.inspector.source.staging.model.StagedFile;
import com.webforj.devtools.craftforj.inspector.source.staging.model.ValidationResult;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.Set;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class CompileValidatorTest {

  @TempDir
  Path dir;

  private CompileValidator validator;

  @BeforeEach
  void setUp() {
    validator = new CompileValidator();
  }

  @Test
  @DisplayName("Should accept a compiling source and mark it verified")
  void shouldAcceptCompilingSource() {
    String path = dir.resolve("Ok.java").toString();
    String content = """
        public class Ok {
          private int value;

          public int getValue() {
            return value;
          }
        }
        """;

    ValidationResult result = validator.validate(Map.of(path, content), Set.of(path));

    assertTrue(result.isSuccess());
    assertTrue(result.isVerified(path));
    assertTrue(result.getErrors().isEmpty());
  }

  @Test
  @DisplayName("Should reject a broken source with line and column")
  void shouldRejectBrokenSourceWithLineAndColumn() {
    String path = dir.resolve("Bad.java").toString();
    String content = """
        public class Bad {
          int x =
        }
        """;

    ValidationResult result = validator.validate(Map.of(path, content), Set.of(path));

    assertFalse(result.isSuccess());
    assertFalse(result.getErrors().isEmpty());
    CompileDiagnostic diagnostic = result.getErrors().get(0);
    assertEquals(path, diagnostic.getFile());
    assertTrue(diagnostic.getLine() > 0);
    assertTrue(diagnostic.getColumn() > 0);
  }

  @Test
  @DisplayName("Should attach class name hints for an unresolved simple name")
  void shouldAttachHintsForUnresolvedSimpleName() {
    String path = dir.resolve("Hinted.java").toString();
    String content = """
        public class Hinted {
          StagedFile file;
        }
        """;

    ValidationResult result = validator.validate(Map.of(path, content), Set.of(path));

    assertFalse(result.isSuccess());
    boolean hinted = result.getErrors().stream()
        .anyMatch(error -> error.getDidYouMean().contains(StagedFile.class.getName()));
    assertTrue(hinted);
  }

  @Test
  @DisplayName("Should validate a new class and its consumer together")
  void shouldValidateNewClassAndConsumerTogether() {
    String productPath = dir.resolve("Product.java").toString();
    String consumerPath = dir.resolve("Consumer.java").toString();
    String product = """
        public class Product {
          public String name() {
            return "p";
          }
        }
        """;
    String consumer = """
        public class Consumer {
          public String describe() {
            return new Product().name();
          }
        }
        """;

    ValidationResult result = validator.validate(
        Map.of(productPath, product, consumerPath, consumer), Set.of(productPath, consumerPath));

    assertTrue(result.isSuccess());
    assertTrue(result.isVerified(productPath));
    assertTrue(result.isVerified(consumerPath));
  }

  @Test
  @DisplayName("Should reject the pair when the consumer misspells the new class")
  void shouldRejectConsumerWithTypo() {
    String productPath = dir.resolve("Product.java").toString();
    String consumerPath = dir.resolve("Consumer.java").toString();
    String product = """
        public class Product {
        }
        """;
    String consumer = """
        public class Consumer {
          ProductX product;
        }
        """;

    ValidationResult result = validator.validate(
        Map.of(productPath, product, consumerPath, consumer), Set.of(productPath, consumerPath));

    assertFalse(result.isSuccess());
    assertTrue(result.getErrors().stream().anyMatch(error -> consumerPath.equals(error.getFile())));
  }

  @Test
  @DisplayName("Should degrade to parse only when the baseline itself fails")
  void shouldDegradeWhenBaselineFails(@TempDir Path baselineDir) throws IOException {
    Path file = baselineDir.resolve("Degraded.java");
    String baseline = """
        public class Degraded {
          MissingType field;
        }
        """;
    Files.writeString(file, baseline, StandardCharsets.UTF_8);

    String edited = """
        public class Degraded {
          MissingType field;
          MissingType second;
        }
        """;

    ValidationResult result = validator.validate(Map.of(file.toString(), edited), Set.of());

    assertTrue(result.isSuccess());
    assertFalse(result.isVerified(file.toString()));
  }

  @Test
  @DisplayName("Should still reject unparseable content for a degraded baseline")
  void shouldRejectUnparseableContentForDegradedBaseline(@TempDir Path baselineDir)
      throws IOException {
    Path file = baselineDir.resolve("Degraded.java");
    String baseline = """
        public class Degraded {
          MissingType field;
        }
        """;
    Files.writeString(file, baseline, StandardCharsets.UTF_8);

    String edited = """
        public class Degraded {
          MissingType field
        """;

    ValidationResult result = validator.validate(Map.of(file.toString(), edited), Set.of());

    assertFalse(result.isSuccess());
    assertFalse(result.getErrors().isEmpty());
  }

  @Test
  @DisplayName("Should fall back to parse only validation without a system compiler")
  void shouldFallBackToParseOnlyWithoutCompiler() {
    CompileValidator parseOnly = new CompileValidator(null, "");
    assertFalse(parseOnly.isCompileAvailable());

    String path = dir.resolve("Unverified.java").toString();
    String content = """
        public class Unverified {
          UnknownType field;
        }
        """;

    ValidationResult result = parseOnly.validate(Map.of(path, content), Set.of(path));

    assertTrue(result.isSuccess());
    assertFalse(result.isVerified(path));
  }

  @Test
  @DisplayName("Should reject unparseable content without a system compiler")
  void shouldRejectUnparseableWithoutCompiler() {
    CompileValidator parseOnly = new CompileValidator(null, "");

    String path = dir.resolve("Broken.java").toString();
    String content = """
        public class Broken {
        """;

    ValidationResult result = parseOnly.validate(Map.of(path, content), Set.of(path));

    assertFalse(result.isSuccess());
    assertFalse(result.getErrors().isEmpty());
    assertEquals(path, result.getErrors().get(0).getFile());
  }

  @Test
  @DisplayName("Should compile a source with a package declaration")
  void shouldCompilePackagedSource() {
    String path = dir.resolve("Packaged.java").toString();
    String content = """
        package com.example.demo;

        public class Packaged {
          public String greet() {
            return "hello";
          }
        }
        """;

    ValidationResult result = validator.validate(Map.of(path, content), Set.of(path));

    assertTrue(result.isSuccess());
    assertTrue(result.isVerified(path));
  }

  @Test
  @DisplayName("Should cap hints at five entries")
  void shouldCapHints() {
    String path = dir.resolve("Many.java").toString();
    String content = """
        public class Many {
          List items;
        }
        """;

    ValidationResult result = validator.validate(Map.of(path, content), Set.of(path));

    assertFalse(result.isSuccess());
    for (CompileDiagnostic error : result.getErrors()) {
      assertTrue(error.getDidYouMean().size() <= 5);
    }
  }
}
