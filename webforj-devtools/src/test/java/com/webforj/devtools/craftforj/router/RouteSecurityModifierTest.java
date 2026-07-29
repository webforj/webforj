package com.webforj.devtools.craftforj.router;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.devtools.craftforj.action.CraftforjActionException;
import com.webforj.devtools.craftforj.router.model.SecurityAccess;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

@DisplayName("RouteSecurityModifier")
class RouteSecurityModifierTest {

  @TempDir
  Path tempDir;

  private final RouteSecurityModifier modifier = new RouteSecurityModifier();

  private Path writeSource(String content) throws IOException {
    Path file = tempDir.resolve("DashboardView.java");
    Files.writeString(file, content);

    return file;
  }

  private void assertHasImport(String source, String qualifiedName) {
    assertTrue(source.contains("import " + qualifiedName + ";"));
  }

  private void assertNoImport(String source, String qualifiedName) {
    assertFalse(source.contains("import " + qualifiedName + ";"));
  }

  private String plainSource() {
    return """
        package com.example;

        import com.webforj.router.annotation.Route;

        @Route("/dashboard")
        public class DashboardView {

          public DashboardView() {
            // build view
          }
        }
        """;
  }

  private String protectedSource() {
    return """
        package com.example;

        import com.webforj.router.annotation.Route;
        import jakarta.annotation.security.PermitAll;

        @Route("/dashboard")
        @PermitAll
        public class DashboardView {
        }
        """;
  }

  @Nested
  @DisplayName("apply")
  class Apply {

    @Test
    @DisplayName("Should add PermitAll annotation and import")
    void shouldAddPermitAll() throws IOException {
      Path file = writeSource(plainSource());

      modifier.apply(file, "DashboardView", SecurityAccess.PERMIT_ALL, List.of());

      String result = Files.readString(file);
      assertTrue(result.contains("@PermitAll"));
      assertHasImport(result, "jakarta.annotation.security.PermitAll");
    }

    @Test
    @DisplayName("Should add DenyAll annotation and import")
    void shouldAddDenyAll() throws IOException {
      Path file = writeSource(plainSource());

      modifier.apply(file, "DashboardView", SecurityAccess.DENY_ALL, List.of());

      String result = Files.readString(file);
      assertTrue(result.contains("@DenyAll"));
      assertHasImport(result, "jakarta.annotation.security.DenyAll");
    }

    @Test
    @DisplayName("Should add AnonymousAccess annotation and import")
    void shouldAddAnonymousAccess() throws IOException {
      Path file = writeSource(plainSource());

      modifier.apply(file, "DashboardView", SecurityAccess.ANONYMOUS, List.of());

      String result = Files.readString(file);
      assertTrue(result.contains("@AnonymousAccess"));
      assertHasImport(result, "com.webforj.router.security.annotation.AnonymousAccess");
    }

    @Test
    @DisplayName("Should add RolesAllowed with a single role")
    void shouldAddSingleRole() throws IOException {
      Path file = writeSource(plainSource());

      modifier.apply(file, "DashboardView", SecurityAccess.ROLES_ALLOWED, List.of("ADMIN"));

      String result = Files.readString(file);
      assertTrue(result.contains("@RolesAllowed(\"ADMIN\")"));
      assertHasImport(result, "jakarta.annotation.security.RolesAllowed");
    }

    @Test
    @DisplayName("Should add RolesAllowed with multiple roles")
    void shouldAddMultipleRoles() throws IOException {
      Path file = writeSource(plainSource());

      modifier.apply(file, "DashboardView", SecurityAccess.ROLES_ALLOWED,
          List.of("ADMIN", "MANAGER"));

      String result = Files.readString(file);
      assertTrue(result.contains("\"ADMIN\""));
      assertTrue(result.contains("\"MANAGER\""));
    }

    @Test
    @DisplayName("Should escape quotes and backslashes in role names")
    void shouldEscapeRoleNames() throws IOException {
      Path file = writeSource(plainSource());

      modifier.apply(file, "DashboardView", SecurityAccess.ROLES_ALLOWED, List.of("AD\"MIN"));

      String result = Files.readString(file);
      assertTrue(result.contains("@RolesAllowed(\"AD\\\"MIN\")"));
    }

    @Test
    @DisplayName("Should replace an existing security annotation")
    void shouldReplaceExistingAnnotation() throws IOException {
      Path file = writeSource(protectedSource());

      modifier.apply(file, "DashboardView", SecurityAccess.ANONYMOUS, List.of());

      String result = Files.readString(file);
      assertFalse(result.contains("@PermitAll"));
      assertNoImport(result, "jakarta.annotation.security.PermitAll");
      assertTrue(result.contains("@AnonymousAccess"));
    }

    @Test
    @DisplayName("Should remove security annotation and import for NONE")
    void shouldRemoveAnnotationForNone() throws IOException {
      Path file = writeSource(protectedSource());

      modifier.apply(file, "DashboardView", SecurityAccess.NONE, List.of());

      String result = Files.readString(file);
      assertFalse(result.contains("@PermitAll"));
      assertNoImport(result, "jakarta.annotation.security.PermitAll");
      assertTrue(result.contains("@Route(\"/dashboard\")"));
    }

    @Test
    @DisplayName("Should preserve untouched code")
    void shouldPreserveUntouchedCode() throws IOException {
      Path file = writeSource(plainSource());

      modifier.apply(file, "DashboardView", SecurityAccess.PERMIT_ALL, List.of());

      String result = Files.readString(file);
      assertTrue(result.contains("@Route(\"/dashboard\")"));
      assertTrue(result.contains("// build view"));
      assertTrue(result.contains("public DashboardView()"));
    }

    @Test
    @DisplayName("Should throw when roles are missing for ROLES_ALLOWED")
    void shouldThrowWhenRolesMissing() throws IOException {
      Path file = writeSource(plainSource());

      assertThrows(CraftforjActionException.class,
          () -> modifier.apply(file, "DashboardView", SecurityAccess.ROLES_ALLOWED, List.of()));
    }

    @Test
    @DisplayName("Should throw when class is not in the file")
    void shouldThrowWhenClassNotFound() throws IOException {
      Path file = writeSource(plainSource());

      assertThrows(CraftforjActionException.class,
          () -> modifier.apply(file, "OtherView", SecurityAccess.PERMIT_ALL, List.of()));
    }

    @Test
    @DisplayName("Should throw when file cannot be read")
    void shouldThrowWhenFileMissing() {
      Path file = tempDir.resolve("Missing.java");

      assertThrows(CraftforjActionException.class,
          () -> modifier.apply(file, "Missing", SecurityAccess.PERMIT_ALL, List.of()));
    }

    @Test
    @DisplayName("Should restore the original source on an add and remove round trip")
    void shouldRoundTripWithoutFormattingChanges() throws IOException {
      String original = plainSource();
      Path file = writeSource(original);

      modifier.apply(file, "DashboardView", SecurityAccess.PERMIT_ALL, List.of());
      modifier.apply(file, "DashboardView", SecurityAccess.NONE, List.of());

      assertEquals(original, Files.readString(file));
    }

    @Test
    @DisplayName("Should keep the blank line between imports and the class")
    void shouldKeepBlankLineAfterImports() throws IOException {
      Path file = writeSource(plainSource());

      modifier.apply(file, "DashboardView", SecurityAccess.PERMIT_ALL, List.of());

      String result = Files.readString(file);
      assertTrue(result.contains("\n\n@Route(\"/dashboard\")"));
    }

    @Test
    @DisplayName("Should add the import when the file has no imports")
    void shouldInsertImportWithoutExistingImports() throws IOException {
      Path file = writeSource("""
          package com.example;

          public class DashboardView {
          }
          """);

      modifier.apply(file, "DashboardView", SecurityAccess.PERMIT_ALL, List.of());

      String result = Files.readString(file);
      assertHasImport(result, "jakarta.annotation.security.PermitAll");
      assertTrue(result.contains("package com.example;\n\nimport"));
    }

    @Test
    @DisplayName("Should keep the import when another type in the file still uses it")
    void shouldKeepImportUsedByOtherType() throws IOException {
      Path file = writeSource("""
          package com.example;

          import jakarta.annotation.security.PermitAll;

          @PermitAll
          public class DashboardView {
          }

          @PermitAll
          class OtherView {
          }
          """);

      modifier.apply(file, "DashboardView", SecurityAccess.NONE, List.of());

      String result = Files.readString(file);
      assertHasImport(result, "jakarta.annotation.security.PermitAll");
    }
  }
}
