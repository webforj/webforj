package com.webforj.devtools.craftforj.inspector.source.cases.ownership;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.button.Button;
import com.webforj.devtools.craftforj.inspector.source.SourceCodeModifier;
import com.webforj.devtools.craftforj.inspector.source.cases.support.SourceWriteFixture;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
import com.webforj.devtools.craftforj.source.model.FilePatch;
import com.webforj.devtools.craftforj.source.parser.SourceParserService;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class SourceIdentityCaseTest {

  private static final String SOURCE = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/identity-cases", outlet = MainLayout.class)
      @FrameTitle("Identity cases")
      public class IdentityCasesView extends Composite<FlexLayout> {
        public IdentityCasesView() {
          Button original = new Button("Aliased action");
          Button alias = original;
          alias.setTooltipText("Alias tooltip");
          Button reused = new Button("First assigned");
          reused.setTooltipText("First tooltip");
          getBoundComponent().add(original, reused);
          reused = new Button("Second assigned");
          reused.setTooltipText("Second tooltip");
          getBoundComponent().add(reused);
          Button left = new Button("Same line left"); Button right = new Button("Same line right");
          getBoundComponent().add(left, right);
        }
      }
      """;

  private static final String EXPECTED = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/identity-cases", outlet = MainLayout.class)
      @FrameTitle("Identity cases")
      public class IdentityCasesView extends Composite<FlexLayout> {
        public IdentityCasesView() {
          Button original = new Button("Aliased action");
          Button alias = original;
          alias.setTooltipText("Saved tooltip");
          Button reused = new Button("First assigned");
          reused.setTooltipText("First tooltip");
          getBoundComponent().add(original, reused);
          reused = new Button("Second assigned");
          reused.setTooltipText("Second tooltip");
          getBoundComponent().add(reused);
          Button left = new Button("Same line left"); Button right = new Button("Same line right");
          getBoundComponent().add(left, right);
        }
      }
      """;

  private static final String RESET = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/identity-cases", outlet = MainLayout.class)
      @FrameTitle("Identity cases")
      public class IdentityCasesView extends Composite<FlexLayout> {
        public IdentityCasesView() {
          Button original = new Button("Aliased action");
          Button alias = original;
          Button reused = new Button("First assigned");
          reused.setTooltipText("First tooltip");
          getBoundComponent().add(original, reused);
          reused = new Button("Second assigned");
          reused.setTooltipText("Second tooltip");
          getBoundComponent().add(reused);
          Button left = new Button("Same line left"); Button right = new Button("Same line right");
          getBoundComponent().add(left, right);
        }
      }
      """;

  @TempDir
  Path temporaryDirectory;

  @ParameterizedTest
  @ValueSource(booleans = {false, true})
  @DisplayName("A property edit or reset follows the component's stable local alias")
  void writeThroughAlias(boolean reset) throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = addComponent(fixture, 13);
      List<ChangeRequest> edits =
          List.of(fixture.createChange("action", "HasTooltip", reset ? "" : "Saved tooltip"));
      SourceCodeModifier modifier = fixture.getModifier();
      final String expected = reset ? RESET : EXPECTED;

      assertEquals(List.of(expected),
          modifier.previewPatches(edits).stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(edits);
      assertEquals(expected, Files.readString(file));
    }
  }

  @ParameterizedTest
  @ValueSource(strings = {"Saved tooltip", ""})
  @DisplayName("Reassignment cannot redirect an edit or reset to a different component")
  void refuseReassignedReceiver(String value) throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = addComponent(fixture, 16);
      List<ChangeRequest> edits = List.of(fixture.createChange("action", "HasTooltip", value));
      SourceCodeModifier modifier = fixture.getModifier();
      final String error = "'reused' is reassigned after it is created, so a write could reach a "
          + "different component";

      assertEquals(error, modifier.preview(edits).get(0).getError());
      assertEquals(SOURCE, Files.readString(file));
      assertEquals(error, modifier.apply(edits).get(0).getError());
      assertEquals(SOURCE, Files.readString(file));
    }
  }

  @ParameterizedTest
  @ValueSource(strings = {"Saved tooltip", ""})
  @DisplayName("An ambiguous line remains inspectable but cannot silently select a component for "
      + "writing")
  void refuseAmbiguousCreation(String value) throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = addComponent(fixture, 22);
      assertNull(new SourceParserService().extractVariableName(file, 22, "Button"));
      List<ChangeRequest> edits = List.of(fixture.createChange("action", "HasTooltip", value));
      SourceCodeModifier modifier = fixture.getModifier();
      final String error =
          "Cannot identify Button at line 22: " + "multiple matching creations share this line";

      assertEquals(error, modifier.preview(edits).get(0).getError());
      assertEquals(SOURCE, Files.readString(file));
      assertEquals(error, modifier.apply(edits).get(0).getError());
      assertEquals(SOURCE, Files.readString(file));
    }
  }

  private Path addComponent(SourceWriteFixture fixture, int line) throws IOException {
    final String owner = "com.devtoolsapplayoutspring.views.IdentityCasesView";
    Path file = fixture.addSource(owner, SOURCE);
    fixture.addComponent("action", Button.class,
        List.of(new SourcePoint(owner, "IdentityCasesView.java", line)));
    return file;
  }
}
