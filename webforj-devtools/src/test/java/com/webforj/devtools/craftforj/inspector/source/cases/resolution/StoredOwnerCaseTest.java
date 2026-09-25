package com.webforj.devtools.craftforj.inspector.source.cases.resolution;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.button.Button;
import com.webforj.devtools.craftforj.inspector.source.SourceCodeModifier;
import com.webforj.devtools.craftforj.inspector.source.cases.support.SourceWriteFixture;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
import com.webforj.devtools.craftforj.source.model.FilePatch;
import com.webforj.devtools.craftforj.source.model.SourceLocation;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class StoredOwnerCaseTest {

  private static final String SOURCE = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.Theme;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.component.toast.Toast;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/source-cases", outlet = MainLayout.class)
      @FrameTitle("Source cases")
      public class SourceCasesView extends Composite<FlexLayout> {
        private final Button action = new Button("Outer action");

        public SourceCasesView() {
          getBoundComponent().add(action, new NestedCard());
          action.onClick(event -> Toast.show("Outer action clicked", 3000, Theme.INFO,
              Toast.Placement.BOTTOM_RIGHT));
        }

        static class NestedCard extends Composite<FlexLayout> {
          private final Button action = new Button("Nested action");

          NestedCard() {
            getBoundComponent().add(action);
            action.onClick(event -> Toast.show("Nested action clicked", 3000, Theme.INFO,
                Toast.Placement.BOTTOM_RIGHT));
          }
        }
      }
      """;

  private static final String EXPECTED = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.Theme;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.component.toast.Toast;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/source-cases", outlet = MainLayout.class)
      @FrameTitle("Source cases")
      public class SourceCasesView extends Composite<FlexLayout> {
        private final Button action = new Button("Outer action");

        public SourceCasesView() {
          getBoundComponent().add(action, new NestedCard());
          action.onClick(event -> Toast.show("Outer action clicked", 3000, Theme.INFO,
              Toast.Placement.BOTTOM_RIGHT));
        }

        static class NestedCard extends Composite<FlexLayout> {
          private final Button action = new Button("Nested action");

          NestedCard() {
            getBoundComponent().add(action);
            action.onClick(event -> Toast.show("Nested action clicked", 3000, Theme.INFO,
                Toast.Placement.BOTTOM_RIGHT));
            action.setTooltipText("Nested tooltip");
          }
        }
      }
      """;

  @TempDir
  Path temporaryDirectory;

  @ParameterizedTest
  @ValueSource(ints = {1, 14, 23})
  @DisplayName("A destroyed nested field retains its owner even when its stored line hits the "
      + "outer field")
  void resolveStoredOwner(int storedLine) throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = addNestedAction(fixture);
      ChangeRequest edit = fixture.createChange("nested", "HasTooltip", "Nested tooltip");
      edit.setSource(new SourceLocation(file.toString(), storedLine,
          "com.devtoolsapplayoutspring.views.SourceCasesView$NestedCard", "action",
          Button.class.getName()));
      fixture.removeComponent("nested");
      SourceCodeModifier modifier = fixture.getModifier();

      assertEquals(List.of(EXPECTED),
          modifier.previewPatches(List.of(edit)).stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(List.of(edit));
      assertEquals(EXPECTED, Files.readString(file));
    }
  }

  @Test
  @DisplayName("An unknown owner cannot select a same-named declaration from a stale line")
  void refuseUnknownStoredOwner() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = addNestedAction(fixture);
      ChangeRequest edit = fixture.createChange("nested", "HasTooltip", "Nested tooltip");
      edit.setSource(new SourceLocation(file.toString(), 14,
          "com.devtoolsapplayoutspring.views.SourceCasesView$RemovedCard", "action",
          Button.class.getName()));
      fixture.removeComponent("nested");
      SourceCodeModifier modifier = fixture.getModifier();
      final String error = "Cannot find stored declaration 'action' in "
          + "com.devtoolsapplayoutspring.views.SourceCasesView$RemovedCard";

      assertEquals(error, modifier.preview(List.of(edit)).get(0).getError());
      assertEquals(SOURCE, Files.readString(file));
      assertEquals(error, modifier.apply(List.of(edit)).get(0).getError());
      assertEquals(SOURCE, Files.readString(file));
    }
  }

  @Test
  @DisplayName("A missing runtime source file leaves known application files unchanged")
  void refuseMissingSourceFile() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = addNestedAction(fixture);
      fixture.addComponent("missing", Button.class,
          List.of(new SourcePoint("com.devtoolsapplayoutspring.views.MissingView",
              "MissingView.java", 14)));
      List<ChangeRequest> edits =
          List.of(fixture.createChange("missing", "HasTooltip", "Saved tooltip"));
      SourceCodeModifier modifier = fixture.getModifier();

      assertEquals("The source file for this component was not found",
          modifier.preview(edits).get(0).getError());
      assertEquals(SOURCE, Files.readString(file));
      assertEquals("The source file for this component was not found",
          modifier.apply(edits).get(0).getError());
      assertEquals(SOURCE, Files.readString(file));
    }
  }

  private Path addNestedAction(SourceWriteFixture fixture) throws IOException {
    final String owner = "com.devtoolsapplayoutspring.views.SourceCasesView$NestedCard";
    Path file = fixture.addSource("com.devtoolsapplayoutspring.views.SourceCasesView", SOURCE);
    fixture.addSourceClass(owner, file);
    fixture.addComponent("nested", Button.class,
        List.of(new SourcePoint(owner, "SourceCasesView.java", 23)));
    return file;
  }
}
