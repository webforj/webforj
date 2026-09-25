package com.webforj.devtools.craftforj.inspector.source.cases.values;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.button.Button;
import com.webforj.component.layout.flexlayout.FlexLayout;
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

class LayoutCallbackCaseTest {

  private static final String SOURCE = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/layout-callback-cases", outlet = MainLayout.class)
      @FrameTitle("Layout callback cases")
      public class LayoutCallbackCasesView extends Composite<FlexLayout> {
        private final FlexLayout row = new FlexLayout();

        public LayoutCallbackCasesView() {
          Button action = new Button("Grow action");
          Button other = new Button("Other grow action");
          row.add(action, other);
          row.setItemGrow(1, action, other);
          action.onClick(event -> row.setItemGrow(4, action));
          getBoundComponent().add(row);
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

      @Route(value = "/layout-callback-cases", outlet = MainLayout.class)
      @FrameTitle("Layout callback cases")
      public class LayoutCallbackCasesView extends Composite<FlexLayout> {
        private final FlexLayout row = new FlexLayout();

        public LayoutCallbackCasesView() {
          Button action = new Button("Grow action");
          Button other = new Button("Other grow action");
          row.add(action, other);
          row.setItemGrow(1, other);
          action.onClick(event -> row.setItemGrow(4, action));
          row.setItemGrow(2.0, action);
          getBoundComponent().add(row);
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

      @Route(value = "/layout-callback-cases", outlet = MainLayout.class)
      @FrameTitle("Layout callback cases")
      public class LayoutCallbackCasesView extends Composite<FlexLayout> {
        private final FlexLayout row = new FlexLayout();

        public LayoutCallbackCasesView() {
          Button action = new Button("Grow action");
          Button other = new Button("Other grow action");
          row.add(action, other);
          row.setItemGrow(1, other);
          action.onClick(event -> row.setItemGrow(4, action));
          getBoundComponent().add(row);
        }
      }
      """;

  @TempDir
  Path temporaryDirectory;

  @Test
  @DisplayName("Changing one shared layout item preserves the other item and deferred callback")
  void writeOnlyInitialItemValue() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = addComponents(fixture);
      List<ChangeRequest> edits = List.of(createChange(fixture, file, 2.0));
      SourceCodeModifier modifier = fixture.getModifier();

      assertEquals(List.of(EXPECTED),
          modifier.previewPatches(edits).stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(edits);
      assertEquals(EXPECTED, Files.readString(file));
      modifier.apply(edits);
      assertEquals(EXPECTED, Files.readString(file));
    }
  }

  @Test
  @DisplayName("Resetting one shared layout item preserves the other item and deferred callback")
  void removeOnlyInitialItemValue() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = addComponents(fixture);
      List<ChangeRequest> edits = List.of(createChange(fixture, file, ""));
      SourceCodeModifier modifier = fixture.getModifier();

      assertEquals(List.of(RESET),
          modifier.previewPatches(edits).stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(edits);
      assertEquals(RESET, Files.readString(file));
    }
  }

  private Path addComponents(SourceWriteFixture fixture) throws IOException {
    final String owner = "com.devtoolsapplayoutspring.views.LayoutCallbackCasesView";
    Path file = fixture.addSource(owner, SOURCE);
    fixture.addComponent("row", FlexLayout.class,
        List.of(new SourcePoint(owner, "LayoutCallbackCasesView.java", 12)));
    fixture.addComponent("action", Button.class,
        List.of(new SourcePoint(owner, "LayoutCallbackCasesView.java", 15)));
    return file;
  }

  private ChangeRequest createChange(SourceWriteFixture fixture, Path file, Object value) {
    ChangeRequest edit = fixture.createChange("action", "FlexItemGrow", value);
    edit.setParentId("row");
    edit.setParentSource(
        new SourceLocation(file.toString(), 12, null, "row", FlexLayout.class.getName()));
    return edit;
  }
}
