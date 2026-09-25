package com.webforj.devtools.craftforj.inspector.source.cases.values;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.button.Button;
import com.webforj.devtools.craftforj.inspector.source.SourceCodeModifier;
import com.webforj.devtools.craftforj.inspector.source.cases.support.SourceWriteFixture;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
import com.webforj.devtools.craftforj.source.model.FilePatch;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class AccumulatedListCaseTest {

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
          action.addClassName("outer-action");
          getBoundComponent().add(action, new NestedCard());
          action.onClick(event -> Toast.show("Outer action clicked", 3000, Theme.INFO,
              Toast.Placement.BOTTOM_RIGHT));
        }

        static class NestedCard extends Composite<FlexLayout> {
          private final Button action = new Button("Nested action");

          NestedCard() {
            action.addClassName("first").setTooltipText("Nested tooltip").addClassName("second");
            action.addClassName("third");
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
          action.addClassName("outer-action");
          getBoundComponent().add(action, new NestedCard());
          action.onClick(event -> Toast.show("Outer action clicked", 3000, Theme.INFO,
              Toast.Placement.BOTTOM_RIGHT));
        }

        static class NestedCard extends Composite<FlexLayout> {
          private final Button action = new Button("Nested action");

          NestedCard() {
            action.setTooltipText("Nested tooltip");
            action.addClassName("saved", "keep");
            getBoundComponent().add(action);
            action.onClick(event -> Toast.show("Nested action clicked", 3000, Theme.INFO,
                Toast.Placement.BOTTOM_RIGHT));
          }
        }
      }
      """;

  @TempDir
  Path temporaryDirectory;

  @Test
  @DisplayName("Replacing accumulated class names preserves the fluent tooltip and other component")
  void replaceAllAccumulatedValues() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = addComponent(fixture);
      List<ChangeRequest> edits =
          List.of(fixture.createChange("nested", "HasClassName", List.of("saved", "keep")));
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
  @DisplayName("A non-string class name reports the invalid element and leaves all source "
      + "unchanged")
  void refuseInvalidElement() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = addComponent(fixture);
      List<ChangeRequest> edits =
          List.of(fixture.createChange("nested", "HasClassName", List.of("keep", 12)));
      SourceCodeModifier modifier = fixture.getModifier();
      final String expectedError =
          "Property 'addClassName': expects String items but item 2 is not a String";

      assertEquals(expectedError, modifier.preview(edits).get(0).getError());
      assertEquals(SOURCE, Files.readString(file));
      assertEquals(expectedError, modifier.apply(edits).get(0).getError());
      assertEquals(SOURCE, Files.readString(file));
    }
  }

  private Path addComponent(SourceWriteFixture fixture) throws IOException {
    Path file = fixture.addSource("com.devtoolsapplayoutspring.views.SourceCasesView", SOURCE);
    fixture.addSourceClass("com.devtoolsapplayoutspring.views.SourceCasesView$NestedCard", file);
    fixture.addComponent("nested", Button.class,
        List.of(new SourcePoint("com.devtoolsapplayoutspring.views.SourceCasesView$NestedCard",
            "SourceCasesView.java", 24)));
    return file;
  }
}
