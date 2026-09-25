package com.webforj.devtools.craftforj.inspector.source.cases.operations;

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

class RepeatedSetterCaseTest {

  @TempDir
  Path temporaryDirectory;

  @Test
  @DisplayName("A repeated fluent setter updates the last executed call and removes the earlier "
      + "overridden call")
  void updateLastExecutedSetter() throws IOException {
    final String source = """
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
              action.addClassName("first").setTooltipText("Initial tooltip").addClassName("second")
                  .setTooltipText("Effective tooltip");
              action.addClassName("third");
              getBoundComponent().add(action);
              action.onClick(event -> Toast.show("Nested action clicked", 3000, Theme.INFO,
                  Toast.Placement.BOTTOM_RIGHT));
            }
          }
        }
        """;
    final String expected = """
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
              action.addClassName("first").addClassName("second")
                  .setTooltipText("Edited tooltip");
              action.addClassName("third");
              getBoundComponent().add(action);
              action.onClick(event -> Toast.show("Nested action clicked", 3000, Theme.INFO,
                  Toast.Placement.BOTTOM_RIGHT));
            }
          }
        }
        """;
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = fixture.addSource("com.devtoolsapplayoutspring.views.SourceCasesView", source);
      fixture.addSourceClass("com.devtoolsapplayoutspring.views.SourceCasesView$NestedCard", file);
      fixture.addComponent("nested-action", Button.class,
          List.of(new SourcePoint("com.devtoolsapplayoutspring.views.SourceCasesView$NestedCard",
              "SourceCasesView.java", 24)));
      SourceCodeModifier modifier = fixture.getModifier();
      ChangeRequest edit = fixture.createChange("nested-action", "HasTooltip", "Edited tooltip");

      List<FilePatch> patches = modifier.previewPatches(List.of(edit));
      assertEquals(List.of(expected), patches.stream().map(FilePatch::getPatched).toList());
      assertEquals(source, Files.readString(file));
      modifier.apply(List.of(edit));
      assertEquals(expected, Files.readString(file));
      modifier.apply(List.of(edit));
      assertEquals(expected, Files.readString(file));
    }
  }
}
