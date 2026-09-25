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

/** An edit to a loop definition applies to every instance created at that statement. */
class LoopEditCaseTest {

  @TempDir
  Path temporaryDirectory;

  @Test
  @DisplayName("A loop edit writes one shared setter and erasing its text removes that setter")
  void applyAndEraseSharedTooltip() throws IOException {
    final String source = """
        package com.devtoolsapplayoutspring.views;

        import com.webforj.component.Composite;
        import com.webforj.component.Theme;
        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;
        import com.webforj.component.toast.Toast;
        import com.webforj.router.annotation.FrameTitle;
        import com.webforj.router.annotation.Route;

        @Route(value = "/loop-source-cases", outlet = MainLayout.class)
        @FrameTitle("Loop source cases")
        public class LoopSourceCasesView extends Composite<FlexLayout> {
          public LoopSourceCasesView() {
            for (int number = 1; number <= 5; number++) {
              String label = "Loop action " + number;
              Button action = new Button(label);
              getBoundComponent().add(action);
              action.onClick(event -> Toast.show(label, 3000, Theme.INFO,
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

        @Route(value = "/loop-source-cases", outlet = MainLayout.class)
        @FrameTitle("Loop source cases")
        public class LoopSourceCasesView extends Composite<FlexLayout> {
          public LoopSourceCasesView() {
            for (int number = 1; number <= 5; number++) {
              String label = "Loop action " + number;
              Button action = new Button(label);
              getBoundComponent().add(action);
              action.onClick(event -> Toast.show(label, 3000, Theme.INFO,
                  Toast.Placement.BOTTOM_RIGHT));
              action.setTooltipText("Shared tooltip");
            }
          }
        }
        """;
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file =
          fixture.addSource("com.devtoolsapplayoutspring.views.LoopSourceCasesView", source);
      for (int number = 1; number <= 5; number++) {
        fixture.addComponent("loop-" + number, Button.class,
            List.of(new SourcePoint("com.devtoolsapplayoutspring.views.LoopSourceCasesView",
                "LoopSourceCasesView.java", 17)));
      }
      SourceCodeModifier modifier = fixture.getModifier();
      ChangeRequest edit = fixture.createChange("loop-3", "HasTooltip", "Shared tooltip");

      List<FilePatch> patches = modifier.previewPatches(List.of(edit));
      assertEquals(List.of(expected), patches.stream().map(FilePatch::getPatched).toList());
      assertEquals(source, Files.readString(file));
      modifier.apply(List.of(edit));
      assertEquals(expected, Files.readString(file));

      ChangeRequest erase = fixture.createChange("loop-3", "HasTooltip", "");
      List<FilePatch> erased = modifier.previewPatches(List.of(erase));
      assertEquals(List.of(source), erased.stream().map(FilePatch::getPatched).toList());
      assertEquals(expected, Files.readString(file));
      modifier.apply(List.of(erase));
      assertEquals(source, Files.readString(file));
      modifier.apply(List.of(erase));
      assertEquals(source, Files.readString(file));
    }
  }
}
