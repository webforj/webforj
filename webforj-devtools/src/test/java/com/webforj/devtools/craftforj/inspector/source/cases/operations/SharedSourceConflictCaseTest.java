package com.webforj.devtools.craftforj.inspector.source.cases.operations;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.button.Button;
import com.webforj.devtools.craftforj.inspector.source.SourceCodeModifier;
import com.webforj.devtools.craftforj.inspector.source.cases.support.SourceWriteFixture;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeResult;
import com.webforj.devtools.craftforj.source.model.FilePatch;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class SharedSourceConflictCaseTest {

  private static final String SOURCE = """
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

  private static final String EXPECTED = """
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

  private static final String WIDTH_EXPECTED = """
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
            action.setWidth("200px");
          }
        }
      }
      """;

  @TempDir
  Path temporaryDirectory;

  @Test
  @DisplayName("Different values for instances of one statement report the conflict and leave "
      + "source unchanged")
  void refuseConflictingSharedValues() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = addComponents(fixture);
      List<ChangeRequest> changes =
          List.of(fixture.createChange("loop-1", "HasTooltip", "First tooltip"),
              fixture.createChange("loop-2", "HasTooltip", "Second tooltip"));
      SourceCodeModifier modifier = fixture.getModifier();
      final String error =
          "Property 'TooltipText' has conflicting values for the same source statement. "
              + "Choose one value for all instances.";

      assertEquals(List.of(error, error),
          modifier.preview(changes).stream().map(ChangeResult::getError).toList());
      assertEquals(SOURCE, Files.readString(file));
      assertEquals(List.of(error, error),
          modifier.apply(changes).stream().map(ChangeResult::getError).toList());
      assertEquals(SOURCE, Files.readString(file));
    }
  }

  @Test
  @DisplayName("A conflict on one property leaves the same component's unrelated property write "
      + "intact")
  void keepUnrelatedPropertyOnConflict() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = addComponents(fixture);
      List<ChangeRequest> changes =
          List.of(fixture.createChange("loop-1", "HasTooltip", "First tooltip"),
              fixture.createChange("loop-1", "HasWidth", "200px"),
              fixture.createChange("loop-2", "HasTooltip", "Second tooltip"));
      SourceCodeModifier modifier = fixture.getModifier();
      final String error =
          "Property 'TooltipText' has conflicting values for the same source statement. "
              + "Choose one value for all instances.";

      assertEquals(List.of(WIDTH_EXPECTED),
          modifier.previewPatches(changes).stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      List<ChangeResult> results = modifier.apply(changes);
      assertEquals(List.of(error, error), results.stream().filter(result -> !result.isSuccess())
          .map(ChangeResult::getError).toList());
      assertEquals(1, results.stream().filter(ChangeResult::isSuccess).count());
      assertEquals(WIDTH_EXPECTED, Files.readString(file));
    }
  }

  @Test
  @DisplayName("Matching values for shared instances produce one setter and remain stable on "
      + "repeat")
  void writeMatchingSharedValues() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = addComponents(fixture);
      List<ChangeRequest> changes =
          List.of(fixture.createChange("loop-1", "HasTooltip", "Shared tooltip"),
              fixture.createChange("loop-2", "HasTooltip", "Shared tooltip"));
      SourceCodeModifier modifier = fixture.getModifier();

      assertEquals(List.of(EXPECTED),
          modifier.previewPatches(changes).stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(changes);
      assertEquals(EXPECTED, Files.readString(file));
      modifier.apply(changes);
      assertEquals(EXPECTED, Files.readString(file));
    }
  }

  private Path addComponents(SourceWriteFixture fixture) throws IOException {
    final String owner = "com.devtoolsapplayoutspring.views.LoopSourceCasesView";
    Path file = fixture.addSource(owner, SOURCE);
    for (int number = 1; number <= 5; number++) {
      fixture.addComponent("loop-" + number, Button.class,
          List.of(new SourcePoint(owner, "LoopSourceCasesView.java", 17)));
    }
    return file;
  }
}
