package com.webforj.devtools.craftforj.inspector.source.cases.ownership;

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

class ExecutionScopeCaseTest {

  private static final String SOURCE = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/execution-cases", outlet = MainLayout.class)
      @FrameTitle("Execution cases")
      public class ExecutionCasesView extends Composite<FlexLayout> {
        private final Button action = new Button("Callback action");

        public ExecutionCasesView() {
          action.setTooltipText("Initial tooltip");
          action.onClick(event -> action.setTooltipText("Clicked tooltip"));
          getBoundComponent().add(action);
          boolean show = true;
          if (show) {
            Button action = new Button("Branch action");
            action.setTooltipText("Branch tooltip");
            getBoundComponent().add(action);
          }
          switch (System.getProperty("craftforj.case", "default")) {
            case "default" -> {
              Button action = new Button("Switch action");
              action.setTooltipText("Switch tooltip");
              getBoundComponent().add(action);
            }
            default -> {
            }
          }
          {
            Button action = new Button("Block action");
            getBoundComponent().add(action);
          }
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

      @Route(value = "/execution-cases", outlet = MainLayout.class)
      @FrameTitle("Execution cases")
      public class ExecutionCasesView extends Composite<FlexLayout> {
        private final Button action = new Button("Callback action");

        public ExecutionCasesView() {
          action.setTooltipText("Saved initial tooltip");
          action.onClick(event -> action.setTooltipText("Clicked tooltip"));
          getBoundComponent().add(action);
          boolean show = true;
          if (show) {
            Button action = new Button("Branch action");
            action.setTooltipText("Saved branch tooltip");
            getBoundComponent().add(action);
          }
          switch (System.getProperty("craftforj.case", "default")) {
            case "default" -> {
              Button action = new Button("Switch action");
              action.setTooltipText("Saved switch tooltip");
              getBoundComponent().add(action);
            }
            default -> {
            }
          }
          {
            Button action = new Button("Block action");
            getBoundComponent().add(action);
            action.setTooltipText("Saved block tooltip");
          }
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

      @Route(value = "/execution-cases", outlet = MainLayout.class)
      @FrameTitle("Execution cases")
      public class ExecutionCasesView extends Composite<FlexLayout> {
        private final Button action = new Button("Callback action");

        public ExecutionCasesView() {
          action.onClick(event -> action.setTooltipText("Clicked tooltip"));
          getBoundComponent().add(action);
          boolean show = true;
          if (show) {
            Button action = new Button("Branch action");
            action.setTooltipText("Branch tooltip");
            getBoundComponent().add(action);
          }
          switch (System.getProperty("craftforj.case", "default")) {
            case "default" -> {
              Button action = new Button("Switch action");
              action.setTooltipText("Switch tooltip");
              getBoundComponent().add(action);
            }
            default -> {
            }
          }
          {
            Button action = new Button("Block action");
            getBoundComponent().add(action);
          }
        }
      }
      """;

  @TempDir
  Path temporaryDirectory;

  @Test
  @DisplayName("Initial, branch, switch and nested-block edits preserve the deferred callback")
  void writeWithinExecutionScope() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      final Path file =
          fixture.addSource("com.devtoolsapplayoutspring.views.ExecutionCasesView", SOURCE);
      addComponent(fixture, "callback", 12);
      addComponent(fixture, "branch", 20);
      addComponent(fixture, "switch", 26);
      addComponent(fixture, "block", 34);
      List<ChangeRequest> edits =
          List.of(fixture.createChange("callback", "HasTooltip", "Saved initial tooltip"),
              fixture.createChange("branch", "HasTooltip", "Saved branch tooltip"),
              fixture.createChange("switch", "HasTooltip", "Saved switch tooltip"),
              fixture.createChange("block", "HasTooltip", "Saved block tooltip"));
      SourceCodeModifier modifier = fixture.getModifier();

      assertEquals(List.of(EXPECTED),
          modifier.previewPatches(edits).stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(edits);
      assertEquals(EXPECTED, Files.readString(file));
    }
  }

  @Test
  @DisplayName("Resetting an initial tooltip preserves the click handler's later tooltip")
  void removeOnlyInitialSetter() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = fixture.addSource("com.devtoolsapplayoutspring.views.ExecutionCasesView", SOURCE);
      addComponent(fixture, "callback", 12);
      List<ChangeRequest> edits = List.of(fixture.createChange("callback", "HasTooltip", ""));
      SourceCodeModifier modifier = fixture.getModifier();

      assertEquals(List.of(RESET),
          modifier.previewPatches(edits).stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(edits);
      assertEquals(RESET, Files.readString(file));
    }
  }

  private void addComponent(SourceWriteFixture fixture, String id, int line) {
    fixture.addComponent(id, Button.class,
        List.of(new SourcePoint("com.devtoolsapplayoutspring.views.ExecutionCasesView",
            "ExecutionCasesView.java", line)));
  }
}
