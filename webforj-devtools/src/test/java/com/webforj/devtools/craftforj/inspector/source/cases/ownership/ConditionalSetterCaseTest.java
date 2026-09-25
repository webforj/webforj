package com.webforj.devtools.craftforj.inspector.source.cases.ownership;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.button.Button;
import com.webforj.devtools.craftforj.inspector.source.SourceCodeModifier;
import com.webforj.devtools.craftforj.inspector.source.cases.support.SourceWriteFixture;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class ConditionalSetterCaseTest {

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
          if (Boolean.getBoolean("craftforj.case")) {
            action.setTooltipText("Initial tooltip");
          } else {
            action.setTooltipText("Alternative tooltip");
          }
          action.onClick(event -> action.setTooltipText("Clicked tooltip"));
          getBoundComponent().add(action);
        }
      }
      """;

  @TempDir
  Path temporaryDirectory;

  @ParameterizedTest
  @ValueSource(strings = {"Saved tooltip", ""})
  @DisplayName("Conditional field setters report ambiguity without rewriting either branch")
  void refuseConditionalWrite(String value) throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = fixture.addSource("com.devtoolsapplayoutspring.views.ExecutionCasesView", SOURCE);
      fixture.addComponent("callback", Button.class,
          List.of(new SourcePoint("com.devtoolsapplayoutspring.views.ExecutionCasesView",
              "ExecutionCasesView.java", 12)));
      List<ChangeRequest> edits = List.of(fixture.createChange("callback", "HasTooltip", value));
      SourceCodeModifier modifier = fixture.getModifier();
      final String expectedError = "TooltipText is set under a condition, so one value cannot "
          + "replace it. Edit the source directly.";

      assertEquals(expectedError, modifier.preview(edits).get(0).getError());
      assertEquals(SOURCE, Files.readString(file));
      assertEquals(expectedError, modifier.apply(edits).get(0).getError());
      assertEquals(SOURCE, Files.readString(file));
    }
  }
}
