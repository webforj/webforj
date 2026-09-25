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
import java.util.stream.Stream;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class StringLiteralCaseTest {

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

  private static final String ESCAPED = """
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
            action.setTooltipText("Say \\"hello\\" in C:\\\\work\\\\demo — Grüße ☕");
          }
        }
      }
      """;

  private static final String MULTILINE = """
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
            action.setTooltipText(\"\"\"
              First line\\s
              Path C:\\\\work
              \\"\\"\\" end\\
              \"\"\");
          }
        }
      }
      """;

  private static final String CRLF = """
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
            action.setTooltipText("First\\r\\nSecond\\tline");
          }
        }
      }
      """;

  @TempDir
  Path temporaryDirectory;

  @ParameterizedTest
  @MethodSource("getLiteralCases")
  @DisplayName("Saving escaped, multiline and CRLF text preserves complete Java source")
  void writeStringLiteral(String value, String expected) throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      final String owner = "com.devtoolsapplayoutspring.views.LoopSourceCasesView";
      Path file = fixture.addSource(owner, SOURCE);
      fixture.addComponent("action", Button.class,
          List.of(new SourcePoint(owner, "LoopSourceCasesView.java", 17)));
      ChangeRequest edit = fixture.createChange("action", "HasTooltip", value);
      SourceCodeModifier modifier = fixture.getModifier();

      assertEquals(List.of(expected),
          modifier.previewPatches(List.of(edit)).stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(List.of(edit));
      assertEquals(expected, Files.readString(file));
      modifier.apply(List.of(edit));
      assertEquals(expected, Files.readString(file));
    }
  }

  private static Stream<Arguments> getLiteralCases() {
    return Stream.of(Arguments.of("Say \"hello\" in C:\\work\\demo — Grüße ☕", ESCAPED),
        Arguments.of("First line \nPath C:\\work\n\"\"\" end", MULTILINE),
        Arguments.of("First\r\nSecond\tline", CRLF));
  }
}
