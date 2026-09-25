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
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class CreationFormsCaseTest {

  private static final String SOURCE = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/creation-cases", outlet = MainLayout.class)
      @FrameTitle("Creation cases")
      public class CreationCasesView extends Composite<FlexLayout> {
        private final Button initialized = new Button("Initialized action");

        {
          initialized.setTooltipText("Initializer tooltip");
        }

        public CreationCasesView() {
          getBoundComponent().add(initialized);
          getBoundComponent().add(new Button("Inline action"));
          getBoundComponent().add(createAction());
          getBoundComponent().add(new InnerActions());
          class LocalActions extends Composite<FlexLayout> {
            private final Button action = new Button("Local class action");

            LocalActions() {
              action.setTooltipText("Local class tooltip");
              getBoundComponent().add(action);
            }
          }
          getBoundComponent().add(new LocalActions());
        }

        private Button createAction() {
          return new Button("Returned action");
        }

        class InnerActions extends Composite<FlexLayout> {
          private final Button action = new Button("Inner class action");

          InnerActions() {
            action.setTooltipText("Inner class tooltip");
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

      @Route(value = "/creation-cases", outlet = MainLayout.class)
      @FrameTitle("Creation cases")
      public class CreationCasesView extends Composite<FlexLayout> {
        private final Button initialized = new Button("Initialized action");

        {
        }

        public CreationCasesView() {
          getBoundComponent().add(initialized);
          Button button = new Button("Inline action");
          button.setTooltipText("Saved inline tooltip");
          getBoundComponent().add(button);
          getBoundComponent().add(createAction());
          getBoundComponent().add(new InnerActions());
          class LocalActions extends Composite<FlexLayout> {
            private final Button action = new Button("Local class action");

            LocalActions() {
              action.setTooltipText("Saved local tooltip");
              getBoundComponent().add(action);
            }
          }
          getBoundComponent().add(new LocalActions());
        }

        private Button createAction() {
          Button button = new Button("Returned action");
          button.setTooltipText("Saved returned tooltip");
          return button;
        }

        class InnerActions extends Composite<FlexLayout> {
          private final Button action = new Button("Inner class action");

          InnerActions() {
            action.setTooltipText("Saved inner tooltip");
            getBoundComponent().add(action);
          }
        }
      }
      """;

  @TempDir
  Path temporaryDirectory;

  @ParameterizedTest
  @CsvSource({"false, false", "true, false", "false, true"})
  @DisplayName("Initializer reset and inline, returned, inner-class and local-class writes stay in "
      + "scope")
  void writeCreationFormsTogether(boolean reverse, boolean crlf) throws IOException {
    final String source = crlf ? SOURCE.replace("\n", "\r\n") : SOURCE;
    final String expected = crlf ? EXPECTED.replace("\n", "\r\n") : EXPECTED;
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      final String owner = "com.devtoolsapplayoutspring.views.CreationCasesView";
      Path file = fixture.addSource(owner, source);
      fixture.addSourceClass(owner + "$InnerActions", file);
      fixture.addSourceClass(owner + "$1LocalActions", file);
      addComponent(fixture, "initialized", owner, 12);
      addComponent(fixture, "inline", owner, 20);
      addComponent(fixture, "returned", owner, 35);
      addComponent(fixture, "inner", owner + "$InnerActions", 39);
      addComponent(fixture, "local", owner + "$1LocalActions", 24);
      List<ChangeRequest> edits =
          new ArrayList<>(List.of(fixture.createChange("initialized", "HasTooltip", ""),
              fixture.createChange("inline", "HasTooltip", "Saved inline tooltip"),
              fixture.createChange("returned", "HasTooltip", "Saved returned tooltip"),
              fixture.createChange("inner", "HasTooltip", "Saved inner tooltip"),
              fixture.createChange("local", "HasTooltip", "Saved local tooltip")));
      if (reverse) {
        Collections.reverse(edits);
      }
      SourceCodeModifier modifier = fixture.getModifier();

      assertEquals(List.of(expected),
          modifier.previewPatches(edits).stream().map(FilePatch::getPatched).toList());
      assertEquals(source, Files.readString(file));
      modifier.apply(edits);
      assertEquals(expected, Files.readString(file));
    }
  }

  private void addComponent(SourceWriteFixture fixture, String id, String owner, int line) {
    fixture.addComponent(id, Button.class,
        List.of(new SourcePoint(owner, "CreationCasesView.java", line)));
  }
}
