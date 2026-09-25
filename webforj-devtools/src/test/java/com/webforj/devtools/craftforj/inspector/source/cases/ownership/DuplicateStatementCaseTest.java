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

class DuplicateStatementCaseTest {

  private static final String OWNER =
      "com.devtoolsapplayoutspring.views.DuplicateStatementCasesView";

  private static final String INLINE = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/duplicate-statement-cases", outlet = MainLayout.class)
      @FrameTitle("Duplicate statement cases")
      public class DuplicateStatementCasesView extends Composite<FlexLayout> {
        public DuplicateStatementCasesView() {
          getBoundComponent().add(new Button("Repeated action"));
          getBoundComponent().add(new Button("Repeated action"));
        }
      }
      """;

  private static final String INLINE_EXPECTED = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/duplicate-statement-cases", outlet = MainLayout.class)
      @FrameTitle("Duplicate statement cases")
      public class DuplicateStatementCasesView extends Composite<FlexLayout> {
        public DuplicateStatementCasesView() {
          getBoundComponent().add(new Button("Repeated action"));
          Button button = new Button("Repeated action");
          button.setTooltipText("Saved tooltip");
          getBoundComponent().add(button);
        }
      }
      """;

  private static final String SHADOWED = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/duplicate-statement-cases", outlet = MainLayout.class)
      @FrameTitle("Duplicate statement cases")
      public class DuplicateStatementCasesView extends Composite<FlexLayout> {
        private final Button action = new Button("Field action");

        public DuplicateStatementCasesView() {
          action.setTooltipText("Shared tooltip");
          Button action = new Button("Local action");
          action.setTooltipText("Shared tooltip");
          getBoundComponent().add(this.action, action);
        }
      }
      """;

  private static final String SHADOWED_EXPECTED = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/duplicate-statement-cases", outlet = MainLayout.class)
      @FrameTitle("Duplicate statement cases")
      public class DuplicateStatementCasesView extends Composite<FlexLayout> {
        private final Button action = new Button("Field action");

        public DuplicateStatementCasesView() {
          action.setTooltipText("Shared tooltip");
          Button action = new Button("Local action");
          action.setTooltipText("Edited local tooltip");
          getBoundComponent().add(this.action, action);
        }
      }
      """;

  @TempDir
  Path temporaryDirectory;

  @Test
  @DisplayName("Extracting the second of two identical inline creations leaves the first statement "
      + "intact")
  void extractSecondIdenticalInlineCreation() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = fixture.addSource(OWNER, INLINE);
      fixture.addComponent("second", Button.class,
          List.of(new SourcePoint(OWNER, "DuplicateStatementCasesView.java", 14)));
      List<ChangeRequest> edits =
          List.of(fixture.createChange("second", "HasTooltip", "Saved tooltip"));
      SourceCodeModifier modifier = fixture.getModifier();

      assertEquals(List.of(INLINE_EXPECTED),
          modifier.previewPatches(edits).stream().map(FilePatch::getPatched).toList());
      assertEquals(INLINE, Files.readString(file));
      modifier.apply(edits);
      assertEquals(INLINE_EXPECTED, Files.readString(file));
    }
  }

  @Test
  @DisplayName("A setter statement identical to an earlier field setter still resolves to the "
      + "shadowing local")
  void writeLocalBehindIdenticalFieldSetter() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = fixture.addSource(OWNER, SHADOWED);
      fixture.addComponent("local-action", Button.class,
          List.of(new SourcePoint(OWNER, "DuplicateStatementCasesView.java", 16)));
      List<ChangeRequest> edits =
          List.of(fixture.createChange("local-action", "HasTooltip", "Edited local tooltip"));
      SourceCodeModifier modifier = fixture.getModifier();

      assertEquals(List.of(SHADOWED_EXPECTED),
          modifier.previewPatches(edits).stream().map(FilePatch::getPatched).toList());
      assertEquals(SHADOWED, Files.readString(file));
      modifier.apply(edits);
      assertEquals(SHADOWED_EXPECTED, Files.readString(file));
      modifier.apply(edits);
      assertEquals(SHADOWED_EXPECTED, Files.readString(file));
    }
  }
}
