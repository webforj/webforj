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

class LocalInsertionCaseTest {

  private static final String SOURCE = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.Theme;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.component.toast.Toast;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/shadowing-cases", outlet = MainLayout.class)
      @FrameTitle("Shadowing cases")
      public class ShadowingCasesView extends Composite<FlexLayout> {
        private final Button action = new Button("Field action");

        public ShadowingCasesView() {
          this.action.setTooltipText("Field tooltip");
          Button action = new Button("Local action");
          getBoundComponent().add(this.action, action);
          this.action.onClick(event -> Toast.show("Field action clicked", 3000, Theme.INFO,
              Toast.Placement.BOTTOM_RIGHT));
          action.onClick(event -> Toast.show("Local action clicked", 3000, Theme.INFO,
              Toast.Placement.BOTTOM_RIGHT));
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

      @Route(value = "/shadowing-cases", outlet = MainLayout.class)
      @FrameTitle("Shadowing cases")
      public class ShadowingCasesView extends Composite<FlexLayout> {
        private final Button action = new Button("Field action");

        public ShadowingCasesView() {
          this.action.setTooltipText("Field tooltip");
          Button action = new Button("Local action");
          action.setTooltipText("Saved local tooltip");
          getBoundComponent().add(this.action, action);
          this.action.onClick(event -> Toast.show("Field action clicked", 3000, Theme.INFO,
              Toast.Placement.BOTTOM_RIGHT));
          action.onClick(event -> Toast.show("Local action clicked", 3000, Theme.INFO,
              Toast.Placement.BOTTOM_RIGHT));
        }
      }
      """;

  @TempDir
  Path temporaryDirectory;

  @Test
  @DisplayName("A preceding same-named field call cannot put a local setter before its declaration")
  void insertAfterLocalDeclaration() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = fixture.addSource("com.devtoolsapplayoutspring.views.ShadowingCasesView", SOURCE);
      fixture.addComponent("local", Button.class,
          List.of(new SourcePoint("com.devtoolsapplayoutspring.views.ShadowingCasesView",
              "ShadowingCasesView.java", 18)));
      List<ChangeRequest> edits =
          List.of(fixture.createChange("local", "HasTooltip", "Saved local tooltip"));
      SourceCodeModifier modifier = fixture.getModifier();

      assertEquals(List.of(EXPECTED),
          modifier.previewPatches(edits).stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(edits);
      assertEquals(EXPECTED, Files.readString(file));
    }
  }
}
