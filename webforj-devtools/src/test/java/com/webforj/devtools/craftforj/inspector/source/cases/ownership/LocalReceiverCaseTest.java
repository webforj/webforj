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

class LocalReceiverCaseTest {

  @TempDir
  Path temporaryDirectory;

  @Test
  @DisplayName("A local button edit preserves a later setter on the this-qualified field")
  void writeOnlyLocalReceiver() throws IOException {
    final String source = """
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
            Button action = new Button("Local action");
            action.setTooltipText("Local tooltip");
            this.action.setTooltipText("Restored field tooltip");
            getBoundComponent().add(this.action, action);
            this.action.onClick(event -> Toast.show("Field action clicked", 3000, Theme.INFO,
                Toast.Placement.BOTTOM_RIGHT));
            action.onClick(event -> Toast.show("Local action clicked", 3000, Theme.INFO,
                Toast.Placement.BOTTOM_RIGHT));
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

        @Route(value = "/shadowing-cases", outlet = MainLayout.class)
        @FrameTitle("Shadowing cases")
        public class ShadowingCasesView extends Composite<FlexLayout> {
          private final Button action = new Button("Field action");

          public ShadowingCasesView() {
            Button action = new Button("Local action");
            action.setTooltipText("Edited local tooltip");
            this.action.setTooltipText("Restored field tooltip");
            getBoundComponent().add(this.action, action);
            this.action.onClick(event -> Toast.show("Field action clicked", 3000, Theme.INFO,
                Toast.Placement.BOTTOM_RIGHT));
            action.onClick(event -> Toast.show("Local action clicked", 3000, Theme.INFO,
                Toast.Placement.BOTTOM_RIGHT));
          }
        }
        """;
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = fixture.addSource("com.devtoolsapplayoutspring.views.ShadowingCasesView", source);
      fixture.addComponent("local-action", Button.class,
          List.of(new SourcePoint("com.devtoolsapplayoutspring.views.ShadowingCasesView",
              "ShadowingCasesView.java", 17)));
      SourceCodeModifier modifier = fixture.getModifier();
      ChangeRequest edit =
          fixture.createChange("local-action", "HasTooltip", "Edited local tooltip");

      List<FilePatch> patches = modifier.previewPatches(List.of(edit));
      assertEquals(List.of(expected), patches.stream().map(FilePatch::getPatched).toList());
      assertEquals(source, Files.readString(file));
      modifier.apply(List.of(edit));
      assertEquals(expected, Files.readString(file));
    }
  }
}
