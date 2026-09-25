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

class ShadowedFieldCaseTest {

  @TempDir
  Path temporaryDirectory;

  @Test
  @DisplayName("Field edits, resets and insertion preserve the identically named local button")
  void writeOnlyFieldReceiver() throws IOException {
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
            this.action.setTooltipText("Field tooltip");
            Button action = new Button("Local action");
            action.setTooltipText("Local tooltip");
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
            this.action.setTooltipText("Edited field tooltip");
            Button action = new Button("Local action");
            action.setTooltipText("Local tooltip");
            getBoundComponent().add(this.action, action);
            this.action.onClick(event -> Toast.show("Field action clicked", 3000, Theme.INFO,
                Toast.Placement.BOTTOM_RIGHT));
            action.onClick(event -> Toast.show("Local action clicked", 3000, Theme.INFO,
                Toast.Placement.BOTTOM_RIGHT));
          }
        }
        """;
    final String reset = """
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
            getBoundComponent().add(this.action, action);
            this.action.onClick(event -> Toast.show("Field action clicked", 3000, Theme.INFO,
                Toast.Placement.BOTTOM_RIGHT));
            action.onClick(event -> Toast.show("Local action clicked", 3000, Theme.INFO,
                Toast.Placement.BOTTOM_RIGHT));
          }
        }
        """;
    final String restored = """
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
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = fixture.addSource("com.devtoolsapplayoutspring.views.ShadowingCasesView", source);
      fixture.addComponent("field-action", Button.class,
          List.of(new SourcePoint("com.devtoolsapplayoutspring.views.ShadowingCasesView",
              "ShadowingCasesView.java", 14)));
      SourceCodeModifier modifier = fixture.getModifier();
      ChangeRequest edit =
          fixture.createChange("field-action", "HasTooltip", "Edited field tooltip");

      List<FilePatch> patches = modifier.previewPatches(List.of(edit));
      assertEquals(List.of(expected), patches.stream().map(FilePatch::getPatched).toList());
      assertEquals(source, Files.readString(file));
      modifier.apply(List.of(edit));
      assertEquals(expected, Files.readString(file));

      ChangeRequest erase = fixture.createChange("field-action", "HasTooltip", "");
      List<FilePatch> erased = modifier.previewPatches(List.of(erase));
      assertEquals(List.of(reset), erased.stream().map(FilePatch::getPatched).toList());
      assertEquals(expected, Files.readString(file));
      modifier.apply(List.of(erase));
      assertEquals(reset, Files.readString(file));

      ChangeRequest restore =
          fixture.createChange("field-action", "HasTooltip", "Restored field tooltip");
      List<FilePatch> restoredPatches = modifier.previewPatches(List.of(restore));
      assertEquals(List.of(restored), restoredPatches.stream().map(FilePatch::getPatched).toList());
      assertEquals(reset, Files.readString(file));
      modifier.apply(List.of(restore));
      assertEquals(restored, Files.readString(file));
    }
  }
}
