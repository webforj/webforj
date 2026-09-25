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
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class HelperExecutionCaseTest {

  private static final String SOURCE =
      """
          package com.devtoolsapplayoutspring.views;

          import com.webforj.component.Composite;
          import com.webforj.component.button.Button;
          import com.webforj.component.layout.flexlayout.FlexDirection;
          import com.webforj.component.layout.flexlayout.FlexLayout;
          import com.webforj.router.annotation.FrameTitle;
          import com.webforj.router.annotation.Route;

          @Route(value = "/combined-initialization-cases", outlet = MainLayout.class)
          @FrameTitle("Combined initialization cases")
          public class CombinedInitializationCasesView extends Composite<FlexLayout> {
            public CombinedInitializationCasesView() {
              Button sized = new Button("Combined size");
              sized.setWidth("180px");
              sized.setSize(240, 48).setTooltipText("Keep tooltip");
              sized.setMinSize("120px", "32px");
              sized.setMaxSize(400, 80);
              sized.onClick(event -> sized.setText("Combined size clicked"));
              Button configured = new Button("Helper initialized");
              configured.setTooltipText("Before helper");
              configure(configured);
              Button update = new Button("Change dimensions");
              update.onClick(event -> {
                sized.setWidth("280px");
                sized.setMinHeight("40px");
                sized.setMaxWidth("360px");
              });
              Button reset = new Button("Reset width");
              reset.onClick(event -> sized.setWidth(""));
              getBoundComponent().setDirection(FlexDirection.COLUMN);
              getBoundComponent().setAlignment(com.webforj.component.layout.flexlayout.FlexAlignment.START);
              getBoundComponent().add(sized, configured, update, reset);
            }

            private void configure(Button button) {
              button.setTooltipText("After helper");
              button.onClick(event -> button.setText("Helper initialized clicked"));
            }
          }
          """;

  private static final String FORWARDED =
      """
          package com.devtoolsapplayoutspring.views;

          import com.webforj.component.Composite;
          import com.webforj.component.button.Button;
          import com.webforj.component.layout.flexlayout.FlexDirection;
          import com.webforj.component.layout.flexlayout.FlexLayout;
          import com.webforj.router.annotation.FrameTitle;
          import com.webforj.router.annotation.Route;

          @Route(value = "/combined-initialization-cases", outlet = MainLayout.class)
          @FrameTitle("Combined initialization cases")
          public class CombinedInitializationCasesView extends Composite<FlexLayout> {
            public CombinedInitializationCasesView() {
              Button sized = new Button("Combined size");
              sized.setWidth("180px");
              sized.setSize(240, 48).setTooltipText("Keep tooltip");
              sized.setMinSize("120px", "32px");
              sized.setMaxSize(400, 80);
              sized.onClick(event -> sized.setText("Combined size clicked"));
              Button configured = new Button("Helper initialized");
              configured.setTooltipText("Before helper");
              configure(configured);
              Button update = new Button("Change dimensions");
              update.onClick(event -> {
                sized.setWidth("280px");
                sized.setMinHeight("40px");
                sized.setMaxWidth("360px");
              });
              Button reset = new Button("Reset width");
              reset.onClick(event -> sized.setWidth(""));
              getBoundComponent().setDirection(FlexDirection.COLUMN);
              getBoundComponent().setAlignment(com.webforj.component.layout.flexlayout.FlexAlignment.START);
              getBoundComponent().add(sized, configured, update, reset);
            }

            private void configure(Button button) {
              configure(button, true);
            }

            private void configure(Button button, boolean enabled) {
              Button alias = button;
              alias.setTooltipText("After helper");
              button.onClick(event -> button.setText("Helper initialized clicked"));
            }
          }
          """;

  private static final String WIDTH_EXPECTED =
      """
          package com.devtoolsapplayoutspring.views;

          import com.webforj.component.Composite;
          import com.webforj.component.button.Button;
          import com.webforj.component.layout.flexlayout.FlexDirection;
          import com.webforj.component.layout.flexlayout.FlexLayout;
          import com.webforj.router.annotation.FrameTitle;
          import com.webforj.router.annotation.Route;

          @Route(value = "/combined-initialization-cases", outlet = MainLayout.class)
          @FrameTitle("Combined initialization cases")
          public class CombinedInitializationCasesView extends Composite<FlexLayout> {
            public CombinedInitializationCasesView() {
              Button sized = new Button("Combined size");
              sized.setWidth("180px");
              sized.setSize(240, 48).setTooltipText("Keep tooltip");
              sized.setMinSize("120px", "32px");
              sized.setMaxSize(400, 80);
              sized.onClick(event -> sized.setText("Combined size clicked"));
              Button configured = new Button("Helper initialized");
              configured.setTooltipText("Before helper");
              configured.setWidth("260px");
              configure(configured);
              Button update = new Button("Change dimensions");
              update.onClick(event -> {
                sized.setWidth("280px");
                sized.setMinHeight("40px");
                sized.setMaxWidth("360px");
              });
              Button reset = new Button("Reset width");
              reset.onClick(event -> sized.setWidth(""));
              getBoundComponent().setDirection(FlexDirection.COLUMN);
              getBoundComponent().setAlignment(com.webforj.component.layout.flexlayout.FlexAlignment.START);
              getBoundComponent().add(sized, configured, update, reset);
            }

            private void configure(Button button) {
              button.setTooltipText("After helper");
              button.onClick(event -> button.setText("Helper initialized clicked"));
            }
          }
          """;

  private static final String SAVED =
      """
          package com.devtoolsapplayoutspring.views;

          import com.webforj.component.Composite;
          import com.webforj.component.button.Button;
          import com.webforj.component.layout.flexlayout.FlexDirection;
          import com.webforj.component.layout.flexlayout.FlexLayout;
          import com.webforj.router.annotation.FrameTitle;
          import com.webforj.router.annotation.Route;

          @Route(value = "/combined-initialization-cases", outlet = MainLayout.class)
          @FrameTitle("Combined initialization cases")
          public class CombinedInitializationCasesView extends Composite<FlexLayout> {
            public CombinedInitializationCasesView() {
              Button sized = new Button("Combined size");
              sized.setWidth("180px");
              sized.setSize(240, 48).setTooltipText("Keep tooltip");
              sized.setMinSize("120px", "32px");
              sized.setMaxSize(400, 80);
              sized.onClick(event -> sized.setText("Combined size clicked"));
              Button configured = new Button("Helper initialized");
              configure(configured);
              configured.setTooltipText("Saved tooltip");
              Button update = new Button("Change dimensions");
              update.onClick(event -> {
                sized.setWidth("280px");
                sized.setMinHeight("40px");
                sized.setMaxWidth("360px");
              });
              Button reset = new Button("Reset width");
              reset.onClick(event -> sized.setWidth(""));
              getBoundComponent().setDirection(FlexDirection.COLUMN);
              getBoundComponent().setAlignment(com.webforj.component.layout.flexlayout.FlexAlignment.START);
              getBoundComponent().add(sized, configured, update, reset);
            }

            private void configure(Button button) {
              button.setTooltipText("After helper");
              button.onClick(event -> button.setText("Helper initialized clicked"));
            }
          }
          """;

  private static final String RESET =
      """
          package com.devtoolsapplayoutspring.views;

          import com.webforj.component.Composite;
          import com.webforj.component.button.Button;
          import com.webforj.component.layout.flexlayout.FlexDirection;
          import com.webforj.component.layout.flexlayout.FlexLayout;
          import com.webforj.router.annotation.FrameTitle;
          import com.webforj.router.annotation.Route;

          @Route(value = "/combined-initialization-cases", outlet = MainLayout.class)
          @FrameTitle("Combined initialization cases")
          public class CombinedInitializationCasesView extends Composite<FlexLayout> {
            public CombinedInitializationCasesView() {
              Button sized = new Button("Combined size");
              sized.setWidth("180px");
              sized.setSize(240, 48).setTooltipText("Keep tooltip");
              sized.setMinSize("120px", "32px");
              sized.setMaxSize(400, 80);
              sized.onClick(event -> sized.setText("Combined size clicked"));
              Button configured = new Button("Helper initialized");
              configure(configured);
              Button update = new Button("Change dimensions");
              update.onClick(event -> {
                sized.setWidth("280px");
                sized.setMinHeight("40px");
                sized.setMaxWidth("360px");
              });
              Button reset = new Button("Reset width");
              reset.onClick(event -> sized.setWidth(""));
              getBoundComponent().setDirection(FlexDirection.COLUMN);
              getBoundComponent().setAlignment(com.webforj.component.layout.flexlayout.FlexAlignment.START);
              getBoundComponent().add(sized, configured, update, reset);
            }

            private void configure(Button button) {
              button.setTooltipText("After helper");
              button.onClick(event -> button.setText("Helper initialized clicked"));
            }
          }
          """;

  private static final String FORWARDED_SAVED =
      """
          package com.devtoolsapplayoutspring.views;

          import com.webforj.component.Composite;
          import com.webforj.component.button.Button;
          import com.webforj.component.layout.flexlayout.FlexDirection;
          import com.webforj.component.layout.flexlayout.FlexLayout;
          import com.webforj.router.annotation.FrameTitle;
          import com.webforj.router.annotation.Route;

          @Route(value = "/combined-initialization-cases", outlet = MainLayout.class)
          @FrameTitle("Combined initialization cases")
          public class CombinedInitializationCasesView extends Composite<FlexLayout> {
            public CombinedInitializationCasesView() {
              Button sized = new Button("Combined size");
              sized.setWidth("180px");
              sized.setSize(240, 48).setTooltipText("Keep tooltip");
              sized.setMinSize("120px", "32px");
              sized.setMaxSize(400, 80);
              sized.onClick(event -> sized.setText("Combined size clicked"));
              Button configured = new Button("Helper initialized");
              configure(configured);
              configured.setTooltipText("Saved tooltip");
              Button update = new Button("Change dimensions");
              update.onClick(event -> {
                sized.setWidth("280px");
                sized.setMinHeight("40px");
                sized.setMaxWidth("360px");
              });
              Button reset = new Button("Reset width");
              reset.onClick(event -> sized.setWidth(""));
              getBoundComponent().setDirection(FlexDirection.COLUMN);
              getBoundComponent().setAlignment(com.webforj.component.layout.flexlayout.FlexAlignment.START);
              getBoundComponent().add(sized, configured, update, reset);
            }

            private void configure(Button button) {
              configure(button, true);
            }

            private void configure(Button button, boolean enabled) {
              Button alias = button;
              alias.setTooltipText("After helper");
              button.onClick(event -> button.setText("Helper initialized clicked"));
            }
          }
          """;

  private static final String FORWARDED_RESET =
      """
          package com.devtoolsapplayoutspring.views;

          import com.webforj.component.Composite;
          import com.webforj.component.button.Button;
          import com.webforj.component.layout.flexlayout.FlexDirection;
          import com.webforj.component.layout.flexlayout.FlexLayout;
          import com.webforj.router.annotation.FrameTitle;
          import com.webforj.router.annotation.Route;

          @Route(value = "/combined-initialization-cases", outlet = MainLayout.class)
          @FrameTitle("Combined initialization cases")
          public class CombinedInitializationCasesView extends Composite<FlexLayout> {
            public CombinedInitializationCasesView() {
              Button sized = new Button("Combined size");
              sized.setWidth("180px");
              sized.setSize(240, 48).setTooltipText("Keep tooltip");
              sized.setMinSize("120px", "32px");
              sized.setMaxSize(400, 80);
              sized.onClick(event -> sized.setText("Combined size clicked"));
              Button configured = new Button("Helper initialized");
              configure(configured);
              Button update = new Button("Change dimensions");
              update.onClick(event -> {
                sized.setWidth("280px");
                sized.setMinHeight("40px");
                sized.setMaxWidth("360px");
              });
              Button reset = new Button("Reset width");
              reset.onClick(event -> sized.setWidth(""));
              getBoundComponent().setDirection(FlexDirection.COLUMN);
              getBoundComponent().setAlignment(com.webforj.component.layout.flexlayout.FlexAlignment.START);
              getBoundComponent().add(sized, configured, update, reset);
            }

            private void configure(Button button) {
              configure(button, true);
            }

            private void configure(Button button, boolean enabled) {
              Button alias = button;
              alias.setTooltipText("After helper");
              button.onClick(event -> button.setText("Helper initialized clicked"));
            }
          }
          """;

  @TempDir
  Path temporaryDirectory;

  @ParameterizedTest
  @CsvSource({"false, Saved tooltip", "false, ''", "true, Saved tooltip", "true, ''"})
  @DisplayName("A helper-owned property is written after the helper call, and reset removes only "
      + "the caller's write")
  void writeAfterHelper(boolean forwarded, String value) throws IOException {
    final String source = forwarded ? FORWARDED : SOURCE;
    final String expected = forwarded ? value.isEmpty() ? FORWARDED_RESET : FORWARDED_SAVED
        : value.isEmpty() ? RESET : SAVED;
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      String owner = "com.devtoolsapplayoutspring.views.CombinedInitializationCasesView";
      Path file = fixture.addSource(owner, source);
      fixture.addComponent("configured", Button.class,
          List.of(new SourcePoint(owner, "CombinedInitializationCasesView.java", 20)));
      List<ChangeRequest> edits = List.of(fixture.createChange("configured", "HasTooltip", value));
      SourceCodeModifier modifier = fixture.getModifier();

      assertEquals(List.of(expected),
          modifier.previewPatches(edits).stream().map(FilePatch::getPatched).toList());
      assertEquals(source, Files.readString(file));
      modifier.apply(edits);
      assertEquals(expected, Files.readString(file));
      modifier.apply(edits);
      assertEquals(expected, Files.readString(file));
    }
  }

  @Test
  @DisplayName("A helper that writes a different property and a deferred callback do not block a "
      + "width edit")
  void writePropertyUnaffectedByHelper() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      String owner = "com.devtoolsapplayoutspring.views.CombinedInitializationCasesView";
      Path file = fixture.addSource(owner, SOURCE);
      fixture.addComponent("configured", Button.class,
          List.of(new SourcePoint(owner, "CombinedInitializationCasesView.java", 20)));
      List<ChangeRequest> edits = List.of(fixture.createChange("configured", "HasWidth", "260px"));
      SourceCodeModifier modifier = fixture.getModifier();
      assertEquals(List.of(WIDTH_EXPECTED),
          modifier.previewPatches(edits).stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(edits);
      assertEquals(WIDTH_EXPECTED, Files.readString(file));
      modifier.apply(edits);
      assertEquals(WIDTH_EXPECTED, Files.readString(file));
    }
  }
}
