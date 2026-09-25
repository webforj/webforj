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

class InitializerChainCaseTest {

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
            private final Button sized = new Button("Combined size").setWidth("180px").setSize(240, 48).setTooltipText("Keep tooltip");

            public CombinedInitializationCasesView() {
              sized.setMinSize("120px", "32px");
              sized.setMaxSize(400, 80);
              sized.onClick(event -> sized.setText("Combined size clicked"));
              Button configured = new Button("Helper initialized").setWidth("160px");
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

  private static final String EXPECTED =
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
            private final Button sized = new Button("Combined size").setWidth("280px").setHeight(48).setTooltipText("Keep tooltip");

            public CombinedInitializationCasesView() {
              sized.setMinSize("120px", "32px");
              sized.setMaxSize(400, 80);
              sized.onClick(event -> sized.setText("Combined size clicked"));
              Button configured = new Button("Helper initialized").setWidth("260px");
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
            private final Button sized = new Button("Combined size").setHeight(48).setTooltipText("Keep tooltip");

            public CombinedInitializationCasesView() {
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

  @TempDir
  Path temporaryDirectory;

  @Test
  @DisplayName("Field and local initializer chains update and reset the declared component without "
      + "leaving old writes")
  void writeInitializerChains() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      String owner = "com.devtoolsapplayoutspring.views.CombinedInitializationCasesView";
      final Path file = fixture.addSource(owner, SOURCE);
      fixture.addComponent("sized", Button.class,
          List.of(new SourcePoint(owner, "CombinedInitializationCasesView.java", 13)));
      fixture.addComponent("configured", Button.class,
          List.of(new SourcePoint(owner, "CombinedInitializationCasesView.java", 19)));
      List<ChangeRequest> edits = List.of(fixture.createChange("sized", "HasWidth", "280px"),
          fixture.createChange("configured", "HasWidth", "260px"));
      SourceCodeModifier modifier = fixture.getModifier();
      assertEquals(List.of(EXPECTED),
          modifier.previewPatches(edits).stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(edits);
      assertEquals(EXPECTED, Files.readString(file));
      modifier.apply(edits);
      assertEquals(EXPECTED, Files.readString(file));
      List<ChangeRequest> resets = List.of(fixture.createChange("sized", "HasWidth", ""),
          fixture.createChange("configured", "HasWidth", ""));
      assertEquals(List.of(RESET),
          modifier.previewPatches(resets).stream().map(FilePatch::getPatched).toList());
      assertEquals(EXPECTED, Files.readString(file));
      modifier.apply(resets);
      assertEquals(RESET, Files.readString(file));
      modifier.apply(resets);
      assertEquals(RESET, Files.readString(file));
    }
  }
}
