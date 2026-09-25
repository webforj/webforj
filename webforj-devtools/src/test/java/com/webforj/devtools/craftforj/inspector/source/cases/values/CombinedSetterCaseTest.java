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
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class CombinedSetterCaseTest {

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
            public CombinedInitializationCasesView() {
              Button sized = new Button("Combined size");
              sized.setWidth("50%").setHeight("60px").setTooltipText("Keep tooltip");
              sized.setMinWidth("100px").setMinHeight("40px");
              sized.setMaxWidth("360px").setMaxHeight("90px");
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
              sized.setHeight("60px").setTooltipText("Keep tooltip");
              sized.setMinWidth("100px");
              sized.setMaxHeight("90px");
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

  private static final String DIRECT_RESET =
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
              sized.setHeight(48).setTooltipText("Keep tooltip");
              sized.setMinWidth("120px");
              sized.setMaxHeight(80);
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

  private static final String COMPUTED =
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
              sized.setSize(Float.parseFloat("240"), 48).setTooltipText("Keep tooltip");
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
  @DisplayName("Combined dimension writes preserve overloads, siblings, callbacks and repeated "
      + "saves")
  void writeAndResetCombinedDimensions() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      String owner = "com.devtoolsapplayoutspring.views.CombinedInitializationCasesView";
      Path file = fixture.addSource(owner, SOURCE);
      fixture.addComponent("sized", Button.class,
          List.of(new SourcePoint(owner, "CombinedInitializationCasesView.java", 14)));
      List<ChangeRequest> edits = List.of(fixture.createChange("sized", "HasWidth", "50%"),
          fixture.createChange("sized", "HasHeight", "60px"),
          fixture.createChange("sized", "HasMinWidth", "100px"),
          fixture.createChange("sized", "HasMinHeight", "40px"),
          fixture.createChange("sized", "HasMaxWidth", "360px"),
          fixture.createChange("sized", "HasMaxHeight", "90px"));
      SourceCodeModifier modifier = fixture.getModifier();

      assertEquals(List.of(EXPECTED),
          modifier.previewPatches(edits).stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(edits);
      assertEquals(EXPECTED, Files.readString(file));
      modifier.apply(edits);
      assertEquals(EXPECTED, Files.readString(file));
      List<ChangeRequest> resets = List.of(fixture.createChange("sized", "HasWidth", ""),
          fixture.createChange("sized", "HasMinHeight", ""),
          fixture.createChange("sized", "HasMaxWidth", ""));
      assertEquals(List.of(RESET),
          modifier.previewPatches(resets).stream().map(FilePatch::getPatched).toList());
      assertEquals(EXPECTED, Files.readString(file));
      modifier.apply(resets);
      assertEquals(RESET, Files.readString(file));
      modifier.apply(resets);
      assertEquals(RESET, Files.readString(file));
    }
  }

  @Test
  @DisplayName("Resetting one dimension of each combined call preserves the other numeric or "
      + "string argument")
  void resetCombinedDimensionsDirectly() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      String owner = "com.devtoolsapplayoutspring.views.CombinedInitializationCasesView";
      Path file = fixture.addSource(owner, SOURCE);
      fixture.addComponent("sized", Button.class,
          List.of(new SourcePoint(owner, "CombinedInitializationCasesView.java", 14)));
      List<ChangeRequest> resets = List.of(fixture.createChange("sized", "HasWidth", ""),
          fixture.createChange("sized", "HasMinHeight", ""),
          fixture.createChange("sized", "HasMaxWidth", ""));
      SourceCodeModifier modifier = fixture.getModifier();
      assertEquals(List.of(DIRECT_RESET),
          modifier.previewPatches(resets).stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(resets);
      assertEquals(DIRECT_RESET, Files.readString(file));
      modifier.apply(resets);
      assertEquals(DIRECT_RESET, Files.readString(file));
    }
  }

  @ParameterizedTest
  @ValueSource(strings = {"50%", ""})
  @DisplayName("Computed combined arguments refuse a split that could change evaluation order")
  void refuseComputedCombinedArguments(String value) throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      String owner = "com.devtoolsapplayoutspring.views.CombinedInitializationCasesView";
      Path file = fixture.addSource(owner, COMPUTED);
      fixture.addComponent("sized", Button.class,
          List.of(new SourcePoint(owner, "CombinedInitializationCasesView.java", 14)));
      List<ChangeRequest> edits = List.of(fixture.createChange("sized", "HasWidth", value));
      SourceCodeModifier modifier = fixture.getModifier();
      final String error =
          "'setSize' is called with computed arguments, so Width cannot be split out of it";
      assertEquals(error, modifier.preview(edits).get(0).getError());
      assertEquals(COMPUTED, Files.readString(file));
      assertEquals(error, modifier.apply(edits).get(0).getError());
      assertEquals(COMPUTED, Files.readString(file));
    }
  }
}
