package com.webforj.devtools.craftforj.inspector.source.cases.ownership;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.button.Button;
import com.webforj.component.layout.appnav.AppNav;
import com.webforj.devtools.craftforj.inspector.source.SourceCodeModifier;
import com.webforj.devtools.craftforj.inspector.source.cases.support.SourceWriteFixture;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
import com.webforj.devtools.craftforj.source.model.FilePatch;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class HelperBoundaryCaseTest {

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
              ButtonConfiguration.configure(configured);
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

          }
          """;

  private static final String HELPER = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.button.Button;

      public final class ButtonConfiguration {
        public static void configure(Button button) {
          button.setTooltipText("After helper");
          button.onClick(event -> button.setText("Helper initialized clicked"));
        }
      }
      """;

  private static final String ACCESSOR = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.layout.appnav.AppNav;
      import com.webforj.component.layout.appnav.AppNavItem;
      import com.webforj.component.layout.flexlayout.FlexDirection;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/accessor-cases", outlet = MainLayout.class)
      @FrameTitle("Accessor cases")
      public class AccessorCasesView extends Composite<FlexLayout> {
        public AccessorCasesView() {
          AppNav navigation = new AppNav();
          navigation.getSearch().setFieldVisible(true)
              .setEmptyMessage("No matching pages");
          navigation.getPinning().setEnabled(true).setTitle("Favorites").setAutosave(false);
          navigation.addItem(new AppNavItem("Sources", SourceCasesView.class).setPinned(true));
          navigation.addItem(new AppNavItem("Properties", PropertyCasesView.class));
          AppNav secondary = new AppNav();
          secondary.getSearch().setFieldVisible(true).setPlaceholder("Other pages");
          secondary.addItem(new AppNavItem("Temporal", TemporalBoundsCasesView.class));
          configure(navigation.getSearch());
          getBoundComponent().setDirection(FlexDirection.COLUMN);
          getBoundComponent().add(navigation, secondary);
        }
        private void configure(AppNav.Search search) {
          AppNav.Search alias = search;
          alias.setPlaceholder("Helper placeholder");
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
              ButtonConfiguration.configure(configured);
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
              ButtonConfiguration.configure(configured);
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

          }
          """;

  private static final String ACCESSOR_SAVED = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.layout.appnav.AppNav;
      import com.webforj.component.layout.appnav.AppNavItem;
      import com.webforj.component.layout.flexlayout.FlexDirection;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/accessor-cases", outlet = MainLayout.class)
      @FrameTitle("Accessor cases")
      public class AccessorCasesView extends Composite<FlexLayout> {
        public AccessorCasesView() {
          AppNav navigation = new AppNav();
          navigation.getSearch().setFieldVisible(true)
              .setEmptyMessage("No matching pages");
          navigation.getPinning().setEnabled(true).setTitle("Favorites").setAutosave(false);
          navigation.addItem(new AppNavItem("Sources", SourceCasesView.class).setPinned(true));
          navigation.addItem(new AppNavItem("Properties", PropertyCasesView.class));
          AppNav secondary = new AppNav();
          secondary.getSearch().setFieldVisible(true).setPlaceholder("Other pages");
          secondary.addItem(new AppNavItem("Temporal", TemporalBoundsCasesView.class));
          configure(navigation.getSearch());
          navigation.getSearch().setPlaceholder("Saved placeholder");
          getBoundComponent().setDirection(FlexDirection.COLUMN);
          getBoundComponent().add(navigation, secondary);
        }
        private void configure(AppNav.Search search) {
          AppNav.Search alias = search;
          alias.setPlaceholder("Helper placeholder");
        }
      }
      """;

  @TempDir
  Path temporaryDirectory;

  @ParameterizedTest
  @ValueSource(strings = {"Saved tooltip", ""})
  @DisplayName("A cross-file helper write is followed by the caller's own write, and reset leaves "
      + "the helper in charge")
  void writeAfterExternalHelper(String value) throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      String owner = "com.devtoolsapplayoutspring.views.CombinedInitializationCasesView";
      Path file = fixture.addSource(owner, SOURCE);
      final Path helper =
          fixture.addSource("com.devtoolsapplayoutspring.views.ButtonConfiguration", HELPER);
      fixture.addComponent("configured", Button.class,
          List.of(new SourcePoint(owner, "CombinedInitializationCasesView.java", 20)));
      List<ChangeRequest> edits = List.of(fixture.createChange("configured", "HasTooltip", value));
      final String expected = value.isEmpty() ? RESET : SAVED;
      SourceCodeModifier modifier = fixture.getModifier();

      assertEquals(List.of(expected),
          modifier.previewPatches(edits).stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(edits);
      assertEquals(expected, Files.readString(file));
      modifier.apply(edits);
      assertEquals(expected, Files.readString(file));
      assertEquals(HELPER, Files.readString(helper));
    }
  }

  @ParameterizedTest
  @ValueSource(strings = {"Saved tooltip", ""})
  @DisplayName("Unavailable helper source is reported explicitly and leaves the file unchanged")
  void refuseUnresolvedHelper(String value) throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      String owner = "com.devtoolsapplayoutspring.views.CombinedInitializationCasesView";
      Path file = fixture.addSource(owner, SOURCE);
      fixture.addComponent("configured", Button.class,
          List.of(new SourcePoint(owner, "CombinedInitializationCasesView.java", 20)));
      List<ChangeRequest> edits = List.of(fixture.createChange("configured", "HasTooltip", value));
      final String error = "TooltipText cannot be written: the source of 'configure', which "
          + "receives this component, is not available";
      SourceCodeModifier modifier = fixture.getModifier();

      assertEquals(error, modifier.preview(edits).get(0).getError());
      assertEquals(SOURCE, Files.readString(file));
      assertEquals(error, modifier.apply(edits).get(0).getError());
      assertEquals(SOURCE, Files.readString(file));
    }
  }

  @ParameterizedTest
  @ValueSource(strings = {"Saved placeholder", ""})
  @DisplayName("An accessor passed through a parameter alias receives its write after the helper "
      + "and nothing on reset")
  void writeAfterAccessorHelper(String value) throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      String owner = "com.devtoolsapplayoutspring.views.AccessorCasesView";
      final Path file = fixture.addSource(owner, ACCESSOR);
      AppNav navigation = fixture.addComponent("navigation", AppNav.class,
          List.of(new SourcePoint(owner, "AccessorCasesView.java", 15)));
      AppNav.Search search = mock(AppNav.Search.class);
      when(navigation.getSearch()).thenReturn(search);
      List<ChangeRequest> edits =
          List.of(fixture.createChange("navigation", "AppNavSearchPlaceholder", value));
      final String expected = value.isEmpty() ? ACCESSOR : ACCESSOR_SAVED;
      final List<String> patches = value.isEmpty() ? List.of() : List.of(ACCESSOR_SAVED);
      SourceCodeModifier modifier = fixture.getModifier();

      assertEquals(patches,
          modifier.previewPatches(edits).stream().map(FilePatch::getPatched).toList());
      assertNull(modifier.preview(edits).get(0).getError());
      assertEquals(ACCESSOR, Files.readString(file));
      modifier.apply(edits);
      assertEquals(expected, Files.readString(file));
      modifier.apply(edits);
      assertEquals(expected, Files.readString(file));
    }
  }
}
