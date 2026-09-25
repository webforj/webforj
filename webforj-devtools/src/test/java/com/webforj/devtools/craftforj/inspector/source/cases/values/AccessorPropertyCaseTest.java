package com.webforj.devtools.craftforj.inspector.source.cases.values;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.webforj.component.ComponentSourceRegistry.SourcePoint;
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
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class AccessorPropertyCaseTest {

  private static final String SOURCE = """
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
          navigation.getSearch().setFieldVisible(true).setPlaceholder("Find pages")
              .setEmptyMessage("No matching pages");
          navigation.getPinning().setEnabled(true).setTitle("Quick pages").setAutosave(false);
          navigation.addItem(new AppNavItem("Sources", SourceCasesView.class).setPinned(true));
          navigation.addItem(new AppNavItem("Properties", PropertyCasesView.class));
          AppNav secondary = new AppNav();
          secondary.getSearch().setFieldVisible(true).setPlaceholder("Other pages");
          secondary.addItem(new AppNavItem("Temporal", TemporalBoundsCasesView.class));
          getBoundComponent().setDirection(FlexDirection.COLUMN);
          getBoundComponent().add(navigation, secondary);
        }
      }
      """;
  private static final String EXPECTED = """
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
          navigation.getSearch().setFieldVisible(true).setPlaceholder("Find examples")
              .setEmptyMessage("No matching pages");
          navigation.getPinning().setEnabled(true).setTitle("Favorites").setAutosave(false);
          navigation.addItem(new AppNavItem("Sources", SourceCasesView.class).setPinned(true));
          navigation.addItem(new AppNavItem("Properties", PropertyCasesView.class));
          AppNav secondary = new AppNav();
          secondary.getSearch().setFieldVisible(true).setPlaceholder("Other pages");
          secondary.addItem(new AppNavItem("Temporal", TemporalBoundsCasesView.class));
          getBoundComponent().setDirection(FlexDirection.COLUMN);
          getBoundComponent().add(navigation, secondary);
        }
      }
      """;
  private static final String RESET = """
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
          getBoundComponent().setDirection(FlexDirection.COLUMN);
          getBoundComponent().add(navigation, secondary);
        }
      }
      """;

  @TempDir
  Path temporaryDirectory;

  @Test
  @DisplayName("Nested accessor updates and resets preserve the rest of each fluent chain")
  void updateAndClearAccessorProperty() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      final Path file =
          fixture.addSource("com.devtoolsapplayoutspring.views.AccessorCasesView", SOURCE);
      AppNav navigation = fixture.addComponent("navigation", AppNav.class,
          List.of(new SourcePoint("com.devtoolsapplayoutspring.views.AccessorCasesView",
              "AccessorCasesView.java", 15)));
      when(navigation.getSearch()).thenReturn(mock(AppNav.Search.class));
      when(navigation.getPinning()).thenReturn(mock(AppNav.Pinning.class));
      SourceCodeModifier modifier = fixture.getModifier();
      List<ChangeRequest> changes =
          List.of(fixture.createChange("navigation", "AppNavSearchPlaceholder", "Find examples"),
              fixture.createChange("navigation", "AppNavPinningTitle", "Favorites"));

      List<FilePatch> patches = modifier.previewPatches(changes);
      assertEquals(List.of(EXPECTED), patches.stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(changes);
      assertEquals(EXPECTED, Files.readString(file));

      ChangeRequest reset = fixture.createChange("navigation", "AppNavSearchPlaceholder", "");
      List<FilePatch> resetPatches = modifier.previewPatches(List.of(reset));
      assertEquals(List.of(RESET), resetPatches.stream().map(FilePatch::getPatched).toList());
      assertEquals(EXPECTED, Files.readString(file));
      modifier.apply(List.of(reset));
      assertEquals(RESET, Files.readString(file));
    }
  }
}
