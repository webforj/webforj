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
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class AccessorAliasCaseTest {

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
          AppNav.Search search = navigation.getSearch();
          search.setFieldVisible(true).setPlaceholder("Find pages")
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
          AppNav.Search search = navigation.getSearch();
          search.setFieldVisible(true).setPlaceholder("Find examples")
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
          AppNav.Search search = navigation.getSearch();
          search.setFieldVisible(true)
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

  private static final String INSERTED = """
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
          AppNav.Search search = navigation.getSearch();
          navigation.getSearch().setPlaceholder("Restored pages");
          search.setFieldVisible(true)
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

  private static final String REASSIGNED = """
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
          AppNav.Search search = navigation.getSearch();
          search.setFieldVisible(true).setPlaceholder("Find pages")
              .setEmptyMessage("No matching pages");
          navigation.getPinning().setEnabled(true).setTitle("Quick pages").setAutosave(false);
          navigation.addItem(new AppNavItem("Sources", SourceCasesView.class).setPinned(true));
          navigation.addItem(new AppNavItem("Properties", PropertyCasesView.class));
          AppNav secondary = new AppNav();
          secondary.getSearch().setFieldVisible(true).setPlaceholder("Other pages");
          secondary.addItem(new AppNavItem("Temporal", TemporalBoundsCasesView.class));
          search = secondary.getSearch();
          search.setPlaceholder("Other alias");
          getBoundComponent().setDirection(FlexDirection.COLUMN);
          getBoundComponent().add(navigation, secondary);
        }
      }
      """;

  @TempDir
  Path temporaryDirectory;

  @Test
  @DisplayName("Accessor alias update, reset and reinsertion preserve sibling settings and another "
      + "component")
  void writeAccessorAlias() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = addNavigation(fixture, SOURCE);
      SourceCodeModifier modifier = fixture.getModifier();
      ChangeRequest edit =
          fixture.createChange("navigation", "AppNavSearchPlaceholder", "Find examples");

      assertEquals(List.of(EXPECTED),
          modifier.previewPatches(List.of(edit)).stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(List.of(edit));
      assertEquals(EXPECTED, Files.readString(file));
      ChangeRequest reset = fixture.createChange("navigation", "AppNavSearchPlaceholder", "");
      assertEquals(List.of(RESET),
          modifier.previewPatches(List.of(reset)).stream().map(FilePatch::getPatched).toList());
      assertEquals(EXPECTED, Files.readString(file));
      modifier.apply(List.of(reset));
      assertEquals(RESET, Files.readString(file));
      ChangeRequest restore =
          fixture.createChange("navigation", "AppNavSearchPlaceholder", "Restored pages");
      assertEquals(List.of(INSERTED),
          modifier.previewPatches(List.of(restore)).stream().map(FilePatch::getPatched).toList());
      assertEquals(RESET, Files.readString(file));
      modifier.apply(List.of(restore));
      assertEquals(INSERTED, Files.readString(file));
    }
  }

  @ParameterizedTest
  @ValueSource(booleans = {false, true})
  @DisplayName("Reassigned accessor aliases report a precise refusal and preserve both components")
  void refuseReassignedAccessor(boolean reset) throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = addNavigation(fixture, REASSIGNED);
      SourceCodeModifier modifier = fixture.getModifier();
      ChangeRequest edit = fixture.createChange("navigation", "AppNavSearchPlaceholder",
          reset ? "" : "Find examples");
      final String error = "The alias of 'getSearch' on 'navigation' is reassigned after it is "
          + "created, so a write could reach a different component";

      assertEquals(error, modifier.preview(List.of(edit)).get(0).getError());
      assertEquals(REASSIGNED, Files.readString(file));
      assertEquals(error, modifier.apply(List.of(edit)).get(0).getError());
      assertEquals(REASSIGNED, Files.readString(file));
    }
  }

  private Path addNavigation(SourceWriteFixture fixture, String source) throws IOException {
    Path file = fixture.addSource("com.devtoolsapplayoutspring.views.AccessorCasesView", source);
    AppNav navigation = fixture.addComponent("navigation", AppNav.class,
        List.of(new SourcePoint("com.devtoolsapplayoutspring.views.AccessorCasesView",
            "AccessorCasesView.java", 15)));
    AppNav.Search search = mock(AppNav.Search.class);
    when(navigation.getSearch()).thenReturn(search);
    return file;
  }
}
