package com.webforj.devtools.craftforj.inspector.source.cases.values;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.button.Button;
import com.webforj.component.layout.appnav.AppNav;
import com.webforj.component.layout.flexlayout.FlexAlignment;
import com.webforj.component.layout.flexlayout.FlexLayout;
import com.webforj.component.progressbar.ProgressBar;
import com.webforj.devtools.craftforj.inspector.source.SourceCodeModifier;
import com.webforj.devtools.craftforj.inspector.source.cases.support.SourceWriteFixture;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
import com.webforj.devtools.craftforj.source.model.FilePatch;
import com.webforj.devtools.craftforj.source.model.SourceLocation;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class BoundValueFamiliesCaseTest {

  private static final String SOURCE = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.appnav.AppNav;
      import com.webforj.component.layout.appnav.AppNavItem;
      import com.webforj.component.layout.flexlayout.FlexAlignment;
      import com.webforj.component.layout.flexlayout.FlexDirection;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.component.progressbar.ProgressBar;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/bound-value-cases", outlet = MainLayout.class)
      @FrameTitle("Bound value cases")
      public class BoundValueCasesView extends Composite<FlexLayout> {
        public BoundValueCasesView() {
          ProgressBar progress = new ProgressBar(40, "Bound progress");
          progress.setMin(10);
          progress.setMax(90);
          FlexLayout row = new FlexLayout();
          Button action = new Button("Update bound values");
          row.add(action, new Button("Other aligned item"));
          row.setHeight("120px");
          row.setItemAlignment(FlexAlignment.START, action);
          SearchNavigation navigation = new SearchNavigation();
          action.onClick(event -> {
            progress.setMin(20);
            progress.setMax(80);
            row.setItemAlignment(FlexAlignment.END, action);
            navigation.setSearchPlaceholder("Changed bound search");
          });
          getBoundComponent().setDirection(FlexDirection.COLUMN);
          getBoundComponent().add(progress, row, navigation);
        }

        private static class SearchNavigation extends Composite<AppNav> {
          SearchNavigation() {
            AppNav.Search search = getBoundComponent().getSearch();
            search.setFieldVisible(true).setPlaceholder("Bound search");
            getBoundComponent().addItem(new AppNavItem("Sources", SourceCasesView.class));
          }

          void setSearchPlaceholder(String value) {
            getBoundComponent().getSearch().setPlaceholder(value);
          }
        }
      }
      """;

  private static final String EXPECTED = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.appnav.AppNav;
      import com.webforj.component.layout.appnav.AppNavItem;
      import com.webforj.component.layout.flexlayout.FlexAlignment;
      import com.webforj.component.layout.flexlayout.FlexDirection;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.component.progressbar.ProgressBar;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/bound-value-cases", outlet = MainLayout.class)
      @FrameTitle("Bound value cases")
      public class BoundValueCasesView extends Composite<FlexLayout> {
        public BoundValueCasesView() {
          ProgressBar progress = new ProgressBar(40, "Bound progress");
          progress.setMin(30);
          progress.setMax(70);
          FlexLayout row = new FlexLayout();
          Button action = new Button("Update bound values");
          row.add(action, new Button("Other aligned item"));
          row.setHeight("120px");
          row.setItemAlignment(FlexAlignment.END, action);
          SearchNavigation navigation = new SearchNavigation();
          action.onClick(event -> {
            progress.setMin(20);
            progress.setMax(80);
            row.setItemAlignment(FlexAlignment.END, action);
            navigation.setSearchPlaceholder("Changed bound search");
          });
          getBoundComponent().setDirection(FlexDirection.COLUMN);
          getBoundComponent().add(progress, row, navigation);
        }

        private static class SearchNavigation extends Composite<AppNav> {
          SearchNavigation() {
            AppNav.Search search = getBoundComponent().getSearch();
            search.setFieldVisible(true).setPlaceholder("Saved bound search");
            getBoundComponent().addItem(new AppNavItem("Sources", SourceCasesView.class));
          }

          void setSearchPlaceholder(String value) {
            getBoundComponent().getSearch().setPlaceholder(value);
          }
        }
      }
      """;

  private static final String RESET = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.appnav.AppNav;
      import com.webforj.component.layout.appnav.AppNavItem;
      import com.webforj.component.layout.flexlayout.FlexAlignment;
      import com.webforj.component.layout.flexlayout.FlexDirection;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.component.progressbar.ProgressBar;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/bound-value-cases", outlet = MainLayout.class)
      @FrameTitle("Bound value cases")
      public class BoundValueCasesView extends Composite<FlexLayout> {
        public BoundValueCasesView() {
          ProgressBar progress = new ProgressBar(40, "Bound progress");
          FlexLayout row = new FlexLayout();
          Button action = new Button("Update bound values");
          row.add(action, new Button("Other aligned item"));
          row.setHeight("120px");
          SearchNavigation navigation = new SearchNavigation();
          action.onClick(event -> {
            progress.setMin(20);
            progress.setMax(80);
            row.setItemAlignment(FlexAlignment.END, action);
            navigation.setSearchPlaceholder("Changed bound search");
          });
          getBoundComponent().setDirection(FlexDirection.COLUMN);
          getBoundComponent().add(progress, row, navigation);
        }

        private static class SearchNavigation extends Composite<AppNav> {
          SearchNavigation() {
            AppNav.Search search = getBoundComponent().getSearch();
            search.setFieldVisible(true);
            getBoundComponent().addItem(new AppNavItem("Sources", SourceCasesView.class));
          }

          void setSearchPlaceholder(String value) {
            getBoundComponent().getSearch().setPlaceholder(value);
          }
        }
      }
      """;

  @TempDir
  Path temporaryDirectory;

  @Test
  @DisplayName("Integer bounds, item enum and bound accessor write/reset preserve later callback "
      + "behavior")
  void writeBoundValueFamilies() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = addComponents(fixture);
      SourceCodeModifier modifier = fixture.getModifier();
      List<ChangeRequest> changes = createChanges(fixture, file, false);

      assertEquals(List.of(EXPECTED),
          modifier.previewPatches(changes).stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(changes);
      assertEquals(EXPECTED, Files.readString(file));
      List<ChangeRequest> reset = createChanges(fixture, file, true);
      assertEquals(List.of(RESET),
          modifier.previewPatches(reset).stream().map(FilePatch::getPatched).toList());
      assertEquals(EXPECTED, Files.readString(file));
      modifier.apply(reset);
      assertEquals(RESET, Files.readString(file));
    }
  }

  @Test
  @DisplayName("Invalid layout-item enums report their exact property error without changing "
      + "source")
  void refuseInvalidItemEnum() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = addComponents(fixture);
      ChangeRequest invalid = fixture.createChange("action", "FlexItemAlignment", "MISSING");
      invalid.setParentId("row");
      invalid.setParentSource(
          new SourceLocation(file.toString(), 21, null, "row", FlexLayout.class.getName()));
      SourceCodeModifier modifier = fixture.getModifier();
      final String error = "Property 'Alignment': invalid enum value 'MISSING'";

      assertEquals(error, modifier.preview(List.of(invalid)).get(0).getError());
      assertEquals(SOURCE, Files.readString(file));
      assertEquals(error, modifier.apply(List.of(invalid)).get(0).getError());
      assertEquals(SOURCE, Files.readString(file));
    }
  }

  private Path addComponents(SourceWriteFixture fixture) throws IOException {
    final String owner = "com.devtoolsapplayoutspring.views.BoundValueCasesView";
    Path file = fixture.addSource(owner, SOURCE);
    fixture.addComponent("progress", ProgressBar.class,
        List.of(new SourcePoint(owner, "BoundValueCasesView.java", 18)));
    fixture.addComponent("row", FlexLayout.class,
        List.of(new SourcePoint(owner, "BoundValueCasesView.java", 21)));
    fixture.addComponent("action", Button.class,
        List.of(new SourcePoint(owner, "BoundValueCasesView.java", 22)));
    fixture.addSourceClass(owner + "$SearchNavigation", file);
    AppNav navigation = fixture.addComponent("navigation", AppNav.class,
        List.of(new SourcePoint(owner + "$SearchNavigation", "BoundValueCasesView.java", 38)));
    AppNav.Search search = mock(AppNav.Search.class);
    when(navigation.getSearch()).thenReturn(search);
    return file;
  }

  private List<ChangeRequest> createChanges(SourceWriteFixture fixture, Path file, boolean reset) {
    ChangeRequest alignment = fixture.createChange("action", "FlexItemAlignment",
        reset ? "" : FlexAlignment.class.getName() + ".END");
    alignment.setParentId("row");
    alignment.setParentSource(
        new SourceLocation(file.toString(), 21, null, "row", FlexLayout.class.getName()));
    return List.of(fixture.createChange("progress", "HasMin", reset ? "" : "30"),
        fixture.createChange("progress", "HasMax", reset ? "" : "70"), alignment,
        fixture.createChange("navigation", "AppNavSearchPlaceholder",
            reset ? "" : "Saved bound search"));
  }
}
