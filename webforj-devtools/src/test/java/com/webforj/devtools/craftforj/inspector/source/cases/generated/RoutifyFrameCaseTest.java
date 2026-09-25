package com.webforj.devtools.craftforj.inspector.source.cases.generated;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.window.Frame;
import com.webforj.devtools.craftforj.inspector.source.SourceCodeModifier;
import com.webforj.devtools.craftforj.inspector.source.cases.support.SourceWriteFixture;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeResult;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/** A Routify-created frame has no application creation site to write. */
class RoutifyFrameCaseTest {

  private static final String APPLICATION = """
      package com.devtoolsapplayoutspring;

      import org.springframework.boot.SpringApplication;
      import org.springframework.boot.autoconfigure.SpringBootApplication;
      import com.webforj.App;
      import com.webforj.annotation.AppProfile;
      import com.webforj.annotation.AppTheme;
      import com.webforj.annotation.Routify;
      import com.webforj.bundle.annotation.BundleEntry;

      @SpringBootApplication
      @Routify(packages = "com.devtoolsapplayoutspring.views")
      @BundleEntry("app.css")
      @AppTheme("system")
      @AppProfile(name = "devtools-applayout-spring", shortName = "devtools-applayout-spring")
      public class Application extends App {

        public static void main(String[] args) {
          SpringApplication.run(Application.class, args);
        }
      }
      """;

  private static final String DASHBOARD = """
      package com.devtoolsapplayoutspring.views;

      import com.devtoolsapplayoutspring.components.Explore;

      import com.webforj.component.Composite;
      import com.webforj.component.layout.flexlayout.FlexAlignment;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/", outlet = MainLayout.class)
      @FrameTitle("Dashboard")
      public class DashboardView extends Composite<FlexLayout> {
        private FlexLayout self = getBoundComponent();

        public DashboardView() {
          self.setHeight("100%");
          self.setAlignment(FlexAlignment.CENTER);
          self.add(new Explore("Your dashboard is empty", "layout-dashboard", "Create widget"));
        }
      }
      """;

  @TempDir
  Path temporaryDirectory;

  @Test
  @DisplayName("Routify frame title refuses a missing application target without editing "
      + "FrameTitle")
  void refuseGeneratedFrameTitle() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path applicationFile =
          fixture.addSource("com.devtoolsapplayoutspring.Application", APPLICATION);
      final Path dashboardFile =
          fixture.addSource("com.devtoolsapplayoutspring.views.DashboardView", DASHBOARD);
      fixture.addComponent("frame", Frame.class,
          List.of(new SourcePoint("com.webforj.App", "App.java", 953)));
      SourceCodeModifier modifier = fixture.getModifier();
      ChangeRequest change = fixture.createChange("frame", "HasText", "Changed frame title");

      List<ChangeResult> preview = modifier.preview(List.of(change));
      assertEquals("The source file for this component was not found", preview.get(0).getError());
      assertEquals(APPLICATION, Files.readString(applicationFile));
      assertEquals(DASHBOARD, Files.readString(dashboardFile));

      List<ChangeResult> applied = modifier.apply(List.of(change));
      assertEquals("The source file for this component was not found", applied.get(0).getError());
      assertEquals(APPLICATION, Files.readString(applicationFile));
      assertEquals(DASHBOARD, Files.readString(dashboardFile));
    }
  }
}
