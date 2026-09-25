package com.webforj.devtools.craftforj.inspector.source.cases.reusable;

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

class ConstructorTextCaseTest {

  private static final String EXPLORE =
      """
          package com.devtoolsapplayoutspring.components;

          import com.webforj.component.Composite;
          import com.webforj.component.Theme;
          import com.webforj.component.button.Button;
          import com.webforj.component.button.ButtonTheme;
          import com.webforj.component.html.elements.Paragraph;
          import com.webforj.component.icons.Icon;
          import com.webforj.component.icons.TablerIcon;
          import com.webforj.component.layout.flexlayout.FlexAlignment;
          import com.webforj.component.layout.flexlayout.FlexDirection;
          import com.webforj.component.layout.flexlayout.FlexJustifyContent;
          import com.webforj.component.layout.flexlayout.FlexLayout;
          import com.webforj.component.toast.Toast;

          public class Explore extends Composite<FlexLayout> {
            private FlexLayout self = getBoundComponent();

            public Explore(String message, String iconName, String ctaLabel) {
              self.addClassName("explore-component");
              self.setStyle("margin", "1em auto");
              self.setHeight("100%");
              self.setDirection(FlexDirection.COLUMN);
              self.setAlignment(FlexAlignment.CENTER);
              self.setJustifyContent(FlexJustifyContent.CENTER);
              self.setMaxWidth(300);
              self.setSpacing(".75em");

              Icon icon = TablerIcon.create(iconName);
              icon.setStyle("font-size", "3rem");

              FlexLayout badge = FlexLayout.create(icon).align().center().justify().center().build();
              badge.setStyle("width", "6rem");
              badge.setStyle("height", "6rem");
              badge.setStyle("border-radius", "50%");
              badge.setStyle("background", "var(--dwc-color-primary-alt)");
              badge.setStyle("color", "var(--dwc-color-on-primary-text-alt)");

              Paragraph messageLabel = new Paragraph(message);
              messageLabel.setStyle("color", "var(--dwc-color-gray-text-light)");
              messageLabel.setStyle("margin", "0");

              Button cta = new Button(ctaLabel)
                  .setPrefixComponent(TablerIcon.create("plus"))
                  .setTheme(ButtonTheme.PRIMARY);
              cta.onClick(ev -> Toast.show("\\"%s\\" is not wired up yet".formatted(ctaLabel), 3000, Theme.INFO,
                  Toast.Placement.BOTTOM_RIGHT));
              cta.setTooltipText("hyyan");

              self.add(badge, messageLabel, cta);
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
  @DisplayName("Constructor text changes only the dashboard argument in preview and save")
  void applyConstructorTextAtUsage() throws IOException {
    final String expected = """
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
            self.add(new Explore("Your dashboard is empty", "layout-dashboard", "Build widget"));
          }
        }
        """;
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      final Path exploreFile =
          fixture.addSource("com.devtoolsapplayoutspring.components.Explore", EXPLORE);
      final Path dashboardFile =
          fixture.addSource("com.devtoolsapplayoutspring.views.DashboardView", DASHBOARD);
      fixture.addComponent("cta", Button.class,
          List.of(
              new SourcePoint("com.devtoolsapplayoutspring.components.Explore", "Explore.java", 43),
              new SourcePoint("com.devtoolsapplayoutspring.views.DashboardView",
                  "DashboardView.java", 19)));
      SourceCodeModifier modifier = fixture.getModifier();
      ChangeRequest change = fixture.createChange("cta", "HasText", "Build widget");
      change.setOriginalValue("Create widget");
      change.setTarget(ChangeRequest.TARGET_USAGE);

      List<FilePatch> patches = modifier.previewPatches(List.of(change));

      assertEquals(List.of(DASHBOARD), patches.stream().map(FilePatch::getOriginal).toList());
      assertEquals(List.of(expected), patches.stream().map(FilePatch::getPatched).toList());
      assertEquals(DASHBOARD, Files.readString(dashboardFile));
      assertEquals(EXPLORE, Files.readString(exploreFile));

      modifier.apply(List.of(change));

      assertEquals(expected, Files.readString(dashboardFile));
      assertEquals(EXPLORE, Files.readString(exploreFile));
    }
  }
}
