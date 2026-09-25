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
import java.util.stream.Stream;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

class ComputedUsageCaseTest {

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

              Button cta = new Button()
                  .setText(ctaLabel)
                  .setPrefixComponent(TablerIcon.create("plus"))
                  .setTheme(ButtonTheme.PRIMARY);
              cta.onClick(ev -> Toast.show("\\"%s\\" is not wired up yet".formatted(ctaLabel), 3000, Theme.INFO,
                  Toast.Placement.BOTTOM_RIGHT));
              cta.setTooltipText("hyyan");

              self.add(badge, messageLabel, cta);
            }
          }
          """;

  private static final String CONSTANT = """
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

        private static final String CTA = "Create widget";

        public DashboardView() {
          self.setHeight("100%");
          self.setAlignment(FlexAlignment.CENTER);
          self.add(new Explore("Your dashboard is empty", "layout-dashboard", CTA));
        }
      }
      """;

  private static final String COMPUTED = """
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

        private static final String CTA = "Create widget";

        public DashboardView() {
          self.setHeight("100%");
          self.setAlignment(FlexAlignment.CENTER);
          self.add(new Explore("Your dashboard is empty", "layout-dashboard", CTA.trim()));
        }
      }
      """;

  private static final String EXPECTED = """
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

        private static final String CTA = "Create widget";

        public DashboardView() {
          self.setHeight("100%");
          self.setAlignment(FlexAlignment.CENTER);
          self.add(new Explore("Your dashboard is empty", "layout-dashboard", "Build widget"));
        }
      }
      """;

  private static final String STALE = """
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

        private static final String CTA = "Create widget";

        public DashboardView() {
          self.setHeight("100%");
          self.setAlignment(FlexAlignment.CENTER);
          self.add(new Explore("Your dashboard is empty", "layout-dashboard", "Changed elsewhere"));
        }
      }
      """;

  @TempDir
  Path temporaryDirectory;

  @ParameterizedTest
  @MethodSource("getUsageSources")
  @DisplayName("A proven setter parameter replaces its constant or computed usage without changing "
      + "shared code")
  void writeComputedArgument(String source) throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path definition =
          fixture.addSource("com.devtoolsapplayoutspring.components.Explore", EXPLORE);
      Path usage = fixture.addSource("com.devtoolsapplayoutspring.views.DashboardView", source);
      ChangeRequest edit = createChange(fixture);
      SourceCodeModifier modifier = fixture.getModifier();

      assertEquals(List.of(EXPECTED),
          modifier.previewPatches(List.of(edit)).stream().map(FilePatch::getPatched).toList());
      assertEquals(source, Files.readString(usage));
      assertEquals(EXPLORE, Files.readString(definition));
      modifier.apply(List.of(edit));
      assertEquals(EXPECTED, Files.readString(usage));
      assertEquals(EXPLORE, Files.readString(definition));
      edit.setOriginalValue("Build widget");
      modifier.apply(List.of(edit));
      assertEquals(EXPECTED, Files.readString(usage));
      assertEquals(EXPLORE, Files.readString(definition));
    }
  }

  @Test
  @DisplayName("A stale usage literal is refused rather than overwritten or redirected to the "
      + "definition")
  void refuseStaleLiteral() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path definition =
          fixture.addSource("com.devtoolsapplayoutspring.components.Explore", EXPLORE);
      Path usage = fixture.addSource("com.devtoolsapplayoutspring.views.DashboardView", STALE);
      ChangeRequest edit = createChange(fixture);
      SourceCodeModifier modifier = fixture.getModifier();
      final String error =
          "The source changed since this value was read. Reload the application before saving.";

      assertEquals(error, modifier.preview(List.of(edit)).get(0).getError());
      assertEquals(STALE, Files.readString(usage));
      assertEquals(EXPLORE, Files.readString(definition));
      assertEquals(error, modifier.apply(List.of(edit)).get(0).getError());
      assertEquals(STALE, Files.readString(usage));
      assertEquals(EXPLORE, Files.readString(definition));
    }
  }

  private ChangeRequest createChange(SourceWriteFixture fixture) {
    fixture.addComponent("cta", Button.class,
        List.of(
            new SourcePoint("com.devtoolsapplayoutspring.components.Explore", "Explore.java", 43),
            new SourcePoint("com.devtoolsapplayoutspring.views.DashboardView", "DashboardView.java",
                21)));
    ChangeRequest change = fixture.createChange("cta", "HasText", "Build widget");
    change.setOriginalValue("Create widget");
    change.setTarget(ChangeRequest.TARGET_USAGE);
    return change;
  }

  private static Stream<String> getUsageSources() {
    return Stream.of(CONSTANT, COMPUTED);
  }
}
