package com.webforj.devtools.craftforj.inspector.source.cases.operations;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.button.Button;
import com.webforj.devtools.craftforj.inspector.source.SourceCodeModifier;
import com.webforj.devtools.craftforj.inspector.source.cases.support.SourceWriteFixture;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeResult;
import com.webforj.devtools.craftforj.source.model.FilePatch;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class MixedTargetCaseTest {

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

  private static final String EXPLORE_EXPECTED =
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
              cta.setTooltipText("Create a widget");

              self.add(badge, messageLabel, cta);
            }
          }
          """;

  private static final String DASHBOARD_EXPECTED = """
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

  @TempDir
  Path temporaryDirectory;

  @ParameterizedTest
  @CsvSource({"false,false", "false,true", "true,false", "true,true"})
  @DisplayName("Mixed usage and definition edits preserve each file and isolate a failed "
      + "definition")
  void writeMixedTargets(boolean failedDefinition, boolean reversed) throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      final Path explore =
          fixture.addSource("com.devtoolsapplayoutspring.components.Explore", EXPLORE);
      final Path dashboard =
          fixture.addSource("com.devtoolsapplayoutspring.views.DashboardView", DASHBOARD);
      fixture.addComponent("cta", Button.class,
          List.of(
              new SourcePoint("com.devtoolsapplayoutspring.components.Explore", "Explore.java", 43),
              new SourcePoint("com.devtoolsapplayoutspring.views.DashboardView",
                  "DashboardView.java", 19)));
      ChangeRequest usage = fixture.createChange("cta", "HasText", "Build widget");
      usage.setOriginalValue("Create widget");
      usage.setTarget(ChangeRequest.TARGET_USAGE);
      ChangeRequest definition = fixture.createChange("cta", "HasTooltip", "Create a widget");
      definition.setTarget(ChangeRequest.TARGET_DEFINITION);
      List<ChangeRequest> edits = new ArrayList<>(List.of(usage, definition));
      if (failedDefinition) {
        edits.add(fixture.createChange("cta", "HasTheme",
            "com.webforj.component.button.ButtonTheme.NO_SUCH_THEME"));
      }
      if (reversed) {
        Collections.reverse(edits);
      }
      SourceCodeModifier modifier = fixture.getModifier();
      final String error = "Property 'setTheme': invalid enum value 'NO_SUCH_THEME'";
      final List<String> errors = failedDefinition ? List.of(error, error) : List.of();
      final Map<String, String> expected = failedDefinition
          ? Map.of(dashboard.toString(), DASHBOARD_EXPECTED)
          : Map.of(dashboard.toString(), DASHBOARD_EXPECTED, explore.toString(), EXPLORE_EXPECTED);

      assertEquals(errors, modifier.preview(edits).stream().map(ChangeResult::getError)
          .filter(Objects::nonNull).toList());
      assertEquals(expected, modifier.previewPatches(edits).stream()
          .collect(Collectors.toMap(FilePatch::getFile, FilePatch::getPatched)));
      assertEquals(DASHBOARD, Files.readString(dashboard));
      assertEquals(EXPLORE, Files.readString(explore));
      assertEquals(errors, modifier.apply(edits).stream().map(ChangeResult::getError)
          .filter(Objects::nonNull).toList());
      assertEquals(DASHBOARD_EXPECTED, Files.readString(dashboard));
      assertEquals(failedDefinition ? EXPLORE : EXPLORE_EXPECTED, Files.readString(explore));
    }
  }
}
