package com.webforj.devtools.craftforj.inspector.source.cases.values;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.icons.Icon;
import com.webforj.component.icons.IconButton;
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

class IconEscapingCaseTest {

  private static final String SOURCE =
      """
          package com.devtoolsapplayoutspring.views;

          import com.webforj.component.Composite;
          import com.webforj.component.button.Button;
          import com.webforj.component.icons.FeatherIcon;
          import com.webforj.component.icons.Icon;
          import com.webforj.component.icons.IconButton;
          import com.webforj.component.icons.IconPoolBuilder;
          import com.webforj.component.layout.flexlayout.FlexDirection;
          import com.webforj.component.layout.flexlayout.FlexLayout;
          import com.webforj.router.annotation.FrameTitle;
          import com.webforj.router.annotation.Route;
          import java.util.Map;

          @Route(value = "/icon-escaping-cases", outlet = MainLayout.class)
          @FrameTitle("Icon escaping cases")
          public class IconEscapingCasesView extends Composite<FlexLayout> {
            public IconEscapingCasesView() {
              IconPoolBuilder.fromMap("quoted\\"pool", Map.of("quoted\\"icon",
                  "<svg xmlns=\\"http://www.w3.org/2000/svg\\" viewBox=\\"0 0 24 24\\"><circle cx=\\"12\\" cy=\\"12\\" r=\\"9\\"/></svg>"));
              Icon direct = new Icon("bell", "feather");
              direct.setLabel("Direct escaped icon");
              Icon factory = FeatherIcon.BELL.create();
              factory.setLabel("Factory escaped icon");
              IconButton button = new IconButton("bell", "feather");
              button.setLabel("Escaped icon button");
              button.onClick(event -> button.setTooltipText("Clicked escaped icon"));
              Button update = new Button("Use quoted icon");
              update.onClick(event -> {
                direct.setPool("quoted\\"pool").setName("quoted\\"icon");
                factory.setPool("quoted\\"pool").setName("quoted\\"icon");
                button.setPool("quoted\\"pool").setName("quoted\\"icon");
              });
              getBoundComponent().setDirection(FlexDirection.COLUMN);
              getBoundComponent().add(direct, factory, button, update);
            }
          }
          """;

  private static final String EXPECTED =
      """
          package com.devtoolsapplayoutspring.views;

          import com.webforj.component.Composite;
          import com.webforj.component.button.Button;
          import com.webforj.component.icons.FeatherIcon;
          import com.webforj.component.icons.Icon;
          import com.webforj.component.icons.IconButton;
          import com.webforj.component.icons.IconPoolBuilder;
          import com.webforj.component.layout.flexlayout.FlexDirection;
          import com.webforj.component.layout.flexlayout.FlexLayout;
          import com.webforj.router.annotation.FrameTitle;
          import com.webforj.router.annotation.Route;
          import java.util.Map;

          @Route(value = "/icon-escaping-cases", outlet = MainLayout.class)
          @FrameTitle("Icon escaping cases")
          public class IconEscapingCasesView extends Composite<FlexLayout> {
            public IconEscapingCasesView() {
              IconPoolBuilder.fromMap("quoted\\"pool", Map.of("quoted\\"icon",
                  "<svg xmlns=\\"http://www.w3.org/2000/svg\\" viewBox=\\"0 0 24 24\\"><circle cx=\\"12\\" cy=\\"12\\" r=\\"9\\"/></svg>"));
              Icon direct = new Icon("quoted\\"icon", "quoted\\"pool");
              direct.setLabel("Direct escaped icon");
              Icon factory = new Icon("quoted\\"icon", "quoted\\"pool");
              factory.setLabel("Factory escaped icon");
              IconButton button = new IconButton("quoted\\"icon", "quoted\\"pool");
              button.setLabel("Escaped icon button");
              button.onClick(event -> button.setTooltipText("Clicked escaped icon"));
              Button update = new Button("Use quoted icon");
              update.onClick(event -> {
                direct.setPool("quoted\\"pool").setName("quoted\\"icon");
                factory.setPool("quoted\\"pool").setName("quoted\\"icon");
                button.setPool("quoted\\"pool").setName("quoted\\"icon");
              });
              getBoundComponent().setDirection(FlexDirection.COLUMN);
              getBoundComponent().add(direct, factory, button, update);
            }
          }
          """;

  @TempDir
  Path temporaryDirectory;

  @Test
  @DisplayName("Custom icon names and pools preserve quotes in constructors and rewritten "
      + "factories")
  void writeEscapedIcons() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      final String owner = "com.devtoolsapplayoutspring.views.IconEscapingCasesView";
      final Path file = fixture.addSource(owner, SOURCE);
      fixture.addComponent("direct", Icon.class,
          List.of(new SourcePoint(owner, "IconEscapingCasesView.java", 21)));
      fixture.addComponent("factory", Icon.class,
          List.of(new SourcePoint(owner, "IconEscapingCasesView.java", 23)));
      fixture.addComponent("button", IconButton.class,
          List.of(new SourcePoint(owner, "IconEscapingCasesView.java", 25)));
      final String value = "quoted\"pool:quoted\"icon";
      List<ChangeRequest> changes = List.of(fixture.createChange("direct", "Icon", value),
          fixture.createChange("factory", "Icon", value),
          fixture.createChange("button", "Icon", value));
      SourceCodeModifier modifier = fixture.getModifier();

      assertEquals(List.of(EXPECTED),
          modifier.previewPatches(changes).stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(changes);
      assertEquals(EXPECTED, Files.readString(file));
      modifier.apply(changes);
      assertEquals(EXPECTED, Files.readString(file));
    }
  }
}
