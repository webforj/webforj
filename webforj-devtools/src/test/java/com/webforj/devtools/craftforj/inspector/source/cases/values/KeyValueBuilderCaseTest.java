package com.webforj.devtools.craftforj.inspector.source.cases.values;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.devtoolsapplayoutspring.contributions.ButtonOpacityContribution;
import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.button.Button;
import com.webforj.component.layout.flexlayout.FlexLayout;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.source.SourceCodeModifier;
import com.webforj.devtools.craftforj.inspector.source.cases.support.SourceWriteFixture;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
import com.webforj.devtools.craftforj.source.model.FilePatch;
import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class KeyValueBuilderCaseTest {

  private static final String SOURCE = """
      package com.devtoolsapplayoutspring.views;

      import com.devtoolsapplayoutspring.contributions.ButtonOpacityContribution;
      import com.webforj.component.Composite;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.flexlayout.FlexDirection;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/key-value-builder-cases", outlet = MainLayout.class)
      @FrameTitle("Key/value and builder cases")
      public class KeyValueBuilderCasesView extends Composite<FlexLayout> {
        public KeyValueBuilderCasesView() {
          Button action = new Button("Opacity action");
          action.setStyle("opacity", "0.9");
          action.setStyle("cursor", "pointer");
          action.setStyle("opacity", "0.8");
          action.onClick(event -> action.setText("Opacity action clicked"));
          FlexLayout built = FlexLayout.create(action).align().center().build();
          built.setWidth("220px");
          ButtonOpacityContribution opacity = new ButtonOpacityContribution();
          Button update = new Button("Change opacity and width");
          update.onClick(event -> {
            opacity.set(action, "0.4");
            built.setWidth("260px");
          });
          Button reset = new Button("Reset opacity");
          reset.onClick(event -> opacity.set(action, ""));
          Button spawn = new Button("Create callback action");
          spawn.onClick(event -> {
            Button created = new Button("Callback action");
            created.setTooltipText("Callback tooltip");
            created.onClick(click -> created.setText("Callback action clicked"));
            getBoundComponent().add(created);
          });
          getBoundComponent().setDirection(FlexDirection.COLUMN);
          getBoundComponent().add(built,
              FlexLayout.create(new Button("Inline builder action")).build().setWidth("200px"),
              update, reset, spawn);
        }
      }
      """;

  private static final String EXPECTED = """
      package com.devtoolsapplayoutspring.views;

      import com.devtoolsapplayoutspring.contributions.ButtonOpacityContribution;
      import com.webforj.component.Composite;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.flexlayout.FlexDirection;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/key-value-builder-cases", outlet = MainLayout.class)
      @FrameTitle("Key/value and builder cases")
      public class KeyValueBuilderCasesView extends Composite<FlexLayout> {
        public KeyValueBuilderCasesView() {
          Button action = new Button("Opacity action");
          action.setStyle("opacity", "0.9");
          action.setStyle("cursor", "pointer");
          action.setStyle("opacity", "0.4");
          action.onClick(event -> action.setText("Opacity action clicked"));
          FlexLayout built = FlexLayout.create(action).align().center().build();
          built.setWidth("300px");
          ButtonOpacityContribution opacity = new ButtonOpacityContribution();
          Button update = new Button("Change opacity and width");
          update.onClick(event -> {
            opacity.set(action, "0.4");
            built.setWidth("260px");
          });
          Button reset = new Button("Reset opacity");
          reset.onClick(event -> opacity.set(action, ""));
          Button spawn = new Button("Create callback action");
          spawn.onClick(event -> {
            Button created = new Button("Callback action");
            created.setTooltipText("Saved callback tooltip");
            created.onClick(click -> created.setText("Callback action clicked"));
            getBoundComponent().add(created);
          });
          getBoundComponent().setDirection(FlexDirection.COLUMN);
          getBoundComponent().add(built,
              FlexLayout.create(new Button("Inline builder action")).build().setWidth("240px"),
              update, reset, spawn);
        }
      }
      """;

  private static final String RESET = """
      package com.devtoolsapplayoutspring.views;

      import com.devtoolsapplayoutspring.contributions.ButtonOpacityContribution;
      import com.webforj.component.Composite;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.flexlayout.FlexDirection;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/key-value-builder-cases", outlet = MainLayout.class)
      @FrameTitle("Key/value and builder cases")
      public class KeyValueBuilderCasesView extends Composite<FlexLayout> {
        public KeyValueBuilderCasesView() {
          Button action = new Button("Opacity action");
          action.setStyle("cursor", "pointer");
          action.onClick(event -> action.setText("Opacity action clicked"));
          FlexLayout built = FlexLayout.create(action).align().center().build();
          built.setWidth("300px");
          ButtonOpacityContribution opacity = new ButtonOpacityContribution();
          Button update = new Button("Change opacity and width");
          update.onClick(event -> {
            opacity.set(action, "0.4");
            built.setWidth("260px");
          });
          Button reset = new Button("Reset opacity");
          reset.onClick(event -> opacity.set(action, ""));
          Button spawn = new Button("Create callback action");
          spawn.onClick(event -> {
            Button created = new Button("Callback action");
            created.setTooltipText("Saved callback tooltip");
            created.onClick(click -> created.setText("Callback action clicked"));
            getBoundComponent().add(created);
          });
          getBoundComponent().setDirection(FlexDirection.COLUMN);
          getBoundComponent().add(built,
              FlexLayout.create(new Button("Inline builder action")).build().setWidth("240px"),
              update, reset, spawn);
        }
      }
      """;

  @TempDir
  Path temporaryDirectory;

  @Test
  @DisplayName("Keyed reset preserves other styles while builder and callback writes target their "
      + "actual components")
  void writeKeyedBuilderAndCallbackProperties() throws IOException {
    try (SourceWriteFixture fixture = createFixture()) {
      final String owner = "com.devtoolsapplayoutspring.views.KeyValueBuilderCasesView";
      final Path file = fixture.addSource(owner, SOURCE);
      fixture.addComponent("action", Button.class,
          List.of(new SourcePoint(owner, "KeyValueBuilderCasesView.java", 15)));
      fixture.addComponent("built", FlexLayout.class,
          List.of(new SourcePoint(owner, "KeyValueBuilderCasesView.java", 20)));
      fixture.addComponent("inline", FlexLayout.class,
          List.of(new SourcePoint(owner, "KeyValueBuilderCasesView.java", 39)));
      fixture.addComponent("created", Button.class,
          List.of(new SourcePoint(owner, "KeyValueBuilderCasesView.java", 32)));
      List<ChangeRequest> changes = List.of(fixture.createChange("action", "ButtonOpacity", "0.4"),
          fixture.createChange("built", "HasWidth", "300px"),
          fixture.createChange("inline", "HasWidth", "240px"),
          fixture.createChange("created", "HasTooltip", "Saved callback tooltip"));
      SourceCodeModifier modifier = fixture.getModifier();

      assertEquals(List.of(EXPECTED),
          modifier.previewPatches(changes).stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(changes);
      assertEquals(EXPECTED, Files.readString(file));
      modifier.apply(changes);
      assertEquals(EXPECTED, Files.readString(file));
      List<ChangeRequest> reset = List.of(fixture.createChange("action", "ButtonOpacity", ""));
      assertEquals(List.of(RESET),
          modifier.previewPatches(reset).stream().map(FilePatch::getPatched).toList());
      assertEquals(EXPECTED, Files.readString(file));
      modifier.apply(reset);
      assertEquals(RESET, Files.readString(file));
      modifier.apply(reset);
      assertEquals(RESET, Files.readString(file));
    }
  }

  private SourceWriteFixture createFixture() throws IOException {
    Path providers = temporaryDirectory.resolve("providers");
    Path service = providers.resolve("META-INF/services/" + FeatureHandler.class.getName());
    Files.createDirectories(service.getParent());
    Files.writeString(service, ButtonOpacityContribution.class.getName());
    final ClassLoader previous = Thread.currentThread().getContextClassLoader();
    try (URLClassLoader loader =
        new URLClassLoader(new URL[] {providers.toUri().toURL()}, previous)) {
      Thread.currentThread().setContextClassLoader(loader);
      try {
        return new SourceWriteFixture(temporaryDirectory.resolve("sources"));
      } finally {
        Thread.currentThread().setContextClassLoader(previous);
      }
    }
  }
}
