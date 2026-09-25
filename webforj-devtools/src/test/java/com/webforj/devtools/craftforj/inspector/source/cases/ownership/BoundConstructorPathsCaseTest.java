package com.webforj.devtools.craftforj.inspector.source.cases.ownership;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.layout.flexlayout.FlexLayout;
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

class BoundConstructorPathsCaseTest {

  private static final String SOURCE = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.flexlayout.FlexDirection;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;
      import java.util.List;

      @Route(value = "/constructor-path-cases", outlet = MainLayout.class)
      @FrameTitle("Constructor path cases")
      public class ConstructorPathsView extends Composite<FlexLayout> {
        public ConstructorPathsView() {
          getBoundComponent().setDirection(FlexDirection.COLUMN);
          Direct first = new Direct();
          Direct second = new Direct("Direct alternative");
          Delegating third = new Delegating();
          Delegating fourth = new Delegating("Delegated alternative");
          Initialized fifth = new Initialized();
          Initialized sixth = new Initialized("Initializer alternative");
          getBoundComponent().add(first, second, third, fourth, fifth, sixth);
          for (Button action : List.of(first.action, second.action, third.action, fourth.action,
              fifth.action, sixth.action)) {
            action.onClick(event -> action.setTooltipText("Clicked " + action.getText()));
          }
        }

        private static class Direct extends Composite<FlexLayout> {
          private final Button action = new Button("Direct default");

          Direct() {
            action.setTooltipText("Direct tooltip");
            getBoundComponent().add(action);
          }

          Direct(String label) {
            action.setText(label);
            getBoundComponent().add(action);
          }
        }

        private static class Delegating extends Composite<FlexLayout> {
          private final Button action = new Button();

          Delegating() {
            this("Delegated default");
          }

          Delegating(String label) {
            action.setText(label);
            getBoundComponent().add(action);
          }
        }

        private static class Initialized extends Composite<FlexLayout> {
          private final Button action = new Button("Initializer default");

          {
            action.setTooltipText("Initializer tooltip");
          }

          Initialized() {
            getBoundComponent().add(action);
          }

          Initialized(String label) {
            action.setText(label);
            action.setTooltipText("Constructor tooltip");
            getBoundComponent().add(action);
          }
        }
      }
      """;

  private static final String EXPECTED = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.flexlayout.FlexDirection;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;
      import java.util.List;

      @Route(value = "/constructor-path-cases", outlet = MainLayout.class)
      @FrameTitle("Constructor path cases")
      public class ConstructorPathsView extends Composite<FlexLayout> {
        public ConstructorPathsView() {
          getBoundComponent().setDirection(FlexDirection.COLUMN);
          Direct first = new Direct();
          Direct second = new Direct("Direct alternative");
          Delegating third = new Delegating();
          Delegating fourth = new Delegating("Delegated alternative");
          Initialized fifth = new Initialized();
          Initialized sixth = new Initialized("Initializer alternative");
          getBoundComponent().add(first, second, third, fourth, fifth, sixth);
          for (Button action : List.of(first.action, second.action, third.action, fourth.action,
              fifth.action, sixth.action)) {
            action.onClick(event -> action.setTooltipText("Clicked " + action.getText()));
          }
        }

        private static class Direct extends Composite<FlexLayout> {
          private final Button action = new Button("Direct default");

          Direct() {
            action.setTooltipText("Direct tooltip");
            getBoundComponent().add(action);
            getBoundComponent().setWidth("240px");
          }

          Direct(String label) {
            action.setText(label);
            getBoundComponent().add(action);
            getBoundComponent().setWidth("240px");
          }
        }

        private static class Delegating extends Composite<FlexLayout> {
          private final Button action = new Button();

          Delegating() {
            this("Delegated default");
          }

          Delegating(String label) {
            action.setText(label);
            getBoundComponent().add(action);
            getBoundComponent().setWidth("240px");
          }
        }

        private static class Initialized extends Composite<FlexLayout> {
          private final Button action = new Button("Initializer default");

          {
            action.setTooltipText("Initializer tooltip");
          }

          Initialized() {
            getBoundComponent().add(action);
            getBoundComponent().setWidth("240px");
          }

          Initialized(String label) {
            action.setText(label);
            action.setTooltipText("Constructor tooltip");
            getBoundComponent().add(action);
            getBoundComponent().setWidth("240px");
          }
        }
      }
      """;

  @TempDir
  Path temporaryDirectory;

  @Test
  @DisplayName("Nested bound-component writes cover constructor paths and preserve the outer "
      + "layout")
  void writeOnlyOwningComposites() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file =
          fixture.addSource("com.devtoolsapplayoutspring.views.ConstructorPathsView", SOURCE);
      addComponents(fixture, file, 32, 46, 63);
      SourceCodeModifier modifier = fixture.getModifier();
      List<ChangeRequest> edits = createChanges(fixture, "240px");

      assertEquals(List.of(EXPECTED),
          modifier.previewPatches(edits).stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(edits);
      assertEquals(EXPECTED, Files.readString(file));

      addComponents(fixture, file, 32, 48, 66);
      modifier.apply(createChanges(fixture, "240px"));
      assertEquals(EXPECTED, Files.readString(file));
      List<ChangeRequest> reset = createChanges(fixture, "");
      assertEquals(List.of(SOURCE),
          modifier.previewPatches(reset).stream().map(FilePatch::getPatched).toList());
      assertEquals(EXPECTED, Files.readString(file));
      modifier.apply(reset);
      assertEquals(SOURCE, Files.readString(file));
    }
  }

  private void addComponents(SourceWriteFixture fixture, Path file, int directLine,
      int delegatedLine, int initializedLine) {
    final String owner = "com.devtoolsapplayoutspring.views.ConstructorPathsView";
    fixture.addSourceClass(owner + "$Direct", file);
    fixture.addSourceClass(owner + "$Delegating", file);
    fixture.addSourceClass(owner + "$Initialized", file);
    fixture.addComponent("direct", FlexLayout.class,
        List.of(new SourcePoint(owner + "$Direct", "ConstructorPathsView.java", directLine)));
    fixture.addComponent("delegated", FlexLayout.class, List
        .of(new SourcePoint(owner + "$Delegating", "ConstructorPathsView.java", delegatedLine)));
    fixture.addComponent("initialized", FlexLayout.class, List
        .of(new SourcePoint(owner + "$Initialized", "ConstructorPathsView.java", initializedLine)));
  }

  private List<ChangeRequest> createChanges(SourceWriteFixture fixture, String value) {
    return List.of(fixture.createChange("direct", "HasWidth", value),
        fixture.createChange("delegated", "HasWidth", value),
        fixture.createChange("initialized", "HasWidth", value));
  }
}
