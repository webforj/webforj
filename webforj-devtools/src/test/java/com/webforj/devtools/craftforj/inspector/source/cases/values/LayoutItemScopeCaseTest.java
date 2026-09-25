package com.webforj.devtools.craftforj.inspector.source.cases.values;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.button.Button;
import com.webforj.component.layout.flexlayout.FlexLayout;
import com.webforj.devtools.craftforj.inspector.source.SourceCodeModifier;
import com.webforj.devtools.craftforj.inspector.source.cases.support.SourceWriteFixture;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
import com.webforj.devtools.craftforj.source.model.SourceLocation;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class LayoutItemScopeCaseTest {

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
          Button action = createAction();
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

        private Button createAction() {
          Button action = new Button("Opacity action");
          return action;
        }
      }
      """;

  @TempDir
  Path temporaryDirectory;

  @ParameterizedTest
  @ValueSource(booleans = {false, true})
  @DisplayName("An item created in another method refuses both write and reset without changing "
      + "source")
  void refuseInvisibleItem(boolean reset) throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      final String owner = "com.devtoolsapplayoutspring.views.KeyValueBuilderCasesView";
      final Path file = fixture.addSource(owner, SOURCE);
      fixture.addComponent("action", Button.class,
          List.of(new SourcePoint(owner, "KeyValueBuilderCasesView.java", 44),
              new SourcePoint(owner, "KeyValueBuilderCasesView.java", 15)));
      fixture.addComponent("built", FlexLayout.class,
          List.of(new SourcePoint(owner, "KeyValueBuilderCasesView.java", 20)));
      ChangeRequest change = fixture.createChange("action", "FlexItemGrow", reset ? "" : 2.0);
      change.setParentId("built");
      change.setParentSource(
          new SourceLocation(file.toString(), 20, owner, "built", FlexLayout.class.getName()));
      SourceCodeModifier modifier = fixture.getModifier();
      final String error =
          "'action' is declared in another block, so the write location cannot see it";

      assertEquals(error, modifier.preview(List.of(change)).get(0).getError());
      assertEquals(SOURCE, Files.readString(file));
      assertEquals(error, modifier.apply(List.of(change)).get(0).getError());
      assertEquals(SOURCE, Files.readString(file));
    }
  }
}
