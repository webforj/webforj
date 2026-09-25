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

class LayoutItemFileCaseTest {

  private static final String PARENT = """
      package com.devtoolsapplayoutspring.views;

      import com.devtoolsapplayoutspring.components.SharedActions;
      import com.webforj.component.Composite;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/layout-callback-cases", outlet = MainLayout.class)
      @FrameTitle("Layout callback cases")
      public class LayoutCallbackCasesView extends Composite<FlexLayout> {
        private final FlexLayout row = new FlexLayout();

        public LayoutCallbackCasesView() {
          Button action = SharedActions.createAction();
          row.add(action);
          row.setItemGrow(1, action);
          getBoundComponent().add(row);
        }
      }
      """;

  private static final String FACTORY = """
      package com.devtoolsapplayoutspring.components;

      import com.webforj.component.button.Button;

      public class SharedActions {
        public static Button createAction() {
          return new Button("Returned action");
        }
      }
      """;

  @TempDir
  Path temporaryDirectory;

  @ParameterizedTest
  @ValueSource(booleans = {false, true})
  @DisplayName("A layout item from another source file reports its limitation without changing "
      + "either file")
  void refuseCrossFileItem(boolean reset) throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      final Path parent =
          fixture.addSource("com.devtoolsapplayoutspring.views.LayoutCallbackCasesView", PARENT);
      final Path factory =
          fixture.addSource("com.devtoolsapplayoutspring.components.SharedActions", FACTORY);
      fixture.addComponent("row", FlexLayout.class,
          List.of(new SourcePoint("com.devtoolsapplayoutspring.views.LayoutCallbackCasesView",
              "LayoutCallbackCasesView.java", 13)));
      fixture.addComponent("action", Button.class,
          List.of(new SourcePoint("com.devtoolsapplayoutspring.components.SharedActions",
              "SharedActions.java", 7)));
      ChangeRequest edit = fixture.createChange("action", "FlexItemGrow", reset ? "" : 2.0);
      edit.setParentId("row");
      edit.setParentSource(
          new SourceLocation(parent.toString(), 13, null, "row", FlexLayout.class.getName()));
      SourceCodeModifier modifier = fixture.getModifier();
      final String error = "Layout item properties require the item and its parent layout to be "
          + "created in the same file, but the item was created in SharedActions.java";

      assertEquals(error, modifier.preview(List.of(edit)).get(0).getError());
      assertEquals(PARENT, Files.readString(parent));
      assertEquals(FACTORY, Files.readString(factory));
      assertEquals(error, modifier.apply(List.of(edit)).get(0).getError());
      assertEquals(PARENT, Files.readString(parent));
      assertEquals(FACTORY, Files.readString(factory));
    }
  }
}
