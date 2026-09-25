package com.webforj.devtools.craftforj.inspector.source.cases.values;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.button.Button;
import com.webforj.component.layout.flexlayout.FlexLayout;
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

class LayoutItemIdentityCaseTest {

  private static final String SOURCE = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/layout-callback-cases", outlet = MainLayout.class)
      @FrameTitle("Layout callback cases")
      public class LayoutCallbackCasesView extends Composite<FlexLayout> {
        private final FlexLayout row = new FlexLayout();
        private final Button action = new Button("Field grow action");

        public LayoutCallbackCasesView() {
          Button action = new Button("Local grow action");
          row.add(this.action, action);
          row.setItemGrow(1, this.action);
          row.setItemGrow(3, action);
          getBoundComponent().add(row);
        }
      }
      """;

  private static final String EXPECTED = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/layout-callback-cases", outlet = MainLayout.class)
      @FrameTitle("Layout callback cases")
      public class LayoutCallbackCasesView extends Composite<FlexLayout> {
        private final FlexLayout row = new FlexLayout();
        private final Button action = new Button("Field grow action");

        public LayoutCallbackCasesView() {
          Button action = new Button("Local grow action");
          row.add(this.action, action);
          row.setItemGrow(2.0, this.action);
          row.setItemGrow(3, action);
          getBoundComponent().add(row);
        }
      }
      """;

  private static final String RESET = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/layout-callback-cases", outlet = MainLayout.class)
      @FrameTitle("Layout callback cases")
      public class LayoutCallbackCasesView extends Composite<FlexLayout> {
        private final FlexLayout row = new FlexLayout();
        private final Button action = new Button("Field grow action");

        public LayoutCallbackCasesView() {
          Button action = new Button("Local grow action");
          row.add(this.action, action);
          row.setItemGrow(3, action);
          getBoundComponent().add(row);
        }
      }
      """;

  private static final String INSERTED = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/layout-callback-cases", outlet = MainLayout.class)
      @FrameTitle("Layout callback cases")
      public class LayoutCallbackCasesView extends Composite<FlexLayout> {
        private final FlexLayout row = new FlexLayout();
        private final Button action = new Button("Field grow action");

        public LayoutCallbackCasesView() {
          Button action = new Button("Local grow action");
          row.add(this.action, action);
          row.setItemGrow(3, action);
          row.setItemGrow(2.0, this.action);
          getBoundComponent().add(row);
        }
      }
      """;

  @TempDir
  Path temporaryDirectory;

  @Test
  @DisplayName("A field item's update, reset and reinsertion preserve a same-named local item")
  void writeOnlySelectedChild() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      final String owner = "com.devtoolsapplayoutspring.views.LayoutCallbackCasesView";
      Path file = fixture.addSource(owner, SOURCE);
      fixture.addComponent("row", FlexLayout.class,
          List.of(new SourcePoint(owner, "LayoutCallbackCasesView.java", 12)));
      fixture.addComponent("field", Button.class,
          List.of(new SourcePoint(owner, "LayoutCallbackCasesView.java", 13)));
      SourceCodeModifier modifier = fixture.getModifier();
      ChangeRequest write = createChange(fixture, file, 2.0);
      final ChangeRequest reset = createChange(fixture, file, "");

      assertEquals(List.of(EXPECTED),
          modifier.previewPatches(List.of(write)).stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(List.of(write));
      assertEquals(EXPECTED, Files.readString(file));
      assertEquals(List.of(RESET),
          modifier.previewPatches(List.of(reset)).stream().map(FilePatch::getPatched).toList());
      assertEquals(EXPECTED, Files.readString(file));
      modifier.apply(List.of(reset));
      assertEquals(RESET, Files.readString(file));
      assertEquals(List.of(INSERTED),
          modifier.previewPatches(List.of(write)).stream().map(FilePatch::getPatched).toList());
      assertEquals(RESET, Files.readString(file));
      modifier.apply(List.of(write));
      assertEquals(INSERTED, Files.readString(file));
    }
  }

  private ChangeRequest createChange(SourceWriteFixture fixture, Path file, Object value) {
    ChangeRequest edit = fixture.createChange("field", "FlexItemGrow", value);
    edit.setParentId("row");
    edit.setParentSource(
        new SourceLocation(file.toString(), 12, null, "row", FlexLayout.class.getName()));
    return edit;
  }
}
