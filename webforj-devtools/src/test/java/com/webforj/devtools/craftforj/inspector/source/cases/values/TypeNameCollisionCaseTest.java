package com.webforj.devtools.craftforj.inspector.source.cases.values;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.button.Button;
import com.webforj.component.icons.Icon;
import com.webforj.component.layout.columnslayout.ColumnsLayout;
import com.webforj.component.layout.flexlayout.FlexAlignment;
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
import java.util.Map;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class TypeNameCollisionCaseTest {

  private static final String SOURCE =
      """
          package com.devtoolsapplayoutspring.views;

          import com.webforj.component.Composite;
          import com.webforj.component.button.Button;
          import com.webforj.component.icons.FeatherIcon;
          import com.webforj.component.icons.Icon;
          import com.webforj.component.layout.columnslayout.ColumnsLayout;
          import com.webforj.component.layout.flexlayout.FlexLayout;
          import com.webforj.router.annotation.FrameTitle;
          import com.webforj.router.annotation.Route;
          import java.util.List;

          @Route(value = "/type-binding-cases", outlet = MainLayout.class)
          @FrameTitle("Property write families")
          public class TypeBindingCasesView extends Composite<FlexLayout> {
            private final FlexLayout row = new FlexLayout();
            private final Button FlexAlignment = new Button("Grow item");
            private final ColumnsLayout columns = new ColumnsLayout();
            private final Button span = new Button("Span item");

            public <Breakpoint, TablerIcon> TypeBindingCasesView() {
              Icon icon = FeatherIcon.BELL.create();
              FlexAlignment.setPrefixComponent(icon);
              row.add(FlexAlignment, new Button("Other flex item"));
              row.setItemGrow(1, FlexAlignment);
              row.setItemAlignment(com.webforj.component.layout.flexlayout.FlexAlignment.START, FlexAlignment);
              columns.setBreakpoints(List.of(new ColumnsLayout.Breakpoint("small", "0px", 2)));
              columns.add(span, new Button("Other column item"));
              columns.setSpan(span, 1);
              getBoundComponent().add(row, columns);
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
          import com.webforj.component.layout.columnslayout.ColumnsLayout;
          import com.webforj.component.layout.flexlayout.FlexLayout;
          import com.webforj.router.annotation.FrameTitle;
          import com.webforj.router.annotation.Route;
          import java.util.List;

          @Route(value = "/type-binding-cases", outlet = MainLayout.class)
          @FrameTitle("Property write families")
          public class TypeBindingCasesView extends Composite<FlexLayout> {
            private final FlexLayout row = new FlexLayout();
            private final Button FlexAlignment = new Button("Grow item");
            private final ColumnsLayout columns = new ColumnsLayout();
            private final Button span = new Button("Span item");

            public <Breakpoint, TablerIcon> TypeBindingCasesView() {
              Icon icon = com.webforj.component.icons.TablerIcon.create("home");
              FlexAlignment.setPrefixComponent(icon);
              row.add(FlexAlignment, new Button("Other flex item"));
              row.setItemGrow(1, FlexAlignment);
              row.setItemAlignment(com.webforj.component.layout.flexlayout.FlexAlignment.END, FlexAlignment);
              columns.setBreakpoints(List.of(
                  new com.webforj.component.layout.columnslayout.ColumnsLayout.Breakpoint("wide", 0, 3)));
              columns.add(span, new Button("Other column item"));
              columns.setSpan(span, 1);
              getBoundComponent().add(row, columns);
            }
          }
          """;

  @TempDir
  Path temporaryDirectory;

  @Test
  @DisplayName("Generated factory, constructor and item enum types preserve colliding application "
      + "names")
  void writeQualifiedTypes() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      final String owner = "com.devtoolsapplayoutspring.views.TypeBindingCasesView";
      final Path file = fixture.addSource(owner, SOURCE);
      fixture.addComponent("row", FlexLayout.class,
          List.of(new SourcePoint(owner, "TypeBindingCasesView.java", 16)));
      fixture.addComponent("item", Button.class,
          List.of(new SourcePoint(owner, "TypeBindingCasesView.java", 17)));
      fixture.addComponent("columns", ColumnsLayout.class,
          List.of(new SourcePoint(owner, "TypeBindingCasesView.java", 18)));
      fixture.addComponent("icon", Icon.class,
          List.of(new SourcePoint(owner, "TypeBindingCasesView.java", 22)));
      ChangeRequest alignment =
          fixture.createChange("item", "FlexItemAlignment", FlexAlignment.class.getName() + ".END");
      alignment.setParentId("row");
      alignment.setParentSource(
          new SourceLocation(file.toString(), 16, null, "row", FlexLayout.class.getName()));
      List<ChangeRequest> edits = List.of(fixture.createChange("icon", "Icon", "tabler:home"),
          fixture.createChange("columns", "ColumnsLayoutBreakpoints",
              List.of(Map.of("name", "wide", "minWidth", "0px", "columns", 3))),
          alignment);
      SourceCodeModifier modifier = fixture.getModifier();

      assertEquals(List.of(EXPECTED),
          modifier.previewPatches(edits).stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(edits);
      assertEquals(EXPECTED, Files.readString(file));
      modifier.apply(edits);
      assertEquals(EXPECTED, Files.readString(file));
    }
  }
}
