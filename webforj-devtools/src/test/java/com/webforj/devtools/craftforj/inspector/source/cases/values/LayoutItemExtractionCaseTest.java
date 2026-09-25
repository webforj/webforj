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

class LayoutItemExtractionCaseTest {

  private static final String SOURCE =
      """
          package com.devtoolsapplayoutspring.views;

          import com.webforj.component.Composite;
          import com.webforj.component.button.Button;
          import com.webforj.component.icons.FeatherIcon;
          import com.webforj.component.icons.Icon;
          import com.webforj.component.layout.columnslayout.ColumnsLayout;
          import com.webforj.component.layout.columnslayout.ColumnsLayout.Breakpoint;
          import com.webforj.component.layout.flexlayout.FlexLayout;
          import com.webforj.router.annotation.FrameTitle;
          import com.webforj.router.annotation.Route;
          import java.util.List;

          @Route(value = "/property-write-families", outlet = MainLayout.class)
          @FrameTitle("Property write families")
          public class PropertyWriteFamiliesView extends Composite<FlexLayout> {
            private final FlexLayout row = new FlexLayout();
            private final com.webforj.component.button.Button grow = new com.webforj.component.button.Button("Grow item");
            private final ColumnsLayout columns = new ColumnsLayout();
            private final com.webforj.component.button.Button span = new com.webforj.component.button.Button("Span item");

            public <Button> PropertyWriteFamiliesView() {
              Icon icon = FeatherIcon.BELL.create();
              grow.setPrefixComponent(icon);
              row.add(grow, new com.webforj.component.button.Button("Other flex item"));
              row.setItemGrow(1, grow);
              columns.setBreakpoints(List.of(new Breakpoint("small", "0px", 2)));
              columns.add(span, new com.webforj.component.button.Button("Other column item"));
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
          import com.webforj.component.layout.columnslayout.ColumnsLayout.Breakpoint;
          import com.webforj.component.layout.flexlayout.FlexLayout;
          import com.webforj.router.annotation.FrameTitle;
          import com.webforj.router.annotation.Route;
          import java.util.List;

          @Route(value = "/property-write-families", outlet = MainLayout.class)
          @FrameTitle("Property write families")
          public class PropertyWriteFamiliesView extends Composite<FlexLayout> {
            private final FlexLayout row = new FlexLayout();
            private final com.webforj.component.button.Button grow = new com.webforj.component.button.Button("Grow item");
            private final ColumnsLayout columns = new ColumnsLayout();
            private final com.webforj.component.button.Button span = new com.webforj.component.button.Button("Span item");

            public <Button> PropertyWriteFamiliesView() {
              Icon icon = FeatherIcon.BELL.create();
              grow.setPrefixComponent(icon);
              com.webforj.component.button.Button button = new com.webforj.component.button.Button("Other flex item");
              row.add(grow, button);
              row.setItemGrow(1, grow);
              row.setItemGrow(2.0, button);
              columns.setBreakpoints(List.of(new Breakpoint("small", "0px", 2)));
              columns.add(span, new com.webforj.component.button.Button("Other column item"));
              columns.setSpan(span, 1);
              getBoundComponent().add(row, columns);
            }
          }
          """;

  @TempDir
  Path temporaryDirectory;

  @Test
  @DisplayName("An inline child keeps its qualified type and sibling item settings when extracted "
      + "for a layout edit")
  void writeExtractedItem() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      final String owner = "com.devtoolsapplayoutspring.views.PropertyWriteFamiliesView";
      final Path file = fixture.addSource(owner, SOURCE);
      fixture.addComponent("row", FlexLayout.class,
          List.of(new SourcePoint(owner, "PropertyWriteFamiliesView.java", 17)));
      fixture.addComponent("item", Button.class,
          List.of(new SourcePoint(owner, "PropertyWriteFamiliesView.java", 25)));
      ChangeRequest change = fixture.createChange("item", "FlexItemGrow", 2.0);
      change.setParentId("row");
      change.setParentSource(
          new SourceLocation(file.toString(), 17, owner, "row", FlexLayout.class.getName()));
      SourceCodeModifier modifier = fixture.getModifier();
      List<ChangeRequest> changes = List.of(change);

      assertEquals(List.of(EXPECTED),
          modifier.previewPatches(changes).stream().map(FilePatch::getPatched).toList(),
          () -> modifier.preview(changes).stream().map(result -> result.getError()).toList()
              .toString());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(changes);
      assertEquals(EXPECTED, Files.readString(file));
      modifier.apply(changes);
      assertEquals(EXPECTED, Files.readString(file));
    }
  }
}
