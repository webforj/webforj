package com.webforj.devtools.craftforj.inspector.source.cases.values;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.button.Button;
import com.webforj.component.icons.Icon;
import com.webforj.component.layout.columnslayout.ColumnsLayout;
import com.webforj.component.layout.flexlayout.FlexLayout;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
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

class PropertyFamiliesCaseTest {

  private static final String SOURCE = """
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
        private final Button grow = new Button("Grow item");
        private final ColumnsLayout columns = new ColumnsLayout();
        private final Button span = new Button("Span item");

        public PropertyWriteFamiliesView() {
          Icon icon = FeatherIcon.BELL.create();
          grow.setPrefixComponent(icon);
          row.add(grow, new Button("Other flex item"));
          row.setItemGrow(1, grow);
          columns.setBreakpoints(List.of(new Breakpoint("small", "0px", 2)));
          columns.add(span, new Button("Other column item"));
          columns.setSpan(span, 1);
          getBoundComponent().add(row, columns);
        }
      }
      """;

  private static final String EXPECTED = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.button.Button;
      import com.webforj.component.icons.FeatherIcon;
      import com.webforj.component.icons.Icon;
      import com.webforj.component.icons.TablerIcon;
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
        private final Button grow = new Button("Grow item");
        private final ColumnsLayout columns = new ColumnsLayout();
        private final Button span = new Button("Span item");

        public PropertyWriteFamiliesView() {
          Icon icon = TablerIcon.create("home");
          grow.setPrefixComponent(icon);
          row.add(grow, new Button("Other flex item"));
          row.setItemGrow(2.0, grow);
          columns.setBreakpoints(List.of(new Breakpoint("wide", 0, 3)));
          columns.add(span, new Button("Other column item"));
          columns.setSpan(span, 2);
          getBoundComponent().add(row, columns);
        }
      }
      """;

  @TempDir
  Path temporaryDirectory;

  @Test
  @DisplayName("Icon, breakpoint, flex-item and columns-item changes produce one complete valid "
      + "file")
  void writePropertyFamiliesTogether() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = addComponents(fixture);
      ChangeRequest grow = fixture.createChange("grow", "FlexItemGrow", 2.0);
      grow.setParentId("row");
      grow.setParentSource(
          new SourceLocation(file.toString(), 17, null, "row", FlexLayout.class.getName()));
      ChangeRequest span = fixture.createChange("span", "ColumnsLayoutItemSpan", 2);
      span.setParentId("columns");
      span.setParentSource(
          new SourceLocation(file.toString(), 19, null, "columns", ColumnsLayout.class.getName()));
      List<ChangeRequest> edits = List.of(fixture.createChange("icon", "Icon", "tabler:home"),
          fixture.createChange("columns", "ColumnsLayoutBreakpoints",
              List.of(Map.of("name", "wide", "minWidth", "0px", "columns", 3))),
          grow, span);
      SourceCodeModifier modifier = fixture.getModifier();

      assertEquals(List.of(EXPECTED),
          modifier.previewPatches(edits).stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(edits);
      assertEquals(EXPECTED, Files.readString(file));
    }
  }

  @Test
  @DisplayName("An invalid icon value reports its format error without changing the factory")
  void refuseInvalidIcon() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = addComponents(fixture);
      List<ChangeRequest> edits = List.of(fixture.createChange("icon", "Icon", "home"));
      SourceCodeModifier modifier = fixture.getModifier();
      final String expectedError = "Icon value must be in 'pool:name' format: home";

      assertEquals(expectedError, modifier.preview(edits).get(0).getError());
      assertEquals(SOURCE, Files.readString(file));
      assertEquals(expectedError, modifier.apply(edits).get(0).getError());
      assertEquals(SOURCE, Files.readString(file));
    }
  }

  @Test
  @DisplayName("An unregistered property reports the missing handler and preserves application "
      + "source")
  void refuseUnregisteredProperty() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = addComponents(fixture);
      FeatureProperty property =
          FeatureProperty.builder("Missing", "UnregisteredFeature").value("saved").build();
      List<ChangeRequest> edits = List.of(new ChangeRequest("grow", property, null));
      SourceCodeModifier modifier = fixture.getModifier();
      final String expectedError = "No handler found for feature type: UnregisteredFeature";

      assertEquals(expectedError, modifier.preview(edits).get(0).getError());
      assertEquals(SOURCE, Files.readString(file));
      assertEquals(expectedError, modifier.apply(edits).get(0).getError());
      assertEquals(SOURCE, Files.readString(file));
    }
  }

  private Path addComponents(SourceWriteFixture fixture) throws IOException {
    final String className = "com.devtoolsapplayoutspring.views.PropertyWriteFamiliesView";
    final Path file = fixture.addSource(className, SOURCE);
    fixture.addComponent("row", FlexLayout.class,
        List.of(new SourcePoint(className, "PropertyWriteFamiliesView.java", 17)));
    fixture.addComponent("grow", Button.class,
        List.of(new SourcePoint(className, "PropertyWriteFamiliesView.java", 18)));
    fixture.addComponent("columns", ColumnsLayout.class,
        List.of(new SourcePoint(className, "PropertyWriteFamiliesView.java", 19)));
    fixture.addComponent("span", Button.class,
        List.of(new SourcePoint(className, "PropertyWriteFamiliesView.java", 20)));
    fixture.addComponent("icon", Icon.class,
        List.of(new SourcePoint(className, "PropertyWriteFamiliesView.java", 23)));
    return file;
  }
}
