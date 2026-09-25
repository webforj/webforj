package com.webforj.devtools.craftforj.inspector.source.cases.values;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.layout.columnslayout.ColumnsLayout;
import com.webforj.devtools.craftforj.inspector.source.SourceCodeModifier;
import com.webforj.devtools.craftforj.inspector.source.cases.support.SourceWriteFixture;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
import com.webforj.devtools.craftforj.source.model.FilePatch;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class BreakpointNormalizationCaseTest {

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
          columns.setBreakpoints(List.of(new Breakpoint("valid", 10, 3)));
          columns.add(span, new Button("Other column item"));
          columns.setSpan(span, 1);
          getBoundComponent().add(row, columns);
        }
      }
      """;

  private static final String LARGE_EXPECTED = """
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
          columns.setBreakpoints(List.of(new Breakpoint("valid", "2147483648px", 3)));
          columns.add(span, new Button("Other column item"));
          columns.setSpan(span, 1);
          getBoundComponent().add(row, columns);
        }
      }
      """;

  private static final String RESET = """
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
          columns.add(span, new Button("Other column item"));
          columns.setSpan(span, 1);
          getBoundComponent().add(row, columns);
        }
      }
      """;

  @TempDir
  Path temporaryDirectory;

  @ParameterizedTest
  @ValueSource(booleans = {false, true})
  @DisplayName("Breakpoint pixels remain decimal or use the String overload beyond the int range")
  void writeNormalizedBreakpoints(boolean large) throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = addColumns(fixture);
      ChangeRequest edit = fixture.createChange("columns", "ColumnsLayoutBreakpoints",
          List.of(Map.of("name", "missing", "columns", 1),
              Map.of("name", "valid", "minWidth", large ? "2147483648px" : "010px", "columns", 3)));
      SourceCodeModifier modifier = fixture.getModifier();
      final String expected = large ? LARGE_EXPECTED : EXPECTED;

      assertEquals(List.of(expected),
          modifier.previewPatches(List.of(edit)).stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(List.of(edit));
      assertEquals(expected, Files.readString(file));
    }
  }

  @ParameterizedTest
  @ValueSource(booleans = {false, true})
  @DisplayName("Empty and entirely incomplete breakpoint lists restore defaults by removing the "
      + "setter")
  void resetBreakpoints(boolean incomplete) throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = addColumns(fixture);
      Object value = incomplete ? List.of(Map.of("name", "missing", "columns", 1)) : List.of();
      List<ChangeRequest> edits =
          List.of(fixture.createChange("columns", "ColumnsLayoutBreakpoints", value));
      SourceCodeModifier modifier = fixture.getModifier();

      assertEquals(List.of(RESET),
          modifier.previewPatches(edits).stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(edits);
      assertEquals(RESET, Files.readString(file));
    }
  }

  private Path addColumns(SourceWriteFixture fixture) throws IOException {
    final String owner = "com.devtoolsapplayoutspring.views.PropertyWriteFamiliesView";
    Path file = fixture.addSource(owner, SOURCE);
    fixture.addComponent("columns", ColumnsLayout.class,
        List.of(new SourcePoint(owner, "PropertyWriteFamiliesView.java", 19)));
    return file;
  }
}
