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
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class IconSourceFormsCaseTest {

  private static final String SOURCE = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.icons.FeatherIcon;
      import com.webforj.component.icons.Icon;
      import com.webforj.component.icons.IconButton;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/icon-source-cases", outlet = MainLayout.class)
      @FrameTitle("Icon source cases")
      public class IconSourceCasesView extends Composite<FlexLayout> {
        public IconSourceCasesView() {
          Icon direct = new Icon("bell", "feather");
          direct.setLabel("Direct icon");
          IconButton wrapped = new IconButton(FeatherIcon.BELL.create());
          wrapped.setLabel("Wrapped icon");
          wrapped.setEnabled(true);
          wrapped.onClick(event -> wrapped.setTooltipText("Clicked icon"));
          String name = "bell";
          Icon computed = new Icon(name, "feather");
          computed.setLabel("Computed icon");
          Icon first = FeatherIcon.BELL.create(), second = FeatherIcon.HOME.create();
          first.setLabel("First shared line");
          second.setLabel("Second shared line");
          getBoundComponent().add(direct, wrapped, computed, first, second);
          getBoundComponent().add(FeatherIcon.BELL.create().setLabel("Inline icon"));
        }
      }
      """;

  private static final String EXPECTED = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.icons.FeatherIcon;
      import com.webforj.component.icons.Icon;
      import com.webforj.component.icons.IconButton;
      import com.webforj.component.icons.TablerIcon;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/icon-source-cases", outlet = MainLayout.class)
      @FrameTitle("Icon source cases")
      public class IconSourceCasesView extends Composite<FlexLayout> {
        public IconSourceCasesView() {
          Icon direct = new Icon("home", "tabler");
          direct.setLabel("Direct icon");
          IconButton wrapped = new IconButton(TablerIcon.create("home"));
          wrapped.setLabel("Wrapped icon");
          wrapped.setEnabled(true);
          wrapped.onClick(event -> wrapped.setTooltipText("Clicked icon"));
          String name = "bell";
          Icon computed = new Icon(name, "feather");
          computed.setLabel("Computed icon");
          Icon first = FeatherIcon.BELL.create(), second = FeatherIcon.HOME.create();
          first.setLabel("First shared line");
          second.setLabel("Second shared line");
          getBoundComponent().add(direct, wrapped, computed, first, second);
          getBoundComponent().add(TablerIcon.create("home").setLabel("Inline icon"));
        }
      }
      """;

  @TempDir
  Path temporaryDirectory;

  @Test
  @DisplayName("Constructor, wrapped factory and inline factory edits preserve surrounding code")
  void writeIconSourceForms() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = addIcons(fixture);
      List<ChangeRequest> edits = List.of(fixture.createChange("direct", "Icon", "tabler:home"),
          fixture.createChange("wrapped", "Icon", "tabler:home"),
          fixture.createChange("inline", "Icon", "tabler:home"));
      SourceCodeModifier modifier = fixture.getModifier();

      assertEquals(List.of(EXPECTED),
          modifier.previewPatches(edits).stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(edits);
      assertEquals(EXPECTED, Files.readString(file));
    }
  }

  @ParameterizedTest
  @CsvSource(delimiter = '|',
      value = {
          "computed|Cannot rewrite icon creation 'new Icon(...)'. "
              + "Expected string literal name and pool arguments.",
          "ambiguous|Cannot identify Icon at line 24: multiple matching creations share this line"})
  @DisplayName("Computed or ambiguous icon expressions report precise errors and preserve source")
  void refuseUnresolvedIcon(String id, String error) throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = addIcons(fixture);
      List<ChangeRequest> edits = List.of(fixture.createChange(id, "Icon", "tabler:home"));
      SourceCodeModifier modifier = fixture.getModifier();

      assertEquals(error, modifier.preview(edits).get(0).getError());
      assertEquals(SOURCE, Files.readString(file));
      assertEquals(error, modifier.apply(edits).get(0).getError());
      assertEquals(SOURCE, Files.readString(file));
    }
  }

  private Path addIcons(SourceWriteFixture fixture) throws IOException {
    final String owner = "com.devtoolsapplayoutspring.views.IconSourceCasesView";
    final Path file = fixture.addSource(owner, SOURCE);
    fixture.addComponent("direct", Icon.class,
        List.of(new SourcePoint(owner, "IconSourceCasesView.java", 15)));
    fixture.addComponent("wrapped", IconButton.class,
        List.of(new SourcePoint(owner, "IconSourceCasesView.java", 17)));
    fixture.addComponent("computed", Icon.class,
        List.of(new SourcePoint(owner, "IconSourceCasesView.java", 22)));
    fixture.addComponent("ambiguous", Icon.class,
        List.of(new SourcePoint(owner, "IconSourceCasesView.java", 24)));
    fixture.addComponent("inline", Icon.class,
        List.of(new SourcePoint(owner, "IconSourceCasesView.java", 28)));
    return file;
  }
}
