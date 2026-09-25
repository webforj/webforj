package com.webforj.devtools.craftforj.inspector.source.cases.ownership;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.icons.Icon;
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

class FactoryChainCaseTest {

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
          getBoundComponent().add(FeatherIcon.BELL.create().setLabel("Saved inline icon"));
        }
      }
      """;

  private static final String RESET = """
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
          getBoundComponent().add(FeatherIcon.BELL.create());
        }
      }
      """;

  private static final String ABSENT_EXPECTED = """
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
          Icon icon = FeatherIcon.BELL.create().setLabel("Inline icon");
          icon.setTooltipText("Saved tooltip");
          getBoundComponent().add(icon);
        }
      }
      """;

  @TempDir
  Path temporaryDirectory;

  @Test
  @DisplayName("An inline factory's fluent setter is updated and removed without introducing an "
      + "overridden write")
  void writeFactoryChain() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = addComponent(fixture, "inline", 28);
      SourceCodeModifier modifier = fixture.getModifier();
      List<ChangeRequest> changes =
          List.of(fixture.createChange("inline", "HasLabel", "Saved inline icon"));

      assertEquals(List.of(EXPECTED),
          modifier.previewPatches(changes).stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(changes);
      assertEquals(EXPECTED, Files.readString(file));
      modifier.apply(changes);
      assertEquals(EXPECTED, Files.readString(file));
      List<ChangeRequest> reset = List.of(fixture.createChange("inline", "HasLabel", ""));
      assertEquals(List.of(RESET),
          modifier.previewPatches(reset).stream().map(FilePatch::getPatched).toList());
      assertEquals(EXPECTED, Files.readString(file));
      modifier.apply(reset);
      assertEquals(RESET, Files.readString(file));
    }
  }

  @Test
  @DisplayName("Two same-type factory results on one line refuse scalar edits without guessing a "
      + "declaration")
  void refuseAmbiguousFactories() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = addComponent(fixture, "ambiguous", 24);
      SourceCodeModifier modifier = fixture.getModifier();
      List<ChangeRequest> changes =
          List.of(fixture.createChange("ambiguous", "HasLabel", "Saved label"));
      final String error =
          "Cannot identify Icon at line 24: multiple matching creations share this line";

      assertEquals(error, modifier.preview(changes).get(0).getError());
      assertEquals(SOURCE, Files.readString(file));
      assertEquals(error, modifier.apply(changes).get(0).getError());
      assertEquals(SOURCE, Files.readString(file));
    }
  }

  @Test
  @DisplayName("An absent setter follows the entire extracted factory chain and remains stable on "
      + "repeat")
  void insertAfterFactoryChain() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = addComponent(fixture, "inline", 28);
      SourceCodeModifier modifier = fixture.getModifier();
      List<ChangeRequest> changes =
          List.of(fixture.createChange("inline", "HasTooltip", "Saved tooltip"));

      assertEquals(List.of(ABSENT_EXPECTED),
          modifier.previewPatches(changes).stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(changes);
      assertEquals(ABSENT_EXPECTED, Files.readString(file));
      modifier.apply(changes);
      assertEquals(ABSENT_EXPECTED, Files.readString(file));
    }
  }

  private Path addComponent(SourceWriteFixture fixture, String id, int line) throws IOException {
    final String owner = "com.devtoolsapplayoutspring.views.IconSourceCasesView";
    Path file = fixture.addSource(owner, SOURCE);
    fixture.addComponent(id, Icon.class,
        List.of(new SourcePoint(owner, "IconSourceCasesView.java", line)));
    return file;
  }
}
