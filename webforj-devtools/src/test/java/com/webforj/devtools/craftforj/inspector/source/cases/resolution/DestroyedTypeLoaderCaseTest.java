package com.webforj.devtools.craftforj.inspector.source.cases.resolution;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.badge.Badge;
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

class DestroyedTypeLoaderCaseTest {

  private static final String OWNER = "com.devtoolsapplayoutspring.views.LoaderCasesView";

  private static final String SOURCE = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.badge.Badge;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/loader-cases", outlet = MainLayout.class)
      @FrameTitle("Loader cases")
      public class LoaderCasesView extends Composite<FlexLayout> {
        public LoaderCasesView() {
          Badge badge = new Badge("Initial label");
          badge.setLabel("Earlier label");
          getBoundComponent().add(badge);
        }
      }
      """;

  private static final String EXPECTED = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.badge.Badge;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/loader-cases", outlet = MainLayout.class)
      @FrameTitle("Loader cases")
      public class LoaderCasesView extends Composite<FlexLayout> {
        public LoaderCasesView() {
          Badge badge = new Badge("Initial label");
          badge.setText("Saved text");
          getBoundComponent().add(badge);
        }
      }
      """;

  @TempDir
  Path temporaryDirectory;

  @Test
  @DisplayName("A destroyed badge text edit resolves its aliases when only the module loader knows "
      + "the class")
  void writeDestroyedTextWithoutContextLoader() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = fixture.addSource(OWNER, SOURCE);
      fixture.addComponent("badge", Badge.class,
          List.of(new SourcePoint(OWNER, "LoaderCasesView.java", 13)));
      ChangeRequest change = fixture.createChange("badge", "HasText", "Saved text");
      change.setSource(
          new SourceLocation(file.toString(), 13, OWNER, "badge", Badge.class.getName()));
      fixture.removeComponent("badge");
      SourceCodeModifier modifier = fixture.getModifier();

      Thread thread = Thread.currentThread();
      ClassLoader previous = thread.getContextClassLoader();
      thread.setContextClassLoader(new ClassLoader(null) {});
      try {
        assertEquals(List.of(EXPECTED),
            modifier.previewPatches(List.of(change)).stream().map(FilePatch::getPatched).toList());
        assertEquals(SOURCE, Files.readString(file));
        modifier.apply(List.of(change));
        assertEquals(EXPECTED, Files.readString(file));
      } finally {
        thread.setContextClassLoader(previous);
      }
    }
  }
}
