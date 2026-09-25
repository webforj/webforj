package com.webforj.devtools.craftforj.inspector.source.cases.ownership;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.button.Button;
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

class HelperReceiverTypeCaseTest {

  private static final String SOURCE = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;
      import org.slf4j.Logger;
      import org.slf4j.LoggerFactory;

      @Route(value = "/receiver-type-cases", outlet = MainLayout.class)
      @FrameTitle("Receiver type cases")
      public class ReceiverTypeCasesView extends Composite<FlexLayout> {
        private static final Logger LOGGER = LoggerFactory.getLogger(ReceiverTypeCasesView.class);

        public ReceiverTypeCasesView() {
          Button configured = new Button("Logged action");
          configured.setTooltipText("Before logging");
          LOGGER.debug("Created {}", configured);
          var layout = new FlexLayout();
          layout.add(configured);
          this.getBoundComponent().add(layout);
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
      import org.slf4j.Logger;
      import org.slf4j.LoggerFactory;

      @Route(value = "/receiver-type-cases", outlet = MainLayout.class)
      @FrameTitle("Receiver type cases")
      public class ReceiverTypeCasesView extends Composite<FlexLayout> {
        private static final Logger LOGGER = LoggerFactory.getLogger(ReceiverTypeCasesView.class);

        public ReceiverTypeCasesView() {
          Button configured = new Button("Logged action");
          configured.setTooltipText("Saved tooltip");
          LOGGER.debug("Created {}", configured);
          var layout = new FlexLayout();
          layout.add(configured);
          this.getBoundComponent().add(layout);
        }
      }
      """;

  @TempDir
  Path temporaryDirectory;

  @Test
  @DisplayName("Passing the component to a logger, an inferred layout and a qualified bound "
      + "component does not block the edit")
  void writeThroughNonHelperReceivers() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      String owner = "com.devtoolsapplayoutspring.views.ReceiverTypeCasesView";
      Path file = fixture.addSource(owner, SOURCE);
      fixture.addComponent("configured", Button.class,
          List.of(new SourcePoint(owner, "ReceiverTypeCasesView.java", 17)));
      List<ChangeRequest> edits =
          List.of(fixture.createChange("configured", "HasTooltip", "Saved tooltip"));
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
