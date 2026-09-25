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

class QualifiedReceiverCaseTest {

  private static final String SOURCE = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.Theme;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.component.toast.Toast;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/qualified-receiver-cases", outlet = MainLayout.class)
      @FrameTitle("Source cases")
      public class QualifiedReceiverCasesView extends Composite<FlexLayout> {
        private final Button action = new Button("Outer action");

        public QualifiedReceiverCasesView() {
          action.addClassName("outer-action");
          getBoundComponent().add(action, new NestedCard(action));
          action.onClick(event -> Toast.show("Outer action clicked", 3000, Theme.INFO,
              Toast.Placement.BOTTOM_RIGHT));
        }

        class NestedCard extends Composite<FlexLayout> {
          private final Button action = new Button("Nested action");

          NestedCard(Button action) {
            QualifiedReceiverCasesView.this.action.setTooltipText("Outer tooltip");
            action.setTooltipText("Parameter tooltip");
            this.action.addClassName("nested-action");
            getBoundComponent().add(this.action);
            this.action.onClick(event -> Toast.show("Nested action clicked", 3000, Theme.INFO,
                Toast.Placement.BOTTOM_RIGHT));
            this.action.setTooltipText("Nested tooltip");
          }
        }
      }
      """;

  private static final String EXPECTED = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.Theme;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.component.toast.Toast;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/qualified-receiver-cases", outlet = MainLayout.class)
      @FrameTitle("Source cases")
      public class QualifiedReceiverCasesView extends Composite<FlexLayout> {
        private final Button action = new Button("Outer action");

        public QualifiedReceiverCasesView() {
          action.addClassName("outer-action");
          getBoundComponent().add(action, new NestedCard(action));
          action.onClick(event -> Toast.show("Outer action clicked", 3000, Theme.INFO,
              Toast.Placement.BOTTOM_RIGHT));
        }

        class NestedCard extends Composite<FlexLayout> {
          private final Button action = new Button("Nested action");

          NestedCard(Button action) {
            QualifiedReceiverCasesView.this.action.setTooltipText("Outer tooltip");
            action.setTooltipText("Parameter tooltip");
            this.action.addClassName("nested-action");
            getBoundComponent().add(this.action);
            this.action.onClick(event -> Toast.show("Nested action clicked", 3000, Theme.INFO,
                Toast.Placement.BOTTOM_RIGHT));
            this.action.setTooltipText("Saved nested tooltip");
          }
        }
      }
      """;

  private static final String RESET = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.Theme;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.component.toast.Toast;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/qualified-receiver-cases", outlet = MainLayout.class)
      @FrameTitle("Source cases")
      public class QualifiedReceiverCasesView extends Composite<FlexLayout> {
        private final Button action = new Button("Outer action");

        public QualifiedReceiverCasesView() {
          action.addClassName("outer-action");
          getBoundComponent().add(action, new NestedCard(action));
          action.onClick(event -> Toast.show("Outer action clicked", 3000, Theme.INFO,
              Toast.Placement.BOTTOM_RIGHT));
        }

        class NestedCard extends Composite<FlexLayout> {
          private final Button action = new Button("Nested action");

          NestedCard(Button action) {
            QualifiedReceiverCasesView.this.action.setTooltipText("Outer tooltip");
            action.setTooltipText("Parameter tooltip");
            this.action.addClassName("nested-action");
            getBoundComponent().add(this.action);
            this.action.onClick(event -> Toast.show("Nested action clicked", 3000, Theme.INFO,
                Toast.Placement.BOTTOM_RIGHT));
          }
        }
      }
      """;

  @TempDir
  Path temporaryDirectory;

  @Test
  @DisplayName("Inner field update and reset preserve constructor parameters and Outer.this "
      + "references")
  void writeOnlyInnerField() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      final String owner = "com.devtoolsapplayoutspring.views.QualifiedReceiverCasesView";
      Path file = fixture.addSource(owner, SOURCE);
      fixture.addSourceClass(owner + "$NestedCard", file);
      fixture.addComponent("nested", Button.class,
          List.of(new SourcePoint(owner + "$NestedCard", "QualifiedReceiverCasesView.java", 24)));
      SourceCodeModifier modifier = fixture.getModifier();
      List<ChangeRequest> edit =
          List.of(fixture.createChange("nested", "HasTooltip", "Saved nested tooltip"));

      assertEquals(List.of(EXPECTED),
          modifier.previewPatches(edit).stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(edit);
      assertEquals(EXPECTED, Files.readString(file));
      List<ChangeRequest> reset = List.of(fixture.createChange("nested", "HasTooltip", ""));
      assertEquals(List.of(RESET),
          modifier.previewPatches(reset).stream().map(FilePatch::getPatched).toList());
      assertEquals(EXPECTED, Files.readString(file));
      modifier.apply(reset);
      assertEquals(RESET, Files.readString(file));
      modifier.apply(reset);
      assertEquals(RESET, Files.readString(file));
    }
  }
}
