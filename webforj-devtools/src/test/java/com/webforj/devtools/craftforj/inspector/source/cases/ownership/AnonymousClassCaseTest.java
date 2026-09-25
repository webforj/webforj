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

class AnonymousClassCaseTest {

  @TempDir
  Path temporaryDirectory;

  @Test
  @DisplayName("An anonymous field receives its setter in its instance initializer, preserving the "
      + "outer field")
  void applySetterInAnonymousOwner() throws IOException {
    final String source = """
        package com.devtoolsapplayoutspring.views;

        import com.webforj.component.Composite;
        import com.webforj.component.Theme;
        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;
        import com.webforj.component.toast.Toast;
        import com.webforj.router.annotation.FrameTitle;
        import com.webforj.router.annotation.Route;

        @Route(value = "/anonymous-source-cases", outlet = MainLayout.class)
        @FrameTitle("Anonymous source cases")
        public class AnonymousSourceCasesView extends Composite<FlexLayout> {
          private final Button action = new Button("Outer action");

          public AnonymousSourceCasesView() {
            Composite<FlexLayout> card = new Composite<FlexLayout>() {
              private final Button action = new Button("Anonymous action");

              {
                getBoundComponent().add(action);
                action.onClick(event -> Toast.show("Anonymous action clicked", 3000, Theme.INFO,
                    Toast.Placement.BOTTOM_RIGHT));
              }
            };
            getBoundComponent().add(action, card);
            action.onClick(event -> Toast.show("Outer action clicked", 3000, Theme.INFO,
                Toast.Placement.BOTTOM_RIGHT));
          }
        }
        """;
    final String expected = """
        package com.devtoolsapplayoutspring.views;

        import com.webforj.component.Composite;
        import com.webforj.component.Theme;
        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;
        import com.webforj.component.toast.Toast;
        import com.webforj.router.annotation.FrameTitle;
        import com.webforj.router.annotation.Route;

        @Route(value = "/anonymous-source-cases", outlet = MainLayout.class)
        @FrameTitle("Anonymous source cases")
        public class AnonymousSourceCasesView extends Composite<FlexLayout> {
          private final Button action = new Button("Outer action");

          public AnonymousSourceCasesView() {
            Composite<FlexLayout> card = new Composite<FlexLayout>() {
              private final Button action = new Button("Anonymous action");

              {
                getBoundComponent().add(action);
                action.onClick(event -> Toast.show("Anonymous action clicked", 3000, Theme.INFO,
                    Toast.Placement.BOTTOM_RIGHT));
                action.setTooltipText("Anonymous tooltip");
              }
            };
            getBoundComponent().add(action, card);
            action.onClick(event -> Toast.show("Outer action clicked", 3000, Theme.INFO,
                Toast.Placement.BOTTOM_RIGHT));
          }
        }
        """;
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file =
          fixture.addSource("com.devtoolsapplayoutspring.views.AnonymousSourceCasesView", source);
      fixture.addSourceClass("com.devtoolsapplayoutspring.views.AnonymousSourceCasesView$1", file);
      fixture.addComponent("anonymous-action", Button.class,
          List.of(new SourcePoint("com.devtoolsapplayoutspring.views.AnonymousSourceCasesView$1",
              "AnonymousSourceCasesView.java", 18)));
      SourceCodeModifier modifier = fixture.getModifier();
      ChangeRequest change =
          fixture.createChange("anonymous-action", "HasTooltip", "Anonymous tooltip");

      List<FilePatch> patches = modifier.previewPatches(List.of(change));

      assertEquals(List.of(source), patches.stream().map(FilePatch::getOriginal).toList());
      assertEquals(List.of(expected), patches.stream().map(FilePatch::getPatched).toList());
      assertEquals(source, Files.readString(file));

      modifier.apply(List.of(change));

      assertEquals(expected, Files.readString(file));
    }
  }
}
