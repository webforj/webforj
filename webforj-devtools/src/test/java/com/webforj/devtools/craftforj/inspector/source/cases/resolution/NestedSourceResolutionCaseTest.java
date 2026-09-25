package com.webforj.devtools.craftforj.inspector.source.cases.resolution;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mockStatic;

import com.devtoolsapplayoutspring.views.ResolvedButtonView;
import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.button.Button;
import com.webforj.devtools.craftforj.ProjectRootResolver;
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
import org.mockito.MockedStatic;

class NestedSourceResolutionCaseTest {

  @TempDir
  Path temporaryDirectory;

  @Test
  @DisplayName("A real nested class resolves its enclosing file and writes the nested button")
  void resolveAndWriteNestedSource() throws IOException {
    final String source = """
        package com.devtoolsapplayoutspring.views;

        import com.webforj.component.Composite;
        import com.webforj.component.Theme;
        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;
        import com.webforj.component.toast.Toast;

        /** Buttons whose nested class resolves to this enclosing source file. */
        public class ResolvedButtonView extends Composite<FlexLayout> {

          /** Creates the nested button group. */
          public ResolvedButtonView() {
            getBoundComponent().add(new ButtonGroup());
          }

          /** Holds the action button. */
          static class ButtonGroup extends Composite<FlexLayout> {
            private final Button action = new Button("Resolved action");

            ButtonGroup() {
              action.setTooltipText("Resolved tooltip");
              getBoundComponent().add(action);
              action.onClick(event -> Toast.show("Resolved action clicked", 3000, Theme.INFO,
                  Toast.Placement.BOTTOM_RIGHT));
            }
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

        /** Buttons whose nested class resolves to this enclosing source file. */
        public class ResolvedButtonView extends Composite<FlexLayout> {

          /** Creates the nested button group. */
          public ResolvedButtonView() {
            getBoundComponent().add(new ButtonGroup());
          }

          /** Holds the action button. */
          static class ButtonGroup extends Composite<FlexLayout> {
            private final Button action = new Button("Resolved action");

            ButtonGroup() {
              action.setTooltipText("Written through resolver");
              getBoundComponent().add(action);
              action.onClick(event -> Toast.show("Resolved action clicked", 3000, Theme.INFO,
                  Toast.Placement.BOTTOM_RIGHT));
            }
          }
        }
        """;
    try (MockedStatic<ProjectRootResolver> project = mockStatic(ProjectRootResolver.class);
        SourceWriteFixture fixture =
            new SourceWriteFixture(temporaryDirectory.resolve("src/main/java"))) {
      project.when(() -> ProjectRootResolver.resolve(any(), any())).thenReturn(temporaryDirectory);
      fixture.useRealSourceResolution();
      Path file = fixture.addSource(ResolvedButtonView.class.getName(), source);
      fixture.addComponent("resolved-action", Button.class,
          List.of(new SourcePoint(ResolvedButtonView.class.getName() + "$ButtonGroup",
              "ResolvedButtonView.java", 19)));
      SourceCodeModifier modifier = fixture.getModifier();
      ChangeRequest edit =
          fixture.createChange("resolved-action", "HasTooltip", "Written through resolver");

      List<FilePatch> patches = modifier.previewPatches(List.of(edit));
      assertEquals(List.of(expected), patches.stream().map(FilePatch::getPatched).toList());
      assertEquals(source, Files.readString(file));
      modifier.apply(List.of(edit));
      assertEquals(expected, Files.readString(file));
    }
  }
}
