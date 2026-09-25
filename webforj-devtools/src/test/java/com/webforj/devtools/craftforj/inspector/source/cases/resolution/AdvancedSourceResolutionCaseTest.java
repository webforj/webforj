package com.webforj.devtools.craftforj.inspector.source.cases.resolution;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mockStatic;

import com.devtoolsapplayoutspring.views.AdvancedSourceResolutionView;
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

class AdvancedSourceResolutionCaseTest {

  private static final String SOURCE = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.flexlayout.FlexLayout;

      /** Source discovery through local, anonymous and inherited component owners. */
      public class AdvancedSourceResolutionView extends InheritedActions {

        /** Creates the three component groups. */
        public AdvancedSourceResolutionView() {
          class LocalActions extends Composite<FlexLayout> {
            private final Button action = new Button("Local resolved action");

            LocalActions() {
              action.setTooltipText("Local tooltip");
              getBoundComponent().add(action);
            }
          }
          Composite<FlexLayout> anonymous = new Composite<>() {
            private final Button action = new Button("Anonymous resolved action");

            {
              action.setTooltipText("Anonymous tooltip");
              getBoundComponent().add(action);
            }
          };
          getBoundComponent().add(new LocalActions(), anonymous);
        }
      }

      /** A non-public superclass stored in its public subclass's source file. */
      class InheritedActions extends Composite<FlexLayout> {
        private final Button action = new Button("Inherited resolved action");

        InheritedActions() {
          action.setTooltipText("Inherited tooltip");
          getBoundComponent().add(action);
        }
      }
      """;

  private static final String EXPECTED = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.flexlayout.FlexLayout;

      /** Source discovery through local, anonymous and inherited component owners. */
      public class AdvancedSourceResolutionView extends InheritedActions {

        /** Creates the three component groups. */
        public AdvancedSourceResolutionView() {
          class LocalActions extends Composite<FlexLayout> {
            private final Button action = new Button("Local resolved action");

            LocalActions() {
              action.setTooltipText("Saved local");
              getBoundComponent().add(action);
            }
          }
          Composite<FlexLayout> anonymous = new Composite<>() {
            private final Button action = new Button("Anonymous resolved action");

            {
              action.setTooltipText("Saved anonymous");
              getBoundComponent().add(action);
            }
          };
          getBoundComponent().add(new LocalActions(), anonymous);
        }
      }

      /** A non-public superclass stored in its public subclass's source file. */
      class InheritedActions extends Composite<FlexLayout> {
        private final Button action = new Button("Inherited resolved action");

        InheritedActions() {
          action.setTooltipText("Saved inherited");
          getBoundComponent().add(action);
        }
      }
      """;

  @TempDir
  Path temporaryDirectory;

  @Test
  @DisplayName("Real local, anonymous and non-public superclass owners resolve and edit their own "
      + "fields")
  void resolveAllClassForms() throws IOException {
    try (MockedStatic<ProjectRootResolver> project = mockStatic(ProjectRootResolver.class);
        SourceWriteFixture fixture =
            new SourceWriteFixture(temporaryDirectory.resolve("src/main/java"))) {
      project.when(() -> ProjectRootResolver.resolve(any(), any())).thenReturn(temporaryDirectory);
      fixture.useRealSourceResolution();
      final Path file = fixture.addSource(AdvancedSourceResolutionView.class.getName(), SOURCE);
      fixture.addComponent("local", Button.class,
          List.of(new SourcePoint(AdvancedSourceResolutionView.class.getName() + "$1LocalActions",
              "AdvancedSourceResolutionView.java", 13)));
      fixture.addComponent("anonymous", Button.class,
          List.of(new SourcePoint(AdvancedSourceResolutionView.class.getName() + "$1",
              "AdvancedSourceResolutionView.java", 21)));
      fixture.addComponent("inherited", Button.class,
          List.of(new SourcePoint("com.devtoolsapplayoutspring.views.InheritedActions",
              "AdvancedSourceResolutionView.java", 34)));
      List<ChangeRequest> edits =
          List.of(fixture.createChange("local", "HasTooltip", "Saved local"),
              fixture.createChange("anonymous", "HasTooltip", "Saved anonymous"),
              fixture.createChange("inherited", "HasTooltip", "Saved inherited"));
      SourceCodeModifier modifier = fixture.getModifier();

      assertEquals(List.of(EXPECTED),
          modifier.previewPatches(edits).stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(edits);
      assertEquals(EXPECTED, Files.readString(file));
    }
  }
}
