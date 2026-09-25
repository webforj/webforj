package com.webforj.devtools.craftforj.inspector.source.cases.values;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.Component;
import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.button.Button;
import com.webforj.component.icons.Icon;
import com.webforj.devtools.craftforj.inspector.source.SourceCodeModifier;
import com.webforj.devtools.craftforj.inspector.source.cases.support.SourceWriteFixture;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
import com.webforj.devtools.craftforj.source.model.FilePatch;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Stream;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class ExtractedTypeCaseTest {

  private static final String CONSTRUCTOR =
      """
          package com.devtoolsapplayoutspring.views;

          import com.webforj.component.Composite;
          import com.webforj.component.button.Button;
          import com.webforj.component.layout.flexlayout.FlexLayout;
          import com.webforj.router.annotation.FrameTitle;
          import com.webforj.router.annotation.Route;

          @Route(value = "/creation-cases", outlet = MainLayout.class)
          @FrameTitle("Creation cases")
          public class CreationCasesView extends Composite<FlexLayout> {
            private final com.webforj.component.button.Button initialized = new com.webforj.component.button.Button("Initialized action");

            {
              initialized.setTooltipText("Initializer tooltip");
            }

            public <Button> CreationCasesView() {
              getBoundComponent().add(initialized);
              getBoundComponent().add(new com.webforj.component.button.Button("Inline action"));
              getBoundComponent().add(createAction());
              getBoundComponent().add(new InnerActions());
              class LocalActions extends Composite<FlexLayout> {
                private final com.webforj.component.button.Button action = new com.webforj.component.button.Button("Local class action");

                LocalActions() {
                  action.setTooltipText("Local class tooltip");
                  getBoundComponent().add(action);
                }
              }
              getBoundComponent().add(new LocalActions());
            }

            private com.webforj.component.button.Button createAction() {
              return new com.webforj.component.button.Button("Returned action");
            }

            class InnerActions extends Composite<FlexLayout> {
              private final com.webforj.component.button.Button action = new com.webforj.component.button.Button("Inner class action");

              InnerActions() {
                action.setTooltipText("Inner class tooltip");
                getBoundComponent().add(action);
              }
            }
          }
          """;

  private static final String CONSTRUCTOR_EXPECTED =
      """
          package com.devtoolsapplayoutspring.views;

          import com.webforj.component.Composite;
          import com.webforj.component.button.Button;
          import com.webforj.component.layout.flexlayout.FlexLayout;
          import com.webforj.router.annotation.FrameTitle;
          import com.webforj.router.annotation.Route;

          @Route(value = "/creation-cases", outlet = MainLayout.class)
          @FrameTitle("Creation cases")
          public class CreationCasesView extends Composite<FlexLayout> {
            private final com.webforj.component.button.Button initialized = new com.webforj.component.button.Button("Initialized action");

            {
              initialized.setTooltipText("Initializer tooltip");
            }

            public <Button> CreationCasesView() {
              getBoundComponent().add(initialized);
              com.webforj.component.button.Button button = new com.webforj.component.button.Button("Inline action");
              button.setTooltipText("Saved tooltip");
              getBoundComponent().add(button);
              getBoundComponent().add(createAction());
              getBoundComponent().add(new InnerActions());
              class LocalActions extends Composite<FlexLayout> {
                private final com.webforj.component.button.Button action = new com.webforj.component.button.Button("Local class action");

                LocalActions() {
                  action.setTooltipText("Local class tooltip");
                  getBoundComponent().add(action);
                }
              }
              getBoundComponent().add(new LocalActions());
            }

            private com.webforj.component.button.Button createAction() {
              return new com.webforj.component.button.Button("Returned action");
            }

            class InnerActions extends Composite<FlexLayout> {
              private final com.webforj.component.button.Button action = new com.webforj.component.button.Button("Inner class action");

              InnerActions() {
                action.setTooltipText("Inner class tooltip");
                getBoundComponent().add(action);
              }
            }
          }
          """;

  private static final String FACTORY =
      """
          package com.devtoolsapplayoutspring.views;

          import com.webforj.component.Composite;
          import com.webforj.component.icons.FeatherIcon;
          import com.webforj.component.icons.Icon;
          import com.webforj.component.icons.IconButton;
          import com.webforj.component.layout.flexlayout.FlexLayout;
          import com.webforj.router.annotation.FrameTitle;
          import com.webforj.router.annotation.Route;

          @Route(value = "/icon-source-cases", outlet = MainLayout.class)
          @FrameTitle("com.webforj.component.icons.Icon source cases")
          public class IconSourceCasesView extends Composite<FlexLayout> {
            public <Icon> IconSourceCasesView() {
              com.webforj.component.icons.Icon direct = new com.webforj.component.icons.Icon("bell", "feather");
              direct.setLabel("Direct icon");
              IconButton wrapped = new IconButton(FeatherIcon.BELL.create());
              wrapped.setLabel("Wrapped icon");
              wrapped.setEnabled(true);
              wrapped.onClick(event -> wrapped.setTooltipText("Clicked icon"));
              String name = "bell";
              com.webforj.component.icons.Icon computed = new com.webforj.component.icons.Icon(name, "feather");
              computed.setLabel("Computed icon");
              com.webforj.component.icons.Icon first = FeatherIcon.BELL.create(), second = FeatherIcon.HOME.create();
              first.setLabel("First shared line");
              second.setLabel("Second shared line");
              getBoundComponent().add(direct, wrapped, computed, first, second);
              getBoundComponent().add(FeatherIcon.BELL.create().setLabel("Inline icon"));
            }
          }
          """;

  private static final String FACTORY_EXPECTED =
      """
          package com.devtoolsapplayoutspring.views;

          import com.webforj.component.Composite;
          import com.webforj.component.icons.FeatherIcon;
          import com.webforj.component.icons.Icon;
          import com.webforj.component.icons.IconButton;
          import com.webforj.component.layout.flexlayout.FlexLayout;
          import com.webforj.router.annotation.FrameTitle;
          import com.webforj.router.annotation.Route;

          @Route(value = "/icon-source-cases", outlet = MainLayout.class)
          @FrameTitle("com.webforj.component.icons.Icon source cases")
          public class IconSourceCasesView extends Composite<FlexLayout> {
            public <Icon> IconSourceCasesView() {
              com.webforj.component.icons.Icon direct = new com.webforj.component.icons.Icon("bell", "feather");
              direct.setLabel("Direct icon");
              IconButton wrapped = new IconButton(FeatherIcon.BELL.create());
              wrapped.setLabel("Wrapped icon");
              wrapped.setEnabled(true);
              wrapped.onClick(event -> wrapped.setTooltipText("Clicked icon"));
              String name = "bell";
              com.webforj.component.icons.Icon computed = new com.webforj.component.icons.Icon(name, "feather");
              computed.setLabel("Computed icon");
              com.webforj.component.icons.Icon first = FeatherIcon.BELL.create(), second = FeatherIcon.HOME.create();
              first.setLabel("First shared line");
              second.setLabel("Second shared line");
              getBoundComponent().add(direct, wrapped, computed, first, second);
              com.webforj.component.icons.Icon icon = FeatherIcon.BELL.create().setLabel("Inline icon");
              icon.setTooltipText("Saved tooltip");
              getBoundComponent().add(icon);
            }
          }
          """;

  @TempDir
  Path temporaryDirectory;

  @ParameterizedTest
  @MethodSource("getCreationSources")
  @DisplayName("Constructor and factory extraction retain the component type despite a same-named "
      + "type parameter")
  void writeQualifiedDeclaration(String owner, Class<? extends Component> type, int line,
      String source, String expected) throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      final String className = "com.devtoolsapplayoutspring.views." + owner;
      Path file = fixture.addSource(className, source);
      fixture.addComponent("selected", type,
          List.of(new SourcePoint(className, owner + ".java", line)));
      List<ChangeRequest> changes =
          List.of(fixture.createChange("selected", "HasTooltip", "Saved tooltip"));
      SourceCodeModifier modifier = fixture.getModifier();

      assertEquals(List.of(expected),
          modifier.previewPatches(changes).stream().map(FilePatch::getPatched).toList());
      assertEquals(source, Files.readString(file));
      modifier.apply(changes);
      assertEquals(expected, Files.readString(file));
      modifier.apply(changes);
      assertEquals(expected, Files.readString(file));
    }
  }

  private static Stream<Arguments> getCreationSources() {
    return Stream.of(
        Arguments.of("CreationCasesView", Button.class, 20, CONSTRUCTOR, CONSTRUCTOR_EXPECTED),
        Arguments.of("IconSourceCasesView", Icon.class, 28, FACTORY, FACTORY_EXPECTED));
  }
}
