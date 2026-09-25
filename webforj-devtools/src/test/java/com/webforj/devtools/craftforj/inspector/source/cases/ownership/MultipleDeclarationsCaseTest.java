package com.webforj.devtools.craftforj.inspector.source.cases.ownership;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.Component;
import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.button.Button;
import com.webforj.component.field.TextField;
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

class MultipleDeclarationsCaseTest {

  private static final String SOURCE = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/declaration-cases", outlet = MainLayout.class)
      @FrameTitle("Declaration cases")
      public class DeclarationCasesView extends Composite<FlexLayout> {
        private final Button fieldFirst = new Button("Field first"),
            fieldSecond = new Button("Field second");

        public DeclarationCasesView() {
          Button localFirst = new Button("Local first"),
              localSecond = new Button("Local second");
          fieldFirst.setTooltipText("First field tooltip");
          fieldSecond.setTooltipText("Second field tooltip");
          localFirst.setTooltipText("First local tooltip");
          localSecond.setTooltipText("Second local tooltip");
          getBoundComponent().add(fieldFirst, fieldSecond, localFirst, localSecond);
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

      @Route(value = "/declaration-cases", outlet = MainLayout.class)
      @FrameTitle("Declaration cases")
      public class DeclarationCasesView extends Composite<FlexLayout> {
        private final Button fieldFirst = new Button("Field first"),
            fieldSecond = new Button("Field second");

        public DeclarationCasesView() {
          Button localFirst = new Button("Local first"),
              localSecond = new Button("Local second");
          fieldFirst.setTooltipText("First field tooltip");
          fieldSecond.setTooltipText("Edited field");
          localFirst.setTooltipText("First local tooltip");
          localSecond.setTooltipText("Edited local");
          getBoundComponent().add(fieldFirst, fieldSecond, localFirst, localSecond);
        }
      }
      """;

  private static final String RESET = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/declaration-cases", outlet = MainLayout.class)
      @FrameTitle("Declaration cases")
      public class DeclarationCasesView extends Composite<FlexLayout> {
        private final Button fieldFirst = new Button("Field first"),
            fieldSecond = new Button("Field second");

        public DeclarationCasesView() {
          Button localFirst = new Button("Local first"),
              localSecond = new Button("Local second");
          fieldFirst.setTooltipText("First field tooltip");
          localFirst.setTooltipText("First local tooltip");
          getBoundComponent().add(fieldFirst, fieldSecond, localFirst, localSecond);
        }
      }
      """;

  private static final String MIXED_SOURCE =
      """
          package com.devtoolsapplayoutspring.views;

          import com.webforj.component.Composite;
          import com.webforj.component.DwcComponent;
          import com.webforj.component.button.Button;
          import com.webforj.component.field.TextField;
          import com.webforj.component.layout.flexlayout.FlexLayout;
          import com.webforj.router.annotation.FrameTitle;
          import com.webforj.router.annotation.Route;

          @Route(value = "/declaration-cases", outlet = MainLayout.class)
          @FrameTitle("Declaration cases")
          public class DeclarationCasesView extends Composite<FlexLayout> {
            private final DwcComponent<?> fieldFirst = new TextField("Field first"), fieldSecond = new Button("Field second");

            public DeclarationCasesView() {
              DwcComponent<?> localFirst = new Button("Local first"), localSecond = new TextField("Local second");
              fieldFirst.setTooltipText("First field tooltip");
              fieldSecond.setTooltipText("Second field tooltip");
              localFirst.setTooltipText("First local tooltip");
              localSecond.setTooltipText("Second local tooltip");
              getBoundComponent().add(fieldFirst, fieldSecond, localFirst, localSecond);
            }
          }
          """;

  private static final String MIXED_EXPECTED =
      """
          package com.devtoolsapplayoutspring.views;

          import com.webforj.component.Composite;
          import com.webforj.component.DwcComponent;
          import com.webforj.component.button.Button;
          import com.webforj.component.field.TextField;
          import com.webforj.component.layout.flexlayout.FlexLayout;
          import com.webforj.router.annotation.FrameTitle;
          import com.webforj.router.annotation.Route;

          @Route(value = "/declaration-cases", outlet = MainLayout.class)
          @FrameTitle("Declaration cases")
          public class DeclarationCasesView extends Composite<FlexLayout> {
            private final DwcComponent<?> fieldFirst = new TextField("Field first"), fieldSecond = new Button("Field second");

            public DeclarationCasesView() {
              DwcComponent<?> localFirst = new Button("Local first"), localSecond = new TextField("Local second");
              fieldFirst.setTooltipText("First field tooltip");
              fieldSecond.setTooltipText("Edited field");
              localFirst.setTooltipText("First local tooltip");
              localSecond.setTooltipText("Edited local");
              getBoundComponent().add(fieldFirst, fieldSecond, localFirst, localSecond);
            }
          }
          """;

  private static final String MIXED_RESET =
      """
          package com.devtoolsapplayoutspring.views;

          import com.webforj.component.Composite;
          import com.webforj.component.DwcComponent;
          import com.webforj.component.button.Button;
          import com.webforj.component.field.TextField;
          import com.webforj.component.layout.flexlayout.FlexLayout;
          import com.webforj.router.annotation.FrameTitle;
          import com.webforj.router.annotation.Route;

          @Route(value = "/declaration-cases", outlet = MainLayout.class)
          @FrameTitle("Declaration cases")
          public class DeclarationCasesView extends Composite<FlexLayout> {
            private final DwcComponent<?> fieldFirst = new TextField("Field first"), fieldSecond = new Button("Field second");

            public DeclarationCasesView() {
              DwcComponent<?> localFirst = new Button("Local first"), localSecond = new TextField("Local second");
              fieldFirst.setTooltipText("First field tooltip");
              localFirst.setTooltipText("First local tooltip");
              getBoundComponent().add(fieldFirst, fieldSecond, localFirst, localSecond);
            }
          }
          """;

  @TempDir
  Path temporaryDirectory;

  @ParameterizedTest
  @MethodSource("getDeclarationCases")
  @DisplayName("Writing the second field or local in one declaration preserves the first")
  void writeOnlySelectedDeclarator(String source, String expected, String resetSource,
      Class<? extends Component> localType, int fieldLine, int localLine) throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      final String owner = "com.devtoolsapplayoutspring.views.DeclarationCasesView";
      final Path file = fixture.addSource(owner, source);
      fixture.addComponent("field", Button.class,
          List.of(new SourcePoint(owner, "DeclarationCasesView.java", fieldLine)));
      fixture.addComponent("local", localType,
          List.of(new SourcePoint(owner, "DeclarationCasesView.java", localLine)));
      SourceCodeModifier modifier = fixture.getModifier();
      List<ChangeRequest> edits =
          List.of(fixture.createChange("field", "HasTooltip", "Edited field"),
              fixture.createChange("local", "HasTooltip", "Edited local"));

      assertEquals(List.of(expected),
          modifier.previewPatches(edits).stream().map(FilePatch::getPatched).toList());
      assertEquals(source, Files.readString(file));
      modifier.apply(edits);
      assertEquals(expected, Files.readString(file));
      List<ChangeRequest> reset = List.of(fixture.createChange("field", "HasTooltip", ""),
          fixture.createChange("local", "HasTooltip", ""));
      assertEquals(List.of(resetSource),
          modifier.previewPatches(reset).stream().map(FilePatch::getPatched).toList());
      assertEquals(expected, Files.readString(file));
      modifier.apply(reset);
      assertEquals(resetSource, Files.readString(file));
    }
  }

  private static Stream<Arguments> getDeclarationCases() {
    return Stream.of(Arguments.of(SOURCE, EXPECTED, RESET, Button.class, 13, 17),
        Arguments.of(MIXED_SOURCE, MIXED_EXPECTED, MIXED_RESET, TextField.class, 14, 17));
  }
}
