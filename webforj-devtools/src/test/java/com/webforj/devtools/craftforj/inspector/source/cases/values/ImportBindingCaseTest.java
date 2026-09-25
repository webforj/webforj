package com.webforj.devtools.craftforj.inspector.source.cases.values;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.field.DateField;
import com.webforj.component.progressbar.ProgressBar;
import com.webforj.component.slider.Slider;
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

class ImportBindingCaseTest {

  private static final String SOURCE =
      """
          package com.devtoolsapplayoutspring.views;

          import com.webforj.component.Composite;
          import com.webforj.component.button.Button;
          import com.webforj.component.field.DateField;
          import com.webforj.component.layout.flexlayout.FlexDirection;
          import com.webforj.component.layout.flexlayout.FlexLayout;
          import com.webforj.component.progressbar.ProgressBar;
          import com.webforj.component.slider.Slider;
          import com.webforj.component.slider.Slider.Orientation;
          import com.webforj.router.annotation.FrameTitle;
          import com.webforj.router.annotation.Route;

          @Route(value = "/import-binding-cases", outlet = MainLayout.class)
          @FrameTitle("Import binding cases")
          public class ImportBindingCasesView extends Composite<FlexLayout> {
            public ImportBindingCasesView() {
              ProgressBar progress = new ProgressBar(40, "Import progress");
              progress.setOrientation(com.webforj.component.progressbar.ProgressBar.Orientation.HORIZONTAL);
              progress.setHeight("80px");
              Slider slider = new Slider(40);
              slider.setOrientation(Orientation.HORIZONTAL);
              slider.setHeight("80px");
              String LocalDate = "Local date";
              DateField date = new DateField(LocalDate, java.time.LocalDate.of(2026, 3, 10));
              date.setMin(java.time.LocalDate.of(2026, 1, 1));
              Button update = new Button("Change import values");
              update.onClick(event -> {
                progress.setOrientation(com.webforj.component.progressbar.ProgressBar.Orientation.VERTICAL);
                slider.setOrientation(Orientation.VERTICAL);
                date.setMin(java.time.LocalDate.of(2026, 2, 1));
                date.setValue(java.time.LocalDate.of(2026, 2, 10));
              });
              getBoundComponent().setDirection(FlexDirection.COLUMN);
              getBoundComponent().add(progress, slider, date, update);
            }
          }
          """;

  private static final String EXPECTED =
      """
          package com.devtoolsapplayoutspring.views;

          import com.webforj.component.Composite;
          import com.webforj.component.button.Button;
          import com.webforj.component.field.DateField;
          import com.webforj.component.layout.flexlayout.FlexDirection;
          import com.webforj.component.layout.flexlayout.FlexLayout;
          import com.webforj.component.progressbar.ProgressBar;
          import com.webforj.component.slider.Slider;
          import com.webforj.component.slider.Slider.Orientation;
          import com.webforj.router.annotation.FrameTitle;
          import com.webforj.router.annotation.Route;

          @Route(value = "/import-binding-cases", outlet = MainLayout.class)
          @FrameTitle("Import binding cases")
          public class ImportBindingCasesView extends Composite<FlexLayout> {
            public ImportBindingCasesView() {
              ProgressBar progress = new ProgressBar(40, "Import progress");
              progress.setOrientation(com.webforj.component.progressbar.ProgressBar.Orientation.VERTICAL);
              progress.setHeight("80px");
              Slider slider = new Slider(40);
              slider.setOrientation(Orientation.VERTICAL);
              slider.setHeight("80px");
              String LocalDate = "Local date";
              DateField date = new DateField(LocalDate, java.time.LocalDate.of(2026, 3, 10));
              date.setMin(java.time.LocalDate.parse("2026-02-01"));
              Button update = new Button("Change import values");
              update.onClick(event -> {
                progress.setOrientation(com.webforj.component.progressbar.ProgressBar.Orientation.VERTICAL);
                slider.setOrientation(Orientation.VERTICAL);
                date.setMin(java.time.LocalDate.of(2026, 2, 1));
                date.setValue(java.time.LocalDate.of(2026, 2, 10));
              });
              getBoundComponent().setDirection(FlexDirection.COLUMN);
              getBoundComponent().add(progress, slider, date, update);
            }
          }
          """;

  private static final String WILDCARD =
      """
          package com.devtoolsapplayoutspring.views;

          import com.webforj.component.Composite;
          import com.webforj.component.button.Button;
          import com.webforj.component.field.DateField;
          import com.webforj.component.layout.flexlayout.FlexDirection;
          import com.webforj.component.layout.flexlayout.FlexLayout;
          import com.webforj.component.progressbar.ProgressBar;
          import com.webforj.component.slider.Slider;
          import com.webforj.component.slider.Slider.*;
          import com.webforj.router.annotation.FrameTitle;
          import com.webforj.router.annotation.Route;

          @Route(value = "/import-binding-cases", outlet = MainLayout.class)
          @FrameTitle("Import binding cases")
          public class ImportBindingCasesView extends Composite<FlexLayout> {
            public ImportBindingCasesView() {
              ProgressBar progress = new ProgressBar(40, "Import progress");
              progress.setOrientation(com.webforj.component.progressbar.ProgressBar.Orientation.HORIZONTAL);
              progress.setHeight("80px");
              Slider slider = new Slider(40);
              slider.setOrientation(Orientation.HORIZONTAL);
              slider.setHeight("80px");
              String LocalDate = "Local date";
              DateField date = new DateField(LocalDate, java.time.LocalDate.of(2026, 3, 10));
              date.setMin(java.time.LocalDate.of(2026, 1, 1));
              Button update = new Button("Change import values");
              update.onClick(event -> {
                progress.setOrientation(com.webforj.component.progressbar.ProgressBar.Orientation.VERTICAL);
                slider.setOrientation(Orientation.VERTICAL);
                date.setMin(java.time.LocalDate.of(2026, 2, 1));
                date.setValue(java.time.LocalDate.of(2026, 2, 10));
              });
              getBoundComponent().setDirection(FlexDirection.COLUMN);
              getBoundComponent().add(progress, slider, date, update);
            }
          }
          """;

  private static final String WILDCARD_EXPECTED =
      """
          package com.devtoolsapplayoutspring.views;

          import com.webforj.component.Composite;
          import com.webforj.component.button.Button;
          import com.webforj.component.field.DateField;
          import com.webforj.component.layout.flexlayout.FlexDirection;
          import com.webforj.component.layout.flexlayout.FlexLayout;
          import com.webforj.component.progressbar.ProgressBar;
          import com.webforj.component.slider.Slider;
          import com.webforj.component.slider.Slider.*;
          import com.webforj.router.annotation.FrameTitle;
          import com.webforj.router.annotation.Route;

          @Route(value = "/import-binding-cases", outlet = MainLayout.class)
          @FrameTitle("Import binding cases")
          public class ImportBindingCasesView extends Composite<FlexLayout> {
            public ImportBindingCasesView() {
              ProgressBar progress = new ProgressBar(40, "Import progress");
              progress.setOrientation(com.webforj.component.progressbar.ProgressBar.Orientation.VERTICAL);
              progress.setHeight("80px");
              Slider slider = new Slider(40);
              slider.setOrientation(com.webforj.component.slider.Slider.Orientation.VERTICAL);
              slider.setHeight("80px");
              String LocalDate = "Local date";
              DateField date = new DateField(LocalDate, java.time.LocalDate.of(2026, 3, 10));
              date.setMin(java.time.LocalDate.parse("2026-02-01"));
              Button update = new Button("Change import values");
              update.onClick(event -> {
                progress.setOrientation(com.webforj.component.progressbar.ProgressBar.Orientation.VERTICAL);
                slider.setOrientation(Orientation.VERTICAL);
                date.setMin(java.time.LocalDate.of(2026, 2, 1));
                date.setValue(java.time.LocalDate.of(2026, 2, 10));
              });
              getBoundComponent().setDirection(FlexDirection.COLUMN);
              getBoundComponent().add(progress, slider, date, update);
            }
          }
          """;

  private static final String CLEAN =
      """
          package com.devtoolsapplayoutspring.views;

          import com.webforj.component.Composite;
          import com.webforj.component.button.Button;
          import com.webforj.component.field.DateField;
          import com.webforj.component.layout.flexlayout.FlexDirection;
          import com.webforj.component.layout.flexlayout.FlexLayout;
          import com.webforj.component.progressbar.ProgressBar;
          import com.webforj.component.slider.Slider;
          import com.webforj.router.annotation.FrameTitle;
          import com.webforj.router.annotation.Route;

          @Route(value = "/import-binding-cases", outlet = MainLayout.class)
          @FrameTitle("Import binding cases")
          public class ImportBindingCasesView extends Composite<FlexLayout> {
            public ImportBindingCasesView() {
              ProgressBar progress = new ProgressBar(40, "Import progress");
              progress.setOrientation(com.webforj.component.progressbar.ProgressBar.Orientation.HORIZONTAL);
              progress.setHeight("80px");
              Slider slider = new Slider(40);
              slider.setOrientation(com.webforj.component.slider.Slider.Orientation.HORIZONTAL);
              slider.setHeight("80px");
              String dateLabel = "Local date";
              DateField date = new DateField(dateLabel, java.time.LocalDate.of(2026, 3, 10));
              date.setMin(java.time.LocalDate.of(2026, 1, 1));
              Button update = new Button("Change import values");
              update.onClick(event -> {
                progress.setOrientation(com.webforj.component.progressbar.ProgressBar.Orientation.VERTICAL);
                slider.setOrientation(com.webforj.component.slider.Slider.Orientation.VERTICAL);
                date.setMin(java.time.LocalDate.of(2026, 2, 1));
                date.setValue(java.time.LocalDate.of(2026, 2, 10));
              });
              getBoundComponent().setDirection(FlexDirection.COLUMN);
              getBoundComponent().add(progress, slider, date, update);
            }
          }
          """;

  private static final String CLEAN_EXPECTED =
      """
          package com.devtoolsapplayoutspring.views;

          import com.webforj.component.Composite;
          import com.webforj.component.button.Button;
          import com.webforj.component.field.DateField;
          import com.webforj.component.layout.flexlayout.FlexDirection;
          import com.webforj.component.layout.flexlayout.FlexLayout;
          import com.webforj.component.progressbar.ProgressBar.Orientation;
          import com.webforj.component.progressbar.ProgressBar;
          import com.webforj.component.slider.Slider;
          import com.webforj.router.annotation.FrameTitle;
          import com.webforj.router.annotation.Route;
          import java.time.LocalDate;

          @Route(value = "/import-binding-cases", outlet = MainLayout.class)
          @FrameTitle("Import binding cases")
          public class ImportBindingCasesView extends Composite<FlexLayout> {
            public ImportBindingCasesView() {
              ProgressBar progress = new ProgressBar(40, "Import progress");
              progress.setOrientation(Orientation.VERTICAL);
              progress.setHeight("80px");
              Slider slider = new Slider(40);
              slider.setOrientation(com.webforj.component.slider.Slider.Orientation.VERTICAL);
              slider.setHeight("80px");
              String dateLabel = "Local date";
              DateField date = new DateField(dateLabel, java.time.LocalDate.of(2026, 3, 10));
              date.setMin(LocalDate.parse("2026-02-01"));
              Button update = new Button("Change import values");
              update.onClick(event -> {
                progress.setOrientation(com.webforj.component.progressbar.ProgressBar.Orientation.VERTICAL);
                slider.setOrientation(com.webforj.component.slider.Slider.Orientation.VERTICAL);
                date.setMin(java.time.LocalDate.of(2026, 2, 1));
                date.setValue(java.time.LocalDate.of(2026, 2, 10));
              });
              getBoundComponent().setDirection(FlexDirection.COLUMN);
              getBoundComponent().add(progress, slider, date, update);
            }
          }
          """;

  @TempDir
  Path temporaryDirectory;

  @ParameterizedTest
  @MethodSource("getBindingCases")
  @DisplayName("Enum and temporal writes preserve explicit, wildcard and newly introduced type "
      + "bindings")
  void bindGeneratedTypes(String source, String expected, int beforeOffset, int afterOffset)
      throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file =
          fixture.addSource("com.devtoolsapplayoutspring.views.ImportBindingCasesView", source);
      addComponents(fixture, beforeOffset);
      SourceCodeModifier modifier = fixture.getModifier();
      List<ChangeRequest> edits = createChanges(fixture);

      assertEquals(List.of(expected),
          modifier.previewPatches(edits).stream().map(FilePatch::getPatched).toList());
      assertEquals(source, Files.readString(file));
      modifier.apply(edits);
      assertEquals(expected, Files.readString(file));
      addComponents(fixture, afterOffset);
      modifier.apply(createChanges(fixture));
      assertEquals(expected, Files.readString(file));
    }
  }

  private void addComponents(SourceWriteFixture fixture, int offset) {
    final String owner = "com.devtoolsapplayoutspring.views.ImportBindingCasesView";
    fixture.addComponent("progress", ProgressBar.class,
        List.of(new SourcePoint(owner, "ImportBindingCasesView.java", 18 + offset)));
    fixture.addComponent("slider", Slider.class,
        List.of(new SourcePoint(owner, "ImportBindingCasesView.java", 21 + offset)));
    fixture.addComponent("date", DateField.class,
        List.of(new SourcePoint(owner, "ImportBindingCasesView.java", 25 + offset)));
  }

  private List<ChangeRequest> createChanges(SourceWriteFixture fixture) {
    return List.of(
        fixture.createChange("progress", "ProgressBarOrientation",
            ProgressBar.Orientation.class.getName() + ".VERTICAL"),
        fixture.createChange("slider", "SliderOrientation",
            Slider.Orientation.class.getName() + ".VERTICAL"),
        fixture.createChange("date", "HasMin", "2026-02-01"));
  }

  private static Stream<Arguments> getBindingCases() {
    return Stream.of(Arguments.of(SOURCE, EXPECTED, 0, 0),
        Arguments.of(WILDCARD, WILDCARD_EXPECTED, 0, 0),
        Arguments.of(CLEAN, CLEAN_EXPECTED, -1, 1));
  }
}
