package com.webforj.devtools.craftforj.inspector.source.cases.values;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.field.DateField;
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

class PackageRootCollisionCaseTest {

  private static final String SOURCE =
      """
          package com.devtoolsapplayoutspring.views;

          import static java.time.LocalDate.of;
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
              String java = "Application value";
              DateField date = new DateField(LocalDate, of(2026, 3, 10));
              date.setMin(of(2026, 1, 1));
              Button update = new Button("Change import values");
              update.onClick(event -> {
                progress.setOrientation(com.webforj.component.progressbar.ProgressBar.Orientation.VERTICAL);
                slider.setOrientation(Orientation.VERTICAL);
                date.setMin(of(2026, 2, 1));
                date.setValue(of(2026, 2, 10));
              });
              getBoundComponent().setDirection(FlexDirection.COLUMN);
              getBoundComponent().add(progress, slider, date, update);
            }
          }
          """;

  private static final String RESET =
      """
          package com.devtoolsapplayoutspring.views;

          import static java.time.LocalDate.of;
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
              String java = "Application value";
              DateField date = new DateField(LocalDate, of(2026, 3, 10));
              Button update = new Button("Change import values");
              update.onClick(event -> {
                progress.setOrientation(com.webforj.component.progressbar.ProgressBar.Orientation.VERTICAL);
                slider.setOrientation(Orientation.VERTICAL);
                date.setMin(of(2026, 2, 1));
                date.setValue(of(2026, 2, 10));
              });
              getBoundComponent().setDirection(FlexDirection.COLUMN);
              getBoundComponent().add(progress, slider, date, update);
            }
          }
          """;

  @TempDir
  Path temporaryDirectory;

  @Test
  @DisplayName("A shadowed package root refuses a generated static call while still allowing a "
      + "setter reset")
  void refuseUnresolvableTypeExpression() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      String owner = "com.devtoolsapplayoutspring.views.ImportBindingCasesView";
      Path file = fixture.addSource(owner, SOURCE);
      fixture.addComponent("date", DateField.class,
          List.of(new SourcePoint(owner, "ImportBindingCasesView.java", 27)));
      List<ChangeRequest> edits = List.of(fixture.createChange("date", "HasMin", "2026-02-01"));
      SourceCodeModifier modifier = fixture.getModifier();
      final String error =
          "Cannot reference 'java.time.LocalDate': package name 'java' is shadowed "
              + "by an application declaration";
      assertEquals(error, modifier.preview(edits).get(0).getError());
      assertEquals(SOURCE, Files.readString(file));
      assertEquals(error, modifier.apply(edits).get(0).getError());
      assertEquals(SOURCE, Files.readString(file));
      List<ChangeRequest> reset = List.of(fixture.createChange("date", "HasMin", ""));
      assertEquals(List.of(RESET),
          modifier.previewPatches(reset).stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(reset);
      assertEquals(RESET, Files.readString(file));
    }
  }
}
