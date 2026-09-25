package com.webforj.devtools.craftforj.inspector.source.cases.values;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.field.DateField;
import com.webforj.component.field.DateTimeField;
import com.webforj.component.field.TimeField;
import com.webforj.devtools.craftforj.inspector.source.SourceCodeModifier;
import com.webforj.devtools.craftforj.inspector.source.cases.support.SourceWriteFixture;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeResult;
import com.webforj.devtools.craftforj.source.model.FilePatch;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Stream;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class TemporalBoundsCaseTest {

  private static final String SOURCE = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.Theme;
      import com.webforj.component.button.Button;
      import com.webforj.component.field.DateField;
      import com.webforj.component.field.DateTimeField;
      import com.webforj.component.field.TimeField;
      import com.webforj.component.layout.flexlayout.FlexDirection;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.component.toast.Toast;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;
      import java.time.LocalDate;
      import java.time.LocalDateTime;
      import java.time.LocalTime;

      @Route(value = "/temporal-bounds-cases", outlet = MainLayout.class)
      @FrameTitle("Temporal bounds cases")
      public class TemporalBoundsCasesView extends Composite<FlexLayout> {
        public TemporalBoundsCasesView() {
          DateField day = new DateField("Day", LocalDate.parse("2026-09-20"));
          day.setMin(LocalDate.parse("2026-09-01"));
          TimeField time = new TimeField("Time", LocalTime.parse("12:00"));
          time.setMax(LocalTime.parse("18:00"));
          DateTimeField appointment =
              new DateTimeField("Appointment", LocalDateTime.parse("2026-09-20T12:00"));
          appointment.setMin(LocalDateTime.parse("2026-09-01T09:00"));
          Button show = new Button("Show values");
          show.onClick(event -> Toast.show(day.getValue() + " / " + time.getValue() + " / "
              + appointment.getValue(), 3000, Theme.INFO, Toast.Placement.BOTTOM_RIGHT));
          getBoundComponent().setDirection(FlexDirection.COLUMN);
          getBoundComponent().add(day, time, appointment, show);
        }
      }
      """;
  private static final String EXPECTED = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.Theme;
      import com.webforj.component.button.Button;
      import com.webforj.component.field.DateField;
      import com.webforj.component.field.DateTimeField;
      import com.webforj.component.field.TimeField;
      import com.webforj.component.layout.flexlayout.FlexDirection;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.component.toast.Toast;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;
      import java.time.LocalDate;
      import java.time.LocalDateTime;
      import java.time.LocalTime;

      @Route(value = "/temporal-bounds-cases", outlet = MainLayout.class)
      @FrameTitle("Temporal bounds cases")
      public class TemporalBoundsCasesView extends Composite<FlexLayout> {
        public TemporalBoundsCasesView() {
          DateField day = new DateField("Day", LocalDate.parse("2026-09-20"));
          day.setMin(LocalDate.parse("2026-09-10"));
          TimeField time = new TimeField("Time", LocalTime.parse("12:00"));
          time.setMax(LocalTime.parse("17:30"));
          DateTimeField appointment =
              new DateTimeField("Appointment", LocalDateTime.parse("2026-09-20T12:00"));
          appointment.setMin(LocalDateTime.parse("2026-09-10T09:30"));
          Button show = new Button("Show values");
          show.onClick(event -> Toast.show(day.getValue() + " / " + time.getValue() + " / "
              + appointment.getValue(), 3000, Theme.INFO, Toast.Placement.BOTTOM_RIGHT));
          getBoundComponent().setDirection(FlexDirection.COLUMN);
          getBoundComponent().add(day, time, appointment, show);
        }
      }
      """;

  @TempDir
  Path temporaryDirectory;

  @Test
  @DisplayName("Date, time and date-time bounds survive JSON and write typed parse expressions")
  void applyTemporalBounds() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file =
          fixture.addSource("com.devtoolsapplayoutspring.views.TemporalBoundsCasesView", SOURCE);
      addFields(fixture);
      SourceCodeModifier modifier = fixture.getModifier();
      List<ChangeRequest> changes = List.of(fixture.createChange("day", "HasMin", "2026-09-10"),
          fixture.createChange("time", "HasMax", "17:30"),
          fixture.createChange("appointment", "HasMin", "2026-09-10T09:30"));

      List<FilePatch> patches = modifier.previewPatches(changes);
      assertEquals(List.of(EXPECTED), patches.stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(changes);
      assertEquals(EXPECTED, Files.readString(file));
    }
  }

  @ParameterizedTest(name = "{0}: {2}")
  @MethodSource("getInvalidValues")
  void refuseInvalidTemporalBound(String id, String feature, String value, String expectedError)
      throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file =
          fixture.addSource("com.devtoolsapplayoutspring.views.TemporalBoundsCasesView", SOURCE);
      addFields(fixture);
      SourceCodeModifier modifier = fixture.getModifier();
      ChangeRequest change = fixture.createChange(id, feature, value);

      List<ChangeResult> preview = modifier.preview(List.of(change));
      assertEquals(expectedError, preview.get(0).getError());
      assertEquals(SOURCE, Files.readString(file));
      List<ChangeResult> applied = modifier.apply(List.of(change));
      assertEquals(expectedError, applied.get(0).getError());
      assertEquals(SOURCE, Files.readString(file));
    }
  }

  private static void addFields(SourceWriteFixture fixture) {
    fixture.addComponent("day", DateField.class,
        List.of(new SourcePoint("com.devtoolsapplayoutspring.views.TemporalBoundsCasesView",
            "TemporalBoundsCasesView.java", 22)));
    fixture.addComponent("time", TimeField.class,
        List.of(new SourcePoint("com.devtoolsapplayoutspring.views.TemporalBoundsCasesView",
            "TemporalBoundsCasesView.java", 24)));
    fixture.addComponent("appointment", DateTimeField.class,
        List.of(new SourcePoint("com.devtoolsapplayoutspring.views.TemporalBoundsCasesView",
            "TemporalBoundsCasesView.java", 27)));
  }

  private static Stream<Arguments> getInvalidValues() {
    return Stream.of(
        Arguments.of("day", "HasMin", "2026-02-29",
            "Property 'setMin': expected a valid LocalDate value in ISO format"),
        Arguments.of("time", "HasMax", "24:01",
            "Property 'setMax': expected a valid LocalTime value in ISO format"),
        Arguments.of("appointment", "HasMin", "2026-02-30T09:30",
            "Property 'setMin': expected a valid LocalDateTime value in ISO format"));
  }
}
