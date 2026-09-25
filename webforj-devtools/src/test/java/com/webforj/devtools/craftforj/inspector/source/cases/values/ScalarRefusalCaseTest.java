package com.webforj.devtools.craftforj.inspector.source.cases.values;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.button.Button;
import com.webforj.component.field.NumberField;
import com.webforj.component.field.TextField;
import com.webforj.devtools.craftforj.inspector.source.SourceCodeModifier;
import com.webforj.devtools.craftforj.inspector.source.cases.support.SourceWriteFixture;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeResult;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Stream;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class ScalarRefusalCaseTest {

  private static final String SOURCE = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.Theme;
      import com.webforj.component.button.Button;
      import com.webforj.component.button.ButtonTheme;
      import com.webforj.component.field.NumberField;
      import com.webforj.component.field.TextField;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.component.toast.Toast;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/property-cases", outlet = MainLayout.class)
      @FrameTitle("Property cases")
      public class PropertyCasesView extends Composite<FlexLayout> {
        public PropertyCasesView() {
          TextField name = new TextField("Name");
          name.setMaxLength(12);
          NumberField quantity = new NumberField("Quantity", 1.0);
          quantity.setStep(0.5);
          Button action = new Button("Submit");
          action.setEnabled(true);
          action.setTheme(ButtonTheme.PRIMARY);
          action.addClassName("original", "keep");
          action.setWidth("220px");
          action.onClick(event -> Toast.show(name.getValue() + " / " + quantity.getValue(),
              3000, Theme.INFO, Toast.Placement.BOTTOM_RIGHT));
          getBoundComponent().add(name, quantity, action);
        }
      }
      """;

  @TempDir
  Path temporaryDirectory;

  @ParameterizedTest(name = "{0}: {2}")
  @MethodSource("getInvalidValues")
  void refuseInvalidScalar(String id, String feature, Object value, String expectedError)
      throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      final Path file =
          fixture.addSource("com.devtoolsapplayoutspring.views.PropertyCasesView", SOURCE);
      fixture.addComponent("name", TextField.class,
          List.of(new SourcePoint("com.devtoolsapplayoutspring.views.PropertyCasesView",
              "PropertyCasesView.java", 18)));
      fixture.addComponent("quantity", NumberField.class,
          List.of(new SourcePoint("com.devtoolsapplayoutspring.views.PropertyCasesView",
              "PropertyCasesView.java", 20)));
      fixture.addComponent("action", Button.class,
          List.of(new SourcePoint("com.devtoolsapplayoutspring.views.PropertyCasesView",
              "PropertyCasesView.java", 22)));
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

  private static Stream<Arguments> getInvalidValues() {
    return Stream.of(
        Arguments.of("name", "HasMaxLength", 5.25,
            "Property 'setMaxLength': expected a whole number in the int range"),
        Arguments.of("name", "HasMaxLength", 4294967297L,
            "Property 'setMaxLength': expected a whole number in the int range"),
        Arguments.of("quantity", "HasStep", "not a number",
            "Property 'setStep': expected a finite double value"),
        Arguments.of("quantity", "HasStep", "NaN",
            "Property 'setStep': expected a finite double value"),
        Arguments.of("quantity", "HasStep", "Infinity",
            "Property 'setStep': expected a finite double value"),
        Arguments.of("action", "HasEnablement", "enabled",
            "Property 'setEnabled': expected true or false"));
  }
}
