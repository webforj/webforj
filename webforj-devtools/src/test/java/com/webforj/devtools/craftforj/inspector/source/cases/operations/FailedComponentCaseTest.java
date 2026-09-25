package com.webforj.devtools.craftforj.inspector.source.cases.operations;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.button.Button;
import com.webforj.component.field.TextField;
import com.webforj.devtools.craftforj.inspector.source.SourceCodeModifier;
import com.webforj.devtools.craftforj.inspector.source.cases.support.SourceWriteFixture;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeResult;
import com.webforj.devtools.craftforj.source.model.FilePatch;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Objects;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class FailedComponentCaseTest {

  @TempDir
  Path temporaryDirectory;

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  @DisplayName("A refused button edit preserves its class names while another component is saved")
  void preserveFailedComponent(boolean successfulComponentFirst) throws IOException {
    final String source = """
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
    final String expected = """
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
            name.setMaxLength(5);
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
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      final Path file =
          fixture.addSource("com.devtoolsapplayoutspring.views.PropertyCasesView", source);
      fixture.addComponent("name", TextField.class,
          List.of(new SourcePoint("com.devtoolsapplayoutspring.views.PropertyCasesView",
              "PropertyCasesView.java", 18)));
      fixture.addComponent("action", Button.class,
          List.of(new SourcePoint("com.devtoolsapplayoutspring.views.PropertyCasesView",
              "PropertyCasesView.java", 22)));
      SourceCodeModifier modifier = fixture.getModifier();
      ChangeRequest clearClasses = fixture.createChange("action", "HasClassName", List.of());
      ChangeRequest invalidTheme = fixture.createChange("action", "HasTheme",
          "com.webforj.component.button.ButtonTheme.NO_SUCH_THEME");
      ChangeRequest shorterName = fixture.createChange("name", "HasMaxLength", 5);
      List<ChangeRequest> changes =
          successfulComponentFirst ? List.of(shorterName, clearClasses, invalidTheme)
              : List.of(clearClasses, invalidTheme, shorterName);
      final String error = "Property 'setTheme': invalid enum value 'NO_SUCH_THEME'";

      assertEquals(List.of(error, error), modifier.preview(changes).stream()
          .map(ChangeResult::getError).filter(Objects::nonNull).toList());
      List<FilePatch> patches = modifier.previewPatches(changes);
      assertEquals(List.of(expected), patches.stream().map(FilePatch::getPatched).toList());
      assertEquals(source, Files.readString(file));

      assertEquals(List.of(error, error), modifier.apply(changes).stream()
          .map(ChangeResult::getError).filter(Objects::nonNull).toList());
      assertEquals(expected, Files.readString(file));
    }
  }
}
