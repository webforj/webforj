package com.webforj.devtools.craftforj.inspector.source.cases.values;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.field.NumberField;
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

class NumericBoundsCaseTest {

  @TempDir
  Path temporaryDirectory;

  @Test
  @DisplayName("Numeric minimum and maximum text edits write Double setter arguments")
  void applyNumericBounds() throws IOException {
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
            name.setMaxLength(5);
            NumberField quantity = new NumberField("Quantity", 1.0);
            quantity.setStep(0.25);
            Button action = new Button("Submit");
            action.setEnabled(false);
            action.setTheme(ButtonTheme.SUCCESS);
            action.addClassName("edited", "keep");
            action.setWidth("50%");
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
            quantity.setStep(0.25);
            quantity.setMin(0.5);
            quantity.setMax(5.5);
            Button action = new Button("Submit");
            action.setEnabled(false);
            action.setTheme(ButtonTheme.SUCCESS);
            action.addClassName("edited", "keep");
            action.setWidth("50%");
            action.onClick(event -> Toast.show(name.getValue() + " / " + quantity.getValue(),
                3000, Theme.INFO, Toast.Placement.BOTTOM_RIGHT));
            getBoundComponent().add(name, quantity, action);
          }
        }
        """;
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = fixture.addSource("com.devtoolsapplayoutspring.views.PropertyCasesView", source);
      fixture.addComponent("quantity", NumberField.class,
          List.of(new SourcePoint("com.devtoolsapplayoutspring.views.PropertyCasesView",
              "PropertyCasesView.java", 20)));
      SourceCodeModifier modifier = fixture.getModifier();
      List<ChangeRequest> changes = List.of(fixture.createChange("quantity", "HasMin", "0.5"),
          fixture.createChange("quantity", "HasMax", "5.5"));

      List<FilePatch> patches = modifier.previewPatches(changes);
      assertEquals(List.of(expected), patches.stream().map(FilePatch::getPatched).toList());
      assertEquals(source, Files.readString(file));
      modifier.apply(changes);
      assertEquals(expected, Files.readString(file));
    }
  }
}
