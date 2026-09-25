package com.webforj.devtools.craftforj.inspector.source.cases.values;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.button.Button;
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

class ListPropertyCaseTest {

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

  @Test
  @DisplayName("Class names replace the complete varargs list and repeated saves do not duplicate "
      + "it")
  void applyAndRepeatClassNames() throws IOException {
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
            name.setMaxLength(12);
            NumberField quantity = new NumberField("Quantity", 1.0);
            quantity.setStep(0.5);
            Button action = new Button("Submit");
            action.setEnabled(true);
            action.setTheme(ButtonTheme.PRIMARY);
            action.addClassName("edited", "keep");
            action.setWidth("220px");
            action.onClick(event -> Toast.show(name.getValue() + " / " + quantity.getValue(),
                3000, Theme.INFO, Toast.Placement.BOTTOM_RIGHT));
            getBoundComponent().add(name, quantity, action);
          }
        }
        """;
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = fixture.addSource("com.devtoolsapplayoutspring.views.PropertyCasesView", SOURCE);
      fixture.addComponent("action", Button.class,
          List.of(new SourcePoint("com.devtoolsapplayoutspring.views.PropertyCasesView",
              "PropertyCasesView.java", 22)));
      SourceCodeModifier modifier = fixture.getModifier();
      ChangeRequest change =
          fixture.createChange("action", "HasClassName", List.of("edited", "keep"));

      List<FilePatch> patches = modifier.previewPatches(List.of(change));
      assertEquals(List.of(expected), patches.stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));

      modifier.apply(List.of(change));
      assertEquals(expected, Files.readString(file));
      modifier.apply(List.of(change));
      assertEquals(expected, Files.readString(file));
    }
  }

  @Test
  @DisplayName("An empty class-name list removes the existing addClassName call")
  void removeClassNames() throws IOException {
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
            name.setMaxLength(12);
            NumberField quantity = new NumberField("Quantity", 1.0);
            quantity.setStep(0.5);
            Button action = new Button("Submit");
            action.setEnabled(true);
            action.setTheme(ButtonTheme.PRIMARY);
            action.setWidth("220px");
            action.onClick(event -> Toast.show(name.getValue() + " / " + quantity.getValue(),
                3000, Theme.INFO, Toast.Placement.BOTTOM_RIGHT));
            getBoundComponent().add(name, quantity, action);
          }
        }
        """;
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = fixture.addSource("com.devtoolsapplayoutspring.views.PropertyCasesView", SOURCE);
      fixture.addComponent("action", Button.class,
          List.of(new SourcePoint("com.devtoolsapplayoutspring.views.PropertyCasesView",
              "PropertyCasesView.java", 22)));
      SourceCodeModifier modifier = fixture.getModifier();
      ChangeRequest change = fixture.createChange("action", "HasClassName", List.of());

      List<FilePatch> patches = modifier.previewPatches(List.of(change));
      assertEquals(List.of(expected), patches.stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));

      modifier.apply(List.of(change));
      assertEquals(expected, Files.readString(file));
    }
  }
}
