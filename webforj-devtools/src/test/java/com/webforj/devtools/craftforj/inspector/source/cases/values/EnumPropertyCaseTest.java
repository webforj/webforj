package com.webforj.devtools.craftforj.inspector.source.cases.values;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.button.Button;
import com.webforj.devtools.craftforj.inspector.source.SourceCodeModifier;
import com.webforj.devtools.craftforj.inspector.source.cases.support.SourceWriteFixture;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeResult;
import com.webforj.devtools.craftforj.source.model.FilePatch;
import com.webforj.devtools.craftforj.source.model.SourceLocation;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class EnumPropertyCaseTest {

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

  @ParameterizedTest
  @ValueSource(booleans = {false, true})
  @DisplayName("Live and destroyed components resolve enum types and preserve existing imports")
  void applyEnumProperty(boolean destroyed) throws IOException {
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
            action.setTheme(ButtonTheme.SUCCESS);
            action.addClassName("original", "keep");
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
      ChangeRequest change = fixture.createChange("action", "HasTheme",
          "com.webforj.component.button.ButtonTheme.SUCCESS");
      if (destroyed) {
        change.setSource(new SourceLocation(file.toString(), 1,
            "com.devtoolsapplayoutspring.views.PropertyCasesView", "action",
            Button.class.getName()));
        fixture.removeComponent("action");
      }

      List<FilePatch> patches = modifier.previewPatches(List.of(change));
      assertEquals(List.of(expected), patches.stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));

      modifier.apply(List.of(change));
      assertEquals(expected, Files.readString(file));
    }
  }

  @Test
  @DisplayName("A stale live source line pointing at another component type is refused")
  void refuseStaleLiveLocation() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = fixture.addSource("com.devtoolsapplayoutspring.views.PropertyCasesView", SOURCE);
      fixture.addComponent("action", Button.class,
          List.of(new SourcePoint("com.devtoolsapplayoutspring.views.PropertyCasesView",
              "PropertyCasesView.java", 18)));
      List<ChangeRequest> edits = List.of(fixture.createChange("action", "HasTheme",
          "com.webforj.component.button.ButtonTheme.SUCCESS"));
      SourceCodeModifier modifier = fixture.getModifier();
      final String error = "No Button is declared at line 18. The source changed since the "
          + "application was compiled. Reload the application before saving.";

      assertEquals(error, modifier.preview(edits).get(0).getError());
      assertEquals(SOURCE, Files.readString(file));
      assertEquals(error, modifier.apply(edits).get(0).getError());
      assertEquals(SOURCE, Files.readString(file));
    }
  }

  @Test
  @DisplayName("An invalid enum constant reports the property error and leaves the source "
      + "unchanged")
  void refuseInvalidEnumConstant() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = fixture.addSource("com.devtoolsapplayoutspring.views.PropertyCasesView", SOURCE);
      fixture.addComponent("action", Button.class,
          List.of(new SourcePoint("com.devtoolsapplayoutspring.views.PropertyCasesView",
              "PropertyCasesView.java", 22)));
      SourceCodeModifier modifier = fixture.getModifier();
      ChangeRequest change = fixture.createChange("action", "HasTheme",
          "com.webforj.component.button.ButtonTheme.NO_SUCH_THEME");

      List<ChangeResult> preview = modifier.preview(List.of(change));
      assertEquals("Property 'setTheme': invalid enum value 'NO_SUCH_THEME'",
          preview.get(0).getError());
      assertEquals(SOURCE, Files.readString(file));

      List<ChangeResult> applied = modifier.apply(List.of(change));
      assertEquals("Property 'setTheme': invalid enum value 'NO_SUCH_THEME'",
          applied.get(0).getError());
      assertEquals(SOURCE, Files.readString(file));
    }
  }
}
