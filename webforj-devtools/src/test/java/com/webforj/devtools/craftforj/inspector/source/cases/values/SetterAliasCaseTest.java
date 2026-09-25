package com.webforj.devtools.craftforj.inspector.source.cases.values;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.badge.Badge;
import com.webforj.component.dialog.Dialog;
import com.webforj.component.html.elements.Anchor;
import com.webforj.component.layout.columnslayout.ColumnsLayout;
import com.webforj.devtools.craftforj.inspector.source.SourceCodeModifier;
import com.webforj.devtools.craftforj.inspector.source.cases.support.SourceWriteFixture;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
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

class SetterAliasCaseTest {

  private static final String OWNER = "com.devtoolsapplayoutspring.views.AliasPropertyCasesView";

  private static final String SOURCE =
      """
          package com.devtoolsapplayoutspring.views;

          import com.webforj.component.Composite;
          import com.webforj.component.badge.Badge;
          import com.webforj.component.button.Button;
          import com.webforj.component.dialog.Dialog;
          import com.webforj.component.html.elements.Anchor;
          import com.webforj.component.layout.columnslayout.ColumnsLayout;
          import com.webforj.component.layout.flexlayout.FlexDirection;
          import com.webforj.component.layout.flexlayout.FlexLayout;
          import com.webforj.router.annotation.FrameTitle;
          import com.webforj.router.annotation.Route;

          @Route(value = "/alias-property-cases", outlet = MainLayout.class)
          @FrameTitle("Alias property cases")
          public class AliasPropertyCasesView extends Composite<FlexLayout> {
            public AliasPropertyCasesView() {
              Badge badge = new Badge("Initial label");
              badge.setLabel("Earlier label");
              badge.setText("Alias label");
              Anchor link = new Anchor("#", "Alias link");
              link.setUrl("#aliases");
              ColumnsLayout columns = new ColumnsLayout(new Button("First column"), new Button("Second column"));
              columns.setSpacing(12);
              Dialog dialog = new Dialog();
              dialog.setCloseable(true);
              dialog.add(new Button("Close alias dialog", event -> dialog.close()));
              Button open = new Button("Open alias dialog", event -> dialog.open());
              Button update = new Button("Change alias properties");
              update.onClick(event -> {
                badge.setLabel("Saved label");
                link.setHref("#saved");
                columns.setHorizontalSpacing("24px");
                dialog.setCancelOnEscKey(false);
              });
              getBoundComponent().setDirection(FlexDirection.COLUMN);
              getBoundComponent().add(badge, link, columns, open, update, dialog);
            }
          }
          """;

  private static final String EXPECTED =
      """
          package com.devtoolsapplayoutspring.views;

          import com.webforj.component.Composite;
          import com.webforj.component.badge.Badge;
          import com.webforj.component.button.Button;
          import com.webforj.component.dialog.Dialog;
          import com.webforj.component.html.elements.Anchor;
          import com.webforj.component.layout.columnslayout.ColumnsLayout;
          import com.webforj.component.layout.flexlayout.FlexDirection;
          import com.webforj.component.layout.flexlayout.FlexLayout;
          import com.webforj.router.annotation.FrameTitle;
          import com.webforj.router.annotation.Route;

          @Route(value = "/alias-property-cases", outlet = MainLayout.class)
          @FrameTitle("Alias property cases")
          public class AliasPropertyCasesView extends Composite<FlexLayout> {
            public AliasPropertyCasesView() {
              Badge badge = new Badge("Initial label");
              badge.setLabel("Saved label");
              Anchor link = new Anchor("#", "Alias link");
              link.setHref("#saved");
              ColumnsLayout columns = new ColumnsLayout(new Button("First column"), new Button("Second column"));
              columns.setVerticalSpacing(12).setHorizontalSpacing("24px");
              Dialog dialog = new Dialog();
              dialog.setCancelOnOutsideClick(true).setCancelOnEscKey(false);
              dialog.add(new Button("Close alias dialog", event -> dialog.close()));
              Button open = new Button("Open alias dialog", event -> dialog.open());
              Button update = new Button("Change alias properties");
              update.onClick(event -> {
                badge.setLabel("Saved label");
                link.setHref("#saved");
                columns.setHorizontalSpacing("24px");
                dialog.setCancelOnEscKey(false);
              });
              getBoundComponent().setDirection(FlexDirection.COLUMN);
              getBoundComponent().add(badge, link, columns, open, update, dialog);
            }
          }
          """;

  private static final String RESET =
      """
          package com.devtoolsapplayoutspring.views;

          import com.webforj.component.Composite;
          import com.webforj.component.badge.Badge;
          import com.webforj.component.button.Button;
          import com.webforj.component.dialog.Dialog;
          import com.webforj.component.html.elements.Anchor;
          import com.webforj.component.layout.columnslayout.ColumnsLayout;
          import com.webforj.component.layout.flexlayout.FlexDirection;
          import com.webforj.component.layout.flexlayout.FlexLayout;
          import com.webforj.router.annotation.FrameTitle;
          import com.webforj.router.annotation.Route;

          @Route(value = "/alias-property-cases", outlet = MainLayout.class)
          @FrameTitle("Alias property cases")
          public class AliasPropertyCasesView extends Composite<FlexLayout> {
            public AliasPropertyCasesView() {
              Badge badge = new Badge("Initial label");
              Anchor link = new Anchor("#", "Alias link");
              ColumnsLayout columns = new ColumnsLayout(new Button("First column"), new Button("Second column"));
              columns.setVerticalSpacing(12);
              Dialog dialog = new Dialog();
              dialog.setCancelOnOutsideClick(true);
              dialog.add(new Button("Close alias dialog", event -> dialog.close()));
              Button open = new Button("Open alias dialog", event -> dialog.open());
              Button update = new Button("Change alias properties");
              update.onClick(event -> {
                badge.setLabel("Saved label");
                link.setHref("#saved");
                columns.setHorizontalSpacing("24px");
                dialog.setCancelOnEscKey(false);
              });
              getBoundComponent().setDirection(FlexDirection.COLUMN);
              getBoundComponent().add(badge, link, columns, open, update, dialog);
            }
          }
          """;

  private static final String TEXT_EXPECTED =
      """
          package com.devtoolsapplayoutspring.views;

          import com.webforj.component.Composite;
          import com.webforj.component.badge.Badge;
          import com.webforj.component.button.Button;
          import com.webforj.component.dialog.Dialog;
          import com.webforj.component.html.elements.Anchor;
          import com.webforj.component.layout.columnslayout.ColumnsLayout;
          import com.webforj.component.layout.flexlayout.FlexDirection;
          import com.webforj.component.layout.flexlayout.FlexLayout;
          import com.webforj.router.annotation.FrameTitle;
          import com.webforj.router.annotation.Route;

          @Route(value = "/alias-property-cases", outlet = MainLayout.class)
          @FrameTitle("Alias property cases")
          public class AliasPropertyCasesView extends Composite<FlexLayout> {
            public AliasPropertyCasesView() {
              Badge badge = new Badge("Initial label");
              badge.setText("Saved text");
              Anchor link = new Anchor("#", "Alias link");
              link.setUrl("#aliases");
              ColumnsLayout columns = new ColumnsLayout(new Button("First column"), new Button("Second column"));
              columns.setSpacing(12);
              Dialog dialog = new Dialog();
              dialog.setCloseable(true);
              dialog.add(new Button("Close alias dialog", event -> dialog.close()));
              Button open = new Button("Open alias dialog", event -> dialog.open());
              Button update = new Button("Change alias properties");
              update.onClick(event -> {
                badge.setLabel("Saved label");
                link.setHref("#saved");
                columns.setHorizontalSpacing("24px");
                dialog.setCancelOnEscKey(false);
              });
              getBoundComponent().setDirection(FlexDirection.COLUMN);
              getBoundComponent().add(badge, link, columns, open, update, dialog);
            }
          }
          """;

  @TempDir
  Path temporaryDirectory;

  @Test
  @DisplayName("Equivalent and shared-argument setters preserve other properties across write, "
      + "reset and repeat")
  void writeAndResetAliases() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = addComponents(fixture);
      SourceCodeModifier modifier = fixture.getModifier();
      List<ChangeRequest> edits =
          List.of(fixture.createChange("badge", "BadgeLabel", "Saved label"),
              fixture.createChange("link", "AnchorHref", "#saved"),
              fixture.createChange("columns", "ColumnsLayoutHorizontalSpacing", "24px"),
              fixture.createChange("dialog", "DialogCancelOnEscKey", false));
      assertEquals(List.of(EXPECTED),
          modifier.previewPatches(edits).stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(edits);
      assertEquals(EXPECTED, Files.readString(file));
      registerComponents(fixture, -1);
      modifier.apply(edits);
      assertEquals(EXPECTED, Files.readString(file));
      List<ChangeRequest> resets = List.of(fixture.createChange("badge", "BadgeLabel", ""),
          fixture.createChange("link", "AnchorHref", ""),
          fixture.createChange("columns", "ColumnsLayoutHorizontalSpacing", ""),
          fixture.createChange("dialog", "DialogCancelOnEscKey", ""));
      assertEquals(List.of(RESET),
          modifier.previewPatches(resets).stream().map(FilePatch::getPatched).toList());
      assertEquals(EXPECTED, Files.readString(file));
      modifier.apply(resets);
      assertEquals(RESET, Files.readString(file));
    }
  }

  @ParameterizedTest
  @ValueSource(booleans = {false, true})
  @DisplayName("Generic text edits use concrete implementation aliases for live and destroyed "
      + "components")
  void writeConcreteTextAlias(boolean destroyed) throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = addComponents(fixture);
      ChangeRequest change = fixture.createChange("badge", "HasText", "Saved text");
      if (destroyed) {
        change.setSource(
            new SourceLocation(file.toString(), 18, OWNER, "badge", Badge.class.getName()));
        fixture.removeComponent("badge");
      }
      SourceCodeModifier modifier = fixture.getModifier();
      assertEquals(List.of(TEXT_EXPECTED),
          modifier.previewPatches(List.of(change)).stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(List.of(change));
      assertEquals(TEXT_EXPECTED, Files.readString(file));
    }
  }

  @Test
  @DisplayName("Conflicting text and label edits cannot silently overwrite one shared source "
      + "property")
  void refuseConflictingAliases() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file = addComponents(fixture);
      List<ChangeRequest> edits = List.of(fixture.createChange("badge", "HasText", "First"),
          fixture.createChange("badge", "BadgeLabel", "Second"));
      SourceCodeModifier modifier = fixture.getModifier();
      final String error =
          "Conflicting edits target the same source property: 'setText' and 'setLabel'";
      assertEquals(error, modifier.preview(edits).get(0).getError());
      assertEquals(SOURCE, Files.readString(file));
      assertEquals(error, modifier.apply(edits).get(0).getError());
      assertEquals(SOURCE, Files.readString(file));
    }
  }

  private Path addComponents(SourceWriteFixture fixture) throws IOException {
    Path file = fixture.addSource(OWNER, SOURCE);
    registerComponents(fixture, 0);
    return file;
  }

  private void registerComponents(SourceWriteFixture fixture, int shift) {
    fixture.addComponent("badge", Badge.class,
        List.of(new SourcePoint(OWNER, "AliasPropertyCasesView.java", 18)));
    fixture.addComponent("link", Anchor.class,
        List.of(new SourcePoint(OWNER, "AliasPropertyCasesView.java", 21 + shift)));
    fixture.addComponent("columns", ColumnsLayout.class,
        List.of(new SourcePoint(OWNER, "AliasPropertyCasesView.java", 23 + shift)));
    fixture.addComponent("dialog", Dialog.class,
        List.of(new SourcePoint(OWNER, "AliasPropertyCasesView.java", 25 + shift)));
  }
}
