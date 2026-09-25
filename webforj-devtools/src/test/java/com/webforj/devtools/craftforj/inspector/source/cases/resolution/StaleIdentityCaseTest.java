package com.webforj.devtools.craftforj.inspector.source.cases.resolution;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.button.Button;
import com.webforj.devtools.craftforj.inspector.source.SourceCodeModifier;
import com.webforj.devtools.craftforj.inspector.source.cases.support.SourceWriteFixture;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
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

class StaleIdentityCaseTest {

  private static final String LIVE =
      """
          package com.devtoolsapplayoutspring.views;

          import com.webforj.component.Composite;
          import com.webforj.component.button.Button;
          import com.webforj.component.layout.flexlayout.FlexDirection;
          import com.webforj.component.layout.flexlayout.FlexLayout;
          import com.webforj.router.annotation.FrameTitle;
          import com.webforj.router.annotation.Route;

          @Route(value = "/combined-initialization-cases", outlet = MainLayout.class)
          @FrameTitle("Combined initialization cases")
          public class CombinedInitializationCasesView extends Composite<FlexLayout> {
            public CombinedInitializationCasesView() {
              Button sized = new Button("Combined size");
              sized.setWidth("180px");
              sized.setSize(240, 48).setTooltipText("Keep tooltip");
              sized.setMinSize("120px", "32px");
              sized.setMaxSize(400, 80);
              sized.onClick(event -> sized.setText("Combined size clicked"));
              Button configured = new Button("Helper initialized");
              configured.setTooltipText("Before helper");
              configure(configured);
              Button update = new Button("Change dimensions");
              update.onClick(event -> {
                sized.setWidth("280px");
                sized.setMinHeight("40px");
                sized.setMaxWidth("360px");
              });
              Button reset = new Button("Reset width");
              reset.onClick(event -> sized.setWidth(""));
              getBoundComponent().setDirection(FlexDirection.COLUMN);
              getBoundComponent().setAlignment(com.webforj.component.layout.flexlayout.FlexAlignment.START);
              getBoundComponent().add(sized, configured, update, reset);
            }

            private void configure(Button button) {
              button.setTooltipText("After helper");
              button.onClick(event -> button.setText("Helper initialized clicked"));
            }
          }
          """;

  private static final String ANONYMOUS = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.Theme;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.component.toast.Toast;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/anonymous-source-cases", outlet = MainLayout.class)
      @FrameTitle("Anonymous source cases")
      public class AnonymousSourceCasesView extends Composite<FlexLayout> {
        private final Button action = new Button("Outer action");

        public AnonymousSourceCasesView() {
          Composite<FlexLayout> card = new Composite<FlexLayout>() {
            private final Button action = new Button("Anonymous action");

            {
              getBoundComponent().add(action);
              action.onClick(event -> Toast.show("Anonymous action clicked", 3000, Theme.INFO,
                  Toast.Placement.BOTTOM_RIGHT));
              action.setTooltipText("Anonymous tooltip");
            }
          };
          getBoundComponent().add(action, card);
          action.onClick(event -> Toast.show("Outer action clicked", 3000, Theme.INFO,
              Toast.Placement.BOTTOM_RIGHT));
        }
      }
      """;

  private static final String LOCAL = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/creation-cases", outlet = MainLayout.class)
      @FrameTitle("Creation cases")
      public class CreationCasesView extends Composite<FlexLayout> {
        private final Button initialized = new Button("Initialized action");

        {
          initialized.setTooltipText("Initializer tooltip");
        }

        public CreationCasesView() {
          getBoundComponent().add(initialized);
          getBoundComponent().add(new Button("Inline action"));
          getBoundComponent().add(createAction());
          getBoundComponent().add(new InnerActions());
          class LocalActions extends Composite<FlexLayout> {
            private final Button action = new Button("Local class action");

            LocalActions() {
              action.setTooltipText("Local class tooltip");
              getBoundComponent().add(action);
            }
          }
          getBoundComponent().add(new LocalActions());
        }

        private Button createAction() {
          return new Button("Returned action");
        }

        class InnerActions extends Composite<FlexLayout> {
          private final Button action = new Button("Inner class action");

          InnerActions() {
            action.setTooltipText("Inner class tooltip");
            getBoundComponent().add(action);
          }
        }
      }
      """;

  private static final String NESTED = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.Theme;
      import com.webforj.component.button.Button;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.component.toast.Toast;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/source-cases", outlet = MainLayout.class)
      @FrameTitle("Source cases")
      public class SourceCasesView extends Composite<FlexLayout> {
        private final Button action = new Button("Outer action");

        public SourceCasesView() {
          action.addClassName("outer-action");
          getBoundComponent().add(action, new NestedCard(), new ResolvedButtonView());
          action.onClick(event -> Toast.show("Outer action clicked", 3000, Theme.INFO,
              Toast.Placement.BOTTOM_RIGHT));
        }

        static class NestedCard extends Composite<FlexLayout> {
          private final Button action = new Button("Nested action");

          NestedCard() {
            action.addClassName("first").setTooltipText("Nested tooltip").addClassName("second");
            action.addClassName("third");
            getBoundComponent().add(action);
            action.onClick(event -> Toast.show("Nested action clicked", 3000, Theme.INFO,
                Toast.Placement.BOTTOM_RIGHT));
          }
        }
      }
      """;

  @TempDir
  Path temporaryDirectory;

  @Test
  @DisplayName("A stale runtime line hitting another Button cannot override the stored declaration "
      + "identity")
  void refuseSameTypeLiveShift() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      String owner = "com.devtoolsapplayoutspring.views.CombinedInitializationCasesView";
      Path file = fixture.addSource(owner, LIVE);
      fixture.addComponent("configured", Button.class,
          List.of(new SourcePoint(owner, "CombinedInitializationCasesView.java", 14)));
      ChangeRequest change = fixture.createChange("configured", "HasTooltip", "Saved tooltip");
      change.setSource(
          new SourceLocation(file.toString(), 14, owner, "configured", Button.class.getName()));
      SourceCodeModifier modifier = fixture.getModifier();
      final String error =
          "Stored declaration 'configured' no longer matches the runtime source location. Reload "
              + "the application before saving.";
      assertEquals(error, modifier.preview(List.of(change)).get(0).getError());
      assertEquals(LIVE, Files.readString(file));
      assertEquals(error, modifier.apply(List.of(change)).get(0).getError());
      assertEquals(LIVE, Files.readString(file));
    }
  }

  @Test
  @DisplayName("Two same-type creations on the runtime line report ambiguity, not a stale stored "
      + "declaration")
  void reportAmbiguityBeforeStaleIdentity() throws IOException {
    final String source =
        """
            package com.devtoolsapplayoutspring.views;

            import com.webforj.component.Composite;
            import com.webforj.component.button.Button;
            import com.webforj.component.layout.flexlayout.FlexLayout;
            import com.webforj.router.annotation.FrameTitle;
            import com.webforj.router.annotation.Route;

            @Route(value = "/ambiguous-cases", outlet = MainLayout.class)
            @FrameTitle("Ambiguous cases")
            public class AmbiguousCasesView extends Composite<FlexLayout> {
              public AmbiguousCasesView() {
                Button first = new Button("First action"); Button configured = new Button("Second action");
                getBoundComponent().add(first, configured);
              }
            }
            """;
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      String owner = "com.devtoolsapplayoutspring.views.AmbiguousCasesView";
      Path file = fixture.addSource(owner, source);
      fixture.addComponent("configured", Button.class,
          List.of(new SourcePoint(owner, "AmbiguousCasesView.java", 13)));
      ChangeRequest change = fixture.createChange("configured", "HasTooltip", "Saved tooltip");
      change.setSource(
          new SourceLocation(file.toString(), 13, owner, "configured", Button.class.getName()));
      SourceCodeModifier modifier = fixture.getModifier();
      final String error =
          "Cannot identify Button at line 13: multiple matching creations share this line";
      assertEquals(error, modifier.preview(List.of(change)).get(0).getError());
      assertEquals(source, Files.readString(file));
      assertEquals(error, modifier.apply(List.of(change)).get(0).getError());
      assertEquals(source, Files.readString(file));
    }
  }

  @ParameterizedTest
  @ValueSource(booleans = {false, true})
  @DisplayName("Destroyed local and anonymous owners cannot be recovered by a stale line or a "
      + "shared variable name")
  void refuseUnstableStoredOwner(boolean local) throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      final String source = local ? LOCAL : ANONYMOUS;
      String top = "com.devtoolsapplayoutspring.views."
          + (local ? "CreationCasesView" : "AnonymousSourceCasesView");
      String owner = top + (local ? "$1LocalActions" : "$1");
      Path file = fixture.addSource(top, source);
      fixture.addSourceClass(owner, file);
      fixture.addComponent("action", Button.class, List.of(new SourcePoint(owner,
          local ? "CreationCasesView.java" : "AnonymousSourceCasesView.java", 14)));
      ChangeRequest change = fixture.createChange("action", "HasTooltip", "Saved tooltip");
      change.setSource(
          new SourceLocation(file.toString(), 14, owner, "action", Button.class.getName()));
      fixture.removeComponent("action");
      final String error = "This component was removed and its class '" + owner
          + "' has no stable name, so its declaration cannot be found. "
          + "Reload the application before saving.";
      SourceCodeModifier modifier = fixture.getModifier();
      assertEquals(error, modifier.preview(List.of(change)).get(0).getError());
      assertEquals(source, Files.readString(file));
      assertEquals(error, modifier.apply(List.of(change)).get(0).getError());
      assertEquals(source, Files.readString(file));
    }
  }

  @Test
  @DisplayName("A stale nested runtime location cannot select an outer field with the same name "
      + "and type")
  void refuseWrongLiveOwner() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      String top = "com.devtoolsapplayoutspring.views.SourceCasesView";
      String owner = top + "$NestedCard";
      Path file = fixture.addSource(top, NESTED);
      fixture.addSourceClass(owner, file);
      fixture.addComponent("nested", Button.class,
          List.of(new SourcePoint(owner, "SourceCasesView.java", 14)));
      List<ChangeRequest> edits =
          List.of(fixture.createChange("nested", "HasTooltip", "Saved tooltip"));
      final String error = "Runtime source location no longer belongs to '" + owner
          + "'. Reload the application before saving.";
      SourceCodeModifier modifier = fixture.getModifier();
      assertEquals(error, modifier.preview(edits).get(0).getError());
      assertEquals(NESTED, Files.readString(file));
      assertEquals(error, modifier.apply(edits).get(0).getError());
      assertEquals(NESTED, Files.readString(file));
    }
  }
}
