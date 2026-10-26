package com.webforj.automation.see;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;
import com.webforj.component.Composite;
import com.webforj.component.button.Button;
import com.webforj.component.html.elements.Paragraph;
import com.webforj.component.html.elements.Span;
import com.webforj.component.layout.flexlayout.FlexLayout;
import com.webforj.component.window.Window;
import com.webforj.concern.HasComponents;
import com.webforj.concern.HasText;
import com.webforj.concern.HasVisibility;
import java.util.List;
import java.util.Set;
import org.junit.jupiter.api.Test;

class SnapshotRendererTest {

  @Test
  void shouldDescribeTheChildrenOfTheWrappedComponent() {
    Toolbar toolbar = new Toolbar();

    assertEquals("""
        - Toolbar:
          - Button "Save"
        """, Snapshot.render(toolbar));
  }

  @Test
  void shouldDescribeTheComponentThatDeclaresVisibilityWithoutSupportingIt() {
    Banner banner = new Banner();

    assertEquals("- Banner\n", Snapshot.render(banner));
  }

  @Test
  void shouldLeaveOutTheDestroyedChild() {
    Button gone = new Button("Gone");
    FlexLayout layout = new FlexLayout(new Button("Kept"), gone);
    gone.destroy();

    assertEquals("""
        - FlexLayout:
          - Button "Kept"
        """, Snapshot.render(layout));
  }

  @Test
  void shouldListTheWrappedComponentOnlyThroughItsWrapper() {
    Toolbar toolbar = new Toolbar();
    Shelf shelf = new Shelf(toolbar, ComponentUtil.getBoundComponent(toolbar), new Button("Help"));

    assertEquals("""
        - Shelf:
          - Toolbar:
            - Button "Save"
          - Button "Help"
        """, Snapshot.render(shelf));
  }

  @Test
  void shouldListTheWrappedComponentOfTheNestedWrapperOnlyThroughThatWrapper() {
    Paragraph note = new Paragraph("Due");
    Composite<FlexLayout> notes = new Composite<>() {
      {
        getBoundComponent().add(note);
      }
    };
    Shelf shelf = new Shelf(notes, ComponentUtil.getBoundComponent(note));

    assertEquals("""
        - Shelf:
          - ::
            - Paragraph
        """.replace("- :", "- " + notes.getClass().getSimpleName()), Snapshot.render(shelf));
  }

  @Test
  void shouldDescribeTheTextTheComponentSerializerHoldsOnTheServerByDefault() {
    SnapshotSerializerRegistry serializers =
        new SnapshotSerializerRegistry(List.of(new ParagraphTextOnServer()));
    SnapshotRenderer renderer = new SnapshotRenderer(serializers, new SnapshotOptions());
    Paragraph note = new Paragraph("Due");
    note.add(new Span("today"));

    SnapshotNode node = renderer.describe(note);

    assertEquals("Due", node.getText());
  }

  @Test
  void shouldRejectTwoApplicationSerializersWritingOneKey() {
    SnapshotSerializerRegistry serializers =
        new SnapshotSerializerRegistry(List.of(new ComponentTagger(), new ButtonTagger()));
    SnapshotRenderer renderer = new SnapshotRenderer(serializers, new SnapshotOptions());
    Button save = new Button("Save");

    IllegalStateException failure =
        assertThrows(IllegalStateException.class, () -> renderer.describe(save));

    assertEquals("The snapshot serializers " + ComponentTagger.class.getName() + " and "
        + ButtonTagger.class.getName() + " both write <tag> on Button. One of them has to own it.",
        failure.getMessage());
  }

  private static final class ParagraphTextOnServer implements SnapshotSerializer<Paragraph> {

    @Override
    public Class<Paragraph> getContract() {
      return Paragraph.class;
    }

    @Override
    public Set<Class<?>> getServerHeldContracts() {
      return Set.of(HasText.class);
    }
  }

  private static final class Toolbar extends Composite<FlexLayout> {
    private Toolbar() {
      getBoundComponent().add(new Button("Save"));
    }
  }

  private static final class Shelf extends Component implements HasComponents {
    private final List<Component> components;

    private Shelf(Component... components) {
      this.components = List.of(components);
    }

    @Override
    public List<Component> getComponents() {
      return components;
    }

    @Override
    protected void onCreate(Window window) {
      // no-op
    }

    @Override
    protected void onDestroy() {
      // no-op
    }
  }

  private static final class Banner extends Composite<Gauge> implements HasVisibility<Banner> {
    @Override
    protected Gauge initBoundComponent() {
      return new Gauge("bar");
    }
  }

  private static final class ComponentTagger implements SnapshotSerializer<Component> {
    @Override
    public Class<Component> getContract() {
      return Component.class;
    }

    @Override
    public void describe(Component target, SnapshotNode node) {
      node.setAttribute("tag", "component");
    }
  }

  private static final class ButtonTagger implements SnapshotSerializer<Button> {
    @Override
    public Class<Button> getContract() {
      return Button.class;
    }

    @Override
    public void describe(Button target, SnapshotNode node) {
      node.setAttribute("tag", "button");
    }
  }
}
