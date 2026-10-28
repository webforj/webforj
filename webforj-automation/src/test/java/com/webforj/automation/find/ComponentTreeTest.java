package com.webforj.automation.find;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mockStatic;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.automation.FrameEnvironment;
import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;
import com.webforj.component.Composite;
import com.webforj.component.button.Button;
import com.webforj.component.field.TextField;
import com.webforj.component.html.elements.Div;
import com.webforj.component.html.elements.H3;
import com.webforj.component.html.elements.Img;
import com.webforj.component.layout.flexlayout.FlexLayout;
import com.webforj.component.window.Frame;
import com.webforj.concern.HasPrefix;
import com.webforj.concern.HasSuffix;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class ComponentTreeTest {

  @Test
  void shouldExposeFramesAsRoots() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of());
      assertEquals(0, ComponentTree.getRoots().size());

      app.when(App::getFrames).thenReturn(List.of(frame));

      assertEquals(List.of(frame), ComponentTree.getRoots());
    }
  }

  @Test
  void shouldVisitEveryComponentExactlyOnce() {
    Div root = new Div();
    Div container = new Div();
    Button button = new Button("Save");
    container.add(button);
    root.add(container);

    List<Component> visited = new ArrayList<>();
    ComponentTree.walk(root, visited::add);

    long containerVisits = visited.stream().filter(component -> component == container).count();
    long buttonVisits = visited.stream().filter(component -> component == button).count();

    assertEquals(1, containerVisits);
    assertEquals(1, buttonVisits);
  }

  @Test
  void shouldListTheComponentsAddedToTheFrameWithoutTheElementsTheyRegister() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      Composite<Div> view = new Composite<>() {
        {
          getBoundComponent().add(new H3("Title"));
        }
      };
      frame.add(view);
      app.when(App::getFrames).thenReturn(List.of(frame));

      assertEquals(List.of(view), ComponentTree.getChildren(frame));
      assertEquals("""
          - Frame "Orders":
            - : [composite=Div] [node=div]:
              - H3 [composite=Element] [node=h3]
          """.replace("- :", "- " + view.getClass().getSimpleName()), ComponentTree.renderAll());
    }
  }

  @Test
  void shouldPrintTheTreeWithComponentFacts() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      Button button = new Button("Save");
      button.setName("save-button");
      frame.add(button);
      app.when(App::getFrames).thenReturn(List.of(frame));

      String tree = ComponentTree.renderAll();

      assertTrue(tree.startsWith("- Frame"));
      assertTrue(tree.contains("  - Button \"Save\" [name=save-button]"));
    }
  }

  @Test
  void shouldPrintPlaceholderWithoutFrames() {
    try (MockedStatic<App> app = mockStatic(App.class)) {
      app.when(App::getFrames).thenReturn(List.<Frame>of());

      assertEquals("<no frames>", ComponentTree.renderAll());
    }
  }

  @Test
  void shouldFindComponentsInPrefixAndSuffixSlots() {
    Div root = new Div();
    Button button = new Button("Save");
    Img image = new Img("logo.png");
    button.setPrefixComponent(image);
    TextField field = new TextField();
    Button clear = new Button("Clear");
    field.setSuffixComponent(clear);
    root.add(button, field);

    assertEquals(List.of(image), ComponentLocator.of(Img.class).within(button).all());
    assertEquals(List.of(clear), ComponentLocator.of(Button.class).within(field).all());
  }

  @Test
  void shouldIgnoreDestroyedComponentsStillReferencedByTheirContainer() {
    Button gone = new Button("Notice");
    FlexLayout root = new FlexLayout(gone);
    gone.destroy();

    assertEquals(0, ComponentLocator.of(Button.class).within(root).count());
  }

  @Test
  void shouldIgnoreCompositesWhoseBoundComponentWasDestroyed() {
    Div root = new Div();
    DestroyableBox box = new DestroyableBox();
    root.add(box);
    ComponentUtil.getBoundComponent(box).destroy();

    assertEquals(0, ComponentLocator.of(DestroyableBox.class).within(root).count());
  }

  @Test
  void shouldSkipSlotsTheBoundComponentDoesNotHave() {
    SlotlessBox box = new SlotlessBox();

    assertEquals(List.of(), ComponentTree.getChildren(box));
  }

  @Test
  void shouldLeaveDestroyedComponentsOutOfThePrintedTree() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      Button notice = new Button("Notice");
      frame.add(notice);
      app.when(App::getFrames).thenReturn(List.of(frame));
      String open = ComponentTree.renderAll();

      notice.destroy();

      assertTrue(open.contains("Notice"));
      assertFalse(ComponentTree.renderAll().contains("Notice"));
    }
  }

  static class DestroyableBox extends Composite<Div> {
  }

  static class SlotlessBox extends Composite<Div>
      implements HasPrefix<SlotlessBox>, HasSuffix<SlotlessBox> {
  }
}
