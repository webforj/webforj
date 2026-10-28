package com.webforj.automation.act.userinteraction;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mockStatic;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.automation.FrameEnvironment;
import com.webforj.automation.act.MouseOptions;
import com.webforj.automation.act.UserInteraction;
import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;
import com.webforj.component.Composite;
import com.webforj.component.button.Button;
import com.webforj.component.button.event.ButtonClickEvent;
import com.webforj.component.element.event.ElementClickEvent;
import com.webforj.component.field.TextField;
import com.webforj.component.html.elements.Div;
import com.webforj.component.html.elements.Paragraph;
import com.webforj.component.tree.Tree;
import com.webforj.component.window.Frame;
import com.webforj.component.window.event.WindowClickEvent;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class UserInteractionClickTest {
  private MockedStatic<Environment> current;
  private MockedStatic<App> app;
  private Frame frame;

  @BeforeEach
  void initFrame() throws Exception {
    Environment environment = FrameEnvironment.create(null);
    current = mockStatic(Environment.class);
    app = mockStatic(App.class);
    current.when(Environment::getCurrent).thenReturn(environment);
    frame = new Frame("Orders");
    app.when(App::getFrames).thenReturn(List.of(frame));
  }

  @AfterEach
  void closeMocks() {
    app.close();
    current.close();
  }

  @Test
  void shouldClickButtonsThroughTheProductionPath() {
    Button button = new Button("Save");
    List<String> clicks = new ArrayList<>();
    button.addClickListener(event -> clicks.add("clicked"));
    frame.add(button);

    UserInteraction.click(button);

    assertEquals(List.of("clicked"), clicks);
  }

  @Test
  void shouldClickElementBackedComponents() {
    Div div = new Div();
    List<String> clicks = new ArrayList<>();
    div.addEventListener(ElementClickEvent.class, event -> clicks.add("clicked"));
    frame.add(div);

    UserInteraction.click(div);

    assertEquals(List.of("clicked"), clicks);
  }

  @Test
  void shouldClickWindows() {
    List<Integer> clicks = new ArrayList<>();
    frame.onClick(event -> clicks.add(event.getClickCount()));

    UserInteraction.click(frame);

    assertEquals(List.of(1), clicks);
  }

  @Test
  void shouldLetClicksReachTheWindowAroundTheComponent() {
    Paragraph paragraph = new Paragraph("Hello");
    List<Component> origins = new ArrayList<>();
    List<Integer> counts = new ArrayList<>();
    frame.onClick(event -> origins.add(event.getOriginalComponent()));
    frame.onClick(event -> counts.add(event.getClickCount()));
    frame.add(paragraph);

    UserInteraction.click(paragraph, new MouseOptions().setClickCount(2));
    UserInteraction.click(frame);

    assertEquals(List.of(ComponentUtil.getBoundComponent(paragraph), frame), origins);
    assertEquals(List.of(2, 1), counts);
  }

  @Test
  void shouldLetTheClickReachTheClickListenersOfEveryAncestor() {
    Div outer = new Div();
    Div inner = new Div();
    Paragraph paragraph = new Paragraph("Hello");
    List<String> order = new ArrayList<>();
    paragraph.onClick(event -> order.add("paragraph"));
    inner.onClick(event -> order.add("inner"));
    outer.onClick(event -> order.add("outer"));
    frame.onClick(event -> order.add("frame"));
    inner.add(paragraph);
    outer.add(inner);
    frame.add(outer);

    UserInteraction.click(paragraph);

    assertEquals(List.of("paragraph", "inner", "outer", "frame"), order);
  }

  @Test
  void shouldLetTheClickReachTheComponentAroundTheComposite() {
    List<Integer> counts = new ArrayList<>();
    Button button = new Button("Save");
    Composite<Div> card = new Composite<>() {
      {
        getBoundComponent().onClick(event -> counts.add(event.getDetail()));
        getBoundComponent().add(button);
      }
    };
    frame.add(card);

    UserInteraction.click(button, new MouseOptions().setClickCount(2));

    assertEquals(List.of(2), counts);
  }

  @Test
  void shouldLetButtonClicksReachTheWindowAroundTheButton() {
    Button button = new Button("Save");
    List<ButtonClickEvent> buttonClicks = new ArrayList<>();
    List<WindowClickEvent> windowClicks = new ArrayList<>();
    button.addClickListener(buttonClicks::add);
    frame.onClick(windowClicks::add);
    frame.add(button);

    UserInteraction.click(button);

    assertEquals(1, buttonClicks.size());
    assertEquals(1, windowClicks.size());
    assertEquals(button, windowClicks.get(0).getOriginalComponent());
  }

  @Test
  void shouldClickTheWindowOnceWhenItIsClickedItself() {
    List<WindowClickEvent> clicks = new ArrayList<>();
    frame.onClick(clicks::add);

    UserInteraction.click(frame);

    assertEquals(1, clicks.size());
  }

  @Test
  void shouldRejectClicksOnComponentsWithoutClickBehavior() {
    Tree tree = new Tree();
    TextField field = new TextField();
    frame.add(tree, field);

    IllegalStateException failure =
        assertThrows(IllegalStateException.class, () -> UserInteraction.click(tree));
    assertTrue(failure.getMessage().contains("does not react to clicks"));
    assertThrows(IllegalStateException.class, () -> UserInteraction.click(field));
  }

  @Test
  void shouldLetTheClickReachTheWindowAroundTheElement() {
    Div div = new Div();
    List<String> clicks = new ArrayList<>();
    div.addEventListener(ElementClickEvent.class, event -> clicks.add("element"));
    frame.onClick(event -> clicks.add("window"));
    frame.add(div);

    UserInteraction.click(div);

    assertEquals(List.of("element", "window"), clicks);
  }

  @Test
  void shouldRunTheClickEventTheComponentDeclares() {
    Gauge gauge = new Gauge();
    List<Object> clicks = new ArrayList<>();
    gauge.onClick(event -> clicks.add(event.getData().get("clientX")));
    frame.add(gauge);

    UserInteraction.click(gauge, new MouseOptions().setX(12));

    assertEquals(List.of(12), clicks);
  }
}
