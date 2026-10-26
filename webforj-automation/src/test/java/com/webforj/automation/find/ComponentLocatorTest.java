package com.webforj.automation.find;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mockStatic;

import com.webforj.App;
import com.webforj.component.ComponentUtil;
import com.webforj.component.Composite;
import com.webforj.component.Expanse;
import com.webforj.component.button.Button;
import com.webforj.component.button.ButtonTheme;
import com.webforj.component.element.ElementComposite;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.field.TextField;
import com.webforj.component.html.elements.Div;
import com.webforj.component.window.Frame;
import com.webforj.concern.HasLabel;
import com.webforj.data.selection.SingleSelectable;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class ComponentLocatorTest {
  private static final String CANCEL_TEXT = "Cancel";

  @Test
  void shouldFindComponentsByType() {
    Div root = new Div();
    Button save = new Button("Save");
    Button cancel = new Button(CANCEL_TEXT);
    TextField field = new TextField();
    root.add(save, cancel, field);

    List<Button> buttons = ComponentLocator.of(Button.class).within(root).all();

    assertEquals(List.of(save, cancel), buttons);
    assertEquals(2, ComponentLocator.of(Button.class).within(root).count());
    assertTrue(ComponentLocator.of(TextField.class).within(root).isPresent());
  }

  @Test
  void shouldFindTheComponentsTheWrapperBinds() {
    Button save = new Button("Save");
    Composite<Div> panel = new Composite<>() {
      {
        getBoundComponent().add(save);
      }
    };
    Composite<Div> page = new Composite<>() {
      {
        getBoundComponent().add(panel);
      }
    };

    assertEquals(
        List.of(ComponentUtil.getBoundComponent(page), ComponentUtil.getBoundComponent(panel)),
        ComponentLocator.of(Div.class).within(page).all());
    assertSame(save, ComponentLocator.of(Button.class).within(page).locate());
  }

  @Test
  void shouldFindComponentsInsideComposites() {
    Div root = new Div();
    Div container = new Div();
    Button nested = new Button("Nested");
    container.add(nested);
    root.add(container);

    assertSame(nested, ComponentLocator.of(Button.class).within(root).locate());
  }

  @Test
  void shouldFilterByText() {
    Div root = new Div();
    root.add(new Button("Save"), new Button(CANCEL_TEXT));

    assertEquals("Save",
        ComponentLocator.of(Button.class).within(root).byText("Save").locate().getText());
    assertEquals(CANCEL_TEXT,
        ComponentLocator.of(Button.class).within(root).byTextContaining("ance").locate().getText());
  }

  @Test
  void shouldFilterByName() {
    Div root = new Div();
    Button button = new Button("Save");
    button.setName("save-button");
    root.add(button, new Button("Other"));

    assertSame(button,
        ComponentLocator.of(Button.class).within(root).byName("save-button").locate());
  }

  @Test
  void shouldFilterByAttribute() {
    Div root = new Div();
    Button button = new Button("Save");
    button.setAttribute("id", "save");
    root.add(button, new Button("Other"));

    assertSame(button,
        ComponentLocator.of(Button.class).within(root).byAttribute("id", "save").locate());
  }

  @Test
  void shouldFilterByAttributePresence() {
    Div root = new Div();
    Button button = new Button("Save");
    button.setAttribute("id", "save");
    root.add(button, new Button("Other"));

    assertSame(button, ComponentLocator.of(Button.class).within(root).byAttribute("id").locate());
  }

  @Test
  void shouldFilterByLabel() {
    Div root = new Div();
    TextField email = new TextField();
    email.setLabel("Email");
    TextField name = new TextField();
    name.setLabel("Name");
    root.add(email, name);

    assertSame(email, ComponentLocator.of(TextField.class).within(root).byLabel("Email").locate());
  }

  @Test
  void shouldFilterByPlaceholder() {
    Div root = new Div();
    TextField email = new TextField();
    email.setPlaceholder("name@example.com");
    root.add(email, new TextField());

    assertSame(email, ComponentLocator.of(TextField.class).within(root)
        .byPlaceholder("name@example.com").locate());
  }

  @Test
  void shouldFilterByValue() {
    Div root = new Div();
    TextField first = new TextField();
    first.setValue("alpha");
    TextField second = new TextField();
    second.setValue("beta");
    root.add(first, second);

    assertSame(second, ComponentLocator.of(TextField.class).within(root).byValue("beta").locate());
  }

  @Test
  void shouldFilterByTheme() {
    Div root = new Div();
    Button primary = new Button("Save");
    primary.setTheme(ButtonTheme.PRIMARY);
    root.add(primary, new Button("Other"));

    assertSame(primary,
        ComponentLocator.of(Button.class).within(root).byTheme(ButtonTheme.PRIMARY).locate());
  }

  @Test
  void shouldFilterByExpanse() {
    Div root = new Div();
    Button large = new Button("Save");
    large.setExpanse(Expanse.LARGE);
    root.add(large, new Button("Other"));

    assertSame(large,
        ComponentLocator.of(Button.class).within(root).byExpanse(Expanse.LARGE).locate());
  }

  @Test
  void shouldFilterByStyle() {
    Div root = new Div();
    Button red = new Button("Save");
    red.setStyle("color", "red");
    root.add(red, new Button("Other"));

    assertSame(red,
        ComponentLocator.of(Button.class).within(root).byStyle("color", "red").locate());
  }

  @Test
  void shouldFilterByStylePresence() {
    Div root = new Div();
    Div flex = new Div();
    flex.setStyle("display", "flex");
    root.add(flex, new Div());

    assertSame(flex, ComponentLocator.of(Div.class).within(root).byStyle("display").locate());
  }

  @Test
  void shouldFilterByClassName() {
    Div root = new Div();
    Button primary = new Button("Save");
    primary.addClassName("primary");
    root.add(primary, new Button("Other"));

    assertSame(primary,
        ComponentLocator.of(Button.class).within(root).byClassName("primary").locate());
  }

  @Test
  void shouldNotMatchLabelsOfComponentsWithoutLabelSupport() {
    Div root = new Div();
    root.add(new LabelessBox());
    ComponentLocator<LabelessBox> boxes =
        ComponentLocator.of(LabelessBox.class).within(root).byLabel("Email");

    assertDoesNotThrow(boxes::all);
    assertFalse(boxes.isPresent());
  }

  @Test
  void shouldFilterByDescendantMatch() {
    Div root = new Div();
    Div saving = new Div();
    Div cancelling = new Div();
    saving.add(new Button("Save"));
    cancelling.add(new Button(CANCEL_TEXT));
    root.add(saving, cancelling);

    assertEquals(List.of(root, saving), ComponentLocator.of(Div.class).within(root)
        .has(ComponentLocator.of(Button.class).byText("Save")).all());
  }

  @Test
  void shouldNotCountTheCandidateAsItsOwnDescendant() {
    Div root = new Div();
    root.add(new Button("Save"));

    assertFalse(ComponentLocator.of(Button.class).within(root)
        .has(ComponentLocator.of(Button.class)).isPresent());
  }

  @Test
  void shouldFilterByCondition() {
    Div root = new Div();
    Button enabled = new Button("On");
    Button disabled = new Button("Off");
    disabled.setEnabled(false);
    root.add(enabled, disabled);

    assertSame(disabled, ComponentLocator.of(Button.class).within(root)
        .filter(button -> !button.isEnabled()).locate());
  }

  @Test
  void shouldFilterCustomComponentsByTheirOwnState() {
    Div root = new Div();
    CustomPicker sizes = new CustomPicker("small", "large");
    CustomPicker colors = new CustomPicker("red", "green");
    colors.select("green");
    root.add(sizes, colors);

    assertSame(colors, ComponentLocator.of(CustomPicker.class).within(root)
        .filter(picker -> "green".equals(picker.getSelected())).locate());
  }

  @Test
  void shouldScopeQueriesToSubtrees() {
    Div root = new Div();
    Div left = new Div();
    Div right = new Div();
    Button inLeft = new Button("Left");
    Button inRight = new Button("Right");
    left.add(inLeft);
    right.add(inRight);
    root.add(left, right);

    assertSame(inRight, ComponentLocator.of(Button.class).within(right).locate());
    assertEquals(2, ComponentLocator.of(Button.class).within(root).count());
  }

  @Test
  void shouldFailSingleWithTreeOnZeroOrManyMatches() {
    Div root = new Div();
    root.add(new Button("One"), new Button("Two"));
    ComponentLocator<Button> buttons = ComponentLocator.of(Button.class).within(root);
    ComponentLocator<TextField> fields = ComponentLocator.of(TextField.class).within(root);

    IllegalStateException many = assertThrows(IllegalStateException.class, buttons::locate);
    assertTrue(many.getMessage().contains("found 2"));
    assertTrue(many.getMessage().contains("Button"));

    IllegalStateException none = assertThrows(IllegalStateException.class, fields::locate);
    assertTrue(none.getMessage().contains("found 0"));
  }

  @Test
  void shouldFailSingleWithTheWholeTreeWhenNoRootIsGiven() {
    try (MockedStatic<App> app = mockStatic(App.class)) {
      app.when(App::getFrames).thenReturn(List.<Frame>of());
      ComponentLocator<Button> buttons = ComponentLocator.of(Button.class);

      IllegalStateException none = assertThrows(IllegalStateException.class, buttons::locate);

      assertTrue(none.getMessage().contains("found 0"));
      assertTrue(none.getMessage().contains("<no frames>"));
    }
  }

  @Test
  void shouldReturnFirstMatchAsOptional() {
    Div root = new Div();
    root.add(new Button("One"), new Button("Two"));

    assertEquals("One",
        ComponentLocator.of(Button.class).within(root).first().orElseThrow().getText());
    assertTrue(ComponentLocator.of(TextField.class).within(root).first().isEmpty());
    assertFalse(ComponentLocator.of(TextField.class).within(root).isPresent());
  }

  @Test
  void shouldReturnLastMatchAsOptional() {
    Div root = new Div();
    root.add(new Button("One"), new Button("Two"), new Button("Three"));

    assertEquals("Three",
        ComponentLocator.of(Button.class).within(root).last().orElseThrow().getText());
    assertTrue(ComponentLocator.of(TextField.class).within(root).last().isEmpty());
  }

  @Test
  void shouldReturnEmptyResultsWithoutFrames() {
    try (MockedStatic<App> app = mockStatic(App.class)) {
      app.when(App::getFrames).thenReturn(List.<Frame>of());

      assertEquals(List.of(), ComponentLocator.of(Button.class).all());
      assertFalse(ComponentLocator.of(Button.class).isPresent());
    }
  }

  static class LabelessBox extends Composite<Div> implements HasLabel<LabelessBox> {
  }

  @NodeName("x-picker")
  static class CustomPicker extends ElementComposite
      implements SingleSelectable<CustomPicker, String> {
    private final List<String> items;
    private int selectedIndex = -1;

    CustomPicker(String... items) {
      this.items = List.of(items);
    }

    @Override
    public CustomPicker selectIndex(int index) {
      selectedIndex = index >= 0 && index < items.size() ? index : -1;

      return this;
    }

    @Override
    public CustomPicker selectKey(Object key) {
      return selectIndex(items.indexOf(key));
    }

    @Override
    public CustomPicker select(String item) {
      return selectKey(item);
    }

    @Override
    public CustomPicker deselect() {
      return selectIndex(-1);
    }

    @Override
    public int getSelectedIndex() {
      return selectedIndex;
    }

    @Override
    public Object getSelectedKey() {
      return getSelected();
    }

    @Override
    public String getSelected() {
      return selectedIndex == -1 ? null : items.get(selectedIndex);
    }
  }
}
