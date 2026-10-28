package com.webforj.testbed.assertion;

import static com.webforj.testbed.assertion.WebforjAssertions.verifyThat;
import static com.webforj.testbed.fixture.CustomPickerAssert.verifyThat;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.automation.act.UserInteraction;
import com.webforj.component.Expanse;
import com.webforj.component.button.Button;
import com.webforj.component.button.ButtonTheme;
import com.webforj.component.field.TextField;
import com.webforj.component.html.elements.Div;
import com.webforj.component.html.elements.Paragraph;
import com.webforj.component.list.ChoiceBox;
import com.webforj.component.list.ListBox;
import com.webforj.component.list.ListItem;
import com.webforj.component.table.Table;
import com.webforj.testbed.TestbedTestSupport;
import com.webforj.testbed.WebforjTestbed;
import com.webforj.testbed.fixture.CustomPicker;
import com.webforj.testbed.fixture.CustomPickerAssert;
import com.webforj.testbed.fixture.PlainComponent;
import java.util.List;
import org.junit.jupiter.api.Test;

class WebforjAssertionsTest extends TestbedTestSupport {

  @Test
  void shouldChainComponentAssertions() {
    Button button = new Button("Save");
    button.setName("save-button");
    button.setAttribute("id", "save");
    button.setStyle("color", "red");
    button.setTheme(ButtonTheme.PRIMARY);
    button.setExpanse(Expanse.LARGE);
    button.setTooltipText("Saves the form");
    WebforjTestbed.getCurrent().add(button);

    verifyThat(button).isAttached().isVisible().isEnabled().hasText("Save").hasTextContaining("Sav")
        .hasName("save-button").hasAttribute("id", "save").hasStyle("color", "red")
        .hasTheme(ButtonTheme.PRIMARY).hasExpanse(Expanse.LARGE).hasTooltipText("Saves the form");
  }

  @Test
  void shouldAssertNegativeStates() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    Button button = new Button("Save");

    verifyThat(button).isNotAttached();

    context.add(button);
    button.setVisible(false);
    button.setEnabled(false);

    verifyThat(button).isHidden().isDisabled();

    button.destroy();

    verifyThat(button).isDestroyed();
  }

  @Test
  void shouldFailWithClearMessages() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    Button button = new Button("Save");
    context.add(button);

    ComponentAssert assertion = verifyThat(button);
    AssertionError textFailure =
        assertThrows(AssertionError.class, () -> assertion.hasText("Cancel"));
    assertTrue(textFailure.getMessage().contains("Cancel"));
    assertTrue(textFailure.getMessage().contains("Save"));

    assertThrows(AssertionError.class, assertion::isHidden);
    assertThrows(AssertionError.class, assertion::isDisabled);
    assertThrows(AssertionError.class, assertion::isNotAttached);
    assertThrows(AssertionError.class, assertion::isDestroyed);
    assertThrows(AssertionError.class, () -> assertion.hasName("other"));
    assertThrows(AssertionError.class, () -> assertion.hasAttribute("id", "other"));
    assertThrows(AssertionError.class, () -> assertion.hasStyle("color", "red"));
    assertThrows(AssertionError.class, () -> assertion.hasTextContaining("xyz"));
    assertThrows(AssertionError.class, () -> assertion.hasTheme(ButtonTheme.DANGER));
    assertThrows(AssertionError.class, () -> assertion.hasExpanse(Expanse.SMALL));
    assertThrows(AssertionError.class, () -> assertion.hasTooltipText("other"));
  }

  @Test
  void shouldAssertFieldState() {
    TextField field = new TextField();
    field.setLabel("Name");
    field.setPlaceholder("Your name");
    field.setHelperText("As on the passport");
    field.setRequired(true);
    field.setInvalid(true);
    field.setInvalidMessage("Required");
    WebforjTestbed.getCurrent().add(field);
    field.setValue("typed");

    verifyThat(field).hasValue("typed").hasLabel("Name").hasPlaceholder("Your name")
        .hasHelperText("As on the passport").isRequired().isEditable().isInvalid()
        .hasInvalidMessage("Required").hasProperty("label", "Name");

    field.setReadOnly(true);
    field.setRequired(false);
    field.setInvalid(false);

    verifyThat(field).isReadOnly().isOptional().isValid();

    ComponentAssert assertion = verifyThat(field);
    assertThrows(AssertionError.class, () -> assertion.hasValue("other"));
    assertThrows(AssertionError.class, () -> assertion.hasLabel("other"));
    assertThrows(AssertionError.class, () -> assertion.hasPlaceholder("other"));
    assertThrows(AssertionError.class, () -> assertion.hasHelperText("other"));
    assertThrows(AssertionError.class, assertion::isRequired);
    assertThrows(AssertionError.class, assertion::isEditable);
    assertThrows(AssertionError.class, assertion::isInvalid);
    assertThrows(AssertionError.class, () -> assertion.hasInvalidMessage("other"));
    assertThrows(AssertionError.class, () -> assertion.hasProperty("label", "other"));
  }

  @Test
  void shouldAssertTheFocusTheUserMoved() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    TextField name = new TextField();
    TextField email = new TextField();
    context.add(name, email);

    UserInteraction.focus(name);

    verifyThat(name).isFocused();
    verifyThat(email).isNotFocused();

    UserInteraction.focus(email);

    verifyThat(name).isNotFocused();
    verifyThat(email).isFocused();

    verifyThat(email).isFocused();
    ComponentAssert emailAssertion = verifyThat(email);
    ComponentAssert nameAssertion = verifyThat(name);
    assertThrows(AssertionError.class, emailAssertion::isNotFocused);
    assertThrows(AssertionError.class, nameAssertion::isFocused);
  }

  @Test
  void shouldAssertChildrenAndHtml() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    Div div = new Div();
    Paragraph paragraph = new Paragraph("hello");
    div.add(paragraph);
    div.setHtml("<b>hi</b>");
    context.add(div);

    verifyThat(div).hasComponent(paragraph).hasComponentCount(1).hasHtml("<b>hi</b>");

    ComponentAssert assertion = verifyThat(div);
    Paragraph otherParagraph = new Paragraph();
    assertThrows(AssertionError.class, () -> assertion.hasComponent(otherParagraph));
    assertThrows(AssertionError.class, () -> assertion.hasComponentCount(2));
    assertThrows(AssertionError.class, () -> assertion.hasHtml("<i>hi</i>"));
  }

  @Test
  void shouldFailNamingTheMissingContract() {
    PlainComponent component = new PlainComponent();

    ComponentAssert assertion = verifyThat(component);
    assertTrue(assertThrows(AssertionError.class, assertion::isVisible).getMessage()
        .contains("HasVisibility"));
    assertTrue(assertThrows(AssertionError.class, () -> assertion.hasValue("x")).getMessage()
        .contains("ValueAware"));
    assertTrue(assertThrows(AssertionError.class, () -> assertion.hasItemCount(1)).getMessage()
        .contains("HasRepository"));
    assertTrue(assertThrows(AssertionError.class, () -> assertion.hasSelected("x")).getMessage()
        .contains("ItemSingleSelectionAware"));
    assertTrue(assertThrows(AssertionError.class, assertion::isFocused).getMessage()
        .contains("HasFocusStatus"));
  }

  @Test
  void shouldAssertItemsAndSelectionThroughTheDataContracts() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    Table<String> table = new Table<>();
    table.addColumn("name", item -> item);
    table.setItems(List.of("Ada", "Alan"));
    context.add(table);

    verifyThat(table).hasItemCount(2).hasNoSelection();

    table.select("Ada");

    verifyThat(table).hasSelected("Ada");

    ComponentAssert assertion = verifyThat(table);
    assertThrows(AssertionError.class, () -> assertion.hasItemCount(5));
    assertThrows(AssertionError.class, () -> assertion.hasSelected("Nobody"));
    assertThrows(AssertionError.class, assertion::hasNoSelection);
  }

  @Test
  void shouldAssertSelectionByIndexAndKey() {
    ListBox listBox = new ListBox();
    listBox.add("apple", "Apple");
    listBox.add("pear", "Pear");
    ChoiceBox choiceBox = new ChoiceBox();
    choiceBox.add("one", "One");
    choiceBox.add("two", "Two");
    WebforjTestbed.getCurrent().add(listBox, choiceBox);

    verifyThat(listBox).hasNoSelection();
    verifyThat(choiceBox).hasNoSelection();

    ListItem pear = listBox.getByKey("pear");
    ListItem two = choiceBox.getByKey("two");
    listBox.select(pear);
    choiceBox.select(two);

    verifyThat(listBox).hasSelected(pear).hasSelectedIndex(1).hasSelectedKey("pear");
    verifyThat(choiceBox).hasSelected(two).hasSelectedIndex(1).hasSelectedKey("two");

    ComponentAssert listBoxAssertion = verifyThat(listBox);
    ComponentAssert choiceBoxAssertion = verifyThat(choiceBox);
    assertThrows(AssertionError.class, () -> listBoxAssertion.hasSelectedIndex(0));
    assertThrows(AssertionError.class, () -> listBoxAssertion.hasSelectedKey("apple"));
    assertThrows(AssertionError.class, () -> choiceBoxAssertion.hasSelectedIndex(0));
    assertThrows(AssertionError.class, () -> choiceBoxAssertion.hasSelectedKey("one"));
    assertThrows(AssertionError.class, choiceBoxAssertion::hasNoSelection);
  }

  @Test
  void shouldAssertCustomComponentsThroughTheSameContracts() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    CustomPicker picker = new CustomPicker("red", "green");
    context.add(picker);

    verifyThat(picker).hasNoSelection();

    picker.select("green");

    verifyThat(picker).hasSelected("green").hasSelectedIndex(1).hasSelectedKey("green");
    CustomPickerAssert assertion = verifyThat(picker);
    assertThrows(AssertionError.class, () -> assertion.hasSelected("red"));
  }

  @Test
  void shouldChainTheAssertionsOfCustomComponentsWithTheSharedOnes() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    CustomPicker picker = new CustomPicker("red", "green");
    context.add(picker);
    picker.select("green");

    verifyThat(picker).isAttached().hasPicked("green").hasSelectedIndex(1);
    CustomPickerAssert assertion = verifyThat(picker);
    AssertionError failure = assertThrows(AssertionError.class, () -> assertion.hasPicked("red"));

    assertTrue(failure.getMessage().contains("Expected CustomPicker to have picked red"));
  }

  @Test
  void shouldAssertIterableComponentsNextToPlainAssertions() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    ListBox listBox = new ListBox();
    listBox.add("apple", "Apple");
    ListItem pear = listBox.add("pear", "Pear");
    context.add(listBox);
    listBox.select(pear);

    verifyThat(listBox).isAttached().hasSelected(pear);
    assertThat(listBox).hasSize(2).contains(pear);
  }
}
