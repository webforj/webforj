package com.webforj.testbed.snapshot;

import com.webforj.component.Component;
import com.webforj.component.button.Button;
import com.webforj.component.button.ButtonTheme;
import com.webforj.component.element.Element;
import com.webforj.component.field.ColorField;
import com.webforj.component.field.DateField;
import com.webforj.component.field.DateTimeField;
import com.webforj.component.field.MaskedDateField;
import com.webforj.component.field.MaskedDateFieldSpinner;
import com.webforj.component.field.MaskedNumberField;
import com.webforj.component.field.MaskedNumberFieldSpinner;
import com.webforj.component.field.MaskedTextField;
import com.webforj.component.field.MaskedTextFieldSpinner;
import com.webforj.component.field.MaskedTimeField;
import com.webforj.component.field.MaskedTimeFieldSpinner;
import com.webforj.component.field.NumberField;
import com.webforj.component.field.PasswordField;
import com.webforj.component.field.TextArea;
import com.webforj.component.field.TextField;
import com.webforj.component.field.TextFieldSpinner;
import com.webforj.component.field.TimeField;
import com.webforj.component.list.ChoiceBox;
import com.webforj.component.list.ComboBox;
import com.webforj.component.list.ListBox;
import com.webforj.component.list.MultipleSelectableList;
import com.webforj.component.navigator.Navigator;
import com.webforj.component.optiondialog.FileChooserFilter;
import com.webforj.component.optioninput.CheckBox;
import com.webforj.component.optioninput.RadioButton;
import com.webforj.component.optioninput.RadioButtonGroup;
import com.webforj.component.progressbar.ProgressBar;
import com.webforj.component.slider.Slider;
import com.webforj.component.tabbedpane.Tab;
import com.webforj.component.tabbedpane.TabbedPane;
import com.webforj.component.text.Label;
import com.webforj.component.tree.Tree;
import com.webforj.component.tree.TreeNode;
import com.webforj.component.upload.Upload;
import com.webforj.component.window.Frame;
import com.webforj.component.window.Panel;
import com.webforj.testbed.WebforjTestbed;
import java.awt.Color;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.Month;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;

/**
 * One populated instance of every foundation component, with the content its snapshot shows.
 */
final class SnapshotFixtures {
  private static final LocalDate DATE = LocalDate.of(2024, Month.JANUARY, 15);
  private static final LocalTime TIME = LocalTime.of(10, 30);
  private static final Map<Class<? extends Component>, Function<WebforjTestbed, Component>> MAKERS =
      makers();

  private SnapshotFixtures() {}

  static Optional<Component> build(Class<? extends Component> componentClass,
      WebforjTestbed context) {
    Function<WebforjTestbed, Component> maker = MAKERS.get(componentClass);

    return maker == null ? Optional.empty() : Optional.of(maker.apply(context));
  }

  private static Map<Class<? extends Component>, Function<WebforjTestbed, Component>> makers() {
    Map<Class<? extends Component>, Function<WebforjTestbed, Component>> makers =
        new LinkedHashMap<>();
    makers.put(Button.class, SnapshotFixtures::button);
    makers.put(CheckBox.class,
        context -> new CheckBox("Gift wrap").setChecked(true).setIndeterminate(true));
    makers.put(ChoiceBox.class, SnapshotFixtures::choiceBox);
    makers.put(ColorField.class, context -> new ColorField().setValue(Color.RED));
    makers.put(ComboBox.class, SnapshotFixtures::comboBox);
    makers.put(DateField.class, context -> new DateField().setValue(DATE));
    makers.put(DateTimeField.class,
        context -> new DateTimeField().setValue(LocalDateTime.of(DATE, TIME)));
    makers.put(Element.class, SnapshotFixtures::element);
    makers.put(Frame.class, SnapshotFixtures::frame);
    makers.put(Label.class, context -> new Label("Welcome back"));
    makers.put(ListBox.class, SnapshotFixtures::listBox);
    makers.put(MaskedDateField.class, context -> new MaskedDateField().setValue(DATE));
    makers.put(MaskedDateFieldSpinner.class,
        context -> new MaskedDateFieldSpinner().setValue(DATE));
    makers.put(MaskedNumberField.class, context -> new MaskedNumberField().setValue(120.5));
    makers.put(MaskedNumberFieldSpinner.class,
        context -> new MaskedNumberFieldSpinner().setValue(120.5));
    makers.put(MaskedTextField.class,
        context -> new MaskedTextField().setMask("AAA-000").setValue("ABC-123"));
    makers.put(MaskedTextFieldSpinner.class,
        context -> new MaskedTextFieldSpinner().setValue("ABC-123"));
    makers.put(MaskedTimeField.class, context -> new MaskedTimeField().setValue(TIME));
    makers.put(MaskedTimeFieldSpinner.class,
        context -> new MaskedTimeFieldSpinner().setValue(TIME));
    makers.put(Navigator.class,
        context -> named(new Navigator(95, 10), "pages").setAutoDisable(true));
    makers.put(NumberField.class,
        context -> new NumberField().setMin(0d).setMax(100d).setStep(0.5).setValue(42.5));
    makers.put(Panel.class, SnapshotFixtures::panel);
    makers.put(PasswordField.class, SnapshotFixtures::passwordField);
    makers.put(ProgressBar.class,
        context -> new ProgressBar().setValue(40).setText("Syncing").setIndeterminate(true));
    makers.put(RadioButton.class, context -> new RadioButton("Large").setChecked(true));
    makers.put(RadioButtonGroup.class, context -> new RadioButtonGroup("size",
        new RadioButton("Large"), new RadioButton("Small")));
    makers.put(Slider.class, context -> new Slider(30, Slider.Orientation.VERTICAL));
    makers.put(TabbedPane.class, SnapshotFixtures::tabbedPane);
    makers.put(TextArea.class, context -> new TextArea().setValue("Notes\nSecond line"));
    makers.put(TextField.class, SnapshotFixtures::textField);
    makers.put(TextFieldSpinner.class, context -> new TextFieldSpinner().setValue("Ada"));
    makers.put(TimeField.class, context -> new TimeField().setValue(TIME));
    makers.put(Tree.class, SnapshotFixtures::tree);
    makers.put(Upload.class, SnapshotFixtures::upload);

    return makers;
  }

  private static <T extends Component> T named(T component, String name) {
    component.setName(name);

    return component;
  }

  private static Component choiceBox(WebforjTestbed context) {
    ChoiceBox choiceBox = named(new ChoiceBox(), "country");
    choiceBox.setLabel("Country");
    choiceBox.add("de", "Germany");
    choiceBox.add("fr", "France");
    choiceBox.selectKey("de");

    return choiceBox;
  }

  private static Component comboBox(WebforjTestbed context) {
    ComboBox comboBox = named(new ComboBox(), "customer");
    comboBox.add("ada", "Ada");
    comboBox.add("linus", "Linus");
    comboBox.setText("Ad");

    return comboBox;
  }

  private static Component element(WebforjTestbed context) {
    Element section = new Element("section");
    section.setAttribute("role", "region");
    section.add(new Button("Inside"));

    return section;
  }

  private static Component frame(WebforjTestbed context) {
    Frame frame = context.getFrame();
    frame.setTitle("Orders");
    frame.add(new Label("In the frame"));

    return frame;
  }

  private static Component listBox(WebforjTestbed context) {
    ListBox listBox = named(new ListBox(), "fruits");
    listBox.setSelectionMode(MultipleSelectableList.SelectionMode.MULTIPLE);
    listBox.add("apple", "Apple");
    listBox.add("pear", "Pear");
    listBox.add("plum", "Plum");
    listBox.selectKey("pear", "plum");

    return listBox;
  }

  private static Component panel(WebforjTestbed context) {
    Panel panel = named(new Panel(), "sidebar");
    panel.add(new Label("Menu"), new Button("Orders"));

    return panel;
  }

  private static Component passwordField(WebforjTestbed context) {
    PasswordField field = named(new PasswordField(), "pin");
    field.setLabel("PIN");
    field.setRequired(true);
    field.setValue("secret");

    return field;
  }

  private static Component tabbedPane(WebforjTestbed context) {
    TabbedPane pane = named(new TabbedPane(), "main");
    Tab orders = new Tab("Orders");
    orders.setSuffixComponent(new Label("3"));
    orders.setTooltip("Open orders");
    pane.addTab(orders, new Label("orders here"));
    Tab invoices = new Tab("Invoices");
    invoices.setClosable(true);
    pane.addTab(invoices, new Label("invoices here"));
    Tab archive = new Tab("Archive");
    archive.setEnabled(false);
    pane.addTab(archive, new Label("archived"));

    return pane;
  }

  private static Component upload(WebforjTestbed context) {
    Upload upload = named(new Upload(), "attachments");
    FileChooserFilter documents = new FileChooserFilter("Documents", "*.pdf;*.docx");
    upload.setFilters(List.of(documents, new FileChooserFilter("Images", "*.png")));
    upload.setActiveFilter(documents);
    upload.setSelectionMode(Upload.SelectionMode.MULTIPLE);
    upload.setMaxFiles(3);
    upload.setDrop(true);

    return upload;
  }

  private static Component textField(WebforjTestbed context) {
    TextField field = named(new TextField(), "customer");
    field.setLabel("Customer");
    field.setPlaceholder("Full name");
    field.setValue("Ada");
    field.setHelperText("As printed on the invoice");
    field.setTooltipText("The billing contact");
    field.setSuffixComponent(new Label("@"));

    return field;
  }

  private static Component button(WebforjTestbed context) {
    Button button = named(new Button("Save"), "save");
    button.setTheme(ButtonTheme.PRIMARY);
    button.setPrefixComponent(new Label("+"));

    return button;
  }

  private static Component tree(WebforjTestbed context) {
    Tree tree = named(new Tree(), "folders");
    TreeNode year = Tree.node("2025", "2025");
    TreeNode quarter = Tree.node("q1", "Q1");
    year.add(quarter);
    tree.add(year, Tree.node("2024", "2024"));
    context.add(tree);
    tree.expand(year);
    tree.select(quarter);

    return tree;
  }
}
