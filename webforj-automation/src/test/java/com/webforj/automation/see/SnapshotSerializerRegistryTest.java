package com.webforj.automation.see;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import com.webforj.Environment;
import com.webforj.automation.FrameEnvironment;
import com.webforj.automation.ObjectTableEnvironment;
import com.webforj.component.Component;
import com.webforj.component.Composite;
import com.webforj.component.Expanse;
import com.webforj.component.avatar.Avatar;
import com.webforj.component.button.Button;
import com.webforj.component.button.ButtonTheme;
import com.webforj.component.card.Card;
import com.webforj.component.element.Element;
import com.webforj.component.field.MaskedTextField;
import com.webforj.component.field.NumberField;
import com.webforj.component.field.PasswordField;
import com.webforj.component.field.TextField;
import com.webforj.component.html.elements.Div;
import com.webforj.component.html.elements.Paragraph;
import com.webforj.component.html.elements.Span;
import com.webforj.component.layout.appnav.AppNavItem;
import com.webforj.component.layout.appnav.AppNavLabel;
import com.webforj.component.layout.flexlayout.FlexLayout;
import com.webforj.component.list.ChoiceBox;
import com.webforj.component.login.Login;
import com.webforj.component.navigator.Navigator;
import com.webforj.component.optiondialog.FileChooserFilter;
import com.webforj.component.optioninput.CheckBox;
import com.webforj.component.tabbedpane.TabbedPane;
import com.webforj.component.text.Label;
import com.webforj.component.upload.Upload;
import com.webforj.concern.HasExpanse;
import com.webforj.concern.HasText;
import com.webforj.router.RouteRegistry;
import com.webforj.router.Router;
import com.webforj.router.annotation.Route;
import java.util.List;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.MockedStatic;

class SnapshotSerializerRegistryTest {

  @Nested
  class Routes {

    @Test
    void shouldPrintTheRouteTheViewIsRegisteredUnder() throws Exception {
      RouteRegistry registry = new RouteRegistry();
      registry.register("/orders", OrdersPanel.class);
      Router router = mock(Router.class);
      when(router.getRegistry()).thenReturn(registry);
      Environment environment = FrameEnvironment.create(router);

      try (MockedStatic<Environment> current = mockStatic(Environment.class)) {
        current.when(Environment::getCurrent).thenReturn(environment);

        assertEquals("- OrdersPanel [route=/orders]\n", Snapshot.render(new OrdersPanel()));
        assertEquals("- Div\n", Snapshot.render(new Div()));
      }
    }

    @Test
    void shouldPrintTheRouteTheViewDeclaresWithoutRouter() {
      assertEquals("- InvoicesView [route=/invoices]\n", Snapshot.render(new InvoicesView()));
    }

    @Test
    void shouldPrintNoRouteForTheComponentThatIsNoRoute() {
      assertEquals("- OrdersPanel\n", Snapshot.render(new OrdersPanel()));
    }
  }

  @Nested
  class Composites {

    @Test
    void shouldPrintTheBoundComponentAndTheNodeTheChainEndsIn() {

      SnapshotOptions composites = new SnapshotOptions().setComposites(true);

      assertEquals("- Div [composite=Element] [node=div]\n",
          Snapshot.render(new Div(), composites));
      assertEquals("- Element [node=section]\n",
          Snapshot.render(new Element("section"), composites));
      assertEquals("- Button \"Save\"\n", Snapshot.render(new Button("Save"), composites));
      Composite<FlexLayout> toolbar = new Composite<>() {};
      assertEquals(
          "- " + toolbar.getClass().getSimpleName() + " [composite=FlexLayout] [node=div]\n",
          Snapshot.render(toolbar, composites));
      Composite<Button> action = new Composite<>() {};
      assertEquals("- " + action.getClass().getSimpleName() + " [composite=Button]\n",
          Snapshot.render(action, composites));
    }

    @Test
    void shouldLeaveTheBoundComponentOutByDefault() {
      Composite<FlexLayout> toolbar = new Composite<>() {};

      assertEquals("- Div\n", Snapshot.render(new Div()));
      assertEquals("- " + toolbar.getClass().getSimpleName() + "\n", Snapshot.render(toolbar));
    }
  }

  static class OrdersPanel extends Div {
  }

  @Route("/invoices")
  static class InvoicesView extends Div {
  }

  @Nested
  class ApplicationSerializers {

    @Test
    void shouldRunAfterTheContractsInTheOrderOfTheirContractNames() {
      SnapshotSerializerRegistry serializers = new SnapshotSerializerRegistry(
          List.of(new TextMarker("+text"), new ComponentMarker("+component")));
      SnapshotRenderer renderer = new SnapshotRenderer(serializers, new SnapshotOptions());

      SnapshotNode node = renderer.describe(new Button("Save"));

      assertEquals("Save+component+text", node.getText());
    }

    @Test
    void shouldLoadTheOnesThatLoadWhenAnotherCannotLoad() {
      List<SnapshotSerializer<?>> applicable =
          SnapshotSerializerRegistry.getCurrent().forComponent(new Gauge("bar"));

      assertTrue(applicable.stream().anyMatch(GaugeSerializer.class::isInstance));
    }

    @Test
    void shouldRejectTwoForOneContract() {
      List<SnapshotSerializer<?>> serializers = List.of(new TextMarker("+a"), new TextMarker("+b"));

      IllegalStateException failure = assertThrows(IllegalStateException.class,
          () -> new SnapshotSerializerRegistry(serializers));

      assertEquals("The snapshot serializers " + TextMarker.class.getName() + " and "
          + TextMarker.class.getName() + " both handle " + HasText.class.getName()
          + ". Register one serializer per contract.", failure.getMessage());
    }
  }

  @Nested
  class CurrentRegistry {

    @Test
    void shouldReturnTheRegistryTheAppHoldsInItsObjectTable() throws Exception {
      Environment environment = ObjectTableEnvironment.create();

      try (MockedStatic<Environment> current = mockStatic(Environment.class, CALLS_REAL_METHODS)) {
        current.when(Environment::getCurrent).thenReturn(environment);

        assertSame(SnapshotSerializerRegistry.getCurrent(),
            SnapshotSerializerRegistry.getCurrent());
      }
    }

    @Test
    void shouldReturnNewRegistryWhenTheAppHoldsNone() {
      assertNotSame(SnapshotSerializerRegistry.getCurrent(),
          SnapshotSerializerRegistry.getCurrent());
    }
  }

  @Nested
  class ContractBindings {

    @Test
    void shouldPrintTheNodeNameOfAnElement() {
      assertEquals("- Element [node=x-card]\n", Snapshot.render(new Element("x-card")));
    }

    @Test
    void shouldPrintTheTextWithBrowserState() {
      Paragraph note = new Paragraph("Totals follow");

      assertEquals("- Paragraph \"Totals follow\"\n", Snapshot.render(note, createBrowserState()));
    }

    @Test
    void shouldLeaveOutTheTextByDefault() {
      assertEquals("- Paragraph\n", Snapshot.render(new Paragraph("Note")));
    }

    @Test
    void shouldPrintTheTextBesideTheChildrenInNamedSlotsWithBrowserState() {
      AppNavLabel analytics = new AppNavLabel("Analytics", new Label("*"));
      AppNavItem dashboard = new AppNavItem("Dashboard", "/dashboard");
      dashboard.addItem(new AppNavItem("Reports", "/reports"));
      Paragraph note = new Paragraph("Due");
      note.add(new Span("today"));

      assertEquals("""
          - FlexLayout:
            - AppNavLabel "Analytics":
              - Label "*" [slot=prefix]
            - AppNavItem "Dashboard" [path=/dashboard]:
              - AppNavItem "Reports" [slot=items] [path=/reports]
            - Paragraph:
              - Span "today"
          """, Snapshot.render(new FlexLayout(analytics, dashboard, note), createBrowserState()));
    }

    @Test
    void shouldPrintTheNameUnlessGeneratedOrRepeatingTheText() {
      Button save = new Button("Save");
      save.setName("save");
      Button open = new Button("Open");
      open.setName("123e4567-e89b-12d3-a456-426614174000");
      Button close = new Button("Close");
      close.setName("Close");

      assertEquals("""
          - FlexLayout:
            - Button "Save" [name=save]
            - Button "Open"
            - Button "Close"
          """, Snapshot.render(new FlexLayout(save, open, close)));
    }

    @Test
    void shouldPrintTheNamedSlotOfTheChildOfAnElement() {
      Element card = new Element("x-card");
      card.add("footer", new Button("Close"));
      card.add(new Button("Open"));

      assertEquals("""
          - Element [node=x-card]:
            - Button "Close" [slot=footer]
            - Button "Open"
          """, Snapshot.render(card));
    }

    @Test
    void shouldPrintTheLabel() {
      TextField customer = new TextField();
      customer.setLabel("Customer");

      assertEquals("- TextField [label=Customer]\n", Snapshot.render(customer));
    }

    @Test
    void shouldPrintThePlaceholder() {
      TextField customer = new TextField();
      customer.setPlaceholder("Full name");

      assertEquals("- TextField [placeholder=\"Full name\"]\n", Snapshot.render(customer));
    }

    @Test
    void shouldPrintTheValueWithBrowserStateUnlessFalse() {
      CheckBox gift = new CheckBox("Gift wrap");
      gift.setChecked(true);
      CheckBox card = new CheckBox("Card");

      assertEquals("""
          - FlexLayout:
            - CheckBox "Gift wrap" [value=true]
            - CheckBox "Card"
          """, Snapshot.render(new FlexLayout(gift, card), createBrowserState()));
    }

    @Test
    void shouldFlagTheHiddenComponentWhenHiddenComponentsAreAsked() {
      Button save = new Button("Save");
      save.setVisible(false);

      assertEquals("- Button \"Save\" [hidden]\n",
          Snapshot.render(save, new SnapshotOptions().setHiddenComponents(true)));
    }

    @Test
    void shouldFlagTheDisabledComponent() {
      Button save = new Button("Save");
      save.setEnabled(false);

      assertEquals("- Button \"Save\" [disabled]\n", Snapshot.render(save));
    }

    @Test
    void shouldFlagTheReadOnlyComponent() {
      TextField customer = new TextField();
      customer.setReadOnly(true);

      assertEquals("- TextField [readonly]\n", Snapshot.render(customer));
    }

    @Test
    void shouldFlagTheRequiredComponent() {
      TextField customer = new TextField();
      customer.setRequired(true);

      assertEquals("- TextField [required]\n", Snapshot.render(customer));
    }

    @Test
    void shouldLeaveTheValidationStateOutOfTheDetachedComponent() {
      TextField customer = new TextField();
      customer.setInvalid(true);
      customer.setInvalidMessage("Required");

      assertEquals("- TextField\n", Snapshot.render(customer, createBrowserState()));
    }

    @Test
    void shouldFlagTheFocusedComponentWithBrowserState() {
      Gauge gauge = new Gauge("bar").setFocused(true);

      assertEquals("""
          - Gauge [level=0] [needle=0] [focused]:
            - Scale "bar"
          """, Snapshot.render(gauge, createBrowserState()));
    }

    @Test
    void shouldPrintTheHelperText() {
      TextField customer = new TextField();
      customer.setHelperText("As on the card");

      assertEquals("- TextField [helperText=\"As on the card\"]\n", Snapshot.render(customer));
    }

    @Test
    void shouldPrintTheTooltip() {
      Button save = new Button("Save");
      save.setTooltipText("Stores the order");

      assertEquals("- Button \"Save\" [tooltip=\"Stores the order\"]\n", Snapshot.render(save));
    }

    @Test
    void shouldPrintTheMaskUnlessItAcceptsAnyCharacter() {
      MaskedTextField phone = new MaskedTextField();
      phone.setMask("(000) 000");
      MaskedTextField code = new MaskedTextField();
      code.setMask("XXXX");

      assertEquals("""
          - FlexLayout:
            - MaskedTextField [mask="(000) 000"]
            - MaskedTextField
          """, Snapshot.render(new FlexLayout(phone, code)));
    }

    @Test
    void shouldPrintTheMinimum() {
      NumberField amount = new NumberField();
      amount.setMin(1.0);

      assertEquals("- NumberField [min=1.0]\n", Snapshot.render(amount));
    }

    @Test
    void shouldPrintTheMaximum() {
      NumberField amount = new NumberField();
      amount.setMax(9.0);

      assertEquals("- NumberField [max=9.0]\n", Snapshot.render(amount));
    }

    @Test
    void shouldPrintTheStep() {
      NumberField amount = new NumberField();
      amount.setStep(0.5);

      assertEquals("- NumberField [step=0.5]\n", Snapshot.render(amount));
    }

    @Test
    void shouldPrintTheThemeUnlessDefault() {
      Button save = new Button("Save", ButtonTheme.OUTLINED_PRIMARY);
      Button open = new Button("Open", ButtonTheme.DEFAULT);

      assertEquals("""
          - FlexLayout:
            - Button "Save" [theme=outlined-primary]
            - Button "Open"
          """, Snapshot.render(new FlexLayout(save, open)));
    }

    @Test
    void shouldPrintTheExpanseUnlessMediumOrNone() {
      Button small = new Button("Small");
      small.setExpanse(Expanse.SMALL);
      Button medium = new Button("Medium");
      medium.setExpanse(Expanse.MEDIUM);
      Button none = new Button("None");
      none.setExpanse(Expanse.NONE);
      Button large = new Button("Large");
      large.setExpanse(Expanse.XLARGE);

      assertEquals("""
          - FlexLayout:
            - Button "Small" [expanse=small]
            - Button "Medium"
            - Button "None"
            - Button "Large" [expanse=xlarge]
          """, Snapshot.render(new FlexLayout(small, medium, none, large)));
    }

    @Test
    void shouldRestEveryComponentWithoutItsOwnExpanseRuleAtMedium() {
      List<HasExpanse<?, ?>> components = List.of(new Button(), new TextField(), new ChoiceBox(),
          new CheckBox(), new TabbedPane(), new Navigator(), new Card(), new Avatar());

      for (HasExpanse<?, ?> component : components) {
        assertEquals("MEDIUM", ((Enum<?>) component.getExpanse()).name(),
            component.getClass().getSimpleName());
      }
    }

    @Test
    void shouldAddThePrefixComponent() {
      TextField amount = new TextField();
      amount.setPrefixComponent(new Label("$"));

      assertEquals("""
          - TextField:
            - Label "$" [slot=prefix]
          """, Snapshot.render(amount));
    }

    @Test
    void shouldAddTheSuffixComponent() {
      TextField amount = new TextField();
      amount.setSuffixComponent(new Label("EUR"));

      assertEquals("""
          - TextField:
            - Label "EUR" [slot=suffix]
          """, Snapshot.render(amount));
    }

    @Test
    void shouldPrintTheSlotOfThePrefixBeforeTheOtherAttributes() {
      Button currency = new Button("$", ButtonTheme.PRIMARY);
      currency.setName("currency");
      TextField amount = new TextField();
      amount.setPrefixComponent(currency);

      assertEquals("""
          - TextField:
            - Button "$" [slot=prefix] [name=currency] [theme=primary]
          """, Snapshot.render(amount));
    }

    @Test
    void shouldAddThePrefixOnlyOnceWhenTheTreeHoldsItAlready() {
      AppNavItem home = new AppNavItem("Home", "/");
      home.setPrefixComponent(new Label("*"));

      assertEquals("""
          - AppNavItem [path=/]:
            - Label "*" [slot=prefix]
          """, Snapshot.render(home));
    }

    @Test
    void shouldFlagTheActiveFileChooserFilter() {
      FileChooserFilter images = new FileChooserFilter("Images", "*.png");
      Upload upload = new Upload(List.of(new FileChooserFilter("Documents", "*.pdf"), images));
      upload.setActiveFilter(images);
      upload.setDrop(false);

      assertEquals("""
          - Upload [multiple]:
            - Filter "Documents" [pattern="*.pdf"]
            - Filter "Images" [pattern="*.png"] [selected]
          """, Snapshot.render(upload));
    }

    @Test
    void shouldFlagTheFileDrop() {
      Upload upload = new Upload();
      upload.setDrop(true);

      assertEquals("- Upload [drop] [multiple]\n", Snapshot.render(upload));
    }

    @Test
    void shouldPrintTheMaximumNumberOfFiles() {
      Upload upload = new Upload();
      upload.setDrop(false);
      upload.setMaxFiles(3);

      assertEquals("- Upload [maxFiles=3] [multiple]\n", Snapshot.render(upload));
    }

    @Test
    void shouldAddTheFileChooserFilters() {
      Upload upload = new Upload(List.of(new FileChooserFilter("Images", "*.png")));
      upload.setDrop(false);

      assertEquals("""
          - Upload [multiple]:
            - Filter "Images" [pattern="*.png"]
          """, Snapshot.render(upload));
    }
  }

  @Nested
  class ClientHeldEnablement {

    @Test
    void shouldLeaveTheDisabledFlagOfLoginOutByDefault() {
      Login login = new Login();
      login.setEnabled(false);

      assertEquals("- Login\n", Snapshot.render(login));
    }

    @Test
    void shouldFlagTheDisabledLoginWithBrowserState() {
      Login login = new Login();
      login.setEnabled(false);
      login.close();

      assertEquals("- Login [disabled]\n", Snapshot.render(login, createBrowserState()));
    }
  }

  @Nested
  class Passwords {

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void shouldNeverPrintTheValueOrTheText(boolean browserState) {
      PasswordField pin = new PasswordField();
      pin.setLabel("PIN");
      pin.setValue("4711");

      assertEquals("- PasswordField [label=PIN]\n",
          Snapshot.render(pin, new SnapshotOptions().setBrowserState(browserState)));
    }
  }

  private static SnapshotOptions createBrowserState() {
    return new SnapshotOptions().setBrowserState(true);
  }

  private static final class TextMarker implements SnapshotSerializer<HasText<?>> {
    private final String marker;

    private TextMarker(String marker) {
      this.marker = marker;
    }

    @Override
    @SuppressWarnings("unchecked")
    public Class<HasText<?>> getContract() {
      return (Class<HasText<?>>) (Class<?>) HasText.class;
    }

    @Override
    public void describe(HasText<?> target, SnapshotNode node) {
      node.setText(node.getText() + marker);
    }
  }

  private static final class ComponentMarker implements SnapshotSerializer<Component> {
    private final String marker;

    private ComponentMarker(String marker) {
      this.marker = marker;
    }

    @Override
    public Class<Component> getContract() {
      return Component.class;
    }

    @Override
    public void describe(Component target, SnapshotNode node) {
      node.setText(node.getText() + marker);
    }
  }
}
