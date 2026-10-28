package com.webforj.automation.see;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.Theme;
import com.webforj.component.alert.Alert;
import com.webforj.component.button.Button;
import com.webforj.component.field.TextField;
import com.webforj.component.html.elements.Div;
import com.webforj.component.html.elements.Paragraph;
import com.webforj.component.layout.flexlayout.FlexLayout;
import com.webforj.component.optioninput.CheckBox;
import com.webforj.component.text.Label;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class SnapshotTest {

  @Nested
  class Levels {

    @Test
    void shouldDescribeOnlyWhatTheServerHoldsByDefault() {
      FlexLayout form = createForm();

      assertEquals("""
          - FlexLayout:
            - TextField [label=Customer] [placeholder="Full name"] [required]
            - CheckBox "Gift wrap"
            - Button "Save"
            - Paragraph
            - Button "Delete" [disabled]
          """, Snapshot.render(form));
    }

    @Test
    void shouldAddWhatTheBrowserHoldsWithBrowserState() {
      FlexLayout form = createForm();

      assertEquals("""
          - FlexLayout:
            - TextField "Ada" [label=Customer] [placeholder="Full name"] [required]
            - CheckBox "Gift wrap" [value=true]
            - Button "Save"
            - Paragraph "Totals follow"
            - Button "Delete" [disabled]
          """, Snapshot.render(form, new SnapshotOptions().setBrowserState(true)));
    }

    private static FlexLayout createForm() {
      TextField customer = new TextField();
      customer.setLabel("Customer");
      customer.setPlaceholder("Full name");
      customer.setRequired(true);
      customer.setValue("Ada");
      CheckBox gift = new CheckBox("Gift wrap");
      gift.setChecked(true);
      Button delete = new Button("Delete");
      delete.setEnabled(false);

      return new FlexLayout(customer, gift, new Button("Save"), new Paragraph("Totals follow"),
          delete);
    }
  }

  @Nested
  class ClientHeldVisibility {

    @Test
    void shouldDescribeTheClosedAlertByDefault() {
      FlexLayout page = new FlexLayout(createClosedAlert(), new Button("Save"));

      assertEquals("""
          - FlexLayout:
            - Alert "Saved"
            - Button "Save"
          """, Snapshot.render(page));
    }

    @Test
    void shouldLeaveOutTheClosedAlertWithBrowserState() {
      FlexLayout page = new FlexLayout(createClosedAlert(), new Button("Save"));

      assertEquals("""
          - FlexLayout:
            - Button "Save"
          """, Snapshot.render(page, new SnapshotOptions().setBrowserState(true)));
    }

    private static Alert createClosedAlert() {
      Alert alert = new Alert("Saved", Theme.DEFAULT, true);
      alert.close();

      return alert;
    }
  }

  @Nested
  class ContainerText {

    @Test
    void shouldLeaveOutTheTextOfTheComponentThatHoldsOthers() {
      Div card = new Div(new Paragraph("Totals follow"));
      card.setText("Totals follow");

      assertEquals("""
          - Div:
            - Paragraph "Totals follow"
          """, Snapshot.render(card, new SnapshotOptions().setBrowserState(true)));
    }

    @Test
    void shouldPrintTheTextOfTheComponentThatHoldsNothing() {
      Div note = new Div();
      note.setText("Totals follow");

      assertEquals("""
          - Div "Totals follow"
          """, Snapshot.render(note, new SnapshotOptions().setBrowserState(true)));
    }
  }

  @Nested
  class HiddenComponents {

    @Test
    void shouldLeaveOutTheHiddenComponentAndEverythingBelowIt() {
      FlexLayout page = createPage();

      assertEquals("""
          - FlexLayout:
            - Button "Save"
          """, Snapshot.render(page));
    }

    @Test
    void shouldDescribeTheHiddenComponentWithItsFlagWhenAsked() {
      FlexLayout page = createPage();

      assertEquals("""
          - FlexLayout:
            - Button "Save"
            - FlexLayout [hidden]:
              - Button "Edit"
          """, Snapshot.render(page, new SnapshotOptions().setHiddenComponents(true)));
    }

    @Test
    void shouldGiveAnEmptyDescriptionForTheHiddenRoot() {
      Button save = new Button("Save");
      save.setVisible(false);

      assertEquals("", Snapshot.render(save));
    }

    @Test
    void shouldLeaveOutTheHiddenPrefixAndSuffix() {
      TextField amount = createAmount();

      assertEquals("- TextField\n", Snapshot.render(amount));
    }

    @Test
    void shouldDescribeTheHiddenPrefixAndSuffixWhenAsked() {
      TextField amount = createAmount();

      assertEquals("""
          - TextField:
            - Label "$" [slot=prefix] [hidden]
            - Label "EUR" [slot=suffix] [hidden]
          """, Snapshot.render(amount, new SnapshotOptions().setHiddenComponents(true)));
    }

    private static FlexLayout createPage() {
      FlexLayout details = new FlexLayout(new Button("Edit"));
      details.setVisible(false);

      return new FlexLayout(new Button("Save"), details);
    }

    private static TextField createAmount() {
      Label currency = new Label("$");
      currency.setVisible(false);
      Label code = new Label("EUR");
      code.setVisible(false);
      TextField amount = new TextField();
      amount.setPrefixComponent(currency);
      amount.setSuffixComponent(code);

      return amount;
    }
  }

  @Nested
  class ApplicationSerializers {

    @Test
    void shouldDescribeTheServerStateOfAnApplicationComponentByDefault() {
      Gauge gauge = createGauge();

      assertEquals("""
          - Gauge [label=Pressure] [zone=green] [level=3]:
            - Scale "bar"
            - Mark "low"
            - Mark "high"
          """, Snapshot.render(gauge));
    }

    @Test
    void shouldDescribeTheBrowserStateOfAnApplicationComponentWhenAsked() {
      Gauge gauge = createGauge();

      assertEquals("""
          - Gauge [label=Pressure] [zone=green] [level=3] [needle=7]:
            - Scale "bar"
            - Mark "low"
            - Mark "high"
          """, Snapshot.render(gauge, new SnapshotOptions().setBrowserState(true)));
    }

    private static Gauge createGauge() {
      return new Gauge("bar", "low", "high").setLabel("Pressure").setZone("green").setLevel(3)
          .setNeedle(7);
    }
  }
}
