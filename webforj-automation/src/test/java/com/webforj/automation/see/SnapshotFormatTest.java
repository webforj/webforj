package com.webforj.automation.see;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.webforj.component.button.Button;
import com.webforj.component.field.NumberField;
import com.webforj.component.field.TextField;
import com.webforj.component.googlecharts.GoogleChart;
import com.webforj.component.layout.flexlayout.FlexLayout;
import com.webforj.component.optioninput.CheckBox;
import com.webforj.component.text.Label;
import java.util.List;
import org.junit.jupiter.api.Test;

class SnapshotFormatTest {

  @Test
  void shouldPrintTheValuesBeforeTheFlags() {
    NumberField amount = new NumberField();
    amount.setRequired(true);
    amount.setReadOnly(true);
    amount.setEnabled(false);
    amount.setVisible(false);
    amount.setTooltipText("Total");
    amount.setHelperText("In euro");
    amount.setStep(0.5);
    amount.setMax(100.0);
    amount.setMin(0.0);
    amount.setPlaceholder("0");
    amount.setLabel("Amount");
    amount.setName("amount");
    SnapshotOptions options = new SnapshotOptions().setBrowserState(true).setHiddenComponents(true);

    assertEquals("""
        - NumberField [name=amount] [label=Amount] [placeholder=0] [min=0.0] [max=100.0] \
        [step=0.5] [helperText="In euro"] [tooltip=Total] [hidden] [disabled] [readonly] \
        [required]
        """, SnapshotFormat.renderTree(amount, options));
  }

  @Test
  void shouldPrintTheApplicationKeysInTheOrderTheyWereWritten() {
    Gauge gauge = new Gauge("bar").setLabel("Pressure").setZone("red").setLevel(9);
    gauge.setName("boiler");

    assertEquals("""
        - Gauge [name=boiler] [label=Pressure] [zone=red] [level=9]:
          - Scale "bar"
        """, SnapshotFormat.renderTree(gauge, new SnapshotOptions()));
  }

  @Test
  void shouldQuoteTheLineWhenTheTextReadsAsStructure() {
    FlexLayout totals = new FlexLayout(new Label("Total: 12"), new Label("Item #1"),
        new Label("Say \"hi\""), new Button("Next:"));

    assertEquals("""
        - FlexLayout:
          - "Label \\"Total: 12\\""
          - "Label \\"Item #1\\""
          - Label "Say \\"hi\\""
          - Button "Next:"
        """, SnapshotFormat.renderTree(totals, new SnapshotOptions()));
  }

  @Test
  void shouldQuoteAnAttributeValueThatIsNotBare() {
    Button save = new Button("Save");
    save.setName("save [now]");

    assertEquals("- Button \"Save\" [name=\"save [now]\"]\n",
        SnapshotFormat.renderTree(save, new SnapshotOptions()));
  }

  @Test
  void shouldQuoteAnAttributeKeyThatIsNotBare() {
    GoogleChart chart = new GoogleChart(GoogleChart.Type.PIE);
    chart.setData(List.of(List.of("Task", "Hours per Day"), List.of("Work", 11)));

    assertEquals("""
        - GoogleChart [type=pie]:
          - Row [Task=Work] ["Hours per Day"=11]
        """, SnapshotFormat.renderTree(chart, new SnapshotOptions()));
  }

  @Test
  void shouldCountTheItemsBeyondTheWidthWhenTheSerializerStatesTheirNumber() {
    Gauge gauge = new Gauge("bar", "low", "mid", "high").setMarkCount(3);

    assertEquals("""
        - Gauge [level=0]:
          - Scale "bar"
          - Mark "low"
          - … 2 more
        """, SnapshotFormat.renderTree(gauge, new SnapshotOptions().setMaxWidth(1)));
  }

  @Test
  void shouldRefuseTheCountBelowTheItemsTheSerializerAdded() {
    Gauge gauge = new Gauge("bar", "low", "mid").setMarkCount(1);
    SnapshotOptions options = new SnapshotOptions().setMaxWidth(1);

    IllegalStateException failure =
        assertThrows(IllegalStateException.class, () -> SnapshotFormat.renderTree(gauge, options));

    assertEquals("The snapshot of Gauge holds 2 items but states a count of 1.",
        failure.getMessage());
  }

  @Test
  void shouldElideTheItemsBeyondTheWidthAndKeepTheStructure() {
    FlexLayout panel = new FlexLayout(new Gauge("bar", "low", "mid", "high", "max"));

    assertEquals("""
        - FlexLayout:
          - Gauge [level=0]:
            - Scale "bar"
            - Mark "low"
            - Mark "mid"
            - …
        """, SnapshotFormat.renderTree(panel, new SnapshotOptions().setMaxWidth(2)));
  }

  @Test
  void shouldIndentEachLevelOfNesting() {
    FlexLayout page =
        new FlexLayout(new FlexLayout(new FlexLayout(new Button("Deep"))), new Button("Top"));

    assertEquals("""
        - FlexLayout:
          - FlexLayout:
            - FlexLayout:
              - Button "Deep"
          - Button "Top"
        """, SnapshotFormat.renderTree(page, new SnapshotOptions()));
  }

  @Test
  void shouldLeaveOutTheNameLabelAndValueThatRepeatTheText() {
    Button save = new Button("Save");
    save.setName("Save");
    TextField customer = new TextField();
    customer.setValue("Ada");
    FlexLayout form = new FlexLayout(save, new CheckBox("Gift wrap"), customer);

    assertEquals("""
        - FlexLayout:
          - Button "Save"
          - CheckBox "Gift wrap"
          - TextField "Ada"
        """, SnapshotFormat.renderTree(form, new SnapshotOptions().setBrowserState(true)));
  }

  @Test
  void shouldPrintTheNameLabelAndValueThatDifferFromTheText() {
    Button save = new Button("Save");
    save.setName("save");
    TextField customer = new TextField();
    customer.setLabel("Customer");
    customer.setValue("Ada");
    CheckBox gift = new CheckBox("Gift wrap");
    gift.setChecked(true);
    FlexLayout form = new FlexLayout(save, customer, gift);

    assertEquals("""
        - FlexLayout:
          - Button "Save" [name=save]
          - TextField "Ada" [label=Customer]
          - CheckBox "Gift wrap" [value=true]
        """, SnapshotFormat.renderTree(form, new SnapshotOptions().setBrowserState(true)));
  }
}
