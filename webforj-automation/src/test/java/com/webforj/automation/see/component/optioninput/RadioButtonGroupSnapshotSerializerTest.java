package com.webforj.automation.see.component.optioninput;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.automation.see.Snapshot;
import com.webforj.automation.see.SnapshotOptions;
import com.webforj.component.layout.flexlayout.FlexLayout;
import com.webforj.component.optioninput.RadioButton;
import com.webforj.component.optioninput.RadioButtonGroup;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class RadioButtonGroupSnapshotSerializerTest {

  @ParameterizedTest
  @ValueSource(booleans = {false, true})
  void shouldAddTheButtonsBelowTheGroupAtBothLevels(boolean browserState) {
    RadioButtonGroup group =
        new RadioButtonGroup(new RadioButton("Large"), new RadioButton("Small"));

    assertEquals("""
        - RadioButtonGroup:
          - RadioButton "Large"
          - RadioButton "Small"
        """, Snapshot.render(group, new SnapshotOptions().setBrowserState(browserState)));
  }

  @ParameterizedTest
  @ValueSource(booleans = {false, true})
  void shouldGatherTheButtonsOfTheContainerUnderTheGroupAtBothLevels(boolean browserState) {
    RadioButton large = new RadioButton("Large");
    RadioButton small = new RadioButton("Small");
    RadioButtonGroup group = new RadioButtonGroup(large, small);
    FlexLayout form = new FlexLayout(group, large, small);

    assertEquals("""
        - FlexLayout:
          - RadioButtonGroup:
            - RadioButton "Large"
            - RadioButton "Small"
        """, Snapshot.render(form, new SnapshotOptions().setBrowserState(browserState)));
  }
}
