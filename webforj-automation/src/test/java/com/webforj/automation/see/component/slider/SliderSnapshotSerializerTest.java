package com.webforj.automation.see.component.slider;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.automation.see.Snapshot;
import com.webforj.automation.see.SnapshotOptions;
import com.webforj.component.slider.Slider;
import org.junit.jupiter.api.Test;

class SliderSnapshotSerializerTest {

  @Test
  void shouldFlagTheUprightSliderByDefault() {
    Slider slider = createUprightSlider();

    assertEquals("- Slider [min=0] [max=100] [vertical]\n", Snapshot.render(slider));
  }

  @Test
  void shouldFlagTheUprightSliderWithBrowserState() {
    Slider slider = createUprightSlider();

    assertEquals("- Slider [value=30] [min=0] [max=100] [vertical]\n",
        Snapshot.render(slider, new SnapshotOptions().setBrowserState(true)));
  }

  @Test
  void shouldLeaveTheLevelSliderUnflagged() {
    Slider slider = new Slider();

    assertEquals("- Slider [min=0] [max=100]\n", Snapshot.render(slider));
  }

  @Test
  void shouldPrintTheTextByDefault() {
    Slider volume = new Slider();
    volume.setText("Volume");

    assertEquals("- Slider \"Volume\" [min=0] [max=100]\n", Snapshot.render(volume));
  }

  private static Slider createUprightSlider() {
    Slider slider = new Slider(30);
    slider.setOrientation(Slider.Orientation.VERTICAL);

    return slider;
  }
}
