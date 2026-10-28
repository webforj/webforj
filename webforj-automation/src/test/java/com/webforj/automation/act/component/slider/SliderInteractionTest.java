package com.webforj.automation.act.component.slider;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mockStatic;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.automation.FrameEnvironment;
import com.webforj.automation.act.UserInteraction;
import com.webforj.automation.act.exception.InteractionRejectedException;
import com.webforj.component.slider.Slider;
import com.webforj.component.window.Frame;
import java.util.List;
import java.util.function.Consumer;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class SliderInteractionTest {

  @Test
  void shouldSlideWithinTheRangeOfTheSlider() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Slider slider = new Slider(10, 0, 50);
      frame.add(slider);

      UserInteraction.perform(slider, SliderInteraction.slide(42));

      assertEquals(42, slider.getValue());
      Consumer<Slider> slideOutOfRange = SliderInteraction.slide(51);
      assertTrue(assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.perform(slider, slideOutOfRange)).getMessage()
          .contains("from 0 to 50"));
    }
  }
}
