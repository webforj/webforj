package com.webforj.automation.act.concern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mockStatic;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.automation.FrameEnvironment;
import com.webforj.automation.act.CustomBasket;
import com.webforj.automation.act.CustomPicker;
import com.webforj.automation.act.CustomValue;
import com.webforj.automation.act.UserInteraction;
import com.webforj.automation.act.exception.InteractionRejectedException;
import com.webforj.component.window.Frame;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class SelectableInteractionTest {

  @Test
  void shouldSelectInTheComponentThatBringsTheSelectionContracts() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      CustomPicker picker = new CustomPicker("small", "medium", "large");
      frame.add(picker);

      UserInteraction.select(picker, "medium");
      assertEquals("medium", picker.getSelected());

      UserInteraction.selectIndex(picker, 2);
      assertEquals("large", picker.getSelected());

      UserInteraction.selectKey(picker, "small");
      assertEquals("small", picker.getSelected());
    }
  }

  @Test
  void shouldRejectSelectingWhatTheComponentDoesNotHold() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      CustomPicker picker = new CustomPicker("small");
      CustomValue<String> value = new CustomValue<>("");
      frame.add(picker, value);

      assertTrue(assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.select(picker, "large")).getMessage().contains("large"));
      assertTrue(assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.selectKey(picker, "large")).getMessage().contains("large"));
      assertTrue(assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.selectIndex(picker, 4)).getMessage().contains("4"));
      assertTrue(
          assertThrows(InteractionRejectedException.class, () -> UserInteraction.select(picker, 7))
              .getMessage().contains("Integer"));
      assertTrue(assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.deselect(picker, "small")).getMessage()
          .contains("does not deselect items"));
      assertTrue(
          assertThrows(InteractionRejectedException.class, () -> UserInteraction.select(value, "x"))
              .getMessage().contains("does not select items"));
      assertNull(picker.getSelected());
    }
  }

  @Test
  void shouldSelectAndDeselectInTheComponentThatSelectsSeveralItems() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      CustomBasket basket = new CustomBasket("apple", "pear");
      frame.add(basket);

      UserInteraction.select(basket, "apple");
      UserInteraction.select(basket, "pear");
      UserInteraction.deselect(basket, "apple");
      UserInteraction.deselect(basket, "apple");

      assertEquals(List.of("pear"), basket.getSelectedItems());
      assertTrue(assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.select(basket, "plum")).getMessage().contains("plum"));
      assertTrue(assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.selectIndex(basket, 0)).getMessage()
          .contains("does not select by index"));
      assertTrue(assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.selectKey(basket, "pear")).getMessage()
          .contains("does not select by key"));
    }
  }
}
