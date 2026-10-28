package com.webforj.testbed;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.Environment;
import com.webforj.component.button.Button;
import com.webforj.component.field.TextField;
import com.webforj.component.window.Frame;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class WebforjTestbedTest {

  @AfterEach
  void tearDown() {
    WebforjTestbed.tearDown();
  }

  @Test
  void shouldSetUpAndTearDownEnvironment() {
    WebforjTestbed context = WebforjTestbed.setup();

    assertTrue(WebforjTestbed.isPresent());
    assertTrue(Environment.isPresent());
    assertSame(context, WebforjTestbed.getCurrent());

    WebforjTestbed.tearDown();

    assertFalse(WebforjTestbed.isPresent());
    assertFalse(Environment.isPresent());
  }

  @Test
  void shouldRejectDoubleSetup() {
    WebforjTestbed.setup();

    assertThrows(IllegalStateException.class, WebforjTestbed::setup);
  }

  @Test
  void shouldFailFastWhenNoEnvironmentIsPresent() {
    assertThrows(IllegalStateException.class, WebforjTestbed::getCurrent);
  }

  @Test
  void shouldTolerateTearDownWithoutSetup() {
    WebforjTestbed.tearDown();

    assertFalse(WebforjTestbed.isPresent());
  }

  @Nested
  class ComponentModeTest {

    @Test
    void shouldCreateRootFrameOnFirstAccess() {
      WebforjTestbed context = WebforjTestbed.setup();

      Frame frame = context.getFrame();

      assertNotNull(frame);
      assertSame(frame, context.getFrame());
    }

    @Test
    void shouldAttachComponentsThroughFullLifecycle() {
      WebforjTestbed context = WebforjTestbed.setup();

      Button button = new Button("Save");
      TextField field = new TextField();
      context.add(button, field);

      assertTrue(button.isAttached());
      assertTrue(field.isAttached());
      assertEquals("Save", button.getText());
    }

    @Test
    void shouldKeepStateWrittenBeforeAndAfterAttach() {
      WebforjTestbed context = WebforjTestbed.setup();

      Button button = new Button();
      button.setText("Before");
      button.setEnabled(false);

      context.add(button);

      assertEquals("Before", button.getText());
      assertFalse(button.isEnabled());

      button.setText("After");

      assertEquals("After", button.getText());
    }

    @Test
    void shouldRunWithoutAppAndRouter() {
      WebforjTestbed context = WebforjTestbed.setup();

      assertTrue(context.getApp().isEmpty());
      assertTrue(context.getRouter().isEmpty());
    }

    @Test
    void shouldIsolateStateBetweenEnvironments() {
      WebforjTestbed first = WebforjTestbed.setup();
      Frame firstFrame = first.getFrame();
      WebforjTestbed.tearDown();

      WebforjTestbed second = WebforjTestbed.setup();

      assertNotSame(firstFrame, second.getFrame());
    }
  }
}
