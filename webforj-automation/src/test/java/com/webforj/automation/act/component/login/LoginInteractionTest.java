package com.webforj.automation.act.component.login;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mockStatic;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.automation.FrameEnvironment;
import com.webforj.automation.act.UserInteraction;
import com.webforj.automation.act.exception.InteractionRejectedException;
import com.webforj.component.login.Login;
import com.webforj.component.login.LoginI18n;
import com.webforj.component.window.Frame;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class LoginInteractionTest {

  @Test
  void shouldDisableTheLoginThatStaysOpenBeforeItReportsTheSubmission() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Login login = new Login();
      List<String> submitted = new ArrayList<>();
      login.onSubmit(event -> {
        submitted.add(event.getUsername() + ":" + event.getPassword() + ":" + event.isRememberMe()
            + ":" + event.getData().get("customer") + ":" + login.isEnabled());
        login.setError(true).setEnabled(true);
      });
      frame.add(login);
      login.open();

      UserInteraction.perform(login, LoginInteraction
          .submit(Map.of("username", "ada", "password", "secret", "customer", "42")));

      assertEquals(List.of("ada:secret:false:42:false"), submitted);
      assertEquals("ada", login.getUsername());
      assertEquals("secret", login.getPassword());
      assertFalse(login.isRememberme());
      assertTrue(login.isEnabled());
      assertTrue(login.isOpened());
    }
  }

  @Test
  void shouldCloseTheLoginThatClosesOnItsOwn() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Login login = new Login().setAutoClose(true);
      LoginI18n i18n = new LoginI18n();
      i18n.setCancel("Cancel");
      login.setI18n(i18n);
      List<Boolean> opened = new ArrayList<>();
      login.onCancel(event -> opened.add(login.isOpened()));
      login.onSubmit(event -> opened.add(login.isOpened()));
      frame.add(login);
      login.open();

      UserInteraction.cancel(login);
      login.open();
      UserInteraction.perform(login, LoginInteraction.submit(Map.of("rememberMe", true)));

      assertEquals(List.of(false, false), opened);
      assertFalse(login.isOpened());
    }
  }

  @Test
  void shouldRejectCancellingTheLoginWithoutCancelButton() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Login login = new Login();
      List<String> cancelled = new ArrayList<>();
      login.onCancel(event -> cancelled.add("cancel"));
      frame.add(login);
      login.open();

      assertTrue(
          assertThrows(InteractionRejectedException.class, () -> UserInteraction.cancel(login))
              .getMessage().contains("no cancel button"));
      assertEquals(List.of(), cancelled);
      assertTrue(login.isOpened());
    }
  }
}
