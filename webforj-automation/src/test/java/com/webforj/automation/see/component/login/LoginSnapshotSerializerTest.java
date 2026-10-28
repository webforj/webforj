package com.webforj.automation.see.component.login;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.automation.see.Snapshot;
import com.webforj.automation.see.SnapshotOptions;
import com.webforj.component.login.Login;
import org.junit.jupiter.api.Test;

class LoginSnapshotSerializerTest {

  @Test
  void shouldDescribeTheOpenedLoginWithoutItsStateByDefault() {
    Login login = new Login();
    login.setEnabled(true);
    login.open();

    assertEquals("- Login\n", Snapshot.render(login));
  }

  @Test
  void shouldFlagTheOpenedLoginWithBrowserState() {
    Login login = new Login();
    login.setEnabled(true);
    login.open();

    assertEquals("- Login [opened]\n",
        Snapshot.render(login, new SnapshotOptions().setBrowserState(true)));
  }

  @Test
  void shouldLeaveTheClosedLoginUnflaggedWithBrowserState() {
    Login login = new Login();
    login.setEnabled(true);
    login.close();

    assertEquals("- Login\n", Snapshot.render(login, new SnapshotOptions().setBrowserState(true)));
  }
}
