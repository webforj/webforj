package com.webforj.testbed.snapshot;

import static com.webforj.testbed.assertion.WebforjAssertions.verifyThat;

import com.webforj.component.button.Button;
import com.webforj.component.field.PasswordField;
import com.webforj.component.field.TextField;
import com.webforj.component.layout.flexlayout.FlexLayout;
import com.webforj.component.text.Label;
import com.webforj.testbed.TestbedTestSupport;
import com.webforj.testbed.WebforjTestbed;
import org.junit.jupiter.api.Test;

class ComposedSnapshotTest extends TestbedTestSupport {

  @Test
  void shouldMatchTheLoginFormSnapshot() {
    FlexLayout form = new FlexLayout();
    form.setName("login-form");

    TextField user = new TextField();
    user.setName("username");
    user.setText("Ada");

    PasswordField password = new PasswordField();
    password.setName("password");

    FlexLayout actions = new FlexLayout();
    actions.add(new Button("Sign in"), new Button("Cancel"));

    form.add(new Label("Welcome back"), user, password, actions);
    WebforjTestbed.getCurrent().add(form);

    verifyThat(form).matchesSnapshot();
  }
}
