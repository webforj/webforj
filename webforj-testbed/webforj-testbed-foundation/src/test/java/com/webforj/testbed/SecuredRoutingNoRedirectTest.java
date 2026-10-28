package com.webforj.testbed;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.testbed.fixture.security.SecureVaultView;
import com.webforj.testbed.fixture.security.SecuredStrictApp;
import org.junit.jupiter.api.Test;

class SecuredRoutingNoRedirectTest extends TestbedTestSupport {

  @Override
  protected WebforjTestbedOptions options() {
    return new WebforjTestbedOptions().setApp(SecuredStrictApp.class);
  }

  @Test
  void shouldFailLoudWhenDeniedWithoutRedirectLocation() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    IllegalStateException failure =
        assertThrows(IllegalStateException.class, () -> context.navigate(SecureVaultView.class));

    assertTrue(failure.getMessage().contains("SecureVaultView"));
    assertTrue(failure.getMessage().contains("no authentication or deny location"));
  }
}
