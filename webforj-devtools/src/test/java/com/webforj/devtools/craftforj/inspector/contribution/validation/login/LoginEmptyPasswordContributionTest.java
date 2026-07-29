package com.webforj.devtools.craftforj.inspector.contribution.validation.login;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.login.Login;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class LoginEmptyPasswordContributionTest {

  private final LoginEmptyPasswordContribution contribution = new LoginEmptyPasswordContribution();

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldGet(boolean value) {
    Login login = mock(Login.class);
    when(login.isEmptyPassword()).thenReturn(value);

    var result = contribution.get(login);

    assertTrue(result.isPresent());
    assertEquals("EmptyPassword", result.get().getName());
    assertEquals(PropertyType.BOOLEAN, result.get().getEditorType());
    assertEquals(value, result.get().getValue());
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldSet(boolean value) {
    Login login = mock(Login.class);

    boolean success = contribution.set(login, value);

    assertTrue(success);
    verify(login).setEmptyPassword(value);
  }
}
