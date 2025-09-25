package com.webforj.spring.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;

import com.webforj.Page;
import com.webforj.spring.security.SpringSecurityFormSubmitter.FormType;
import com.webforj.spring.security.SpringSecurityFormSubmitter.LoginFormBuilder;
import com.webforj.spring.security.SpringSecurityFormSubmitter.LogoutFormBuilder;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class SpringSecurityFormSubmitterTest {

  @Mock
  private Page page;

  @Nested
  class BuilderCreation {

    @Test
    void shouldCreateLoginFormBuilderWithCorrectAction() {
      LoginFormBuilder builder = SpringSecurityFormSubmitter.login("/login");
      assertEquals("/login", builder.getAction());
      assertEquals("POST", builder.getMethod());
    }

    @Test
    void shouldCreateLogoutFormBuilderWithCorrectAction() {
      LogoutFormBuilder builder = SpringSecurityFormSubmitter.logout("/logout");
      assertEquals("/logout", builder.getAction());
      assertEquals("POST", builder.getMethod());
    }

    @Test
    void shouldCreateLoginBuilderWithGenericMethod() {
      LoginFormBuilder builder =
          SpringSecurityFormSubmitter.builder(FormType.LOGIN, "/authenticate");
      assertEquals("/authenticate", builder.getAction());
    }

    @Test
    void shouldCreateLogoutBuilderWithGenericMethod() {
      LogoutFormBuilder builder = SpringSecurityFormSubmitter.builder(FormType.LOGOUT, "/signout");
      assertEquals("/signout", builder.getAction());
    }
  }

  @Nested
  class LoginFormBuilderTests {

    private LoginFormBuilder builder;

    @BeforeEach
    void setUp() {
      builder = SpringSecurityFormSubmitter.login("/login");
    }

    @Test
    void shouldSetUsername() {
      builder.username("testuser");
      assertEquals("testuser", builder.getUsername());
      assertTrue(builder.hasField("username"));
    }

    @Test
    void shouldSetPassword() {
      builder.password("secretpass");
      assertEquals("secretpass", builder.getPassword());
      assertTrue(builder.hasField("password"));
    }

    @Test
    void shouldSetRememberMe() {
      builder.rememberMe(true);
      assertTrue(builder.isRememberMe());
      assertEquals("on", builder.getField("remember-me"));
    }

    @Test
    void shouldRemoveRememberMeWhenFalse() {
      builder.rememberMe(true);
      assertTrue(builder.isRememberMe());

      builder.rememberMe(false);
      assertFalse(builder.isRememberMe());
      assertFalse(builder.hasField("remember-me"));
    }

    @Test
    void shouldSubmitLoginForm() {
      try (MockedStatic<Page> pageMock = mockStatic(Page.class)) {
        pageMock.when(Page::getCurrent).thenReturn(page);

        builder.username("testuser").password("testpass").rememberMe(true).submit();

        ArgumentCaptor<String> jsCaptor = ArgumentCaptor.forClass(String.class);
        verify(page).executeJsVoidAsync(jsCaptor.capture());

        String js = jsCaptor.getValue();
        assertTrue(js.contains("\"action\":\"/login\""));
        assertTrue(js.contains("\"username\":\"testuser\""));
        assertTrue(js.contains("\"password\":\"testpass\""));
        assertTrue(js.contains("\"remember-me\":\"on\""));
      }
    }

    @Test
    void shouldAddCustomField() {
      builder.addField("csrf", "token123");
      assertEquals("token123", builder.getField("csrf"));
      assertTrue(builder.hasField("csrf"));
    }

    @Test
    void shouldSubmitWithCustomField() {
      try (MockedStatic<Page> pageMock = mockStatic(Page.class)) {
        pageMock.when(Page::getCurrent).thenReturn(page);

        builder.username("user").password("pass").addField("csrf", "token123").submit();

        ArgumentCaptor<String> jsCaptor = ArgumentCaptor.forClass(String.class);
        verify(page).executeJsVoidAsync(jsCaptor.capture());

        String js = jsCaptor.getValue();
        assertTrue(js.contains("\"csrf\":\"token123\""));
      }
    }

    @Test
    void shouldRemoveField() {
      builder.username("user").password("pass");
      assertTrue(builder.hasField("password"));

      builder.removeField("password");
      assertEquals("user", builder.getUsername());
      assertNull(builder.getPassword());
      assertFalse(builder.hasField("password"));
    }

    @Test
    void shouldClearAllFields() {
      builder.username("user").password("pass").rememberMe(true);
      assertTrue(builder.hasField("username"));
      assertTrue(builder.hasField("password"));
      assertTrue(builder.isRememberMe());

      builder.clearFields();
      assertNull(builder.getUsername());
      assertNull(builder.getPassword());
      assertFalse(builder.isRememberMe());
      assertTrue(builder.getFields().isEmpty());
    }
  }

  @Nested
  class LogoutFormBuilderTests {

    private LogoutFormBuilder builder;

    @BeforeEach
    void setUp() {
      builder = SpringSecurityFormSubmitter.logout("/logout");
    }

    @Test
    void shouldHaveCorrectDefaults() {
      assertEquals("/logout", builder.getAction());
      assertEquals("POST", builder.getMethod());
      assertTrue(builder.getFields().isEmpty());
    }

    @Test
    void shouldSubmitLogoutForm() {
      try (MockedStatic<Page> pageMock = mockStatic(Page.class)) {
        pageMock.when(Page::getCurrent).thenReturn(page);

        builder.submit();

        ArgumentCaptor<String> jsCaptor = ArgumentCaptor.forClass(String.class);
        verify(page).executeJsVoidAsync(jsCaptor.capture());

        String js = jsCaptor.getValue();
        assertTrue(js.contains("\"action\":\"/logout\""));
        assertTrue(js.contains("\"method\":\"POST\""));
      }
    }

    @Test
    void shouldSupportCustomFields() {
      builder.addField("csrf", "token").addField("redirect", "/home");

      assertEquals("token", builder.getField("csrf"));
      assertEquals("/home", builder.getField("redirect"));
      assertTrue(builder.hasField("csrf"));
      assertTrue(builder.hasField("redirect"));
    }

    @Test
    void shouldSubmitWithCustomFields() {
      try (MockedStatic<Page> pageMock = mockStatic(Page.class)) {
        pageMock.when(Page::getCurrent).thenReturn(page);

        builder.addField("csrf", "token").addField("redirect", "/home").submit();

        ArgumentCaptor<String> jsCaptor = ArgumentCaptor.forClass(String.class);
        verify(page).executeJsVoidAsync(jsCaptor.capture());

        String js = jsCaptor.getValue();
        assertTrue(js.contains("\"csrf\":\"token\""));
        assertTrue(js.contains("\"redirect\":\"/home\""));
      }
    }
  }
}
