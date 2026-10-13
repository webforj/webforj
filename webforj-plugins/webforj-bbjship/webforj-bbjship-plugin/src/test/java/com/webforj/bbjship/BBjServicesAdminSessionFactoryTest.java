package com.webforj.bbjship;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;

import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.BBjAdminFactory;
import com.webforj.bbjship.foundation.model.BBjCredentials;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class BBjServicesAdminSessionFactoryTest {

  @Test
  void shouldOpenTheSessionWithTheUserNameAndPassword() throws Exception {
    BBjAdminBase admin = mock(BBjAdminBase.class);
    try (MockedStatic<BBjAdminFactory> factory = mockStatic(BBjAdminFactory.class)) {
      factory.when(() -> BBjAdminFactory.getBBjAdmin("administrator", "secret")).thenReturn(admin);

      BBjAdminBase opened = new BBjServicesAdminSessionFactory()
          .create(BBjCredentials.createUser("administrator", "secret"));

      assertSame(admin, opened);
    }
  }

  @Test
  void shouldOpenTheSessionWithTheAuthorizationToken() throws Exception {
    BBjAdminBase admin = mock(BBjAdminBase.class);
    try (MockedStatic<BBjAdminFactory> factory = mockStatic(BBjAdminFactory.class)) {
      factory.when(() -> BBjAdminFactory.getBBjAdmin("c2VjcmV0")).thenReturn(admin);

      BBjAdminBase opened =
          new BBjServicesAdminSessionFactory().create(BBjCredentials.createToken("c2VjcmV0"));

      assertSame(admin, opened);
    }
  }
}
